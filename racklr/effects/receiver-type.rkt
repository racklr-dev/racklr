#lang racket

;; racklr/effects/receiver-type.rkt
;;
;; Per-function receiver-type environment: tracks the class of local variables
;; and `self`/`cls` attributes from constructor assignments
;; (`x = KnownClass(...)`), simple aliases (`x = y`, `x = self.attr`), and bare
;; class references (`x = KnownClass`), so a later `x.m(...)` call can resolve
;; through the receiver class's derived effect capability (or its in-tree method
;; body, for a class in an untagged module) instead of the flat method-name
;; tables. This is the first half of receiver-type dataflow: the class name is
;; inferred from the call chain (a constructor whose callee names a known class,
;; or a bare class reference), not from a name table.

(require racklr/uir
         racklr/effects/shape
         racklr/effects/walk
         racklr/effects/class-resolve
         racklr/effects/imports
         racklr/effects/library-tokens
         racklr/effects/effect-roots
         racklr/effects/receiver-assign
         racklr/effects/receiver-element
         racklr/effects/type-projection
         racklr/effects/lexicon)

(provide
  receiver-types? receiver-types receiver-types-locals receiver-types-self-attrs
  receiver-types-elem-specs receiver-types-globals
  receiver-types-fn-locals receiver-types-fn-self-attrs
  receiver-types-merge
  infer-receiver-types
  receiver-class-of
  constructor-class
  imported-fn-result-type
  call-result-type
  value-class-of
  return-class-of
  corpus-projected-value-class
  dynamic-class-marker dynamic-class-marker?
  class-union-marker class-union-marker?
  bare-class-value)

(struct receiver-types (locals self-attrs elem-specs globals fn-locals fn-self-attrs)
  #:transparent)
;; locals:     hash name-symbol → class-name-symbol
;; self-attrs: hash attr-symbol → class-name-symbol (on self / cls)
;; globals:    hash name-symbol → class-name-symbol (module-level names, shared
;;             by every function in the component; the free-name fallback below)
;; elem-specs: hash name-symbol → element-spec: a class symbol (the element
;;             class of an iterable the name is bound to) or a list of class
;;             symbols (a tuple the loop destructures, e.g. os.walk yields
;;             (str list list)).
;; fn-locals:  hash name-symbol → (cons receiver-class-or-#f method-name): a
;;             local bound to a bound-method ALIAS (`x = obj.m` / `x =
;;             getattr(obj, "m", default)`); calling it bare resolves as
;;             obj.m(...).
;; fn-self-attrs: hash attr-symbol → the same descriptor for a self/cls
;;             attribute bound to a bound-method alias (`self._read =
;;             stream.read`); `self._read(...)` resolves as stream.read(...).

(define (self-or-cls-var? v [tokens default-effect-tokens])
  (and (or (uir-var? v) (uir-symbol? v))
       (tokens-self-name? tokens (bare-name-symbol v))))

;; A pure stdlib/builtin value constructor name (`Path`, `md5`, `dict`): pure to
;; construct, and therefore also a usable receiver type for a call result.
;; The pure-bare-NAMES table lists BOTH value-constructor types (`Path`, `md5`,
;; `Decimal`, `BytesIO`) and pure FUNCTIONS (`get_resolver`, `get_runner`,
;; `force_str`). A function's result is a DIFFERENT class, so its own name must
;; never become the receiver type (`r = get_resolver(...)` then `r.resolve`
;; would look up "get_resolver".resolve and stay unknown). A name is a
;; constructor TYPE only when it is pure AND names a receiver type: it has a
;; method table (stdlib-type-methods-data) or is a builtin type root. An
;; effectful receiver name (`fetch`, `open`) is absent from the pure table, so
;; it still classifies by its effect domain, not as a pure constructor.
(define (pure-stdlib-type? n [tokens default-effect-tokens])
  (and n
       (let ([s (symbol->string n)])
         (and (set-member? (effect-tokens-pure-stdlib-bare-names tokens) s)
              (or (set-member? (effect-tokens-builtin-type-roots tokens) s)
                  (hash-has-key? (effect-tokens-stdlib-type-methods tokens) s))))))

;; A receiver value is a MODULE (not a class) when its name names an external
;; module in the receiver-modules token table (pylibmc / pymemcache / memcache /
;; redis). Module receivers resolve their method calls to the module's effect
;; domain in classify-all, keeping engine code free of library names.
(define (module-receiver? nm [tokens default-effect-tokens])
  (and nm (hash-has-key? (effect-tokens-receiver-modules tokens)
                         (symbol->string nm))))

;; Class name (symbol) tracked for a receiver expression `base`, or #f. `base`
;; is either a local variable (`qs`), a `self`/`cls` attribute (`self.cursor`),
;; or a bare class reference. Moved above `call-result-type` because
;; `call-result-type` consults it when a call's callee is a class-valued
;; receiver expression (`self.build_file_class(...)`).
(define (receiver-class-of env base [tokens default-effect-tokens])
  (cond
    [(not env) #f]
    [(uir-var? base)
     (define nm (string->symbol (uir-symbol-name (uir-var-name base))))
     (or (hash-ref (receiver-types-locals env) nm #f)
         (hash-ref (receiver-types-globals env) nm #f))]
    [(uir-symbol? base)
     (define nm (string->symbol (uir-symbol-name base)))
     (or (hash-ref (receiver-types-locals env) nm #f)
         (hash-ref (receiver-types-globals env) nm #f))]
    [(member-access-callee? base)
     (define attr (member-access-field base))
     (and attr (self-or-cls-var? (member-access-base base) tokens)
          (hash-ref (receiver-types-self-attrs env) attr #f))]
    [else #f]))

;; A bare class reference used as a VALUE: `x = QuerySet` (not a call). The
;; referenced name is a known corpus class or a pure stdlib/builtin value type;
;; anything else returns #f (an arbitrary name is not a tracked class).
(define (bare-class-value value class-defs [tokens default-effect-tokens]
                           [import-env #f] [comp #f])
  (define nm
    (cond
      [(uir-var? value) (string->symbol (uir-symbol-name (uir-var-name value)))]
      [(uir-symbol? value) (string->symbol (uir-symbol-name value))]
      [(uir-get? value) (get-field-symbol (uir-get-field value))]
      [else #f]))
  (and nm
       (or (hash-has-key? class-defs nm)
           (pure-stdlib-type? nm tokens)
           (imported-class-ref nm comp import-env)
           (module-receiver? nm tokens)
           ;; A bare exception class name: the host's exception-suffix
           ;; convention (PascalCase Error/Exception/Warning) or its pure
           ;; constructor-name table. Constructing an exception is pure — the
           ;; raise is the effect — so `x = Warning` then `x(...)` classifies
           ;; as pure construction, exactly as a tracked stdlib type would.
           (exception-class-name? nm tokens))
       nm))

;; Is `nm` an exception CLASS name (not an effectful function)? The suffix
;; heuristic is the host dialect's own convention (kept case-sensitive: a
;; lowercase `compute_error` must not match), and it already gates
;; `super().__init__` on exception bases in receiver-resolve.rkt's
;; exception-name?. Duplicated here because receiver-resolve requires this
;; module (no reverse dependency).
(define (exception-class-name? nm [tokens default-effect-tokens])
  (and nm
       (let ([s (symbol->string nm)])
         (or (for/or ([suffix (in-set (tokens-exception-suffixes tokens))])
               (string-suffix? s suffix))
             (set-member? (effect-tokens-pure-constructor-names tokens) s)))))

;; Class name the constructor callee names, when it is a known class: a bare
;; callee (`QuerySet(...)`) or a dotted callee whose final field names a known
;; class (`models.CharField(...)`), plus pure stdlib/builtin value constructors
;; (`Path(...)`, `md5(...)`, `dict(...)` — pure to construct). Returns a
;; class-name symbol or #f. Effectful constructors (`open`, `ZipFile`) are
;; deliberately absent: `constructor-class` only names PURE construction.
(define (constructor-class callee class-defs [tokens default-effect-tokens])
  (define nm (callee-name callee))
  (cond
    [(and nm (or (hash-has-key? class-defs nm) (pure-stdlib-type? nm tokens))) nm]
    [(uir-get? callee)
     (define f (get-field-symbol (uir-get-field callee)))
     ;; A dotted callee rooted at self/cls is an attribute method call
     ;; (`self._cache.set(...)`), NOT a module-qualified constructor — the
     ;; final field `set` must not collide with the builtin `set` type here.
     (and f (not (tokens-self-name? tokens (dotted-root callee)))
          (or (hash-has-key? class-defs f) (pure-stdlib-type? f tokens)) f)]
    [else #f]))

;; Result type of an imported module FUNCTION call (`r = httpx.get(...)`, `resp
;; = requests.post(...)`): the callee is a member access `mod.func(...)` whose
;; root resolves through the import graph to a module handle, and the module's
;; function table maps `func` to a result-type name (`httpx.get` → Response).
;; The same table serves an imported CLASS's classmethod constructors
;; (`from datetime import datetime` → `datetime.now()` → datetime), guarded to
;; the case where the entry names that very class. This is the receiver-TYPE
;; half of call-result projection: `r` then tracks Response, and `r.json()`
;; classifies through stdlib-type-methods-data.
(define (imported-module-result-type callee comp import-env tokens)
  (and import-env comp (member-access-callee? callee)
       (let* ([root (dotted-root callee)]
              [func (effect-op-of-callee callee)]
              [modpath (hash-ref (import-env-comp-modpath import-env) comp #f)]
              [e (and modpath (hash-ref (import-env-exports import-env) modpath #f))]
              [h (and e root (hash-ref e (symbol->string root) #f))]
              [path0 (imported-binding-module-path h root)])
         (and root func path0
              (let* ([path path0]
                     [table (hash-ref (effect-tokens-module-result-types tokens) path #f)]
                     [t (and table (hash-ref table (symbol->string func) #f))]
                     [rtn (and t (string->symbol t))]
                     ;; A `from pkg import submod` fn-handle names a MODULE
                     ;; (`from xml.dom import pulldom`), so its table entries
                     ;; are trusted like a module handle's.
                     [module-binding?
                      (or (eq? (handle-kind h) 'module)
                          (and (eq? (handle-kind h) 'fn) (handle-name h)
                               (equal? path (string-append (handle-path h) "."
                                                           (handle-name h)))))])
                (and rtn
                     (or module-binding?
                         ;; A non-module handle (`from datetime import datetime`,
                         ;; which the lowercase class name synthesizes as an fn
                         ;; handle) may borrow the module's table entry only when
                         ;; it maps to that same binding — a classmethod
                         ;; constructor returning an instance of the class, so
                         ;; `Client.get(...)` cannot borrow `httpx.get`.
                         (and (memq (handle-kind h) '(class fn))
                              (equal? (symbol->string rtn) (handle-name h))))
                     rtn))))))

;; Result TYPE of a BARE imported FUNCTION call (`get_resolver(...)`,
;; `salted_hmac(...)`): the callee is a bare name bound in `comp`'s namespace
;; to an fn handle — synthesized for an external `from mod import fn` (a
;; lowercase binding outside the scanned corpus), or harvested for a scanned
;; corpus function. The module's result-type table maps the function name to
;; its result class, mirroring imported-module-result-type's member-access
;; path for `mod.fn(...)`. The fn handle is never a class, so a bare
;; `SomeClass(...)` constructor is unaffected (imported-class-ref handles
;; class handles).
(define (imported-fn-result-type name comp import-env tokens)
  (and import-env comp name
       (let* ([modpath (hash-ref (import-env-comp-modpath import-env) comp #f)]
              [e (and modpath (hash-ref (import-env-exports import-env) modpath #f))]
              [h (and e (hash-ref e (symbol->string name) #f))])
         (and (handle? h) (eq? (handle-kind h) 'fn)
              (handle-path h) (handle-name h)
              (let* ([table (hash-ref (effect-tokens-module-result-types tokens)
                                      (handle-path h) #f)]
                     [t (and table (hash-ref table (handle-name h) #f))])
                (and t (string->symbol t)))))))

;; Module path of an imported module/fn singleton binding (`connections` from
;; `from django.db import connections`): the export resolves to a module or fn
;; handle whose path is the source module. A subscript into that singleton
;; (`connections[database]`) yields an element of the same module's domain, so
;; `x = connections[alias]` tracks `x` as the module-path marker
;; (`django.db`) and a later `x.ops.sql_flush` chain classifies by that domain.
(define (imported-singleton-path name comp import-env)
  (and import-env name
       (let* ([modpath (hash-ref (import-env-comp-modpath import-env) comp #f)]
              [e (and modpath (hash-ref (import-env-exports import-env) modpath #f))]
              [h (and e (hash-ref e (symbol->string name) #f))])
         (and (handle? h)
              (memq (handle-kind h) '(module fn))
              (handle-path h)))))


;; The receiver-type marker for a dynamically-imported / dynamically-created
;; CLASS: `import_string(path)` and `type(...)` return SOME class, but which
;; one is runtime-dependent. A call through this marker (`cls(...)`) is pure
;; construction (see classify-all), but the marker is never projected as a
;; concrete result class: the enclosing function's declared-return-types (or a
;; later concrete projection) names the instance class.
(define dynamic-class-marker 'dynamic-class)
(define (dynamic-class-marker? c) (eq? c dynamic-class-marker))

;; Receiver-type marker for a conditional/or-fold whose EVERY branch is a
;; (pure-to-construct) class reference but the branches name DIFFERENT classes
;; (`stream_class = StringIO if is_str else BytesIO`). The value is SOME class,
;; so a bare call through it is pure construction, but it is never projected as
;; a concrete result/instance class: a union's instance type is ambiguous.
(define class-union-marker 'class-union)
(define (class-union-marker? c) (eq? c class-union-marker))

;; Result TYPE of a call expression: the class named by its callee when that
;; class is known, or the class a class-valued callee expression resolves to.
;; Covers corpus classes, pure stdlib value constructors, and any stdlib type
;; with a method table — `open` and `ZipFile` construct effectfully, so they are
;; result types but NOT pure constructors. `rt` (optional) is the receiver-type
;; environment at the call site, which lets a call through a tracked class
;; value (`self.build_file_class(...)`) carry the class through to the result.
(define (call-result-type callee class-defs [tokens default-effect-tokens] [rt #f]
                           [import-env #f] [comp #f])
  (or (let ([c (and rt (receiver-class-of rt callee tokens))])
        (and c (not (dynamic-class-marker? c)) (not (class-union-marker? c)) c))
      ;; A module-qualified CLASS / FUNCTION reference (`smtplib.SMTP(...)`,
      ;; `email.message.EmailMessage(...)`, `httpx.get(...)`) is more specific
      ;; than the value->type fallback below: it requires the root to resolve
      ;; through the import graph to a real module AND the (module, attribute)
      ;; to be in the DATA lexicon. Checking it first keeps a qualified external
      ;; class from colliding with a same-named corpus class
      ;; (email.message.EmailMessage vs django.core.mail.message.EmailMessage),
      ;; AND keeps `httpx.get(...)` typed Response instead of the coarser module
      ;; marker the value->type projection would return for its root.
      (imported-module-class-ref callee comp import-env tokens)
      (imported-module-result-type callee comp import-env tokens)
      ;; A class-valued callee the receiver-class-of fast path misses: a dotted
      ;; class reference rooted at a module receiver (`self._lib.DriverInfo(...)`
      ;; where `_lib` is the redis module) or a subscript into a tracked tuple.
      ;; value-class-of is the single value->type projection, so it also covers
      ;; the module.Class marker (`redis.DriverInfo`) that receiver-class-of
      ;; (local/self-attr only) does not walk. A non-call callee cannot recurse
      ;; back into call-result-type through value-class-of's call branch.
      (let ([c (and rt (value-class-of callee rt class-defs tokens import-env comp #f))])
        (and c (not (dynamic-class-marker? c)) (not (class-union-marker? c)) c))
      (constructor-class callee class-defs tokens)
      ;; A class factory (`import_string`, `type`): the call's RESULT is a
      ;; runtime-dependent class, tracked as the dynamic-class marker so the
      ;; outer `cls(...)` call classifies as pure construction.
      (let ([nm (callee-name callee)])
        (and nm
             (set-member? (effect-tokens-dynamic-class-factories tokens)
                          (symbol->string nm))
             dynamic-class-marker))
      (let ([nm (callee-name callee)])
        (and nm
             (hash-has-key? (effect-tokens-stdlib-type-methods tokens)
                            (symbol->string nm))
             nm))
      (let ([nm (callee-name callee)])
        (and nm comp import-env (imported-class-ref nm comp import-env)))
      (let ([nm (callee-name callee)])
        (and nm comp import-env (imported-fn-result-type nm comp import-env tokens)))
      (method-result-type callee
                          (λ (base)
                            (or (receiver-class-of rt base tokens)
                                (value-class-of base rt class-defs tokens
                                                import-env comp #f)))
                          tokens)))

;; VALUE TYPE of an attribute on a tracked framework class whose corpus
;; attribute graph cannot see it (an external class): class -> attr ->
;; value-type from the DATA table, so an attribute access (`response.cookies`,
;; `connection.ops`, `Model._meta`) resolves its VALUE class through the same
;; table receiver-chain.rkt walks for multi-level method receivers. Kept here
;; (not imported from receiver-chain.rkt, which requires this module) as a
;; private duplicate of that module's stdlib-class-attr-type.
(define (stdlib-attr-class cls attr [tokens default-effect-tokens])
  (and cls attr
       (let* ([m (hash-ref (effect-tokens-class-attr-types tokens)
                           (symbol->string cls) #f)]
              [r (and m (hash-ref m (symbol->string attr) #f))])
         (and r (string->symbol r)))))

;; Result class of a mapping value access `.get(k)` / `.setdefault(k)` WITHOUT a
;; default argument: the single element the mapping yields. The mapping's
;; element type comes from class-attr-element-types-data (a dict-valued
;; attribute such as MigrationLoader.applied_migrations), reached through the
;; attribute's OWN class because the value-class projection reads only the
;; VALUE table. This is the element-type mirror of mapping-get-result-type's
;; default-argument rule.
(define (mapping-element-get-result value project tokens)
  (and (uir-call? value)
       (member-access-callee? (uir-call-callee value))
       (let* ([callee (uir-call-callee value)]
              [base (member-access-base callee)]
              [op (member-access-field callee)]
              [args (uir-call-args value)])
         (and op
              (set-member? (effect-tokens-mapping-value-methods tokens)
                           (symbol->string op))
              (= (length args) 1)
              (member-access-callee? base)
              (let* ([attr (member-access-field base)]
                     [icls (project (member-access-base base))])
                (class-attr-element-type icls attr tokens))))))

;; Class a VALUE expression resolves to under a receiver-type environment `rt`:
;; a tracked call result, a local alias, a self/cls attribute, or a bare class
;; reference. This is the single value→type projection shared by assignment
;; tracking (infer-receiver-types) and return-type projection (return-class-of).
(define (value-class-of value rt class-defs [tokens default-effect-tokens]
                        [import-env #f] [comp #f] [owner #f])
  (cond
    [(uir-await? value) (value-class-of (uir-await-expr value) rt class-defs
                                        tokens import-env comp owner)]
    ;; A parenthesized expression (`x = (A() if c else B())`) is transparent
    ;; to typing: the front ends wrap conditional/boolean values in a paren
    ;; node, which must not defeat the conditional fold or the literal/call
    ;; projection below.
    [(uir-paren? value) (value-class-of (uir-paren-inner value) rt class-defs
                                        tokens import-env comp owner)]
    ;; Optional chain (`x?.y`): the front ends lower it to a null guard whose
    ;; absent branch is `void 0`. The value's type is the PRESENT branch's
    ;; class — the absent branch only means "no value", so it carries no
    ;; receiver class and must not defeat the projection. This is what makes
    ;; `r = x?.y; r.m()` receiver-type dataflow work for `?.` chains.
    [(and (uir-if? value) (optional-chain-present value))
     => (λ (present) (value-class-of present rt class-defs tokens import-env comp owner))]
    ;; Conditional expression (`x if c else y`): sound only when every branch
    ;; projects to the SAME class; a mixed or unresolvable branch yields #f —
    ;; EXCEPT when every branch is itself a class reference (different pure
    ;; classes): then the value is SOME class, tracked as the class-union
    ;; marker so a bare call through it is pure construction.
    [(uir-if? value)
     (let ([t (value-class-of (uir-if-then value) rt class-defs
                              tokens import-env comp owner)]
           [e (value-class-of (uir-if-else value) rt class-defs
                              tokens import-env comp owner)])
       (cond
         [(and t e (eq? t e)) t]
         [(and (bare-class-value (uir-if-then value) class-defs tokens
                                 import-env comp)
               (bare-class-value (uir-if-else value) class-defs tokens
                                 import-env comp))
          class-union-marker]
         [else #f]))]
    ;; Boolean expression (`x or y` / `x and y`, lowered as (call "or"/"and"
    ;; (list lhs rhs))): same-class fold, sound for the same reason, with the
    ;; same class-union relaxation as the conditional.
    [(and (uir-call? value)
          (let ([c (uir-call-callee value)])
            (and (uir-symbol? c)
                 (memq (string->symbol (uir-symbol-name c)) '(or and)))))
     (let ([args (uir-call-args value)])
       (and (= (length args) 2)
            (let ([a (value-class-of (first args) rt class-defs
                                     tokens import-env comp owner)]
                  [b (value-class-of (second args) rt class-defs
                                     tokens import-env comp owner)])
              (cond
                [(and a b (eq? a b)) a]
                [(and (bare-class-value (first args) class-defs tokens
                                        import-env comp)
                      (bare-class-value (second args) class-defs tokens
                                        import-env comp))
                 class-union-marker]
                [else #f]))))]
    ;; `new C(...)` constructs an instance of C: the receiver type is C when
    ;; the class expression names a tracked class (corpus, stdlib value type, or
    ;; imported class handle), so a fresh JS/TS object seeds the dataflow.
    [(uir-new? value)
     (bare-class-value (uir-new-class value) class-defs tokens import-env comp)]
    ;; A JS/TS `this.attr` value is a dot-call member access, not a uir-get:
    ;; read it through the same implicit-receiver attribute env as Python's
    ;; `self.attr`, so both encodings project identically.
    [(and (member-access-callee? value)
          (let ([attr (member-access-field value)])
            (and attr
                 (self-or-cls-var? (member-access-base value) tokens)
                 (hash-ref (receiver-types-self-attrs rt) attr #f))))
     => (λ (cls) cls)]
    [(uir-call? value)
     (or (operator-result-type
          value
          (λ (v) (value-class-of v rt class-defs tokens import-env comp owner))
          tokens)
         (mapping-get-result-type
          value
          (λ (v) (value-class-of v rt class-defs tokens import-env comp owner))
          tokens)
         (mapping-element-get-result
          value
          (λ (v) (value-class-of v rt class-defs tokens import-env comp owner))
          tokens)
         (call-result-type (uir-call-callee value) class-defs tokens rt import-env comp))]
    ;; Literal receiver types: a local or self-attr bound to a container /
    ;; string literal has that host literal type (the mapping is dialect DATA),
    ;; so `xs = []; xs.append(v)` resolves through the builtin type table
    ;; instead of `unknown`. A Python bytes literal lowers to uir-string; bytes
    ;; methods are also in-memory, so the string type keeps the effect
    ;; classification sound where the exact type is bytes.
    [(literal-value-type value tokens)]
    [(uir-var? value)
     (define nm (string->symbol (uir-symbol-name (uir-var-name value))))
     (or (and owner (tokens-self-name? tokens nm) owner)
         (hash-ref (receiver-types-locals rt) nm #f)
         (hash-ref (receiver-types-globals rt) nm #f)
         (bare-class-value value class-defs tokens import-env comp))]
    [(uir-symbol? value)
     (define nm (string->symbol (uir-symbol-name value)))
     (or (and owner (tokens-self-name? tokens nm) owner)
         (hash-ref (receiver-types-locals rt) nm #f)
         (hash-ref (receiver-types-globals rt) nm #f)
         (bare-class-value value class-defs tokens import-env comp))]
    [(uir-get? value)
     (define attr (get-field-symbol (uir-get-field value)))
     (cond
       [(and attr (self-or-cls-var? (uir-get-base value) tokens))
        (or (hash-ref (receiver-types-self-attrs rt) attr #f)
            ;; An implicit-receiver attribute the corpus graph cannot see
            ;; (a subclass-assigned field or an external parent's __init__):
            ;; the DATA class-attr-types table names its value type.
            (and owner (stdlib-attr-class owner attr tokens)))]
       [else
        ;; A subscript into a tuple-typed local (`tmp[1]` where `tmp`'s
        ;; elem-spec is (str list list)): project the i-th element class, so
        ;; a destructured loop var (`dirnames`) resolves its methods instead
        ;; of staying `unknown`.
        (define base-nm
          (and (uir-var? (uir-get-base value))
               (string->symbol (uir-symbol-name
                                (uir-var-name (uir-get-base value))))))
        (define tspec (and base-nm rt
                           (hash-ref (receiver-types-elem-specs rt) base-nm #f)))
        (define idx (and (uir-number? (uir-get-field value))
                         (string->number (uir-number-value (uir-get-field value)))))
        (cond
          [(and idx (list? tspec) (>= idx 0) (< idx (length tspec)))
           (list-ref tspec idx)]
          ;; `module.Class` where `module` is a tracked module receiver:
          ;; resolve to the module-qualified class marker (pylibmc.Client),
          ;; whose domain is the module's domain in classify-all.
          [else
           (define bm (receiver-class-of rt (uir-get-base value)))
           (cond
             ;; `module.Class` where `module` is an imported module handle:
             ;; the class table names the attribute (smtplib.SMTP → SMTP).
             [(imported-module-class-ref value comp import-env tokens)]
          [(and attr bm (module-receiver? bm tokens))
           (string->symbol (format "~a.~a" bm attr))]
             ;; A tracked framework class's attribute VALUE type
             ;; (`response.cookies` -> SimpleCookie, `connection.ops` ->
             ;; DatabaseOperations): the DATA table names a sub-handle the
             ;; corpus attribute graph cannot see, so an element-type or
             ;; assignment projection crosses the external object boundary.
             [(and attr bm (stdlib-attr-class bm attr tokens))]
             ;; A subscript into an imported module/fn singleton
             ;; (`connections[database]`): the field is the index expression
             ;; (not an attribute name), and the base resolves through the
             ;; import graph to a module/fn handle. Track the module path so a
             ;; later chain (`connection.ops.sql_flush`) classifies by the
             ;; module's domain.
             [(and (not (uir-string? (uir-get-field value)))
                   (uir-var? (uir-get-base value))
                   import-env comp)
              (define bname (string->symbol (uir-symbol-name
                                            (uir-var-name (uir-get-base value)))))
              (define p (imported-singleton-path bname comp import-env))
              (and p (string->symbol p))]
             ;; A subscript into a dict-valued ATTRIBUTE
             ;; (`loader.graph.nodes[key]`): the DATA class-attr-element-types
             ;; table names the container's element class (MigrationGraph.nodes
             ;; -> Migration), so the subscripted value resolves its methods
             ;; instead of staying `unknown`.
             [(and (not (uir-string? (uir-get-field value)))
                   (member-access-callee? (uir-get-base value)))
              (let* ([base (uir-get-base value)]
                     [attr (member-access-field base)]
                     [icls (value-class-of (member-access-base base) rt class-defs
                                           tokens import-env comp owner)])
                (and attr icls (class-attr-element-type icls attr tokens)))]
             [else #f])])])]
    [else #f]))

;; Return type of a function/method body, projected through its receiver-type
;; environment: the single class every non-#f `return` expression resolves to.
;; A body with no return, a mixed set of return types, or a return that does
;; not project to a tracked class yields #f (sound: no projection is better
;; than a wrong one). `rt` is the precomputed receiver env (see
;; infer-receiver-types), so callers share one env instead of rebuilding it.
;; Project a CALL value's result class through the whole-program corpus return
;; table first (`self.other()` / `message.message()` / `self.fetch_command(x)`),
;; then fall through to the local value→class projection. The corpus table is
;; the fixpoint over declared + body-projected returns, so a method whose body
;; is `return self._root.resolve(...)` (InMemoryStorage._resolve) projects
;; InMemoryDirNode exactly as the assignment-tracking closure in
;; infer-receiver-types does. Shared so return-class-of and the closure agree.
(define (corpus-projected-value-class value rt class-defs owner return-types
                                      methods fn-defs known comp-supers
                                      [tokens default-effect-tokens]
                                      [import-env #f] [comp #f])
  ;; An awaited call (`await self.get_response_async(request)`) projects through
  ;; the same corpus return table as a direct call: unwrap the await, exactly
  ;; as value-class-of does, so async bodies don't lose the projection.
  (define v (if (uir-await? value) (uir-await-expr value) value))
  (and (uir-call? v) return-types methods fn-defs known comp-supers
       (corpus-call-return-type (uir-call-callee v) comp class-defs owner
                                methods fn-defs known comp-supers return-types
                                tokens
                                #:receiver-class
                                (λ (base)
                                  (value-class-of base rt class-defs tokens
                                                  import-env comp owner)))))

(define (return-class-of body rt class-defs [tokens default-effect-tokens]
                         [import-env #f] [comp #f] [owner #f]
                         #:return-types [return-types #f]
                         #:methods [methods #f]
                         #:fn-defs [fn-defs #f]
                         #:known [known #f]
                         #:comp-supers [comp-supers #f])
  (define classes '())
  (walk-body body (λ (n)
                    (when (uir-return? n)
                      (define v (uir-return-value n))
                      (define c (or (corpus-projected-value-class
                                     v rt class-defs owner return-types methods
                                     fn-defs known comp-supers tokens import-env comp)
                                    (value-class-of v rt class-defs
                                                    tokens import-env comp owner)))
                      (when c (set! classes (cons c classes))))))
  (define uniq (remove-duplicates classes))
  (and (pair? uniq) (= (length uniq) 1) (car uniq)))

;; Build the receiver-type environment for one function body. Assignments are
;; processed in source (pre-order) order; `class-defs` is the set of known class
;; names (a hash keyed by class-name symbol), which gates which constructor
;; callees / bare class references count as a known receiver type. `base-attrs`
;; (optional) seeds the self-attribute environment with the enclosing class's
;; field/`self.attr` defaults, so a method can resolve `self.build_file_class`
;; even when that attribute is declared at class level rather than in the
;; method body.
(define (infer-receiver-types body class-defs [base-attrs (hash)]
                              [import-env #f] [comp #f] [params '()] [owner #f]
                              #:tokens [tokens default-effect-tokens]
                              #:return-types [return-types #f]
                              #:methods [methods #f]
                              #:fn-defs [fn-defs #f]
                              #:known [known #f]
                              #:comp-supers [comp-supers #f]
                              #:element-types [element-types #f]
                              #:param-element-specs [param-element-specs (hash)]
                              #:globals [globals (hash)]
                              #:base-fn-attrs [base-fn-attrs (hash)])
  (define locals (make-hash))
  (define self-attrs (hash-copy base-attrs))
  (define elem-specs (make-hash))   ;; var -> element-spec (symbol | list)
  (define fn-locals (make-hash))    ;; var -> (cons class-or-#f method-name)
  (define fn-self-attrs (hash-copy base-fn-attrs)) ;; attr -> (cons class method)
  ;; A declared param ELEMENT type (`for message in messages` where `messages`
  ;; is an external entry-point param): the element class seeds the loop
  ;; variable exactly as a for-loop's elem-spec would, so `message.message()`
  ;; resolves through the element class's method table.
  (for ([(nm espec) (in-hash param-element-specs)])
    (hash-set! elem-specs nm espec)
    (when (symbol? espec) (hash-set! locals nm espec)))
  (define rt-now
    (λ () (receiver-types locals self-attrs elem-specs globals
                          fn-locals fn-self-attrs)))
  (define (value-class value)
    (define v (if (uir-await? value) (uir-await-expr value) value))
    (or (and (uir-call? v) return-types methods fn-defs known comp-supers
             (corpus-call-return-type (uir-call-callee v) comp class-defs owner
                                      methods fn-defs known comp-supers return-types
                                      tokens
                                      ;; resolve the receiver through the CURRENT
                                      ;; env so `message.message()` (param typed
                                      ;; EmailMessage) and `self.fetch_command(x)`
                                      ;; project their corpus return types; this is
                                      ;; the same env value-class-of uses below.
                                      #:receiver-class
                                      (λ (base)
                                        (value-class-of base (rt-now)
                                                        class-defs tokens import-env comp owner))))
        (value-class-of value (rt-now)
                        class-defs tokens import-env comp owner)))
  ;; Seed parameter types: from the annotation (`def f(x: QuerySet)`) when there
  ;; is one, else from the parameter's DEFAULT VALUE (`def run(server_cls =
  ;; WSGIServer)`). Both name a known corpus / stdlib / imported class, so a
  ;; later `x.m(...)` resolves through receiver-type dataflow rather than
  ;; falling to `unknown`. Subscripted annotations (`List[Item]`) are not
  ;; tracked yet.
  (for ([p (in-list params)])
    (when (uir-typed-param? p)
      (define pname (and (uir-symbol? (uir-typed-param-name p))
                         (string->symbol (uir-symbol-name (uir-typed-param-name p)))))
      (when pname
        (define ann (uir-typed-param-type p))
        (define def (uir-typed-param-default p))
        (define tname (and ann (uir-symbol? ann)
                           (string->symbol (uir-symbol-name ann))))
        (define pcls (or (and ann (bare-class-value ann class-defs
                                                    tokens import-env comp))
                         ;; A declared or call-chain-collected param typed by an
                         ;; external framework / stdlib class (SMTP,
                         ;; ArgumentParser, Decimal, Message, Field, Node,
                         ;; Model, BaseDatabaseWrapper, ...) resolves through its
                         ;; stdlib method table: accept any class that has one,
                         ;; exactly as call-result-type does for results.
                         (and tname
                              (hash-has-key? (effect-tokens-stdlib-type-methods tokens)
                                             (symbol->string tname))
                              tname)
                         ;; A call-chain-collected module-path marker
                         ;; (`connection = connections[database]` types the
                         ;; param as `django.db`): not a class and not a
                         ;; stdlib receiver type, but a tracked singleton
                         ;; handle whose domain the receiver chain bottoms out
                         ;; at. A class symbol never carries a dot, so the
                         ;; dotted test keeps this marker distinct.
                         (and tname (string-contains? (symbol->string tname) ".")
                              tname)
                         (and (not ann) def (value-class def))))
        (when pcls (hash-set! locals pname pcls)))))
  ;; Element spec of an iterable value, for alias tracking (`entries =
  ;; os.scandir(path)` then `for entry in entries`).
  (define (elem-spec-of value)
    (iterable-element-spec value (rt-now)
                           tokens import-env comp
                           (λ (v) (value-class-of v (rt-now)
                                                  class-defs tokens import-env comp owner))
                           #:class-defs class-defs
                           #:methods methods
                           #:fn-defs fn-defs
                           #:known known
                           #:comp-supers comp-supers
                           #:owner owner
                           #:element-types element-types
                           #:elem-spec-lookup (λ (nm) (hash-ref elem-specs nm #f))))
  ;; A value that is a BOUND-METHOD alias: `obj.m` used as a value, or
  ;; `getattr(obj, "m", default)`. Returns (cons receiver-class-or-#f
  ;; method-name), so a later bare call of the local (or `self.attr` call)
  ;; resolves as `obj.m(...)` in classify-call-all's function-value pre-pass.
  (define (fn-alias-of value)
    (define v (if (uir-await? value) (uir-await-expr value) value))
    (define (lambda-branch? x)
      (let ([x (if (uir-paren? x) (uir-paren-inner x) x)])
        (uir-fn? x)))
    (cond
      [(uir-paren? v) (fn-alias-of (uir-paren-inner v))]
      [(and (uir-call? v)
            (let ([c (uir-call-callee v)])
              (and (bare-name-symbol c)
                   (set-member? bound-method-factories
                                (symbol->string (bare-name-symbol c)))))
            (pair? (uir-call-args v)))
       (define name-arg (and (pair? (cdr (uir-call-args v)))
                             (second (uir-call-args v))))
       (define mname (and name-arg (uir-string? name-arg)
                          (string->symbol (uir-string-value name-arg))))
       (and mname (cons (value-class (first (uir-call-args v))) mname))]
      [(member-access-callee? v)
       (define mname (member-access-field v))
       (and mname (cons (value-class (member-access-base v)) mname))]
      ;; A conditional `x = (obj.m if c else lambda ...)` (or the mirrored
      ;; order): a lambda branch is a separate, non-collected function value
      ;; (walk-body never descends into it), so it contributes no effect to
      ;; this function. Collapsing to the bound-method branch is therefore
      ;; sound and lets a later bare call `x(...)` resolve as `obj.m(...)`.
      [(uir-if? v)
       (define t (fn-alias-of (uir-if-then v)))
       (define e (fn-alias-of (uir-if-else v)))
       (cond
         [(and t (lambda-branch? (uir-if-else v))) t]
         [(and e (lambda-branch? (uir-if-then v))) e]
         [else #f])]
      [else #f]))
  ;; Python `uir-set!` and JS/TS `=` assignments feed one target rule; the
  ;; caller supplies the value projection, host implicit-receiver name test and
  ;; env setters.
  (apply-assignments! body value-class
                      (λ (n) (tokens-self-name? tokens n))
                      (λ (nm cls) (hash-set! locals nm cls))
                      (λ (attr cls) (hash-set! self-attrs attr cls))
                      #:elem-spec-of elem-spec-of
                      #:set-local-elem! (λ (nm espec) (hash-set! elem-specs nm espec))
                      #:fn-alias-of fn-alias-of
                      #:set-local-fn! (λ (nm d) (hash-set! fn-locals nm d))
                      #:set-self-attr-fn!
                      (λ (attr d) (hash-set! fn-self-attrs attr d)))
  ;; `with ctx() as name:` binds the context manager's RESULT type: track the
  ;; target so `with ThreadPoolExecutor() as executor` types `executor` and a
  ;; later `executor.submit(...)` resolves instead of staying `unknown`. Its
  ;; element spec is tracked too (`with os.scandir(p) as entries`).
  (define (process-with! n)
    (for ([item (in-list (uir-with-items n))])
      (when (and (pair? item) (pair? (cdr item)))
        (define target (second item))
        (define nm (bare-name-symbol target))
        (when nm
          (define cls (value-class (first item)))
          (when cls (hash-set! locals nm cls))
          (define espec (elem-spec-of (first item)))
          (when espec (hash-set! elem-specs nm espec))))))
  ;; `for v in ITER:` binds v to ITER's element class (single) or the
  ;; destructuring tuple spec (list). A list spec is stored only in elem-specs
  ;; (a tuple has no method table); a single spec is ALSO a locals class so
  ;; `v.m()` resolves through receiver-class-of.
  (define (process-for! n)
    (define espec (elem-spec-of (uir-for-each-iterable n)))
    (define target (uir-for-each-var n))
    (cond
      ;; Tuple destructure (`for dirpath, dirnames, filenames in os.walk(...)`):
      ;; the element spec is a list of class symbols; seed EACH destructured var
      ;; into locals with its own class, so `dirnames.remove(...)` resolves
      ;; through the list method table instead of staying `unknown`.
      [(and (uir-list? target) (pair? espec) (list? espec))
       (for ([v (in-list (uir-list-items target))]
             [cls (in-list espec)])
         (define vnm (bare-name-symbol v))
         (when (and vnm (symbol? cls)) (hash-set! locals vnm cls))
         (when vnm (hash-set! elem-specs vnm cls)))]
      [else
       (define vnm (bare-name-symbol target))
       (when (and vnm espec)
         (hash-set! elem-specs vnm espec)
         (when (symbol? espec) (hash-set! locals vnm espec)))]))
  ;; A comprehension `(call "<type>-comp" (list result loop-var iter filter))`
  ;; binds loop-var (a var, or a uir-list for tuple destructuring) over iter
  ;; exactly like a for-loop, so the comp var's element methods resolve.
  (define (process-comp! n)
    (and (uir-call? n)
         (let ([callee (uir-call-callee n)])
           (and (uir-symbol? callee)
                (set-member? (tokens-comprehension-callees tokens)
                             (uir-symbol-name callee))
                (let ([args (uir-call-args n)])
                  (and (>= (length args) 3)
                       (let* ([lv (second args)]
                              [iter (third args)]
                              [espec (elem-spec-of iter)]
                              [vars (if (uir-list? lv) (uir-list-items lv) (list lv))])
                         (when espec
                           (for ([v (in-list vars)])
                             (define vnm (bare-name-symbol v))
                             (when vnm
                               (hash-set! elem-specs vnm espec)
                               (when (symbol? espec)
                                 (hash-set! locals vnm espec))))))))))))
  (walk-body body (λ (n)
                    (when (uir-with? n) (process-with! n))
                    (when (uir-for-each? n) (process-for! n))
                    (process-comp! n)))
  ;; Second assignment pass: loop-temp element specs are now seeded (a tuple
  ;; destructure loweres to `tmp[1]` subscripts inside the loop body), so a
  ;; subscript projection that the first pass could not see now resolves.
  ;; Assignments are monotone and idempotent, so the repeat only fills gaps.
  (apply-assignments! body value-class
                      (λ (n) (tokens-self-name? tokens n))
                      (λ (nm cls) (hash-set! locals nm cls))
                      (λ (attr cls) (hash-set! self-attrs attr cls))
                      #:elem-spec-of elem-spec-of
                      #:set-local-elem! (λ (nm espec) (hash-set! elem-specs nm espec))
                      #:fn-alias-of fn-alias-of
                      #:set-local-fn! (λ (nm d) (hash-set! fn-locals nm d))
                      #:set-self-attr-fn!
                      (λ (attr d) (hash-set! fn-self-attrs attr d)))
  (receiver-types locals self-attrs elem-specs globals fn-locals fn-self-attrs))

;; Overlay `overlay`'s entries onto `base` (overlay wins on a key collision).
(define (hash-overlay base overlay)
  (define out (hash-copy base))
  (for ([(k v) (in-hash overlay)]) (hash-set! out k v))
  out)

;; Merge two receiver-type environments; `overlay`'s locals, self-attrs and
;; elem-specs win. Used to layer a function's own receiver tracking over its
;; class's inherited self-attributes (collected across fields, __init__ and all
;; methods).
(define (receiver-types-merge base overlay)
  (receiver-types
   (hash-overlay (receiver-types-locals base) (receiver-types-locals overlay))
   (hash-overlay (receiver-types-self-attrs base) (receiver-types-self-attrs overlay))
   (hash-overlay (receiver-types-elem-specs base) (receiver-types-elem-specs overlay))
   (hash-overlay (receiver-types-globals base) (receiver-types-globals overlay))
   (hash-overlay (receiver-types-fn-locals base) (receiver-types-fn-locals overlay))
   (hash-overlay (receiver-types-fn-self-attrs base)
                 (receiver-types-fn-self-attrs overlay))))
