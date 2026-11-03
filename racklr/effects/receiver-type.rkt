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
         racklr/effects/receiver-predicates
         racklr/effects/receiver-assign
         racklr/effects/type-projection
         racklr/effects/lexicon)

(provide
  receiver-types? receiver-types receiver-types-locals receiver-types-self-attrs
  receiver-types-merge
  infer-receiver-types
  receiver-class-of
  constructor-class
  call-result-type
  value-class-of
  bare-class-value
  annotation-class-of)

(struct receiver-types (locals self-attrs) #:transparent)
;; locals:     hash name-symbol → class-name-symbol
;; self-attrs: hash attr-symbol → class-name-symbol (on self / cls)

;; Class name (symbol) tracked for a receiver expression `base`, or #f. `base`
;; is either a local variable (`qs`), a `self`/`cls` attribute (`self.cursor`),
;; or a bare class reference. Moved above `call-result-type` because
;; `call-result-type` consults it when a call's callee is a class-valued
;; receiver expression (`self.build_file_class(...)`).
(define (receiver-class-of env base [tokens default-effect-tokens])
  (cond
    [(not env) #f]
    [(uir-var? base)
     (hash-ref (receiver-types-locals env)
               (string->symbol (uir-symbol-name (uir-var-name base))) #f)]
    [(uir-symbol? base)
     (hash-ref (receiver-types-locals env)
               (string->symbol (uir-symbol-name base)) #f)]
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
           (module-receiver? nm tokens))
       nm))

;; Class named by a WRITTEN type annotation (`x: KnownClass`, `def f(x:
;; KnownClass)`, class field `attr: KnownClass`), or #f. Only a bare annotation
;; is projected: a subscripted/generic annotation (`list[Item]`) names a
;; container, not its element, so unwrapping it would claim the wrong receiver.
;; The tracked-type set is exactly `bare-class-value`'s (corpus class, pure
;; stdlib value type, imported class, module marker) plus the tracked
;; stdlib/module receiver markers that a relation-propagated annotation may
;; reuse.
(define (annotation-class-of ann class-defs [tokens default-effect-tokens]
                             [import-env #f] [comp #f])
  (define nm (and ann (bare-name-symbol ann)))
  (and nm
       (or (bare-class-value ann class-defs tokens import-env comp)
           (tracked-receiver-type? nm tokens))))

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

;; Result TYPE of a call expression: the class named by its callee when that
;; class is known, or the class a class-valued callee expression resolves to.
;; Covers corpus classes, pure stdlib value constructors, and any stdlib type
;; with a method table — `open` and `ZipFile` construct effectfully, so they are
;; result types but NOT pure constructors. `rt` (optional) is the receiver-type
;; environment at the call site, which lets a call through a tracked class
;; value (`self.build_file_class(...)`) carry the class through to the result.
(define (call-result-type callee class-defs [tokens default-effect-tokens] [rt #f]
                           [import-env #f] [comp #f])
  (or (and rt (receiver-class-of rt callee tokens))
      (constructor-class callee class-defs tokens)
      (let ([nm (callee-name callee)])
        (and nm
             (hash-has-key? (effect-tokens-stdlib-type-methods tokens)
                            (symbol->string nm))
             nm))
      (let ([nm (callee-name callee)])
        (and nm comp import-env (imported-class-ref nm comp import-env)))
      (imported-module-class-ref callee comp import-env tokens)
      (imported-module-result-type callee comp import-env tokens)
      (method-result-type callee
                          (λ (base) (receiver-class-of rt base tokens))
                          tokens)))

;; Class a VALUE expression resolves to under a receiver-type environment `rt`:
;; a tracked call result, a local alias, a self/cls attribute, or a bare class
;; reference. This is the single value→type projection shared by assignment
;; tracking (infer-receiver-types) and return-type projection (return-class-of).
(define (value-class-of value rt class-defs [tokens default-effect-tokens]
                        [import-env #f] [comp #f] [owner #f])
  (cond
    [(uir-await? value) (value-class-of (uir-await-expr value) rt class-defs
                                        tokens import-env comp owner)]
    ;; Optional chain (`x?.y`): the front ends lower it to a null guard whose
    ;; absent branch is `void 0`. The value's type is the PRESENT branch's
    ;; class — the absent branch only means "no value", so it carries no
    ;; receiver class and must not defeat the projection. This is what makes
    ;; `r = x?.y; r.m()` receiver-type dataflow work for `?.` chains.
    [(and (uir-if? value) (optional-chain-present value))
     => (λ (present) (value-class-of present rt class-defs tokens import-env comp owner))]
    ;; Conditional expression (`x if c else y`): sound only when every branch
    ;; projects to the SAME class; a mixed or unresolvable branch yields #f.
    [(uir-if? value)
     (let ([t (value-class-of (uir-if-then value) rt class-defs
                              tokens import-env comp owner)]
           [e (value-class-of (uir-if-else value) rt class-defs
                              tokens import-env comp owner)])
       (and t e (eq? t e) t))]
    ;; Boolean expression (`x or y` / `x and y`, lowered as (call "or"/"and"
    ;; (list lhs rhs))): same-class fold, sound for the same reason.
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
              (and a b (eq? a b) a))))]
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
         (bare-class-value value class-defs tokens import-env comp))]
    [(uir-symbol? value)
     (define nm (string->symbol (uir-symbol-name value)))
     (or (and owner (tokens-self-name? tokens nm) owner)
         (hash-ref (receiver-types-locals rt) nm #f)
         (bare-class-value value class-defs tokens import-env comp))]
    [(uir-get? value)
     (define attr (get-field-symbol (uir-get-field value)))
     (cond
       [(and attr (self-or-cls-var? (uir-get-base value) tokens))
        (hash-ref (receiver-types-self-attrs rt) attr #f)]
       [else
        ;; `module.Class` where `module` is a tracked module receiver:
        ;; resolve to the module-qualified class marker (pylibmc.Client), whose
        ;; domain is the module's domain in classify-all.
        (define bm (receiver-class-of rt (uir-get-base value)))
        (cond
          ;; `module.Class` where `module` is an imported module handle: the
          ;; class table names the attribute (smtplib.SMTP → SMTP).
          [(imported-module-class-ref value comp import-env tokens)]
          [(and attr bm (module-receiver? bm tokens))
           (string->symbol (format "~a.~a" bm attr))]
          ;; A subscript into an imported module/fn singleton
          ;; (`connections[database]`): the field is the index expression (not
          ;; an attribute name), and the base resolves through the import graph
          ;; to a module/fn handle. Track the module path so a later chain
          ;; (`connection.ops.sql_flush`) classifies by the module's domain.
          [(and (not (uir-string? (uir-get-field value)))
                (uir-var? (uir-get-base value))
                import-env comp)
           (define bname (string->symbol (uir-symbol-name
                                         (uir-var-name (uir-get-base value)))))
           (define p (imported-singleton-path bname comp import-env))
           (and p (string->symbol p))]
          [else #f])])]
    [else #f]))

;; Build the receiver-type environment for one function body (return projection
;; lives in return-type.rkt); `class-defs` gates which constructor callees / bare
;; class references count as a known receiver type, and `base-attrs` seeds
;; self-attributes.
(define (infer-receiver-types body class-defs [base-attrs (hash)]
                              [import-env #f] [comp #f] [params '()] [owner #f]
                              #:tokens [tokens default-effect-tokens]
                              #:return-types [return-types #f]
                              #:methods [methods #f]
                              #:fn-defs [fn-defs #f]
                              #:known [known #f]
                              #:comp-supers [comp-supers #f]
                              ;; Injected call-result projection (callee env ->
                              ;; class) so an assignment from a method on a
                              ;; tracked receiver projects too.
                              #:call-result-class [call-result-class #f])
  (define locals (make-hash))
  (define self-attrs (hash-copy base-attrs))
  (define (env) (receiver-types locals self-attrs))
  (define (value-class value)
    (if (not (uir-call? value))
        (value-class-of value (env) class-defs tokens import-env comp owner)
        (or (and call-result-class
                 (call-result-class (uir-call-callee value) (env)))
            ;; Only without an injected projection may the older lax corpus
            ;; lookup run; after a fail-closed #f it could over-claim.
            (and (not call-result-class) return-types methods fn-defs known
                 comp-supers
                 (corpus-call-return-type (uir-call-callee value) comp class-defs
                                          owner methods fn-defs known comp-supers
                                          return-types tokens))
            (value-class-of value (env) class-defs tokens import-env comp owner))))
  ;; Seed parameter types: from the annotation (`def f(x: QuerySet)`) when there
  ;; is one, else from the parameter's DEFAULT VALUE. (Subscripted annotations
  ;; like `List[Item]` are not tracked yet.)
  (for ([p (in-list params)])
    (when (uir-typed-param? p)
      (define pname (and (uir-symbol? (uir-typed-param-name p))
                         (string->symbol (uir-symbol-name (uir-typed-param-name p)))))
      (when pname
        (define ann (uir-typed-param-type p))
        (define def (uir-typed-param-default p))
        (define pcls (or (and ann (or (bare-class-value ann class-defs
                                                        tokens import-env comp)
                                      ;; Seed a propagated stdlib/module-marker
                                      ;; receiver type like an annotation.
                                      (tracked-receiver-type?
                                       (bare-name-symbol ann) tokens)))
                         (and (not ann) def (value-class def))))
        (when pcls (hash-set! locals pname pcls)))))
  ;; Declared-type projection for an annotated binding (`self.attr: KnownClass
  ;; = ...`, lowered to `uir-ann-set!`): a bare annotation naming a tracked
  ;; receiver class (corpus, pure stdlib value type, imported class, or a
  ;; tracked stdlib/module marker) types the target when the initializer does
  ;; not. A subscripted/generic annotation (`list[Item]`) names a container,
  ;; not its element, so it is deliberately not unwrapped.
  (define (annotation-class ann)
    (annotation-class-of ann class-defs tokens import-env comp))
  ;; Python `uir-set!` and JS/TS `=` assignments feed one target rule; the
  ;; caller supplies the value projection, host implicit-receiver name test and
  ;; env setters.
  (apply-assignments! body value-class
                      (λ (n) (tokens-self-name? tokens n))
                      (λ (nm cls) (hash-set! locals nm cls))
                      (λ (attr cls) (hash-set! self-attrs attr cls))
                      annotation-class)
  ;; `with ctx() as name:` binds the context manager's RESULT type: track the
  ;; target so `with ThreadPoolExecutor() as executor` types `executor` and a
  ;; later `executor.submit(...)` resolves instead of staying `unknown`.
  (define (process-with! n)
    (for ([item (in-list (uir-with-items n))])
      (when (and (pair? item) (pair? (cdr item)))
        (define target (second item))
        (define nm (cond
                     [(uir-var? target)
                      (string->symbol (uir-symbol-name (uir-var-name target)))]
                     [(uir-symbol? target)
                      (string->symbol (uir-symbol-name target))]
                     [else #f]))
        (when nm
          (define cls (value-class (first item)))
          (when cls (hash-set! locals nm cls))))))
  (walk-body body (λ (n) (when (uir-with? n) (process-with! n))))
  (receiver-types locals self-attrs))

;; Overlay `overlay`'s entries onto `base` (overlay wins on a key collision).
(define (hash-overlay base overlay)
  (define out (hash-copy base))
  (for ([(k v) (in-hash overlay)]) (hash-set! out k v))
  out)

;; Merge two receiver-type environments; `overlay`'s locals and self-attrs win.
;; Used to layer a function's own receiver tracking over its class's inherited
;; self-attributes (collected across fields, __init__ and all methods).
(define (receiver-types-merge base overlay)
  (receiver-types
   (hash-overlay (receiver-types-locals base) (receiver-types-locals overlay))
   (hash-overlay (receiver-types-self-attrs base) (receiver-types-self-attrs overlay))))
