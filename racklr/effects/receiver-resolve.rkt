#lang racket

;; racklr/effects/receiver-resolve.rkt
;;
;; Effect resolution helpers for the whole-program classifier: given a call's
;; receiver (an imported-singleton module, a tracked class, a corpus class, an
;; imported class outside the corpus, a manager attribute, or a nested
;; attribute chain), derive the typed effect capability from the call chain —
;; never from a library-name table in this module.
;;
;; The library knowledge arrives as the injected lexicon: the token tables
;; (`effect-tokens`, including the receiver-type method table) and the
;; import-tag config + operation vocabulary (`lexicon`). Every helper takes an
;; optional `[lex default-lexicon]`; the default is the Python/Django lexicon.
;;
;; Split out of classify-all.rkt to keep both modules under the 400-line
;; obligation.

(require racklr/uir
         racklr/effects/imports
         racklr/effects/shape
         racklr/effects/class-resolve
         racklr/effects/receiver-type
         racklr/effects/effect-roots
         racklr/effects/library-tokens
         racklr/effects/lexicon)

(provide
 imported-effect imported-fn-effect module-receiver-effect
 module-path-marker-effect receiver-effect imported-class-cap manager-effect
 pure-dunder-name? exception-name?
 imported-parent-effect super-stdlib-fallback self-stdlib-fallback
 receiver-domain-effect)

;; ── import-graph effect resolution ──────────────────────────────────

;; Resolve an imported-name receiver through the import graph to an effect
;; capability (or 'pure). A `name.m(...)` call whose `name` was bound by an
;; import to a module or class resolves structurally instead of by name
;; heuristics:
;;   module handle → the module's effect domain (coarse), or 'pure if tagged so
;;   class  handle → Class.method's derived cap, else the class's module domain
;; Returns an effect-cap, 'pure, or #f (not an imported-name call).
(define (imported-effect callee comp import-env [lex default-lexicon])
  (and import-env
       (let* ([config (lexicon-import-config lex)]
              [modpath (hash-ref (import-env-comp-modpath import-env) comp #f)]
              [exports (import-env-exports import-env)]
              [caps (import-env-caps import-env)]
              [mname (and (member-access-callee? callee) (member-access-field callee))]
              ;; Root of the chain (`connection` in
              ;; `connection.introspection.table_names`), not just the immediate
              ;; base: a deeper imported-singleton chain classifies by the same
              ;; module-domain fallback as `connection.table_names`.
              [root (and (member-access-callee? callee) (dotted-root callee))])
         (and modpath mname root
              (not (tokens-self-name? (lexicon-tokens lex) root))
              (let* ([e (hash-ref exports modpath #f)]
                     [h (and e (hash-ref e (symbol->string root) #f))])
                (and (handle? h)
                     (case (handle-kind h)
                       [(class)
                        (define cap (hash-ref caps (format "~a.~a" (handle-name h) mname) #f))
                        (cond
                          [cap cap]
                          [else
                           (define dom (handle-effect-domain (handle-path h)
                                                             (handle-name h)
                                                             (and mname (symbol->string mname))
                                                             config))
                           (cond [(eq? dom 'pure) 'pure]
                                 [dom (effect-cap dom (method-ops dom (handle-path h)
                                                                  (handle-name h)
                                                                  (symbol->string mname)
                                                                  lex))]
                                 [else #f])])]
                       ;; An external lowercase binding (`from django.db
                       ;; import router` / `connections`) synthesizes an fn
                       ;; handle whose path is the source module: a METHOD call
                       ;; on that singleton (`router.allow_migrate_model`) still
                       ;; resolves to the module's domain (django.db -> db), the
                       ;; same coarse library fallback as a module handle.
                       [(module fn)
                        (define dom (handle-effect-domain (handle-path h)
                                                          (handle-name h)
                                                          (and mname (symbol->string mname))
                                                          config))
                        (cond [(eq? dom 'pure) 'pure]
                              [dom (effect-cap dom '())]
                              [else #f])]
                       [else #f])))))))

;; Effect of a `Class.method` call whose receiver's class was tracked by
;; receiver-type dataflow (`x = KnownClass(...)` / `self.attr = KnownClass(...)`):
;; resolve the class to its defining component and derive the typed capability
;; from that component's module domain. 'pure when the class's module is tagged
;; pure (a pure receiver's method cannot be effectful); #f when the class is
;; unresolvable or its module untagged (falls through to the name heuristics).
;; A bare call to an imported FUNCTION from an unscanned module
;; (`get_migration_name_timestamp()` from django.db.migrations.utils): resolve
;; the module's effect domain. The fn handle is synthesized only for external
;; bindings; a scanned module's functions resolve as corpus edges earlier, and
;; a class handle never reaches here (bare class calls are pure constructors).
(define (imported-fn-effect nm comp import-env [lex default-lexicon])
  (and import-env nm
       (let* ([config (lexicon-import-config lex)]
              [modpath (hash-ref (import-env-comp-modpath import-env) comp #f)]
              [e (and modpath (hash-ref (import-env-exports import-env) modpath #f))]
              [h (and e (hash-ref e (symbol->string nm) #f))])
         (and (handle? h)
              (eq? (handle-kind h) 'fn)
              (handle-path h)
              (let ([dom (handle-effect-domain (handle-path h)
                                               (handle-name h)
                                               (symbol->string nm)
                                               config)])
                (cond
                  [(eq? dom 'pure) 'pure]
                  [dom (effect-cap dom '())]
                  [else #f]))))))

;; Effect domain of a tracked MODULE receiver (`pylibmc`) or a module-qualified
;; class marker (`pylibmc.Client`): the module's receiver-modules domain. This
;; resolves `self._lib.get(...)` (self._lib = pylibmc) and `self._cache.set(...)`
;; (self._cache = pylibmc.Client(...)) to the memcache client's cache domain
;; BEFORE the name-only method fallback would tag `get`/`delete` as db.
;; Returns an effect-cap or #f (not a module receiver).
(define (module-receiver-effect rcls [tokens default-effect-tokens])
  (and rcls
       (let* ([s (symbol->string rcls)]
              [m (regexp-match #rx"^([^.]+)" s)]
              [mod (and m (cadr m))]
              [domain (or (hash-ref (effect-tokens-receiver-modules tokens) s #f)
                          (and mod
                               (hash-ref (effect-tokens-receiver-modules tokens)
                                         mod #f)))])
         (and domain (effect-cap domain '())))))

;; Effect domain of a module-PATH marker — a dotted symbol like `django.db`
;; tracked for a local assigned from a subscript into an imported singleton
;; (`connection = connections[database]`). The marker is not a class name (a
;; class symbol never carries a dot) and not a module-receiver token
;; (`pylibmc.Client`, handled earlier by module-receiver-effect): its domain is
;; the module's effect tag, the same coarse fallback as an imported-singleton
;; method call. Returns an effect-cap, 'pure, or #f.
(define (module-path-marker-effect rcls mname import-env [lex default-lexicon])
  (and rcls mname import-env
       (let ([s (symbol->string rcls)])
         (and (string-contains? s ".")
              (let ([dom (handle-effect-domain s "" (symbol->string mname)
                                               (lexicon-import-config lex))])
                (cond
                  [(eq? dom 'pure) 'pure]
                  [dom (effect-cap dom '())]
                  [else #f]))))))

(define (receiver-effect comp class-name mname class-defs import-env
                         [lex default-lexicon])
  (and import-env
       (or (let ([c (resolve-class-component class-name comp class-defs)])
             (and c
                  (let ([modpath (hash-ref (import-env-comp-modpath import-env) c #f)])
                    (and modpath
                         (let ([domain (handle-effect-domain modpath
                                                           (symbol->string class-name)
                                                           (symbol->string mname)
                                                           (lexicon-import-config lex))])
                           (cond
                             [(eq? domain 'pure) 'pure]
                             [domain (effect-cap domain (method-ops domain modpath
                                                                    (symbol->string class-name)
                                                                    (symbol->string mname)
                                                                    lex))]
                             [else #f]))))))
           (imported-class-cap comp class-name mname import-env lex))))

;; A tracked receiver whose class is NOT in the scanned corpus (`x =
;; MigrationLoader(...)` where MigrationLoader is imported from django.db): the
;; class name resolves through the component's import namespace to a class
;; handle, and the capability derives from the handle's module domain +
;; method-ops — the effect half of imported-constructor tracking.
(define (imported-class-cap comp class-name mname import-env [lex default-lexicon])
  (and import-env class-name mname
       (let* ([config (lexicon-import-config lex)]
              [modpath (hash-ref (import-env-comp-modpath import-env) comp #f)]
              [e (and modpath (hash-ref (import-env-exports import-env) modpath #f))]
              [h (and e (hash-ref e (symbol->string class-name) #f))])
         (and (handle? h)
              (eq? (handle-kind h) 'class)
              (handle-path h)
              (let ([dom (handle-effect-domain (handle-path h)
                                               (handle-name h)
                                               (symbol->string mname)
                                               config)])
                (cond
                  [(eq? dom 'pure) 'pure]
                  [dom (effect-cap dom (method-ops dom (handle-path h)
                                                   (handle-name h)
                                                   (symbol->string mname)
                                                   lex))]
                  [else #f]))))))

;; A dotted call rooted at a known class name (`X.objects.Y`): the manager /
;; related attribute is installed by a framework metaclass, not by any
;; assignment in a method body, so the receiver-type env cannot see it. Resolve
;; the class name to its defining module's domain — the same class→module→domain
;; resolution used for tracked receivers — so the capability is derived from the
;; call chain rather than from an attribute name table.
(define (manager-effect callee comp class-defs import-env [lex default-lexicon])
  (and import-env
       (member-access-callee? callee)
       (let* ([base (member-access-base callee)]
              [root (dotted-root callee)]
              [mname (effect-op-of-callee callee)])
         (and base root mname
              (member-access-callee? base)          ; X.attr.m, not X.m
              (not (tokens-self-name? (lexicon-tokens lex) root))
              (hash-has-key? class-defs root)
              (receiver-effect comp root mname class-defs import-env lex)))))

;; ── tracked receiver class → effect domain ──────────────────────────

;; Class→module→domain capability for a tracked receiver class, or #f. This is
;; the coarse fallback: only the receiver class's home module names the domain.
(define (receiver-domain-effect comp rcls mname class-defs import-env
                                [lex default-lexicon])
  (and rcls mname
       (or (module-receiver-effect rcls (lexicon-tokens lex))
           (module-path-marker-effect rcls mname import-env lex)
           (receiver-effect comp rcls mname class-defs import-env lex))))

;; ── protocol fallbacks ──────────────────────────────────────────────

;; A pure protocol method (`__init__`, `__str__`, ...) — the object protocol.
(define (pure-dunder-name? mname [tokens default-effect-tokens])
  (and mname
       (set-member? (effect-tokens-protocol-method-names tokens)
                    (symbol->string mname))))

;; Exception-class name (suffix heuristic + host exception names), reused from
;; the pure-roots logic to resolve `super().__init__` on an exception base.
(define (exception-name? p [tokens default-effect-tokens])
  (and p
       (let ([s (symbol->string p)])
         (or (for/or ([suffix (in-set (tokens-exception-suffixes tokens))])
               (string-suffix? s suffix))
             (set-member? (effect-tokens-pure-constructor-names tokens) s)))))

;; `self.m()` / `super().m()` inherited from a NON-corpus parent class that was
;; imported from an unscanned framework module (`ASGIRequest(HttpRequest)` with
;; HttpRequest from django.http.request): resolve the parent's class handle
;; through the import namespace and derive the capability from its module
;; domain. The imported-class analogue of the stdlib parent fallback.
(define (imported-parent-effect comp p mname import-env [lex default-lexicon])
  (and import-env p mname
       (let* ([config (lexicon-import-config lex)]
              [modpath (hash-ref (import-env-comp-modpath import-env) comp #f)]
              [e (and modpath (hash-ref (import-env-exports import-env) modpath #f))]
              [h (and e (hash-ref e (symbol->string p) #f))])
         (and (handle? h)
              (eq? (handle-kind h) 'class)
              (handle-path h)
              (let ([dom (handle-effect-domain (handle-path h)
                                               (handle-name h)
                                               (symbol->string mname)
                                               config)])
                (cond
                  [(eq? dom 'pure) 'pure]
                  [dom (effect-cap dom (method-ops dom (handle-path h)
                                                   (handle-name h)
                                                   (symbol->string mname)
                                                   lex))]
                  [else #f]))))))

;; `super().m()` that resolved to no corpus edge: classify by the parent chain.
;; A corpus parent contributes nothing here (its methods were already tried);
;; a non-corpus parent resolves through its stdlib type table or the exception
;; protocol. When every parent is a corpus class that does not define `m`, the
;; target is the object root, whose protocol methods are pure.
(define (super-stdlib-fallback callee comp owner comp-supers class-defs import-env
                               [lex default-lexicon])
  (and (member-access-callee? callee)
       (let* ([tokens (lexicon-tokens lex)]
              [base (member-access-base callee)]
              [mname (member-access-field callee)]
              [parents (hash-ref comp-supers (cons comp owner) '())])
         (and (or (super-call? base)
                  ;; JS/TS `super.m()`: the `super` keyword lowers to a bare
                  ;; symbol, unlike the Python `super()` invocation.
                  (eq? (bare-name-symbol base) 'super))
              mname
              (or (for/or ([pr (in-list parents)])
                    (cond
                      [(car pr) #f]
                      [else
                       (let ([p (cdr pr)])
                         (or (stdlib-type-method-effect p mname tokens)
                             (imported-parent-effect comp p mname import-env lex)
                             (and (exception-name? p tokens)
                                  (pure-dunder-name? mname tokens) 'pure)))]))
                  (and (pure-dunder-name? mname tokens)
                       (andmap car parents)
                       'pure))))))

;; `self.m()` / `cls.m()` that resolved to no corpus edge: classify through a
;; NON-corpus parent's stdlib/framework type table. `self.fail()` on a
;; `unittest.TestCase` subclass is the dominant case — the method is inherited
;; from a stdlib class outside the scanned tree, so the MRO edge lookup misses
;; it, but the parent's `(cons #f name)` super-ref still names the type.
(define (self-stdlib-fallback callee comp owner comp-supers import-env
                              [lex default-lexicon])
  (and owner (member-access-callee? callee)
       (let* ([tokens (lexicon-tokens lex)]
              [mname (member-access-field callee)]
              [base (member-access-base callee)]
              [base-name (bare-name-symbol base)])
         (and mname (tokens-self-name? tokens base-name)
              (for/or ([pr (in-list (hash-ref comp-supers (cons comp owner) '()))])
                (and (not (car pr))
                     (let ([p (cdr pr)])
                       (or (stdlib-type-method-effect p mname tokens)
                           (imported-parent-effect comp p mname import-env lex)
                           (and (exception-name? p tokens)
                                (pure-dunder-name? mname tokens) 'pure)))))))))
