#lang racket

;; racklr/effects/classify-all.rkt
;;
;; Whole-program callee classification: after intra-program edge resolution has
;; failed, classify a call callee structurally — imported-name receivers, the
;; receiver-type dataflow cascade, stdlib/framework parent fallbacks, manager
;; attributes — before deferring to the name heuristics in classify.rkt. Split
;; out of whole-program.rkt so that module keeps only the fixpoint driver.

(require racklr/uir
         racklr/effects/imports
         racklr/effects/shape
         racklr/effects/class-resolve
         racklr/effects/classify
         racklr/effects/receiver-type
         racklr/effects/effect-roots
         racklr/effects/library-tokens)

(provide classify-call-all method-cap)

;; Resolve an imported-name receiver through the import graph to an effect
;; capability (or 'pure). A `name.m(...)` call whose `name` was bound by an
;; import to a module or class resolves structurally instead of by name
;; heuristics:
;;   module handle → the module's effect domain (coarse), or 'pure if tagged so
;;   class  handle → Class.method's derived cap, else the class's module domain
;; Returns an effect-cap, 'pure, or #f (not an imported-name call).
(define (imported-effect callee comp import-env)
  (and import-env
       (let* ([modpath (hash-ref (import-env-comp-modpath import-env) comp #f)]
              [exports (import-env-exports import-env)]
              [caps (import-env-caps import-env)]
              [mname (and (uir-get? callee) (get-field-symbol (uir-get-field callee)))]
              [base (and (uir-get? callee) (uir-get-base callee))]
              [base-name (and base (uir-var? base)
                              (string->symbol (uir-symbol-name (uir-var-name base))))])
         (and modpath mname base-name
              (not (memq base-name '(self cls)))
              (let* ([e (hash-ref exports modpath #f)]
                     [h (and e (hash-ref e (symbol->string base-name) #f))])
                (and (handle? h)
                     (case (handle-kind h)
                       [(class)
                        (define cap (hash-ref caps (format "~a.~a" (handle-name h) mname) #f))
                        (cond
                          [cap cap]
                          [else
                           (define dom (module-effect-tag (handle-path h)))
                           (cond [(eq? dom 'pure) 'pure]
                                 [dom (effect-cap dom (method-ops dom (handle-path h)
                                                                  (handle-name h)
                                                                  (symbol->string mname)))]
                                 [else #f])])]
                       ;; An external lowercase binding (`from django.db
                       ;; import router` / `connections`) synthesizes an fn
                       ;; handle whose path is the source module: a METHOD call
                       ;; on that singleton (`router.allow_migrate_model`) still
                       ;; resolves to the module's domain (django.db -> db), the
                       ;; same coarse library fallback as a module handle.
                       [(module fn)
                        (define dom (module-effect-tag (handle-path h)))
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
(define (imported-fn-effect nm comp import-env)
  (and import-env nm
       (let* ([modpath (hash-ref (import-env-comp-modpath import-env) comp #f)]
              [e (and modpath (hash-ref (import-env-exports import-env) modpath #f))]
              [h (and e (hash-ref e (symbol->string nm) #f))])
         (and (handle? h)
              (eq? (handle-kind h) 'fn)
              (handle-path h)
              (let ([dom (module-effect-tag (handle-path h))])
                (cond
                  [(eq? dom 'pure) 'pure]
                  [dom (effect-cap dom '())]
                  [else #f]))))))

(define (receiver-effect comp class-name mname class-defs import-env)
  (and import-env
       (or (let ([c (resolve-class-component class-name comp class-defs)])
             (and c
                  (let ([modpath (hash-ref (import-env-comp-modpath import-env) c #f)])
                    (and modpath
                         (let ([domain (module-effect-tag modpath)])
                           (cond
                             [(eq? domain 'pure) 'pure]
                             [domain (effect-cap domain (method-ops domain modpath
                                                                    (symbol->string class-name)
                                                                    (symbol->string mname)))]
                             [else #f]))))))
           (imported-class-cap comp class-name mname import-env))))

;; A tracked receiver whose class is NOT in the scanned corpus (`x =
;; MigrationLoader(...)` where MigrationLoader is imported from django.db): the
;; class name resolves through the component's import namespace to a class
;; handle, and the capability derives from the handle's module domain +
;; method-ops — the effect half of imported-constructor tracking.
(define (imported-class-cap comp class-name mname import-env)
  (and import-env class-name mname
       (let* ([modpath (hash-ref (import-env-comp-modpath import-env) comp #f)]
              [e (and modpath (hash-ref (import-env-exports import-env) modpath #f))]
              [h (and e (hash-ref e (symbol->string class-name) #f))])
         (and (handle? h)
              (eq? (handle-kind h) 'class)
              (handle-path h)
              (let ([dom (module-effect-tag (handle-path h))])
                (cond
                  [(eq? dom 'pure) 'pure]
                  [dom (effect-cap dom (method-ops dom (handle-path h)
                                                   (handle-name h)
                                                   (symbol->string mname)))]
                  [else #f]))))))

;; A dotted call rooted at a known class name (`X.objects.Y`): the manager /
;; related attribute is installed by a framework metaclass, not by any
;; assignment in a method body, so the receiver-type env cannot see it. Resolve
;; the class name to its defining module's domain — the same class→module→domain
;; resolution used for tracked receivers — so the capability is derived from the
;; call chain rather than from a `objects` name table.
(define (manager-effect callee comp class-defs import-env)
  (and import-env
       (member-access-callee? callee)
       (let* ([base (member-access-base callee)]
              [root (dotted-root callee)]
              [mname (effect-op-of-callee callee)])
         (and base root mname
              (member-access-callee? base)          ; X.attr.m, not X.m
              (not (memq root '(self cls)))
              (hash-has-key? class-defs root)
              (receiver-effect comp root mname class-defs import-env)))))

;; Attribute `attr` of class `class-name`, resolved across defining components
;; and the superclass chain (most-derived first); #f when the attribute is not
;; tracked. The per-class attribute graph walk that lets a receiver chain cross
;; an object boundary (`self.a.b.m` → class of `a` → class of `a.b`).
(define (class-attr-lookup comp class-name attr class-defs class-attrs comp-supers)
  (define start (resolve-class-component class-name comp class-defs))
  (define seen (make-hash))
  (let loop ([c start] [cls class-name])
    (cond
      [(not (and c cls)) #f]
      [(hash-has-key? seen (cons c cls)) #f]
      [else
       (hash-set! seen (cons c cls) #t)
       (let ([val (hash-ref (hash-ref class-attrs (cons c cls) (hash)) attr #f)])
         (or val
             (for/or ([sr (in-list (hash-ref comp-supers (cons c cls) '()))])
               (let* ([c2 (car sr)] [s (cdr sr)])
                 (and c2 s (loop c2 s))))))])))

;; Class an arbitrary receiver expression (a bare variable, or a nested
;; `a.b.c` get-chain) resolves to. `self.a` / `cls.a` read the merged owner
;; attribute env; `expr.attr` where `expr` resolves to class C reads C's own
;; attribute graph (with MRO), so a `self.a.b.m(...)` chain resolves through two
;; objects instead of falling to `unknown`.
(define (receiver-chain-class rt base comp class-defs class-attrs comp-supers
                              [owner #f] [return-types #f]
                              [methods #f] [fn-defs #f] [known #f])
  (cond
    [(uir-var? base)
     (hash-ref (receiver-types-locals rt)
               (string->symbol (uir-symbol-name (uir-var-name base))) #f)]
    [(uir-get? base)
     (define attr (get-field-symbol (uir-get-field base)))
     (define inner (uir-get-base base))
     (cond
       [(not attr) #f]
       [(and (uir-var? inner)
             (memq (string->symbol (uir-symbol-name (uir-var-name inner))) '(self cls)))
        ;; An assigned self/cls attribute wins; when untracked, fall back to a
        ;; METHOD of the enclosing class whose return type was projected
        ;; (`@cached_property _cache` returning `self._class(...)`), so
        ;; `self._cache.set(...)` resolves to the cached property's result
        ;; class instead of `unknown`.
        (or (hash-ref (receiver-types-self-attrs rt) attr #f)
            (and owner return-types attr
                 (hash-ref return-types
                           (cons comp (qualified-name-symbol owner attr)) #f)))]
       [else
        (define c (receiver-chain-class rt inner comp class-defs class-attrs comp-supers
                                        owner return-types methods fn-defs known))
        (and c (class-attr-lookup comp c attr class-defs class-attrs comp-supers))])]
    [(uir-call? base)
     ;; A `.attr` chain rooted at a call result (`Path(...).is_absolute`,
     ;; `md5(...).hexdigest`, `open(...).close`): the result type is the class
     ;; its callee names, or — for a corpus function/method call — the class
     ;; its callee's projected return type names.
     (or (corpus-call-return-type (uir-call-callee base) comp class-defs owner methods fn-defs known comp-supers return-types)
         (call-result-type (uir-call-callee base) class-defs))]
    [else #f]))

;; A pure protocol dunder (`__init__`, `__str__`, ...) — the object protocol.
(define (pure-dunder-name? mname)
  (and mname
       (set-member? (effect-tokens-pure-dunder-names default-effect-tokens)
                    (symbol->string mname))))

;; Exception-class name (suffix heuristic + framework exception names), reused
;; from the pure-roots logic to resolve `super().__init__` on an exception base.
(define (exception-name? p)
  (and p
       (let ([s (symbol->string p)])
         (or (string-suffix? s "Error")
             (string-suffix? s "Exception")
             (string-suffix? s "Warning")
             (set-member? (effect-tokens-framework-exception-names default-effect-tokens) s)))))

;; `self.m()` / `super().m()` inherited from a NON-corpus parent class that was
;; imported from an unscanned framework module (`ASGIRequest(HttpRequest)` with
;; HttpRequest from django.http.request): resolve the parent's class handle
;; through the import namespace and derive the capability from its module
;; domain. The imported-class analogue of the stdlib parent fallback.
(define (imported-parent-effect comp p mname import-env)
  (and import-env p mname
       (let* ([modpath (hash-ref (import-env-comp-modpath import-env) comp #f)]
              [e (and modpath (hash-ref (import-env-exports import-env) modpath #f))]
              [h (and e (hash-ref e (symbol->string p) #f))])
         (and (handle? h)
              (eq? (handle-kind h) 'class)
              (handle-path h)
              (let ([dom (module-effect-tag (handle-path h))])
                (cond
                  [(eq? dom 'pure) 'pure]
                  [dom (effect-cap dom (method-ops dom (handle-path h)
                                                   (handle-name h)
                                                   (symbol->string mname)))]
                  [else #f]))))))

;; `super().m()` that resolved to no corpus edge: classify by the parent chain.
;; A corpus parent contributes nothing here (its methods were already tried);
;; a non-corpus parent resolves through its stdlib type table or the exception
;; protocol. When every parent is a corpus class that does not define `m`, the
;; target is `object`, whose protocol dunders are pure.
(define (super-stdlib-fallback callee comp owner comp-supers class-defs import-env)
  (and (uir-get? callee) (super-call? (uir-get-base callee))
       (let* ([mname (get-field-symbol (uir-get-field callee))]
              [parents (hash-ref comp-supers (cons comp owner) '())])
         (and mname
              (or (for/or ([pr (in-list parents)])
                    (cond
                      [(car pr) #f]
                      [else
                       (let ([p (cdr pr)])
                         (or (stdlib-type-method-effect p mname)
                             (imported-parent-effect comp p mname import-env)
                             (and (exception-name? p) (pure-dunder-name? mname) 'pure)))]))
                  (and (pure-dunder-name? mname)
                       (andmap car parents)
                       'pure))))))

;; `self.m()` / `cls.m()` that resolved to no corpus edge: classify through a
;; NON-corpus parent's stdlib/framework type table. `self.fail()` on a
;; `unittest.TestCase` subclass is the dominant case — the method is inherited
;; from a stdlib class outside the scanned tree, so the MRO edge lookup misses
;; it, but the parent's `(cons #f name)` super-ref still names the type.
(define (self-stdlib-fallback callee comp owner comp-supers import-env)
  (and owner (uir-get? callee)
       (let* ([mname (get-field-symbol (uir-get-field callee))]
              [base (uir-get-base callee)]
              [base-name (and (uir-var? base)
                              (string->symbol (uir-symbol-name (uir-var-name base))))])
         (and mname (memq base-name '(self cls))
              (for/or ([pr (in-list (hash-ref comp-supers (cons comp owner) '()))])
                (and (not (car pr))
                     (let ([p (cdr pr)])
                       (or (stdlib-type-method-effect p mname)
                           (imported-parent-effect comp p mname import-env)
                           (and (exception-name? p) (pure-dunder-name? mname) 'pure)))))))))

(define (classify-call-all callee known methods class-defs fn-defs comp-supers class-attrs comp owner [import-env #f] [rt #f] [return-types #f])
  (define nm (callee-name callee))
  (define keys (method-callee-keys callee comp owner methods class-defs comp-supers))
  (define key (for/or ([k (in-list keys)] #:when (hash-has-key? known k)) k))
  (cond
    [(and key (hash-has-key? known key)) (cons 'edge key)]
    [(bare-function-key nm comp fn-defs known) => (λ (k) (cons 'edge k))]
    [else
     ;; A locally-tracked receiver (x = KnownClass(...)) shadows any import of
     ;; the same name and, more importantly, wins over the constructor
     ;; heuristic below: `loader.get_migration_by_prefix(...)` is a METHOD on a
     ;; tracked MigrationLoader, not a construction of the (pure-bare-named)
     ;; `get_migration_by_prefix` class.
     (define rcls (and rt (uir-get? callee)
                        (receiver-chain-class rt (uir-get-base callee)
                                              comp class-defs class-attrs comp-supers
                                              owner return-types methods fn-defs known)))
     (define mname (and (uir-get? callee) (get-field-symbol (uir-get-field callee))))
     ;; A call to a known class name is a constructor (pure instantiation), not
     ;; an effectful call — this also seeds the receiver-type dataflow without
     ;; polluting the enclosing function with `unknown`. A class imported from
     ;; an unscanned module (`from django.db.migrations.loader import
     ;; MigrationLoader`) is equally pure to construct: the import graph knows
     ;; it is a class, even though its body is outside the corpus.
     (define ctor (and (not rcls)
                       (or (constructor-class callee class-defs)
                           (and nm comp import-env
                                (imported-class-ref nm comp import-env)))))
     (cond
       [ctor (cons 'pure #f)]
       ;; A tracked class-valued callee (`loader_class()` / `qs_class()` where
       ;; the local was bound to a class): construction is pure, mirroring
       ;; `KnownClass(...)`.
       [(and rt (receiver-class-of rt callee)) (cons 'pure #f)]
       [else
        (define re (and rcls mname (receiver-effect comp rcls mname class-defs import-env)))
        (cond
          [(eq? re 'pure) (cons 'pure #f)]
          [(effect-cap? re) (cons 'effect re)]
          [else
           ;; stdlib/builtin receiver type: a call-result chain
           ;; (`Path(...).is_absolute`) or a tracked local (`x = open(...)`)
           ;; whose type has a method-table entry.
           (define sre (and rcls mname (stdlib-type-method-effect rcls mname)))
           (cond
             [(eq? sre 'pure) (cons 'pure #f)]
             [sre (cons 'effect sre)]
             [else
              ;; `super().m()` with no corpus edge: parent-type classification.
              (define sfe (super-stdlib-fallback callee comp owner comp-supers class-defs import-env))
              (cond
                [(eq? sfe 'pure) (cons 'pure #f)]
                [sfe (cons 'effect sfe)]
                [else
                 ;; `self.m()` / `cls.m()` inherited from a NON-corpus parent
                 ;; (`unittest.TestCase.fail`): classify through its stdlib table.
                 (define sse (self-stdlib-fallback callee comp owner comp-supers import-env))
                 (cond
                   [(eq? sse 'pure) (cons 'pure #f)]
                   [sse (cons 'effect sse)]
                   [else
                    (define me (manager-effect callee comp class-defs import-env))
            (cond
              [(eq? me 'pure) (cons 'pure #f)]
              [(effect-cap? me) (cons 'effect me)]
              [else
               (define ie (imported-effect callee comp import-env))
               (cond
                 [(eq? ie 'pure) (cons 'pure #f)]
                 [(effect-cap? ie) (cons 'effect ie)]
                 [else
                  ;; A tracked receiver whose class is a corpus class in an
                  ;; untagged module has no library domain. Only upgrade a
                  ;; name-heuristic `unknown`: a known effect or pure stays
                  ;; (the name tables are still the authority for effect names
                  ;; on arbitrary receivers). This turns `build_file.cleanup()`
                  ;; into an edge to `BuildFile.cleanup` without overriding a
                  ;; real email/db/io classification.
                  (define bfi (and (not (uir-get? callee)) nm comp
                                   (imported-fn-effect nm comp import-env)))
                  (define nn (cond
                               [(eq? bfi 'pure) (cons 'pure #f)]
                               [(effect-cap? bfi) (cons 'effect bfi)]
                               [else (classify-nonedge callee)]))
                  (cond
                    [(eq? (car nn) 'unknown)
                     (define rc (and rcls mname (resolve-class-component rcls comp class-defs)))
                     (define rkey
                       (and rc (for/or ([k (in-list (mro-method-keys rc rcls mname methods class-defs comp-supers))]
                                       #:when (hash-has-key? known k))
                                 k)))
                     (cond
                       [(and rkey (hash-has-key? known rkey)) (cons 'edge rkey)]
                       [else nn])]
                    [else nn])])])])])])])])]))

;; The derived capability for a call-graph method key `(component . Class.method)`,
;; resolved COMPONENT-AWARE: the component's own module path supplies the domain,
;; so same-named classes in different modules do not collide (unlike the bare-key
;; caps table). Returns an effect-cap or #f. This is the "blessed summary" from
;; the per-library config — a typed op like `db<Model>.write` is asserted at the
;; method's own definition site, so it flows through the fixpoint to every
;; caller even when the method body descends into unclassified plumbing.
(define (method-cap k import-env)
  (and import-env
       (let* ([comp (car k)]
              [modpath (hash-ref (import-env-comp-modpath import-env) comp #f)])
         (and modpath
              (let* ([s (symbol->string (cdr k))]
                     [cls-str (regexp-replace #rx"[.].*$" s "")]
                     [m-str (regexp-replace #rx"^[^.]*[.]" s "")])
                (and (not (string=? cls-str s))
                     (let ([domain (module-effect-tag modpath)])
                       (and domain
                            (not (eq? domain 'pure))
                            (effect-cap domain (method-ops domain modpath cls-str m-str))))))))))
