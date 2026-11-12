#lang racket

;; racklr/effects/type-relations.rkt
;;
;; Materialized type relations + the deterministic join that closes them.
;;
;; The whole-program receiver-type dataflow is a set of first-class relations,
;; not ad-hoc per-pass hashes:
;;
;;   assign    : (comp . fn)         -> { local -> class }
;;   self-attr : (comp . class)      -> { attr  -> class }
;;   param     : ((comp . fn) . idx) -> class
;;   return    : (comp . fn)         -> class
;;
;; `materialize-type-relations` seeds the syntax-visible self-attr relation
;; (class fields + `self.attr = ...` in method bodies). `close-type-relations!`
;; then runs the deterministic join: a monotone fixpoint that closes assign,
;; self-attr, param and return over the whole program. Iteration is over sorted
;; keys and every fold is a strict single-class fold, so the fixed point does
;; not depend on hash iteration order.

(require racklr/uir
         racklr/effects/class-resolve
         racklr/effects/collect
         racklr/effects/walk
         racklr/effects/shape
         racklr/effects/receiver-type
         racklr/effects/library-tokens
         racklr/effects/library-stdlib-types
         racklr/effects/imports
         racklr/effects/lexicon)

(provide type-relations? type-relations
         type-relations-assign type-relations-self-attr
         type-relations-param type-relations-return
         type-relations-elem-specs type-relations-globals
         type-relations-assign-fn type-relations-self-attr-fn
         set-type-relations-assign! set-type-relations-self-attr!
         set-type-relations-param! set-type-relations-return!
         set-type-relations-elem-specs!
         set-type-relations-assign-fn! set-type-relations-self-attr-fn!
         materialize-type-relations
         close-type-relations!
         class-attr-env
         module-globals
         seed-params)

(struct type-relations
  (assign self-attr param return elem-specs globals assign-fn self-attr-fn)
  #:transparent #:mutable)

(define (key<? a b)
  (string<? (format "~a.~a" (car a) (cdr a)) (format "~a.~a" (car b) (cdr b))))

;; Class name of a class-body field initializer (`attr = KnownClass` /
;; `attr = KnownClass(...)`); #f when the initializer does not name a known
;; class. (was field-init-class in whole-program.rkt)
(define (field-class init class-defs import-env comp [tokens default-effect-tokens])
  (cond
    [(bare-class-value init class-defs tokens import-env comp)]
    [(uir-call? init) (call-result-type (uir-call-callee init) class-defs
                                        tokens #f import-env comp)]
    [else #f]))

;; Class-level field initializers keyed by (comp . class) -> { attr -> class }.
(define (class-field-attrs comp-programs class-defs import-env
                           [tokens default-effect-tokens])
  (define h (make-hash))
  (for ([cp (in-list comp-programs)])
    (define comp (car cp))
    (let walk ([n (cdr cp)])
      (cond
        [(uir-class? n)
         (define owner
           (and (uir-class-name n) (uir-symbol? (uir-class-name n))
                (string->symbol (uir-symbol-name (uir-class-name n)))))
         (when owner
           (define key (cons comp owner))
           (define acc (hash-ref h key #f))
           (unless acc (set! acc (make-hash)) (hash-set! h key acc))
           (for ([f (in-list (uir-class-fields n))])
             (when (uir-field? f)
               (define attr (and (uir-field-name f) (uir-symbol? (uir-field-name f))
                                 (string->symbol (uir-symbol-name (uir-field-name f)))))
               (define cls (field-class (uir-field-init f) class-defs import-env comp tokens))
               (when (and attr cls) (hash-set! acc attr cls)))))]
        [(struct? n)
         (for ([f (in-list (cdr (vector->list (struct->vector n))))]) (walk f))]
        [(pair? n) (walk (car n)) (walk (cdr n))])))
  h)

;; Module-level name -> class, from top-level assignments (`_caches = {}` →
;; dict, `urlpatterns = [...]` → list). These are the free-name fallbacks every
;; function in the component shares: a function body that reads `_caches` (or
;; `_caches.setdefault(...)`) resolves the name to the module global's type.
;; Only top-level assignments are read (never a class/function body's locals),
;; and only STATIC projections — a literal, a known constructor, a class
;; reference — so the module-global table needs no fixpoint round of its own.
;; Call-result globals and cross-global chains stay unprojected until the
;; join (they are rare at module scope in the scanned corpora).
(define (module-globals program class-defs import-env comp
                        [tokens default-effect-tokens])
  (define h (make-hash))
  (define rt (receiver-types (hash) (hash) (hash) (hash) (hash) (hash)))
  (define (vc v) (value-class-of v rt class-defs tokens import-env comp #f))
  (let walk ([n program])
    (cond
      [(uir-block? n)
       (for ([s (in-list (uir-block-stmts n))])
         (cond
           [(uir-set!? s)
            (define lhs (uir-set!-name s))
            (define nm (and (or (uir-var? lhs) (uir-symbol? lhs))
                            (bare-name-symbol lhs)))
            (define cls (and nm (vc (uir-set!-value s))))
            (when (and nm cls) (hash-set! h nm cls))]
           [(uir-if? s) (walk (uir-if-then s)) (walk (uir-if-else s))]
           [else (void)]))]
      [else (void)]))
  h)

;; Wrap a propagated param type as a typed-param so the annotation-seeding path
;; in infer-receiver-types picks it up.
(define (seed-params params k param-types)
  (for/list ([i (in-naturals)] [p (in-list params)])
    (define cls (hash-ref param-types (cons k i) #f))
    (cond
      [(and cls (uir-symbol? p))
       (uir-typed-param p (uir-symbol (symbol->string cls)) #f #f)]
      [(and cls (uir-typed-param? p) (not (uir-typed-param-type p)))
       (uir-typed-param (uir-typed-param-name p)
                        (uir-symbol (symbol->string cls))
                        (uir-typed-param-default p)
                        (uir-typed-param-kind p))]
      [else p])))

;; Declared param signatures (library DATA) fill external-entry-point params
;; the corpus call chain could not type: a parameter supplied by a caller
;; OUTSIDE the scanned tree (WSGI/ASGI server, test client, mail backend,
;; database layer) has no in-corpus call site to project it. The table is keyed
;; (module-path . qualified-name) -> ((param-name . class-name) ...); this maps
;; it onto the (cons key idx) param relation and OVERLAYS it only where
;; call-site collection left the param untyped (call-chain evidence wins —
;; never overwritten by a declared fallback).
(define (seed-declared-params param all-keys params-of import-env declared-params)
  (define (param-name p)
    (cond
      [(uir-symbol? p) (string->symbol (uir-symbol-name p))]
      [(uir-typed-param? p)
       (and (uir-symbol? (uir-typed-param-name p))
            (string->symbol (uir-symbol-name (uir-typed-param-name p))))]
      [else #f]))
  (define out (hash-copy param))
  (for ([k (in-list all-keys)])
    (define modpath (and import-env
                         (hash-ref (import-env-comp-modpath import-env) (car k) #f)))
    (define entry (and modpath declared-params
                       (hash-ref declared-params (cons modpath (cdr k)) #f)))
    (when entry
      (define ps (params-of k))
      (define names (for/list ([p (in-list ps)]) (param-name p)))
      (for ([pd (in-list entry)])
        ;; for/or finds the first non-#f index; for/first would silently keep
        ;; the first iteration's value (see collect-params!'s note).
        (define idx (for/or ([i (in-naturals)] [n (in-list names)]
                             #:when (eq? n (car pd)))
                      i))
        (define kk (cons k idx))
        (define existing (and idx (hash-ref out kk #f)))
        (define declared (cdr pd))
        (when (and idx
                   (or (not existing)
                       ;; A declared CONCRETE class outranks a call-chain
                       ;; MODULE-PATH marker (`django.db`): the marker is the
                       ;; coarse singleton-domain fallback, while the declared
                       ;; class names the instance type a result projection
                       ;; needs (BaseDatabaseWrapper.schema_editor() ->
                       ;; BaseDatabaseSchemaEditor). Concrete-vs-concrete
                       ;; conflicts still keep call-chain evidence.
                       (and (symbol? existing) (symbol? declared)
                            (string-contains? (symbol->string existing) ".")
                            (not (string-contains? (symbol->string declared) ".")))))
          (hash-set! out kk declared)))))
  out)

;; Self-attribute assignments collected across every class: class-level field
;; initializers first, then `self.attr = KnownClass(...)` from every method
;; (which overlay the class-level defaults, mirroring instance attributes
;; shadowing class attributes). Keyed by (comp . class). Iteration is over
;; sorted method names, so a conflicting attr (two methods assign different
;; classes) resolves deterministically instead of by hash order.
(define (class-self-attrs comp-programs class-defs import-env
                          [param-types (hash)]
                          [return-types #f]
                          [methods #f] [fn-defs #f] [known #f] [comp-supers #f]
                          #:tokens [tokens default-effect-tokens]
                          #:globals [globals (hash)])
  (define h (class-field-attrs comp-programs class-defs import-env tokens))
  (define hfn (make-hash))  ;; (comp . class) -> { attr -> (cons class method) }
  (for ([cp (in-list comp-programs)])
    (define comp (car cp))
    (define mparams (collect-method-params (cdr cp)))
    (define cmethods (collect-methods (cdr cp)))
    (for ([n (in-list (sort (hash-keys cmethods) symbol<?))])
      (define body (hash-ref cmethods n))
      (define owner (class-name-of-name n))
      (define k (cons comp n))
      (define rt (infer-receiver-types body class-defs (hash) import-env comp
                                       (seed-params (hash-ref mparams n '()) k param-types)
                                       owner
                                       #:tokens tokens
                                       #:return-types return-types
                                       #:methods methods
                                       #:fn-defs fn-defs
                                       #:known known
                                       #:comp-supers comp-supers
                                       #:globals (hash-ref globals comp (hash))))
      (define sa (receiver-types-self-attrs rt))
      (define safn (receiver-types-fn-self-attrs rt))
      (define key (cons comp owner))
      (define cur (hash-ref h key #f))
      (cond
        [(not cur) (unless (zero? (hash-count sa)) (hash-set! h key (hash-copy sa)))]
        [else (for ([(attr kls) (in-hash sa)]) (hash-set! cur attr kls))])
      (define curfn (hash-ref hfn key #f))
      (cond
        [(not curfn) (unless (zero? (hash-count safn)) (hash-set! hfn key (hash-copy safn)))]
        [else (for ([(attr d) (in-hash safn)]) (hash-set! curfn attr d))])))
  (values h hfn))

;; Inherited self-attribute environment for a class, following the MRO across
;; files with the most-derived class first. Returns a receiver-types with no
;; locals (locals are per-function).
(define (class-attr-env comp class-name self-attr class-defs comp-supers
                        [self-attr-fn (hash)])
  (define seen (make-hash))
  (define acc (make-hash))
  (define accfn (make-hash))
  (let loop ([c comp] [cls class-name])
    (when (and c cls (not (hash-has-key? seen (cons c cls))))
      (hash-set! seen (cons c cls) #t)
      (for ([(attr kls) (in-hash (hash-ref self-attr (cons c cls) (hash)))])
        (unless (hash-has-key? acc attr) (hash-set! acc attr kls)))
      (for ([(attr d) (in-hash (hash-ref self-attr-fn (cons c cls) (hash)))])
        (unless (hash-has-key? accfn attr) (hash-set! accfn attr d)))
      (for ([sr (in-list (hash-ref comp-supers (cons c cls) '()))])
        (define c2 (car sr))
        (define s (cdr sr))
        (when (and c2 s) (loop c2 s)))))
  (receiver-types (hash) acc (hash) (hash) (hash) accfn))

;; Syntax-visible seed: the self-attr relation from class fields + method
;; bodies. assign/param/return are empty until the join runs.
(define (materialize-type-relations comp-programs class-defs import-env
                                    #:tokens [tokens default-effect-tokens])
  (define globals (make-hash))   ;; comp -> { name-symbol -> class-symbol }
  (for ([cp (in-list comp-programs)])
    (hash-set! globals (car cp)
               (module-globals (cdr cp) class-defs import-env (car cp) tokens)))
  (define-values (seed-sa seed-safn)
    (class-self-attrs comp-programs class-defs import-env (hash)
                      #:tokens tokens))
  (type-relations (make-hash)
                  seed-sa
                  (make-hash)
                  (make-hash)
                  (make-hash)
                  globals
                  (make-hash)
                  seed-safn))

;; Deterministic join: close assign / self-attr / param / return to a fixed
;; point. Each round recomputes all four relations from the current others;
;; sorted key iteration + strict single-class folds make the result
;; independent of hash order. Terminates by construction up to a cap.
(define (close-type-relations! tr
                               #:comp-programs comp-programs
                               #:class-defs class-defs
                               #:import-env import-env
                               #:fns fns
                               #:methods methods
                               #:fn-params fn-params
                               #:method-params method-params
                               #:fn-defs fn-defs
                               #:comp-supers comp-supers
                               #:fn-parents [fn-parents (hash)]
                               #:declared-params [declared-params #f]
                               #:declared-returns [declared-returns #f]
                               #:element-types [element-types #f]
                               #:param-element-types [param-element-types #f]
                               #:tokens [tokens default-effect-tokens])
  ;; Declared tables default to the library-stdlib-types DATA, the same way the
  ;; reference architecture did; an explicit #:declared-* arg (a caller-injected
  ;; lexicon) overrides it, so a non-Python host supplies its own instead of
  ;; editing the engine.
  (define declared-params* (or declared-params declared-param-types))
  (define declared-returns* (or declared-returns declared-return-types))
  (define element-types* (or element-types declared-element-types))
  (define param-element-types* (or param-element-types declared-param-element-types))
  (define assign (type-relations-assign tr))
  (define self-attr (type-relations-self-attr tr))
  (define param (type-relations-param tr))
  (define return (type-relations-return tr))
  (define elem-specs (type-relations-elem-specs tr))
  (define globals (type-relations-globals tr))
  (define assign-fn (type-relations-assign-fn tr))
  (define self-attr-fn (type-relations-self-attr-fn tr))

  (define all-keys (sort (append (hash-keys fns) (hash-keys methods)) key<?))
  (define known (make-hash))
  (for ([k (in-list all-keys)]) (hash-set! known k #t))

  (define (body-of k) (if (hash-has-key? fns k) (hash-ref fns k) (hash-ref methods k)))
  ;; Enclosing class of a call-graph node: a method's own class, or — for a
  ;; nested function — the class of the method frame it is defined in, so the
  ;; closure's implicit receiver dispatches through that class's MRO.
  (define (owner-of k)
    (or (and (hash-has-key? methods k) (class-name-of-name (cdr k)))
        (fn-owner-class (cdr k)
                        (λ (x) (hash-ref fn-parents (cons (car k) x) #f)))))
  (define (params-of k)
    (if (hash-has-key? fns k) (hash-ref fn-params k '()) (hash-ref method-params k '())))

  ;; Full per-key receiver env: locals from the assign relation, self-attrs from
  ;; the class's MRO-closed self-attr relation, elem-specs from the closed
  ;; per-key elem-spec relation.
  (define (rt-of k)
    (define owner (owner-of k))
    (define class-env
      (and owner (class-attr-env (car k) owner self-attr class-defs comp-supers
                                 self-attr-fn)))
    (receiver-types (hash-ref assign k (hash))
                    (or (and class-env (receiver-types-self-attrs class-env)) (hash))
                    (hash-ref elem-specs k (hash))
                    (hash-ref globals (car k) (hash))
                    (hash-ref assign-fn k (hash))
                    (or (and class-env (receiver-types-fn-self-attrs class-env)) (hash))))

  (define (rebuild-self-attrs!)
    (define-values (new-sa new-safn)
      (class-self-attrs comp-programs class-defs import-env param
                        return methods fn-defs known comp-supers
                        #:tokens tokens
                        #:globals globals))
    (set! self-attr new-sa)
    (set! self-attr-fn new-safn)
    (set-type-relations-self-attr! tr self-attr)
    (set-type-relations-self-attr-fn! tr self-attr-fn))

  ;; Closure-capture globals for a call-graph key: a nested function's free
  ;; variables resolve to its enclosing function frames' locals and typed
  ;; params (a nested `model_installed` reading the enclosing `sync_apps`'s
  ;; `connection` param), overlaid on the shared module globals. The innermost
  ;; enclosing frame wins over outer frames and module globals, matching the
  ;; host's lexical scoping. The enclosing locals/params come from the CURRENT
  ;; fixpoint round's relations, so the closure seed closes with them.
  (define (closure-globals-of k comp base-globals)
    (define out (hash-copy base-globals))
    (define seen (make-hash))
    (let loop ([nm (cdr k)])
      (define parent (and nm (hash-ref fn-parents (cons comp nm) #f)))
      (when (and parent (not (hash-has-key? seen parent)))
        (hash-set! seen parent #t)
        (define pk (cons comp parent))
        (for ([(n cls) (in-hash (hash-ref assign pk (hash)))])
          (unless (hash-has-key? out n) (hash-set! out n cls)))
        (for ([i (in-naturals)] [p (in-list (params-of pk))])
          (define pnm (cond
                        [(uir-symbol? p) (string->symbol (uir-symbol-name p))]
                        [(uir-typed-param? p)
                         (and (uir-symbol? (uir-typed-param-name p))
                              (string->symbol (uir-symbol-name (uir-typed-param-name p))))]
                        [else #f]))
          (define cls (hash-ref param (cons pk i) #f))
          (when (and pnm cls (not (hash-has-key? out pnm)))
            (hash-set! out pnm cls)))
        (loop parent)))
    out)

  (define (rebuild-assign!)
    (for ([k (in-list all-keys)])
      (define comp (car k))
      (define owner (owner-of k))
      (define class-env
        (and owner (class-attr-env comp owner self-attr class-defs comp-supers
                                   self-attr-fn)))
      (define base-attrs (or (and class-env (receiver-types-self-attrs class-env)) (hash)))
      (define base-fn-attrs
        (or (and class-env (receiver-types-fn-self-attrs class-env)) (hash)))
      ;; Declared param ELEMENT types for this key (an external entry-point
      ;; param/local iterated in a for-loop): param-name -> element class.
      (define param-element-specs
        (let* ([modpath (and import-env
                             (hash-ref (import-env-comp-modpath import-env) comp #f))]
               [entry (and modpath param-element-types*
                           (hash-ref param-element-types* (cons modpath (cdr k)) #f))])
          (if entry
              (for/hash ([pd (in-list entry)]) (values (car pd) (cdr pd)))
              (hash))))
      (define rt (infer-receiver-types (body-of k) class-defs base-attrs import-env comp
                                       (seed-params (params-of k) k param) owner
                                       #:tokens tokens
                                       #:return-types return
                                       #:methods methods
                                       #:fn-defs fn-defs
                                       #:known known
                                       #:comp-supers comp-supers
                                       #:element-types element-types*
                                       #:param-element-specs param-element-specs
                                       #:globals (closure-globals-of k comp
                                                                    (hash-ref globals comp (hash)))
                                       #:base-fn-attrs base-fn-attrs))
      (hash-set! assign k (receiver-types-locals rt))
      (hash-set! elem-specs k (receiver-types-elem-specs rt))
      (hash-set! assign-fn k (receiver-types-fn-locals rt)))
    (set-type-relations-assign! tr assign)
    (set-type-relations-elem-specs! tr elem-specs)
    (set-type-relations-assign-fn! tr assign-fn))

  (define (collect-params!)
    (define cands (make-hash))   ;; (cons key idx) -> (listof class-symbol)
    (define unres (make-hash))   ;; (cons key idx) -> count of unresolvable sites
    (define (note! key idx cls)
      (define kk (cons key idx))
      (if cls
          (hash-update! cands kk (λ (l) (cons cls l)) '())
          (hash-update! unres kk add1 0)))
    (for ([k (in-list all-keys)])
      (define comp (car k))
      (define owner (owner-of k))
      (define rt (rt-of k))
      (define (arg-class a) (value-class-of a rt class-defs tokens import-env comp owner))
      ;; Split a call's argument list into positional values and keyword
      ;; bindings (name-symbol . value). A keyword argument is lowered as
      ;; (call "kwarg" (list (var name) value)); anything else is positional
      ;; (`*`/`**` spread included, matching the previous index-based walk).
      (define (split-args args)
        (define positional '())
        (define kwargs '())
        (for ([a (in-list args)])
          (if (and (uir-call? a)
                   (uir-symbol? (uir-call-callee a))
                   (tokens-keyword-arg-callee tokens)
                   (string=? (uir-symbol-name (uir-call-callee a))
                             (tokens-keyword-arg-callee tokens))
                   (= (length (uir-call-args a)) 2)
                   (uir-var? (first (uir-call-args a))))
              (set! kwargs
                    (cons (cons (string->symbol
                                 (uir-symbol-name
                                  (uir-var-name (first (uir-call-args a)))))
                                (second (uir-call-args a)))
                          kwargs))
              (set! positional (cons a positional))))
        (values (reverse positional) (reverse kwargs)))
      (define (param-name p)
        (cond
          [(uir-symbol? p) (string->symbol (uir-symbol-name p))]
          [(uir-typed-param? p)
           (and (uir-symbol? (uir-typed-param-name p))
                (string->symbol (uir-symbol-name (uir-typed-param-name p))))]
          [else #f]))
      (define (param-index ps name)
        ;; `for/first` returns the FIRST iteration's value even when it is #f
        ;; (it does not scan to the first match); `for/or` finds the first
        ;; non-#f index. Using for/first here silently disabled keyword-argument
        ;; parameter propagation for every parameter not at index 0.
        (for/or ([i (in-naturals)] [p (in-list ps)])
          (and (eq? (param-name p) name) i)))
      ;; Record a param binding; `self-offset` skips `self`/`cls` for methods.
      (define (note-positional! key self-offset positional)
        (for ([i (in-naturals)] [a (in-list positional)])
          (define idx (+ i self-offset))
          (when (< idx (length (params-of key)))
            (note! key idx (arg-class a)))))
      (define (note-kwargs! key kwargs)
        (define ps (params-of key))
        (for ([kw (in-list kwargs)])
          (define idx (param-index ps (car kw)))
          (when (and idx (< idx (length ps)))
            (note! key idx (arg-class (cdr kw))))))
      (define (visit! node)
        (when (uir-call? node)
          (define callee (uir-call-callee node))
          (define-values (positional kwargs) (split-args (uir-call-args node)))
          (define nm (callee-name callee))
          (define fk (and nm (bare-function-key nm comp fn-defs known)))
          (cond
            [fk
             (note-positional! fk 0 positional)
             (note-kwargs! fk kwargs)]
            [else
             (define mks (method-callee-keys callee comp owner methods class-defs
                                             comp-supers tokens))
             (cond
               [(pair? mks)
                (define offset (tokens-method-receiver-offset tokens))
                (for ([mk (in-list mks)])
                  (note-positional! mk offset positional)
                  (note-kwargs! mk kwargs))]
               [else
                (define cname (constructor-class callee class-defs tokens))
                (define c (and cname (resolve-class-component cname comp class-defs)))
                (define init-name (tokens-init-method-name tokens))
                (define ik (and c init-name
                                (cons c (qualified-name-symbol
                                         cname (string->symbol init-name)))))
                (when (and ik (hash-has-key? methods ik))
                  (note-positional! ik (tokens-method-receiver-offset tokens) positional)
                  (note-kwargs! ik kwargs))])])))
      (walk-body (body-of k) visit!))
    ;; One class is the sound projection. Several classes are still usable when
    ;; every one is an external MODULE receiver with the SAME receiver-modules
    ;; effect domain (`library=pylibmc | pymemcache | memcache` at different
    ;; cache-backend call sites): the module-domain fallback classifies every
    ;; method on any of them identically, so a deterministic representative
    ;; carries the same coarse domain. Any non-module or mixed-domain candidate
    ;; stays unresolved. The receiver-modules table is read from the INJECTED
    ;; token tables (`tokens` threaded from the lexicon), not the Python default,
    ;; so a non-Python host merges against its own data.
    (define (module-domain c)
      (and c
           (let* ([s (symbol->string c)]
                  [m (regexp-match #rx"^([^.]+)" s)]
                  [tbl (effect-tokens-receiver-modules tokens)])
             (or (hash-ref tbl s #f)
                 (and m (hash-ref tbl (cadr m) #f))))))
    (define (resolve-candidate uniq)
      (cond
        [(null? uniq) #f]
        [(= (length uniq) 1) (car uniq)]
        [else
         (define doms (map module-domain uniq))
         (and (andmap values doms)
              (apply equal? doms)
              (car (sort uniq symbol<?)))]))
    (define out (make-hash))
    (for ([(kk cls-list) (in-hash cands)])
      (define uniq (remove-duplicates cls-list))
      (when (zero? (hash-ref unres kk 0))
        (define c (resolve-candidate uniq))
        (when c (hash-set! out kk c))))
    out)

  ;; Declared return type for a corpus function whose body projects no class:
  ;; keyed (module-path . qualified-name) -> class-name. Body projection wins
  ;; (`(or r dr)`), so a declared entry never overwrites real evidence.
  (define (declared-return-of k)
    (define modpath (and import-env
                         (hash-ref (import-env-comp-modpath import-env) (car k) #f)))
    (and modpath declared-returns*
         (hash-ref declared-returns* (cons modpath (cdr k)) #f)))

  ;; A body-projected class is USABLE as a return type only when it can later
  ;; resolve to a concrete MRO: a corpus class with several definitions
  ;; (`Command`, `Serializer` — one per command module / per serialization
  ;; format) has no single component, so `resolve-class-component` returns #f
  ;; and every caller through it falls to `unknown`. Reject the ambiguous
  ;; class and fall back to the declared return type (`load_command_class` →
  ;; BaseCommand) or, failing that, to no projection: no projection is sounder
  ;; than a class that cannot resolve. External classes (Message, redis.Redis,
  ;; SMTP) and module-path markers have no corpus definition and stay usable.
  (define (usable-return-class r)
    (and r
         (let ([defs (remove-duplicates (hash-ref class-defs r '()))])
           (if (and (pair? defs) (> (length defs) 1)) #f r))))

  (define (collect-returns!)
    (for ([k (in-list all-keys)])
      (define comp (car k))
      (define owner (owner-of k))
      (define r (return-class-of (body-of k) (rt-of k) class-defs
                                 tokens import-env comp owner
                                 #:return-types return
                                 #:methods methods
                                 #:fn-defs fn-defs
                                 #:known known
                                 #:comp-supers comp-supers))
      (hash-set! return k (or (usable-return-class r) (declared-return-of k))))
    (set-type-relations-return! tr return))

  (let loop ([round 0])
    (define prev-assign (hash-copy assign))
    (define prev-self-attr (hash-copy self-attr))
    (define prev-param (hash-copy param))
    (define prev-return (hash-copy return))
    (define prev-elem-specs (hash-copy elem-specs))
    (rebuild-self-attrs!)
    (rebuild-assign!)
    (set! param (collect-params!))
    ;; Declared fallback fills only call-chain gaps (call-chain evidence wins).
    (set! param (seed-declared-params param all-keys params-of import-env
                                     declared-params*))
    (set-type-relations-param! tr param)
    (collect-returns!)
    (set! return (type-relations-return tr))
    (cond
      [(and (equal? prev-assign assign)
            (equal? prev-self-attr self-attr)
            (equal? prev-param param)
            (equal? prev-return return)
            (equal? prev-elem-specs elem-specs))
       tr]
      [(>= round 100)
       (eprintf "type-relations: join did not converge in ~a rounds\n" round)
       tr]
      [else (loop (add1 round))])))
