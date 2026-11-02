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
;; (class fields + `self.attr = ...` in method bodies); `close-type-relations!`
;; closes the four relations deterministically (sorted keys, single-class folds).
;;
;; The single-class projection is not monotone, so the relations can oscillate.
;; `type-relations-widen.rkt` detects a repeated full state and widens exactly
;; the entries that moved in the cycle to unresolved, then freezes them; a
;; converging input never widens. Each widening prints a diagnostic line.

(require racklr/uir
         racklr/effects/class-resolve
         racklr/effects/collect
         racklr/effects/walk
         racklr/effects/shape
         racklr/effects/receiver-type
         racklr/effects/return-type
         racklr/effects/library-tokens
         racklr/effects/lexicon "type-relations-widen.rkt")

(provide type-relations? type-relations
         type-relations-assign type-relations-self-attr
         type-relations-param type-relations-return
         set-type-relations-assign! set-type-relations-self-attr!
         set-type-relations-param! set-type-relations-return!
         materialize-type-relations
         close-type-relations!
         class-attr-env
         seed-params)

(struct type-relations (assign self-attr param return) #:transparent #:mutable)

(define (key<? a b)
  (string<? (format "~a.~a" (car a) (cdr a)) (format "~a.~a" (car b) (cdr b))))

;; Class name of a class-body field initializer (`attr = KnownClass`), #f when
;; the initializer does not name a known class. (was field-init-class.)
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

;; Wrap a propagated param type so the annotation-seeding path picks it up.
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

;; Self-attribute assignments across every class: class fields first, then
;; `self.attr = KnownClass(...)` from each method (overlaying the defaults).
;; Keyed by (comp . class), iterated over sorted method names so a conflicting
;; attr resolves deterministically instead of by hash order.
(define (class-self-attrs comp-programs class-defs import-env
                          [param-types (hash)]
                          [return-types #f]
                          [methods #f] [fn-defs #f] [known #f] [comp-supers #f]
                          #:tokens [tokens default-effect-tokens])
  (define h (class-field-attrs comp-programs class-defs import-env tokens))
  (for ([cp (in-list comp-programs)])
    (define comp (car cp))
    (define mparams (collect-method-params (cdr cp)))
    (define cmethods (collect-methods (cdr cp)))
    (for ([n (in-list (sort (hash-keys cmethods) symbol<?))])
      (define body (hash-ref cmethods n))
      (define owner (class-name-of-name n))
      (define k (cons comp n))
      (define sa (receiver-types-self-attrs
                  (infer-receiver-types body class-defs (hash) import-env comp
                                        (seed-params (hash-ref mparams n '()) k param-types)
                                        owner
                                        #:tokens tokens
                                        #:return-types return-types
                                        #:methods methods
                                        #:fn-defs fn-defs
                                        #:known known
                                        #:comp-supers comp-supers)))
      (define key (cons comp owner))
      (define cur (hash-ref h key #f))
      (cond
        [(not cur) (unless (zero? (hash-count sa)) (hash-set! h key (hash-copy sa)))]
        [else (for ([(attr kls) (in-hash sa)]) (hash-set! cur attr kls))])))
  h)

;; Inherited self-attribute environment, MRO across files (most-derived first);
;; returns receiver-types with no locals (locals are per-function).
(define (class-attr-env comp class-name self-attr class-defs comp-supers)
  (define seen (make-hash))
  (define acc (make-hash))
  (let loop ([c comp] [cls class-name])
    (when (and c cls (not (hash-has-key? seen (cons c cls))))
      (hash-set! seen (cons c cls) #t)
      (for ([(attr kls) (in-hash (hash-ref self-attr (cons c cls) (hash)))])
        (unless (hash-has-key? acc attr) (hash-set! acc attr kls)))
      (for ([sr (in-list (hash-ref comp-supers (cons c cls) '()))])
        (define c2 (car sr))
        (define s (cdr sr))
        (when (and c2 s) (loop c2 s)))))
  (receiver-types (hash) acc))

;; Syntax-visible seed: the self-attr relation from class fields + method
;; bodies. assign/param/return are empty until the join runs.
(define (materialize-type-relations comp-programs class-defs import-env
                                    #:tokens [tokens default-effect-tokens])
  (type-relations (make-hash)
                  (class-self-attrs comp-programs class-defs import-env (hash)
                                    #:tokens tokens)
                  (make-hash)
                  (make-hash)))

;; Deterministic join of assign/self-attr/param/return; see
;; type-relations-widen.rkt for the widening that guarantees termination.
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
                               #:tokens [tokens default-effect-tokens]
                               ;; #:max-rounds is a diagnostic budget only (#f =
                               ;; no budget): #:widen? proves termination.
                               #:max-rounds [max-rounds #f]
                               ;; #:widen? #f is diagnosis-only (raw iteration).
                               #:widen? [widen? #t]
                               ;; Optional box: (list count by-rel) per widening.
                               #:widening-report [widening-report #f]
                               #:trace? [trace? (and (getenv "TYPE_RELATIONS_TRACE") #t)])
  (define assign (type-relations-assign tr))
  (define self-attr (type-relations-self-attr tr))
  (define param (type-relations-param tr))
  (define return (type-relations-return tr))

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
  ;; the class's MRO-closed self-attr relation.
  (define (rt-of k)
    (define owner (owner-of k))
    (define class-env (and owner (class-attr-env (car k) owner self-attr class-defs comp-supers)))
    (receiver-types (hash-ref assign k (hash))
                    (or (and class-env (receiver-types-self-attrs class-env)) (hash))))

  (define (rebuild-self-attrs!)
    (set! self-attr (class-self-attrs comp-programs class-defs import-env param
                                      return methods fn-defs known comp-supers
                                      #:tokens tokens))
    (set-type-relations-self-attr! tr self-attr))

  (define (rebuild-assign!)
    (for ([k (in-list all-keys)])
      (define comp (car k))
      (define owner (owner-of k))
      (define class-env (and owner (class-attr-env comp owner self-attr class-defs comp-supers)))
      (define base-attrs (or (and class-env (receiver-types-self-attrs class-env)) (hash)))
      (define rt (infer-receiver-types (body-of k) class-defs base-attrs import-env comp
                                       (seed-params (params-of k) k param) owner
                                       #:tokens tokens #:return-types return
                                       #:methods methods #:fn-defs fn-defs
                                       #:known known
                                       #:comp-supers comp-supers
                                       #:call-result-class
                                       (λ (c env) (call-result-return-class c comp class-defs
                                                 owner env methods fn-defs known comp-supers
                                                 return tokens import-env))))
      (hash-set! assign k (receiver-types-locals rt)))
    (set-type-relations-assign! tr assign))

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
      ;; A call argument projects through the shared call-result rule.
      (define (arg-class a)
        (or (and (uir-call? a) return
                 (call-result-return-class (uir-call-callee a) comp class-defs
                                           owner rt methods fn-defs known
                                           comp-supers return tokens import-env))
            (value-class-of a rt class-defs tokens import-env comp owner)))
      ;; Split a call's argument list into positional values and keyword
      ;; bindings (name-symbol . value); anything else is positional (`*`/`**`
      ;; spread included, matching the previous index-based walk).
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
             ;; Syntactic keys plus the receiver-type-dataflow half.
             (define mks (call-target-keys callee comp owner rt methods class-defs
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

  (define (collect-returns!)
    ;; Snapshot the previous round so every key reads one relation (simultaneous update).
    (define prev-return (hash-copy return))
    (for ([k (in-list all-keys)])
      (define comp (car k))
      (define owner (owner-of k))
      (hash-set! return k
                 (return-class-of (body-of k) (rt-of k) class-defs
                                  tokens import-env comp owner
                                  #:return-types prev-return
                                  #:methods methods #:fn-defs fn-defs
                                  #:known known #:comp-supers comp-supers)))
    (set-type-relations-return! tr return))

  (define-values (assign* self-attr* param* return*)
    (run-join!
     (λ ()
       (rebuild-self-attrs!)
       (rebuild-assign!)
       (set! param (collect-params!))
       (set-type-relations-param! tr param)
       (collect-returns!)
       (set! return (type-relations-return tr))
       (values assign self-attr param return))
     #:max-rounds max-rounds
     #:widen? widen?
     #:trace? trace?
     #:widening-report widening-report))
  (set-type-relations-assign! tr assign*)
  (set-type-relations-self-attr! tr self-attr*)
  (set-type-relations-param! tr param*)
  (set-type-relations-return! tr return*)
  tr)
