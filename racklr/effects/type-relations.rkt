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
         racklr/effects/self-attr-relation
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
    ;; Var-positional parameters (`*args`): (cons key idx) -> (listof (listof
    ;; class-or-#f)), one positional tuple per call site. A call that spreads
    ;; such a parameter (`Page(*args, **kwargs)`) re-expands the tuple into
    ;; slots, so a constructor whose arguments pass through `*args` still
    ;; grounds its parameters (and thus the `self.attr` assignments in it) from
    ;; the concrete call chain.
    (define var-seq (make-hash))
    (define (note! key idx cls)
      (define kk (cons key idx))
      (if cls
          (hash-update! cands kk (λ (l) (cons cls l)) '())
          (hash-update! unres kk add1 0)))
    ;; Index of the parameter list's var-positional (`*args`) parameter, or #f.
    (define (var-positional-index ps)
      (for/or ([i (in-naturals)] [p (in-list ps)])
        (and (uir-typed-param? p)
             (eq? (uir-typed-param-kind p) 'var-positional)
             i)))
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
      ;; When the callee has a var-positional parameter (`*args`), the fixed
      ;; slots fill the parameters before it and the remaining slots are its
      ;; argument tuple, by position. A call site that passes no extra values
      ;; leaves the tuple absent (unknown), never an empty tuple that could
      ;; over-claim.
      (define (note-positional! key self-offset slots)
        (define ps (params-of key))
        (define vp (var-positional-index ps))
        (define fixed (and vp (max 0 (- vp self-offset))))
        (cond
          [fixed
           (define n-fixed (min (length slots) fixed))
           (for ([i (in-naturals)] [cls (in-list (take slots n-fixed))])
             (note! key (+ i self-offset) cls))
           (define rest-slots (drop slots n-fixed))
           (when (pair? rest-slots)
             (hash-update! var-seq (cons key vp)
                           (λ (l) (cons rest-slots l)) '()))]
          [else
           (for ([i (in-naturals)] [cls (in-list slots)])
             (define idx (+ i self-offset))
             (when (< idx (length ps))
               (note! key idx cls)))]))
      (define (note-kwargs! key kwargs)
        (define ps (params-of key))
        (for ([kw (in-list kwargs)])
          (define idx (param-index ps (car kw)))
          (when (and idx (< idx (length ps)))
            (note! key idx (arg-class (cdr kw))))))
      ;; The Python lowerer encodes a `**expr` argument as a synthetic
      ;; `(call "kw-spread" (list expr))` node; it carries keyword arguments,
      ;; never a positional slot.
      (define (keyword-spread-arg? a)
        (and (uir-call? a)
             (uir-symbol? (uir-call-callee a))
             (string=? (uir-symbol-name (uir-call-callee a)) "kw-spread")))
      ;; Argument classes for one call's positional arguments. A `*expr` spread
      ;; of the ENCLOSING function's own var-positional parameter re-expands into
      ;; the parameter's recorded argument tuple, so `Page(*args, **kwargs)` in a
      ;; method that received them as `*args` still maps its slots positionally.
      (define enclosing-ps (params-of k))
      (define enclosing-vp (var-positional-index enclosing-ps))
      (define enclosing-vp-name
        (and enclosing-vp (param-name (list-ref enclosing-ps enclosing-vp))))
      (define (positional-slots args)
        (append*
         (for/list ([a (in-list args)])
           (cond
             [(keyword-spread-arg? a) '()]
             [else
              (define spread-name
                (and (uir-spread? a) (bare-name-symbol (uir-spread-expr a))))
              (define v (and spread-name enclosing-vp
                             (eq? spread-name enclosing-vp-name)
                             (hash-ref param (cons k enclosing-vp) #f)))
              (cond
                [(and (pair? v) (eq? (car v) 'varargs)) (cdr v)]
                [else (list (arg-class a))])]))))
      (define (visit! node)
        (when (uir-call? node)
          (define callee (uir-call-callee node))
          (define-values (positional kwargs) (split-args (uir-call-args node)))
          (define slots (positional-slots positional))
          (define nm (callee-name callee))
          (define fk (and nm (bare-function-key nm comp fn-defs known)))
          (cond
            [fk
             (note-positional! fk 0 slots)
             (note-kwargs! fk kwargs)]
            [else
             ;; Syntactic keys plus the receiver-type-dataflow half.
             (define mks (call-target-keys callee comp owner rt methods class-defs
                                           comp-supers tokens))
             (cond
               [(pair? mks)
                (define offset (tokens-method-receiver-offset tokens))
                ;; A var-positional callee's tuple must not union the `self`
                ;; classes of several sibling subclass call sites into one
                ;; unresolved implementation (the shared
                ;; `Base._get_page(self, *args, **kwargs)` pattern): only the
                ;; most-derived dispatch key runs from this call, so only it
                ;; collects the arguments. Fixed-signature methods keep flowing
                ;; their arguments through every MRO target — an
                ;; over-approximation that lets a base body still type a
                ;; parameter (the pre-existing behaviour this lever does not
                ;; change).
                (define targets
                  (if (var-positional-index (params-of (car mks)))
                      (list (car mks))
                      mks))
                (for ([mk (in-list targets)])
                  (note-positional! mk offset slots)
                  (note-kwargs! mk kwargs))]
               [else
                (define cname (constructor-class callee class-defs tokens))
                (define c (and cname (resolve-class-component cname comp class-defs)))
                (define init-name (tokens-init-method-name tokens))
                (define ik (and c init-name
                                (cons c (qualified-name-symbol
                                         cname (string->symbol init-name)))))
                (when (and ik (hash-has-key? methods ik))
                  (note-positional! ik (tokens-method-receiver-offset tokens) slots)
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
    ;; Var-positional parameters: every call site must agree on the tuple
    ;; LENGTH for the positions to align; each position then folds to its single
    ;; candidate class (#f when it has none). A position with competing classes
    ;; stays unresolved without invalidating the other positions — that is what
    ;; lets `Page(*args)` type its third argument while the first two are
    ;; outside the static call chain.
    (for ([(kk seqs) (in-hash var-seq)])
      (define seqs* (reverse seqs))
      (define lens (remove-duplicates (map length seqs*)))
      (when (= (length lens) 1)
        (define elems
          (for/list ([i (in-range (car lens))])
            (define classes
              (for/list ([s (in-list seqs*)]
                         #:do [(define c (list-ref s i))]
                         #:when c)
                c))
            (define uniq (remove-duplicates classes))
            (and (= (length uniq) 1) (car uniq))))
        (when (ormap values elems)
          (hash-set! out kk (cons 'varargs elems)))))
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
