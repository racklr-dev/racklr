#lang racket

;; racklr/effects/whole-program.rkt
;;
;; Whole-program (cross-file) effect inference: a single monotone fixpoint over
;; the union of every component's call graph, with component-qualified node
;; identity and cross-file superclass resolution. The cross-file callee
;; classifier lives in classify-all.rkt; the type/effect relation
;; materialization and its deterministic join live in type-relations.rkt; this
;; module is the fixpoint driver that collects the tables, joins the relations,
;; and folds the call graph.

(require racklr/uir
         racklr/effects/class-resolve
         racklr/effects/collect
         racklr/effects/walk
         racklr/effects/rows
         racklr/effects/shape
         racklr/effects/receiver-type
         racklr/effects/classify-all
         racklr/effects/type-relations
         racklr/effects/library-tokens)

(provide infer-effects-all classify-call-all)

;; Binding-kind helpers for the `#:unknown-tally` diagnostic: what kind of
;; value a local variable was bound from, so the census can attribute a
;; `local` receiver to the lever that would resolve it (call-result
;; projection, attribute copy, alias, loop variable, function value, or
;; literal). `unbound` means the name is not assigned in the body (a global,
;; import, closure capture, or class-level name).

(define (var-symbol v)
  (cond
    [(uir-var? v) (string->symbol (uir-symbol-name (uir-var-name v)))]
    [(uir-symbol? v) (string->symbol (uir-symbol-name v))]
    [else #f]))

(define (binding-value-kind v)
  (cond
    [(wrapped-fn v) 'fn]
    [(uir-call? v) 'call-result]
    [(uir-get? v) 'attr]
    [(uir-var? v) 'alias]
    [(uir-symbol? v) 'alias]
    [(uir-fn? v) 'fn]
    [else 'literal]))

;; Comprehension loop variable: `(call "TYPE-comp" (list result loop-var iter
;; filter))` binds `loop-var` over the iterable, so an element method call
;; inside the result expression is a comp-var (needs element-type dataflow),
;; not an unbound name. Returns the loop-var uir-symbol, or #f.
(define (comp-loop-var node)
  (and (uir-call? node)
       (let ([callee (uir-call-callee node)])
         (and (uir-symbol? callee)
              (memq (string->symbol (uir-symbol-name callee))
                    '(list-comp dict-comp set-comp gen-exp))
              (pair? (cdr (uir-call-args node)))
              (second (uir-call-args node))))))

;; Whole-program effect rows: (component . name) → uir-effect-row, computed by
;; a single fixpoint over the union of every program's call graph. Takes a list
;; of `(component . program)` pairs.
(define (infer-effects-all comp-programs #:import-env [import-env #f] #:unknown-tally [unknown-tally #f])
  (define fns (make-hash))           ;; (cons comp name) → body
  (define methods (make-hash))       ;; (cons comp name) → body
  (define comp-supers (make-hash))   ;; (cons comp class) → (listof super-ref: symbol | (first . full-dotted))
  (define class-defs (make-hash))    ;; class-name → (listof comp)
  (define fn-defs (make-hash))       ;; fn-name → (listof comp)
  (define fn-params (make-hash))     ;; (cons comp name) → params
  (define method-params (make-hash)) ;; (cons comp name) → params
  (for ([cp (in-list comp-programs)])
    (define comp (car cp))
    (define program (cdr cp))
    (for ([(n body) (in-hash (collect-functions program))])
      (hash-set! fns (cons comp n) body)
      (hash-update! fn-defs n (λ (l) (cons comp l)) '()))
    (for ([(n ps) (in-hash (collect-function-params program))])
      (hash-set! fn-params (cons comp n) ps))
    (for ([(n body) (in-hash (collect-methods program))])
      (hash-set! methods (cons comp n) body))
    (for ([(n ps) (in-hash (collect-method-params program))])
      (hash-set! method-params (cons comp n) ps))
    (for ([(n _) (in-hash (collect-class-names program))])
      (hash-update! class-defs n (λ (l) (cons comp l)) '()))
    (for ([(n chain) (in-hash (collect-supers program))])
      (hash-set! comp-supers (cons comp n) chain)))

  ;; Resolve every super reference to its (component . real-class-name) pair: a
  ;; bare name resolves via class-defs or an import alias (`from x import C as
  ;; Alias`); a qualified name (`base.Serializer`) follows the module alias
  ;; through the import graph to the class it names. Pairs (not bare symbols)
  ;; are what every super-chain consumer now reads.
  (define mp->comp (modpath-index import-env))
  (for ([(k chain) (in-hash comp-supers)])
    (hash-set! comp-supers k
               (for/list ([s (in-list chain)])
                 (resolve-super-ref s (car k) class-defs import-env mp->comp))))

  ;; Materialize the type relations and close them with the deterministic join:
  ;; assign (locals), self-attr (per-class attrs), param (concrete call-chain
  ;; param types), and return (projected return class).
  (define tr (materialize-type-relations comp-programs class-defs import-env))
  (close-type-relations! tr
                         #:comp-programs comp-programs
                         #:class-defs class-defs
                         #:import-env import-env
                         #:fns fns
                         #:methods methods
                         #:fn-params fn-params
                         #:method-params method-params
                         #:fn-defs fn-defs
                         #:comp-supers comp-supers)
  (define assign (type-relations-assign tr))
  (define self-attr (type-relations-self-attr tr))
  (define return-types (type-relations-return tr))

  (define known (make-hash))
  (for ([(k _) (in-hash fns)]) (hash-set! known k #t))
  (for ([(k _) (in-hash methods)]) (hash-set! known k #t))

  (define all-keys (append (hash-keys fns) (hash-keys methods)))

  ;; Per-key receiver env: locals from the closed assign relation, self-attrs
  ;; from the class's MRO-closed self-attr relation.
  (define (rt-of k)
    (define owner (and (hash-has-key? methods k) (class-name-of-name (cdr k))))
    (define class-env (and owner (class-attr-env (car k) owner self-attr class-defs comp-supers)))
    (receiver-types (hash-ref assign k (hash))
                    (or (and class-env (receiver-types-self-attrs class-env)) (hash))))

  ;; Pass 3: classification + call-graph edges.
  (define direct-rows (make-hash))
  (define edges (make-hash))
  (for ([k (in-list all-keys)])
    (define body (if (hash-has-key? fns k) (hash-ref fns k) (hash-ref methods k)))
    (define comp (car k))
    (define owner (and (hash-has-key? methods k) (class-name-of-name (cdr k))))
    (define rt (rt-of k))
    (define effects
      (let ([cap (and (hash-has-key? methods k) (method-cap k import-env))])
        (if cap (list cap) '())))
    (define callees '())
    (define param-names
      (let ([ps (or (hash-ref fn-params k #f) (hash-ref method-params k #f))])
        (and ps
             (for/list ([p (in-list ps)]
                        #:when (and (uir-typed-param? p)
                                    (uir-symbol? (uir-typed-param-name p))))
               (string->symbol (uir-symbol-name (uir-typed-param-name p)))))))
    (define local-kinds (make-hash))   ;; var -> binding-kind, maintained in walk order
    (define (visit! node)
      ;; Track what each local was bound from, so an unknown `local` receiver
      ;; can be attributed to its binding kind (dominant assignment).
      (cond
        [(and (uir-set!? node) (var-symbol (uir-set!-name node)))
         => (λ (lhs) (hash-set! local-kinds lhs (binding-value-kind (uir-set!-value node))))]
        [(and (uir-ann-set!? node) (uir-ann-set!-value node)
              (var-symbol (uir-ann-set!-lhs node)))
         => (λ (lhs) (hash-set! local-kinds lhs (binding-value-kind (uir-ann-set!-value node))))]
        [(and (uir-for-each? node) (var-symbol (uir-for-each-var node)))
         => (λ (v) (hash-set! local-kinds v 'loop-var))]
        [(comp-loop-var node)
         => (λ (lv)
              (cond
                [(uir-list? lv)
                 (for ([v (in-list (uir-list-items lv))])
                   (define vs (var-symbol v))
                   (when vs (hash-set! local-kinds vs 'comp-var)))]
                [else
                 (define vs (var-symbol lv))
                 (when vs (hash-set! local-kinds vs 'comp-var))]))]
        [(and (uir-let? node) (uir-symbol? (uir-let-name node)))
         (hash-set! local-kinds
                    (string->symbol (uir-symbol-name (uir-let-name node)))
                    (binding-value-kind (uir-let-value node)))]
        [(uir-with? node)
         (for ([item (in-list (uir-with-items node))])
           (define v (and (pair? item) (var-symbol (second item))))
           (when v
             (hash-set! local-kinds v
                        (binding-value-kind (and (pair? item) (first item))))))]
        [(uir-try? node)
         (for ([c (in-list (uir-try-catches node))])
           (define v (and (pair? c) (var-symbol (second c))))
           (when v (hash-set! local-kinds v 'exception)))]
        [else (void)])
      (if (uir-call? node)
            (let ([cls (classify-call-all (uir-call-callee node) known methods
                                        class-defs fn-defs comp-supers self-attr comp owner
                                        import-env rt return-types)])
            (case (car cls)
              [(edge) (set! callees (cons (cdr cls) callees))]
              [(effect) (set! effects (cons (cdr cls) effects))]
              [(pure) (void)]
              [(unknown)
               (set! effects (cons 'unknown effects))
               (when unknown-tally
                 (define callee (uir-call-callee node))
                 (define rx
                   (and (uir-get? callee)
                        (var-symbol (uir-get-base callee))))
                 (set-box! unknown-tally
                           (cons (list (car k) (cdr k)
                                       (receiver-shape callee param-names)
                                       (effect-op-of-callee callee)
                                       rx
                                       (and rx (hash-ref local-kinds rx 'unbound)))
                                 (unbox unknown-tally))))]))
          (void)))
    (walk-body body visit!)
    (hash-set! direct-rows k (effects->row effects))
    (hash-set! edges k (sort (remove-duplicates callees) (λ (a b)
                                                            (string<? (format "~a.~a" (car a) (cdr a))
                                                                      (format "~a.~a" (car b) (cdr b)))))))

  (define rows (make-hash))
  (for ([k (in-list all-keys)]) (hash-set! rows k (hash-ref direct-rows k)))
  (let loop ()
    (define changed? #f)
    (for ([k (in-list all-keys)])
      (define current (hash-ref rows k))
      (define callee-rows
        (map (λ (g) (hash-ref rows g (empty-row))) (hash-ref edges k)))
      (define new-row
        (foldl infer-row-union (hash-ref direct-rows k) callee-rows))
      (if (equal? current new-row)
          (void)
          (begin (set! changed? #t) (hash-set! rows k new-row))))
    (if changed? (loop) (void)))
  rows)
