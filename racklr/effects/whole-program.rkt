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
         (prefix-in alg: racklr/effects/algebra/main)
         racklr/effects/shape
         racklr/effects/receiver-type
         racklr/effects/classify-all
         racklr/effects/type-relations
         racklr/effects/library-tokens
         racklr/effects/imports
         racklr/effects/imports
         racklr/effects/labels
         racklr/effects/provenance
         racklr/effects/lexicon)

(provide infer-effects-all classify-call-all)

;; Binding-kind helpers for the `#:unknown-tally` census: what kind of value a
;; local variable was bound from, so the census can attribute a `local`
;; receiver to the lever that would resolve it (call-result projection,
;; attribute copy, alias, loop variable, function value, or literal).
;; `unbound` means the name is not assigned in the body (a global, import,
;; closure capture, or class-level name).
;;
;; Each tally record is one call site that the REAL classification (whole
;; program, import-aware, receiver-typed) left `unknown`:
;;
;;   (component function shape op receiver binding-kind site higher-order?)
;;
;; shape/op/receiver/binding-kind are the aiming data (classify-all.rkt's
;; receiver-shape); `site` is the exact callee site string (shape.rkt's
;; callee-site-string), so a census names a call site instead of an aggregate
;; count; `higher-order?` is #t only when a bare callee names one of the
;; enclosing function's parameters (the row-hole lever). A bare callee that is
;; a local bound from a function value carries binding-kind 'fn (the
;; function-value-typing lever) and a free/global/imported name carries
;; 'unbound (the lexicon lever), so the three bare-call cases are separable.

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
(define (comp-loop-var node [tokens default-effect-tokens])
  (and (uir-call? node)
       (let ([callee (uir-call-callee node)])
         (and (uir-symbol? callee)
              (set-member? (tokens-comprehension-callees tokens)
                           (uir-symbol-name callee))
              (pair? (cdr (uir-call-args node)))
              (second (uir-call-args node))))))

;; Whole-program effect rows: (component . name) → uir-effect-row, computed by
;; a single fixpoint over the union of every program's call graph. Takes a list
;; of `(component . program)` pairs.
(define (infer-effects-all comp-programs
                           #:import-env [import-env #f]
                           #:unknown-tally [unknown-tally #f]
                           #:provenance-record [rec-box #f]
                           #:lexicon [lex default-lexicon])
  (define tokens (lexicon-tokens lex))
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
  (define tr (materialize-type-relations comp-programs class-defs import-env
                                         #:tokens tokens))
  (close-type-relations! tr
                         #:comp-programs comp-programs
                         #:class-defs class-defs
                         #:import-env import-env
                         #:fns fns
                         #:methods methods
                         #:fn-params fn-params
                         #:method-params method-params
                         #:fn-defs fn-defs
                         #:comp-supers comp-supers
                         #:tokens tokens)
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
  ;; Structured provenance: every call-graph edge (with the callee expression
  ;; the classifier saw) and every direct ground. Propagation needs no path
  ;; storage — the fixpoint only moves effects along these edges, so reachability
  ;; over the recorded call graph is exactly a provenance path.
  (define prov (and rec-box (make-provenance)))
  (for ([k (in-list all-keys)])
    (define body (if (hash-has-key? fns k) (hash-ref fns k) (hash-ref methods k)))
    (define comp (car k))
    (define owner (and (hash-has-key? methods k) (class-name-of-name (cdr k))))
    (define rt (rt-of k))
    (define blessed (and (hash-has-key? methods k)
                        (method-cap k import-env lex)))
    (define effects (if blessed (list blessed) '()))
    ;; A blessed capability is asserted at the method definition by the DATA
    ;; config, so it is its own ground: the tokens are the module path, class,
    ;; and method the capability was derived from.
    (when (and prov blessed)
      (define parts (string-split (symbol->string (cdr k)) "."))
      (provenance-add-ground!
       prov
       (make-declared-site (function-fq k)
                           (effect-cap-domain blessed)
                           (filter values (list (and import-env
                                                     (hash-ref (import-env-comp-modpath import-env)
                                                               comp #f))
                                                (first parts)
                                                (last parts)))
                           (first parts)
                           #:file comp)))
    (define callees '())
    (define param-names
      (let ([ps (or (hash-ref fn-params k #f) (hash-ref method-params k #f))])
        (and ps
             (for/list ([p (in-list ps)]
                        #:when (and (uir-typed-param? p)
                                    (uir-symbol? (uir-typed-param-name p))))
               (string->symbol (uir-symbol-name (uir-typed-param-name p)))))))
    (define local-kinds (make-hash))   ;; var -> binding-kind, maintained in walk order
    ;; Stable positional discriminator of a site: the ordinal of the call node
    ;; in this body's pre-order walk (source structure only, identical across
    ;; runs). Reset per function.
    (define call-position 0)
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
        [(comp-loop-var node tokens)
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
      (cond
        [(uir-call? node)
         (define position call-position)
         (set! call-position (add1 call-position))
         (let* ([callee (uir-call-callee node)]
                ;; The classifier's coverage side channel (only needed when
                ;; provenance is recorded): a reused box is reset per call.
                [cov-box (and prov (box 'structural))]
                [cls (classify-call-all callee known methods
                                        class-defs fn-defs comp-supers self-attr comp owner
                                        import-env rt return-types cov-box
                                        #:lexicon lex)]
                [site (callee-site-string callee)])
           (case (car cls)
             [(edge)
              (set! callees (cons (cdr cls) callees))
              (when prov
                (provenance-add-call! prov
                                      (prov-call (function-fq k)
                                                 (function-fq (cdr cls))
                                                 site
                                                 (make-call-origin (function-fq k) callee
                                                                   #:file comp
                                                                   #:position position))))]
             [(effect)
              (set! effects (cons (cdr cls) effects))
              (when prov
                ;; The coverage label records what the classifier needed to
                ;; establish this ground: no receiver dataflow (a structural
                ;; rule or lexicon hit) is for-all 'universal; a receiver
                ;; resolved through a parameter is a 'census-param worked
                ;; instance; a receiver resolved from a tracked local /
                ;; attribute / call-result value is 'local-literal.
                (provenance-add-ground!
                 prov
                 (make-callee-site (function-fq k) (cdr cls) callee
                                   #:coverage (ground-coverage
                                               callee
                                               (and cov-box (unbox cov-box))
                                               param-names
                                               tokens)
                                   #:file comp
                                   #:position position)))]
             [(pure) (void)]
             [(unknown)
              (set! effects (cons 'unknown effects))
              (when prov
                (provenance-add-ground! prov
                                        (make-callee-site (function-fq k) 'unknown callee
                                                          #:kind 'unresolved
                                                          #:coverage
                                                          (unresolved-coverage
                                                           callee param-names
                                                           tokens)
                                                          #:file comp
                                                          #:position position)))
              (when unknown-tally
                (define shape (receiver-shape callee param-names tokens))
                ;; Receiver root of a member-access chain (`x` in x.m(),
                ;; `self` in self.cache.m()) so a site names the object whose
                ;; type must flow; a bare call has no receiver.
                (define rx
                  (and (member-access-callee? callee) (dotted-root callee)))
                ;; The binding kind refines a `local` receiver and, for a bare
                ;; call, tells the parameter-callback and function-value levers
                ;; apart from a genuinely free name. For `bare-call`:
                ;;   'param    the callee names one of the enclosing
                ;;             function's parameters (row-hole lever)
                ;;   'fn       the callee is a local bound from a function
                ;;             value (function-value-typing lever)
                ;;   'unbound  a free/global/imported name (lexicon lever)
                (define immediate-base
                  (and (uir-get? callee) (var-symbol (uir-get-base callee))))
                (define bare-name
                  (cond
                    [(uir-var? callee)
                     (string->symbol (uir-symbol-name (uir-var-name callee)))]
                    [(uir-symbol? callee)
                     (string->symbol (uir-symbol-name callee))]
                    [else #f]))
                (define binding-kind
                  (cond
                    [(eq? shape 'param) 'param]
                    [(and (eq? shape 'bare-call) bare-name param-names
                          (memq bare-name param-names))
                     'param]
                    [(and (eq? shape 'bare-call) bare-name)
                     (hash-ref local-kinds bare-name 'unbound)]
                    [immediate-base (hash-ref local-kinds immediate-base 'unbound)]
                    [else #f]))
                ;; `higher-order?` marks the callback-parameter lever (row
                ;; holes); a bare call to a function-valued local is the
                ;; separate function-value-typing lever.
                (define higher-order?
                  (and (eq? shape 'bare-call) (eq? binding-kind 'param)))
                (set-box! unknown-tally
                          (cons (list (car k) (cdr k)
                                      shape
                                      (effect-op-of-callee callee)
                                      rx
                                      binding-kind
                                      site
                                      higher-order?)
                                (unbox unknown-tally))))]))]
        [else (void)]))
    (walk-body body visit!)
    (hash-set! direct-rows k (effects->algebra-row effects))
    (hash-set! edges k (sort (remove-duplicates callees) (λ (a b)
                                                            (string<? (format "~a.~a" (car a) (cdr a))
                                                                      (format "~a.~a" (car b) (cdr b)))))))

  ;; The whole-program fixpoint is the algebra's: iterate `join` (the least
  ;; upper bound over rows) from the direct rows until the table stops
  ;; changing. Same operation as the single-file pass, same driver, so the two
  ;; fixpoints cannot drift apart.
  (define rows (alg:least-fixpoint direct-rows edges))
  (when rec-box (set-box! rec-box prov))
  ;; Materialize the algebra rows back to the UIR row contract.
  (for/hash ([(k r) (in-hash rows)])
    (values k (alg:algebra-row->uir r))))
