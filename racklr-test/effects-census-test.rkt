#lang racket

;; racklr-test/effects-census-test.rkt
;;
;; Tests the real-context unknown census (racklr/effects/census.rkt):
;;  - only sites the whole-program context leaves unknown are counted (the
;;    empty-context classification of the same sites is unknown, so the census
;;    is not the old overcount);
;;  - every site carries the aiming data (shape, receiver, binding kind, exact
;;    callee site string) and the dataflow lever that would resolve it;
;;  - coverage counters and aggregations are deterministic.

(require rackunit
         racklr/uir
         racklr/effects/infer
         racklr/effects/census)

;; ── UIR helpers (same constructs the other effects suites use) ───────

(define (sym s) (uir-symbol s))
(define (v s) (uir-var (uir-symbol s)))
(define (dot b f) (uir-get b (uir-symbol f)))
(define (fn name body #:params [params '()])
  (uir-fn (if name (uir-symbol name) #f) params body #f #f))
(define (prog . stmts) (uir-block stmts))
(define (py-class name methods)
  (uir-class (uir-symbol name) '() '() methods))
(define (py-method name body)
  (uir-method (uir-symbol name) '() body 'public))
(define (param name)
  (uir-typed-param (uir-symbol name) #f #f))

;; ── fixtures ─────────────────────────────────────────────────────────

;; One component defines a function and a class; the caller in another
;; component reaches both through the real context: the bare call is a
;; cross-file function edge and the `Known(...)` local is resolved by
;; receiver-type dataflow. A third call is unresolvable (`y` is never bound).
(define (mixed-programs)
  (define a (prog (fn "helper" (uir-null))
                  (py-class "Known" (list (py-method "m" (uir-null))))))
  (define b (prog (fn "run"
                       (prog
                        (uir-set! (sym "x") (uir-call (v "Known") '()))
                        (uir-call (v "helper") '())
                        (uir-call (dot (v "x") "m") '())
                        (uir-call (dot (v "y") "go") '())))))
  (list (cons "a.py" a) (cons "b.py" b)))

;; ── the census counts the real surface, not the empty-context one ────

(define (test-real-context-not-empty-context)
  (define c (census-unknown-sites (mixed-programs)))
  ;; Only `y.go()` survives the whole-program context.
  (check-equal? (census-site-count c) 1)
  (check-equal? (census-unknown-functions c) 1)
  (check-equal? (census-unknown-files c) 1)
  (check-equal? (census-site-site (first (census-sites c))) "y.go")
  ;; The same two sites the real context resolves classify as unknown when
  ;; given no known names — that is the empty-context overcount this census
  ;; replaces.
  (check-equal? (classify-call (v "helper") (hash)) (cons 'unknown #f))
  (check-equal? (classify-call (dot (v "x") "m") (hash)) (cons 'unknown #f)))

(define (test-pure-program-has-no-sites)
  (define c (census-unknown-sites
             (list (cons "a.py" (prog (fn "f" (uir-null)))))))
  (check-equal? (census-site-count c) 0)
  (check-equal? (census-unknown-functions c) 0)
  (check-equal? (census-unknown-files c) 0)
  (check-equal? (census-function-count c) 1)
  (check-equal? (census-file-count c) 1))

;; ── site detail drives the aiming data ───────────────────────────────

(define (one-site-census body #:params [params '()])
  (census-unknown-sites
   (list (cons "app.py" (prog (fn "f" body #:params params))))))

(define (test-param-receiver-shape-and-lever)
  (define c (one-site-census (uir-call (dot (v "x") "go") '())
                             #:params (list (param "x"))))
  (define s (first (census-sites c)))
  (check-equal? (census-site-component s) "app.py")
  (check-equal? (census-site-function s) 'f)
  (check-equal? (census-site-shape s) 'param)
  (check-equal? (census-site-op s) 'go)
  (check-equal? (census-site-receiver s) 'x)
  (check-equal? (census-site-binding-kind s) 'param)
  (check-equal? (census-site-site s) "x.go")
  (check-equal? (census-site-lever s) 'parameter-dataflow)
  (check-false (census-site-higher-order? s)))

(define (test-call-result-receiver-shape-and-lever)
  (define c (one-site-census (uir-call (dot (uir-call (v "make") '()) "run") '())))
  (define s (first (census-sites c)))
  (check-equal? (census-site-shape s) 'call-result)
  (check-equal? (census-site-site s) "(make).run")
  (check-equal? (census-site-lever s) 'return-type-projection))

(define (test-self-attr-shape-and-lever)
  (define c (census-unknown-sites
             (list (cons "app.py"
                         (prog (py-class "A"
                                         (list (py-method "run"
                                                          (uir-call (dot (dot (v "self") "cache")
                                                                         "recompute_all")
                                                                    '())))))))))
  (define s (first (census-sites c)))
  (check-equal? (census-site-shape s) 'self-attr)
  (check-equal? (census-site-receiver s) 'self)
  (check-equal? (census-site-site s) "self.cache.recompute_all")
  (check-equal? (census-site-op s) 'recompute_all)
  (check-equal? (census-site-lever s) 'self-attr-typing))

(define (test-local-binding-kind-and-lever)
  ;; A local bound from an unresolvable call, then used as a receiver: the
  ;; census attributes it to return-type projection via the binding kind.
  (define c (one-site-census
             (prog (uir-set! (sym "g") (uir-call (v "make_thing") '()))
                   (uir-call (dot (v "g") "run") '()))))
  (define s (first (census-sites c)))
  (check-equal? (census-site-shape s) 'local)
  (check-equal? (census-site-binding-kind s) 'call-result)
  (check-equal? (census-site-lever s) 'return-type-projection))

(define (test-loop-var-lever)
  (define c (one-site-census
             (prog (uir-for-each (sym "item") (v "items")
                                 (uir-call (dot (v "item") "frobnicate") '())
                                 #f))))
  (define s (first (census-sites c)))
  (check-equal? (census-site-shape s) 'local)
  (check-equal? (census-site-binding-kind s) 'loop-var)
  (check-equal? (census-site-lever s) 'element-type-dataflow))

(define (test-higher-order-bare-call-lever)
  ;; `cb()` names one of f's parameters: a row hole, not a missing lexicon name.
  (define c (one-site-census (uir-call (v "cb") '())
                             #:params (list (param "cb"))))
  (define s (first (census-sites c)))
  (check-equal? (census-site-shape s) 'bare-call)
  (check-equal? (census-site-op s) 'cb)
  (check-equal? (census-site-site s) "cb")
  (check-true (census-site-higher-order? s))
  (check-equal? (census-site-lever s) 'higher-order-row-hole))

(define (test-bare-call-lexicon-lever)
  (define c (one-site-census (uir-call (v "some_external") '())))
  (define s (first (census-sites c)))
  (check-equal? (census-site-shape s) 'bare-call)
  (check-false (census-site-higher-order? s))
  (check-equal? (census-site-lever s) 'bare-call-lexicon))

(define (test-lever-taxonomy)
  (check-equal? (census-lever-of 'param #f) 'parameter-dataflow)
  (check-equal? (census-lever-of 'call-result #f) 'return-type-projection)
  (check-equal? (census-lever-of 'self-attr #f) 'self-attr-typing)
  (check-equal? (census-lever-of 'self-method #f) 'mro-edge-resolution)
  (check-equal? (census-lever-of 'chained #f) 'multi-level-chain)
  (check-equal? (census-lever-of 'symbol #f) 'import-handle-lexicon)
  (check-equal? (census-lever-of 'local 'attr) 'attribute-alias-typing)
  (check-equal? (census-lever-of 'local 'exception) 'exception-type-dataflow)
  (check-equal? (census-lever-of 'local 'literal) 'literal-constructor-typing)
  (check-equal? (census-lever-of 'local 'unbound) 'unbound-binding-resolution)
  (check-equal? (census-lever-of 'local 'fn) 'function-value-typing)
  (check-equal? (census-lever-of 'local #f) 'local-constructor-tracking)
  (check-equal? (census-lever-of 'bare-call #f #:higher-order? #t)
                'higher-order-row-hole)
  (check-equal? (census-lever-of 'bare-call #f) 'bare-call-lexicon)
  (check-equal? (census-lever-of 'other #f) 'unclassified)
  ;; Every lever in the taxonomy has a report description.
  (for ([lever (in-list census-levers)])
    (check-not-equal? (census-lever-description lever)
                      "no description"
                      (format "missing description for ~a" lever))))

(define (test-component-name-mapping)
  ;; The corpus driver renames components to dotted module paths as part of the
  ;; census call, so records are sorted by the component the report prints.
  (define c (census-unknown-sites
             (list (cons "m1.py" (prog (fn "f" (uir-call (v "z_external") '()))))
                   (cons "m2.py" (prog (fn "g" (uir-call (v "a_external") '())))))
             #:component-name
             (λ (comp)
               (cond [(string=? comp "m1.py") "z.module"]
                     [(string=? comp "m2.py") "a.module"]
                     [else comp]))))
  (check-equal? (map census-site-component (census-sites c))
                '("a.module" "z.module"))
  ;; The default keeps the raw component (existing call shape unchanged).
  (define raw (census-unknown-sites
               (list (cons "m1.py" (prog (fn "f" (uir-call (v "z_external") '())))))))
  (check-equal? (map census-site-component (census-sites raw)) '("m1.py")))

;; ── aggregation and serialization are deterministic ──────────────────

(define (test-aggregates)
  (define c (census-unknown-sites (mixed-programs)))
  (check-equal? (census-shape-counts c) (list (cons 'local 1)))
  (check-equal? (census-lever-counts c) (list (cons 'unbound-binding-resolution 1)))
  (check-equal? (census-binding-kind-counts c) (list (cons 'unbound 1)))
  (check-equal? (census-shape-op-counts c) (list (cons (cons 'local 'go) 1))))

(define (test-tsv-deterministic)
  (define tsv1 (with-output-to-string
                 (λ () (census->tsv (census-unknown-sites (mixed-programs))))))
  (define tsv2 (with-output-to-string
                 (λ () (census->tsv (census-unknown-sites (mixed-programs))))))
  (check-equal? tsv1 tsv2)
  (define lines (string-split tsv1 "\n" #:trim? #f))
  (check-equal? (first lines)
                "component\tfunction\tshape\top\treceiver\tbinding-kind\tlever\tsite\thigher-order?")
  (check-equal? (length (filter (λ (l) (not (string=? l ""))) lines)) 2))

(module+ main
  (test-real-context-not-empty-context)
  (test-pure-program-has-no-sites)
  (test-param-receiver-shape-and-lever)
  (test-call-result-receiver-shape-and-lever)
  (test-self-attr-shape-and-lever)
  (test-local-binding-kind-and-lever)
  (test-loop-var-lever)
  (test-higher-order-bare-call-lever)
  (test-bare-call-lexicon-lever)
  (test-lever-taxonomy)
  (test-component-name-mapping)
  (test-aggregates)
  (test-tsv-deterministic)
  (displayln "All census tests passed."))
