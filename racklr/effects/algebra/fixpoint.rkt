#lang racket

;; racklr/effects/algebra/fixpoint.rkt
;;
;; The generic monotone-fixpoint driver the inference passes run on: given
;;   direct — per-node rows computed from the node's own body (no callees),
;;   edges  — node → callees whose rows the node inherits,
;; iterate `row = join(direct, callee-rows...)` to the least fixed point.
;;
;; Joining is monotone and the algebra is finite per node (one entry per effect
;; name, canonical evidence sets), so the iteration ascends and converges. The
;; driver is deliberately generic over the join so the law tests can exercise
;; the same code inference uses, and it is separate from any classifier: the
;; algebra, not the language, owns the fixpoint.

(require data/queue
         "row.rkt"
         "combinators.rkt")

(provide
  fixpoint-step
  least-fixpoint
  rows-equal?
  rows-leq?
  same-rows?)

;; A row from a table, or the empty row lazily: `hash-ref`'s default argument is
;; evaluated eagerly, and building an empty algebra row allocates a canonical
;; row + provenance table, so the default must not be constructed per lookup.
(define (lookup-row table k)
  (or (hash-ref table k #f) (empty-algebra-row)))

;; Every node that can carry a row: direct nodes plus any callee mentioned by
;; an edge (a callee without a direct row still contributes the empty row).
(define (fixpoint-keys direct edges)
  (remove-duplicates
   (append (hash-keys direct)
           (append-map (λ (k) (hash-ref edges k '()))
                       (hash-keys edges)))))

;; One monotone iteration: every node's row becomes the join of its direct row
;; and its current callees' rows. An unchanged row is reused, so a table that
;; has reached its fixed point compares `eq?` and allocates nothing.
(define (fixpoint-step direct edges rows [join-fn join])
  (for/hash ([k (in-list (fixpoint-keys direct edges))])
    (define direct-row (lookup-row direct k))
    (define callee-rows
      (for/list ([callee (in-list (hash-ref edges k '()))])
        (lookup-row rows callee)))
    (define new-row (foldl join-fn direct-row callee-rows))
    (define current (hash-ref rows k #f))
    (values k (if (and current (same-row? current new-row)) current new-row))))

;; Pointwise structural equality between two row tables. Rows are canonical, so
;; this is `equal?` per node (no join), unlike `same-rows?` which checks the
;; algebra order.
(define (rows-equal? a b)
  (and (= (hash-count a) (hash-count b))
       (for/and ([(k row) (in-hash a)])
         (define other (hash-ref b k #f))
         (and other (same-row? row other)))))

;; Pointwise row order between two row tables: same node set, and `a` at most
;; `b` at every node.
(define (rows-leq? a b)
  (and (= (hash-count a) (hash-count b))
       (for/and ([(k row) (in-hash a)])
         (define other (hash-ref b k #f))
         (and other (row-leq? row other)))))

;; Pointwise row equality between two row tables.
(define (same-rows? a b)
  (and (rows-leq? a b) (rows-leq? b a)))

;; The least fixed point of `fixpoint-step` above the direct rows: the least
;; row table that contains every direct row and is least closed under the
;; call-graph edges.
;;
;; Solved with a worklist, not full sweeps. Starting from the direct rows (the
;; least candidate) every node is pending once; when a node's row changes only
;; its callers are re-enqueued, because they are the only nodes whose join
;; reads it. A node is recomputed only when something it depends on moved, so
;; the driver does work proportional to the propagation, and it reaches the
;; same least fixed point as iterating `fixpoint-step` to stability (the two
;; agree because the update is monotone and the algebra is finite).
(define (least-fixpoint direct edges [join-fn join]
                        #:max-iterations [max-iterations #f])
  (define keys (fixpoint-keys direct edges))
  (define rows (make-hash))
  (for ([k (in-list keys)])
    (hash-set! rows k (lookup-row direct k)))
  ;; callee -> callers, so a changed row re-enqueues exactly its dependents.
  (define callers (make-hash))
  (for ([k (in-list keys)])
    (for ([callee (in-list (hash-ref edges k '()))])
      (hash-update! callers callee (λ (l) (cons k l)) '())))
  (define (compute-row k)
    (foldl join-fn
           (lookup-row direct k)
           (for/list ([callee (in-list (hash-ref edges k '()))])
             (lookup-row rows callee))))
  (define pending (make-queue))
  (define queued (make-hash))
  (define (push! k)
    (unless (hash-has-key? queued k)
      (hash-set! queued k #t)
      (enqueue! pending k)))
  (for ([k (in-list keys)]) (push! k))
  ;; Recompute every node once from the converged rows, without reusing the
  ;; previous objects. Rows that only differ from an earlier iteration in
  ;; presentation (operation order) are made canonical here, so the returned
  ;; table is the same fixpoint the full-sweep iteration would have produced.
  (define (final-pass)
    (for/hash ([k (in-list keys)])
      (values k (compute-row k))))
  (let loop ([processed 0])
    (cond
      [(queue-empty? pending) (final-pass)]
      [(and max-iterations (>= processed max-iterations))
       (error 'least-fixpoint
              "did not converge after ~a node updates over ~a nodes"
              processed (length keys))]
      [else
       (define k (dequeue! pending))
       (hash-remove! queued k)
       (define new-row (compute-row k))
       (cond
         [(same-row? (hash-ref rows k) new-row) (loop (add1 processed))]
         [else
          (hash-set! rows k new-row)
          (for ([caller (in-list (hash-ref callers k '()))])
            (push! caller))
          (loop (add1 processed))])])))
