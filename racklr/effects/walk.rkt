#lang racket

;; racklr/effects/walk.rkt
;;
;; Generic structural walks over UIR nodes. `walk-uir` visits every struct node
;; once; `walk-body` visits everything except nested function boundaries, so a
;; nested function's effects do not leak into its enclosing function.

(require racklr/uir
         racklr/effects/shape)

(provide walk-uir walk-body)

;; Visit every struct node reachable from `n` (a node or a plain list),
;; applying `visit!` to each struct node exactly once (pre-order).
(define (walk-uir n visit!)
  (cond
    [(struct? n)
     (visit! n)
     (for ([f (in-list (cdr (vector->list (struct->vector n))))])
       (walk-uir f visit!))]
    [(pair? n)
     (walk-uir (car n) visit!)
     (walk-uir (cdr n) visit!)]
    [else (void)]))

;; Like walk-uir but does not descend into nested uir-fn nodes: a nested
;; function is a separate call-graph node whose own body is analyzed on its
;; own, so its effects must not leak into the enclosing function.
(define (walk-body n visit!)
  (cond
    [(uir-fn? n) (void)]
    [(uir-method? n) (void)]
    [(uir-class? n) (void)]
    [(arrow-call? n) (void)]
    [(function-expr-call? n) (void)]
    [(struct? n)
     (visit! n)
     (for ([f (in-list (cdr (vector->list (struct->vector n))))])
       (walk-body f visit!))]
    [(pair? n)
     (walk-body (car n) visit!)
     (walk-body (cdr n) visit!)]
    [else (void)]))
