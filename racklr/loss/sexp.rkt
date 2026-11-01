#lang racket

;; racklr/loss/sexp.rkt — helpers over the canonical UIR serialization.
;;
;; The loss verifier compares UIR through `uir->sexp`, the canonical,
;; struct-free representation, so its normalizations are pure tree transforms.
;; These helpers name the node shapes the layer needs; they carry no
;; language-pair behavior.

(require racklr/uir)

(provide tag
         list-tag?
         sym-name
         ref-name
         decl-name
         generated-name?
         all-nodes
         sexp-diff
         (struct-out sexp-path-diff))

(define (tag x) (and (pair? x) (car x)))
(define (list-tag? x t) (and (pair? x) (eq? (car x) t)))
(define (sym-name x) (and (pair? x) (eq? (car x) 'symbol) (cadr x)))

;; Name of a reference node: (symbol s) or (var (symbol s)).
(define (ref-name x)
  (cond [(sym-name x) (sym-name x)]
        [(and (list-tag? x 'var) (sym-name (cadr x))) (sym-name (cadr x))]
        [else #f]))

(define decl-tags
  '(fn class method let set! import type-alias interface enum
       effect-decl effect-handler component))

;; The name a declaration binds, for the tags above (export/import handled by
;; callers that care about sources rather than bindings).
(define (decl-name s)
  (and (pair? s) (memq (tag s) decl-tags)
       (let ([field (cadr s)])
         (ref-name field))))

(define (generated-name? n)
  (and (string? n) (string-prefix? n "__")))

;; Every node in the tree, in pre-order.
(define (all-nodes x)
  (cond [(pair? x) (append (list x) (append* (map all-nodes x)))]
        [else '()]))

(struct sexp-path-diff (path before after) #:transparent)

;; The first `limit` positions where two canonical serializations disagree.
;; A reviewable evidence list for an undeclared loss.
(define (sexp-diff a b [limit 8])
  (define out '())
  (define (path-str p)
    (if (null? p) "$" (string-join (map (λ (x) (format "[~a]" x)) (reverse p)) "")))
  (define (record! path x y)
    (when (< (length out) limit)
      (set! out (cons (sexp-path-diff (path-str path) x y) out))))
  (define (walk a b path)
    (cond [(equal? a b) (void)]
          [(and (pair? a) (pair? b) (= (length a) (length b)))
           (for ([x a] [y b] [i (in-naturals)]) (walk x y (cons i path)))]
          [else (record! path a b)]))
  (walk a b '())
  (reverse out))
