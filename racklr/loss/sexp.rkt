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
         fn-node?
         fn-node-fields
         fn-node-map
         typed-param-node?
         typed-param-node-fields
         typed-param-erase-type
         sexp-replace-at
         ann-set-node->set!
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

;; ── Serialized struct shapes ────────────────────────────────────────
;;
;; UIR structs gain fields by appending, so their serialized lists gain
;; trailing elements. The helpers below name the current slots and preserve
;; every element beyond them, so a schema addition (for example the
;; typed-param `kind` field) survives normalization instead of being silently
;; rebuilt away. These are the only places that know positional field order.

(define (sexp-replace-at lst changes)
  (for/list ([v (in-list lst)] [i (in-naturals)])
    (define c (hash-ref changes i #f))
    (cond [(procedure? c) (c v)]
          [c c]
          [else v])))

;; (fn name params body return-type effects [future fields...])
(define (fn-node? x) (list-tag? x 'fn))

(define (fn-node-fields x)
  (values (list-ref x 1)   ; name
          (list-ref x 2)   ; params
          (list-ref x 3)   ; body
          (list-ref x 4)   ; return-type
          (list-ref x 5))) ; effects

;; Transform only the named slots; #f means "leave this slot alone". Every
;; other element, including fields added by a later schema, is preserved.
(define (fn-node-map x
                     #:name [name #f]
                     #:params [params #f]
                     #:body [body #f]
                     #:ret [ret #f]
                     #:effects [effects #f])
  (define changes (make-hash))
  (when name (hash-set! changes 1 name))
  (when params (hash-set! changes 2 params))
  (when body (hash-set! changes 3 body))
  (when ret (hash-set! changes 4 ret))
  (when effects (hash-set! changes 5 effects))
  (sexp-replace-at x changes))

;; (typed-param name type default [kind ...]) — kind is optional (schema v2).
(define (typed-param-node? x) (list-tag? x 'typed-param))

(define (typed-param-node-fields x)
  (values (list-ref x 1) (list-ref x 2) (list-ref x 3)))

;; Erase only the type slot; the default, any parameter-kind marker, and any
;; future field stay in place.
(define (typed-param-erase-type x)
  (sexp-replace-at x (hash 2 (λ (_) #f))))

;; (ann-set! lhs type value [future fields...]) → (set! lhs value [future...]).
;; The type slot is the intentional erasure; other fields are carried over.
(define (ann-set-node->set! x)
  (define lhs (list-ref x 1))
  (define value (list-ref x 3))
  (list* 'set! lhs (if value value '(null)) (drop x 4)))
