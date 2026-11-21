#lang racket

;; ── Shared CST helpers for the JavaScript lowering split (B75) ──────
;; tok? / cst-kids / tag-of / kids-of / find-kid / find-list /
;; find-node-or-list are used by both stmt.rkt and expr.rkt.

(require racklr/tree)

(provide tok? cst-kids tag-of kids-of find-kid find-list find-node-or-list
         rule-elements first-token-leaf string-literal-text lower-property-name)

(define (tok? x tk-type)
  (and (not (cst-node? x)) (not (null? x)) (not (eq? x 'none))
       (not (pair? x))
       (with-handlers ([exn:fail? (λ (_) #f)])
         (tk-type x) #t)))

(define (cst-kids n) (filter cst-node? (cst-node-children n)))
(define (tag-of n) (cst-node-tag n))
(define (kids-of n) (cst-node-children n))

(define (find-kid n tag)
  (for/or ([k (kids-of n)] #:when (and (cst-node? k) (eq? (tag-of k) tag))) k))

(define (find-list n)
  (for/or ([k (kids-of n)] #:when (pair? k)) k))

(define (find-node-or-list n)
  (for/or ([k (kids-of n)] #:when (or (cst-node? k) (pair? k))) k))

;; Collect the rule nodes named `tag` that a repetition contributes to a CST
;; rule body. The generated parser wraps each repeated element in a `group`
;; node (Comma + element) inside a pair list; a repetition's first element is a
;; direct child. Iterating `kids-of` alone loses every element after the first,
;; which silently dropped parameters, bindings and object members.
(define (rule-elements n tag)
  (define (collect k)
    (cond
      [(and (cst-node? k) (eq? (tag-of k) tag)) (list k)]
      [(and (cst-node? k) (eq? (tag-of k) 'group))
       (append* (for/list ([g (kids-of k)]) (collect g)))]
      [(pair? k) (append* (for/list ([g k]) (collect g)))]
      [else '()]))
  (append* (for/list ([k (kids-of n)]) (collect k))))

;; The first token under node: identifier, keyword, string, numeric, etc.
(define (first-token-leaf node tk-type)
  (cond
    [(tok? node tk-type) node]
    [(cst-node? node)
     (for/first ([k (in-list (kids-of node))]
                 #:when (or (tok? k tk-type) (cst-node? k)))
       (first-token-leaf k tk-type))]
    [(pair? node)
     (for/first ([k (in-list node)]
                 #:when (or (tok? k tk-type) (cst-node? k)))
       (first-token-leaf k tk-type))]
    [else #f]))

;; Text of a non-computed propertyName — identifierName, StringLiteral, or
;; numericLiteral. The UIR keeps the key text; emit-javascript quotes it when
;; the text is not a valid JS identifier (e.g. 'accent-1', 28, '5xl').
(define (lower-property-name node tk-type tk-value)
  (define t (first-token-leaf node tk-type))
  (define raw (and t (tk-value t)))
  (cond
    [(not raw) "?"]
    [(eq? (tk-type t) 'StringLiteral) (string-literal-text raw)]
    [else raw]))

;; A StringLiteral token's value keeps its quotes; a lowered string (and a
;; string-literal property name) keeps the unquoted text.
(define (string-literal-text raw)
  (define len (string-length raw))
  (if (and (>= len 2)
           (let ([first (string-ref raw 0)]
                 [last (string-ref raw (sub1 len))])
             (and (char=? first last)
                  (or (char=? first #\") (char=? first #\')))))
      (substring raw 1 (sub1 len))
      raw))
