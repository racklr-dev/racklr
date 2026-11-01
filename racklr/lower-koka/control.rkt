#lang racket

;; racklr/lower-koka/control.rkt — Koka `if`/`elif`/`return` and `match`.
;;
;; Extracted from expr.rkt to keep every lowering module under the 400-line
;; style cap. Expressions, block expressions and non-trailing-lambda
;; expressions are reached through the hooks in hooks.rkt, so there is no
;; module cycle.
;;
;; Mapping: `if e then a elif ... else b` → nested uir-if (the last `else` may
;; be absent, or an `if e then return x` form); `match e { p | g -> b }` →
;; uir-match with one uir-case per rule (pattern, guard, body). Multiple
;; comma-separated patterns become a tuple pattern.

(require racklr/tree
         racklr/uir
         "helpers.rkt"
         "pattern.rkt"
         "hooks.rkt")

(provide lower-if lower-match)

(define (single-node n)
  (for/first ([c (in-list (children n))] #:when (node? c)) c))

(define (lower-if node)
  (define test ((lower-ntlexpr-fn) (find-node node 'ntlexpr)))
  ;; `if c then return e` has no branches: RETURN is a direct token and the
  ;; returned expression follows it.
  (define return? (find-token node 'RETURN))
  (define elifs (find-node node 'elifs))
  (cond
    [return? (uir-if test (uir-return ((lower-expr-fn) (find-node node 'expr))) (uir-null))]
    [else
     (define then ((lower-blockexpr-fn) (find-node node 'blockexpr)))
     (if elifs
         (uir-if test then (lower-elifs elifs))
         (uir-if test then (uir-null)))]))

(define (lower-elifs node)
  (cond
    [(tokens node) ; ELSE blockexpr
     ((lower-blockexpr-fn) (find-node node 'blockexpr))]
    [else ; ELIF ntlexpr THEN blockexpr elifs
     (define test ((lower-ntlexpr-fn) (find-node node 'ntlexpr)))
     (define then ((lower-blockexpr-fn) (find-node node 'blockexpr)))
     (uir-if test then (lower-elifs (find-node node 'elifs)))]))

(define (lower-match node)
  (define subject ((lower-ntlexpr-fn) (find-node node 'ntlexpr)))
  (define rules (find-node node 'matchrules))
  (define cases
    (for/list ([g (in-list (all-list-items rules))]
               #:when (and (node? g) (eq? (node-tag g) 'group)))
      (define rule (find-node g 'matchrule))
      (and rule (lower-matchrule rule))))
  (uir-match subject (filter values cases)))

(define (lower-matchrule node)
  (define patterns (lower-patterns1 (find-node node 'patterns1)))
  (define pattern
    (case (length patterns)
      [(1) (first patterns)]
      [else (uir-pat-class (uir-symbol (tuple-class-name (length patterns)))
                           patterns '())]))
  (define guard-expr (find-node node 'expr))
  (define guard (and guard-expr ((lower-expr-fn) guard-expr)))
  (define body ((lower-blockexpr-fn) (find-node node 'blockexpr)))
  (uir-case pattern (or guard (uir-null)) body))

(define (lower-patterns1 node)
  (define first (find-node node 'pattern))
  (define rest
    (for/list ([g (in-list (all-list-items node))]
               #:when (and (node? g) (eq? (node-tag g) 'group)))
      (find-node g 'pattern)))
  (for/list ([p (in-list (filter values (cons first rest)))])
    (lower-pattern p)))

(define (tuple-class-name n)
  (string-append "(" (make-string (sub1 n) #\,) ")"))
