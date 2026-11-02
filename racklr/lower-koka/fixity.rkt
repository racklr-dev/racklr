#lang racket

;; racklr/lower-koka/fixity.rkt — Koka operator fixity data + chain
;; re-association.
;;
;; The generated parser keeps every binary operator at one precedence (a flat
;; left-leaning chain), while Koka resolves operator precedence and
;; associativity from fixity declarations plus the standard prelude table.
;; Lowering therefore re-associates each flattened operator chain with Koka's
;; precedence rules before building UIR, so the UIR (and the emitted text)
;; means what the source means. Fixity declarations in the lowered file are
;; registered as they are lowered; operators not covered fall back to the
;; prelude table, then to a left-associative default.
;;
;; `:=` is Koka's assignment: it has the lowest precedence and is
;; right-associative, so `n := n + 1` lowers to `set!(n, n + 1)`.

(require racklr/uir
         "helpers.rkt")

(provide reset-fixities! register-fixity! assemble-operator-chain
         fixity-of normalize-operator-name)

;; name → (precedence . associativity), assoc ∈ 'l 'r 'n (non-associative)
(define prelude-fixities
  (hash ":" (cons 90 'r)
        "^" (cons 80 'r)
        "*" (cons 70 'l) "%" (cons 70 'l) "/" (cons 70 'l)
        "++" (cons 60 'r)
        "+" (cons 60 'l) "-" (cons 60 'l)
        "++." (cons 55 'r)
        "!=" (cons 40 'n) "==" (cons 40 'n) "<=" (cons 40 'n)
        ">=" (cons 40 'n) "<" (cons 40 'n) ">" (cons 40 'n)
        "&&" (cons 30 'r) "||" (cons 20 'r)
        ":=" (cons 1 'r)))

(define current-fixities (make-parameter (hash-copy prelude-fixities)))

(define (reset-fixities!)
  (current-fixities (hash-copy prelude-fixities)))

;; `(*)/ (/)/ (+)/ …` fixity names are declared with parentheses; expression
;; operators are the bare symbol text, so the name is normalized.
(define (normalize-operator-name name)
  (cond
    [(and (> (string-length name) 2)
          (char=? (string-ref name 0) #\()
          (char=? (string-ref name (sub1 (string-length name))) #\)))
     (substring name 1 (sub1 (string-length name)))]
    [else name]))

(define (register-fixity! kind precedence names)
  (define assoc (case kind [(infixl) 'l] [(infixr) 'r] [else 'n]))
  (define table (current-fixities))
  (for ([n (in-list names)])
    (hash-set! table (normalize-operator-name n) (cons precedence assoc))))

(define (fixity-of name)
  (hash-ref (current-fixities) name (cons 50 'l)))

;; ── Re-association ──────────────────────────────────────────────────

;; operands : (listof uir) (one more than ops)
;; ops      : (listof string) (the binary operator between operands)
;; → a left/right-associated uir call tree (or the single operand).
(define (assemble-operator-chain operands ops)
  (define values '())      ; uir stack (last first)
  (define operators '())   ; (name prec assoc) stack
  (define (apply-top!)
    (define op (car operators))
    (set! operators (cdr operators))
    (define name (car op))
    (define rhs (car values))
    (define lhs (cadr values))
    (set! values
          (cons (if (equal? name ":=")
                    (uir-set! lhs rhs)
                    (uir-call (uir-symbol name) (list lhs rhs)))
                (cddr values))))
  (define (precedence op) (cadr op))
  (define (assoc op) (cddr op))
  (define (top-precedence) (if (null? operators) -1 (precedence (car operators))))
  (for ([operand (in-list operands)] [op (in-list (append ops (list #f)))])
    (set! values (cons operand values))
    (when op
      (define entry (cons op (fixity-of op)))
      (define p (precedence entry))
      (let loop ()
        (when (and (pair? operators)
                   (or (> (precedence (car operators)) p)
                       (and (= (precedence (car operators)) p)
                            (eq? (assoc (car operators)) 'l))))
          (apply-top!)
          (loop)))
      (set! operators (cons entry operators))))
  ;; flush the operator stack
  (let loop ()
    (when (pair? operators)
      (apply-top!)
      (loop)))
  (car values))
