#lang racket

;; racklr/self/seed/builders.rkt
;;
;; UIR construction helpers for the P6.2 seed compiler. The seed is a UIR
;; *program*, not Racket host code: emitted to Python, the value of this program
;; is a working compiler for the seed language (see program.rkt). These helpers
;; keep that program readable and pin the shapes emit-python depends on:
;;
;;   * Operators (+ - * / < > == != and or not) and ordinary function calls are
;;     `(uir-call (uir-symbol NAME) args)`. emit-python renders a symbol callee
;;     as infix (`a + b`) or a plain call (`f(x)`); a `uir-var` callee would
;;     render the call form `+(a, b)` and break the emitted compiler.
;;   * Method calls are `(uir-call (uir-get base (uir-string NAME)) args)`,
;;     which emit-python renders `base.NAME(args)`.
;;   * Indexing is `(uir-get base index)` with a non-string index: a uir-var or
;;     uir-number index renders `base[index]`; a uir-string field renders
;;     `base.field`.
;;   * Every named function carries an explicit `uir-effect-row`. Pure stages
;;     get the empty row (emit-python renders `@__withEffects({})`, so total
;;     functions are visibly total); main gets the io row.
;;
;; The helpers are convenience only: the program value is built from UIR
;; structs alone, with no Racket closure embedded in it.

(require racklr/uir)

(provide
 ;; leaves
 sym var str num bool none
 ;; calls / access
 op call mcall index paren lst concat and* or*
 ;; statements
 assign ret body while-loop if-else if1
 ;; functions and effect rows
 pure-def io-def fn-with row io-row entry
 ;; program
 program)

;; ── Leaves ─────────────────────────────────────────────────────────

;; Accept a string (preferred, mirrors the emitted name) or a symbol.
(define (sym s) (uir-symbol (if (symbol? s) (symbol->string s) s)))
(define (var s) (uir-var (sym s)))
(define (str s) (uir-string s))
;; uir-number keeps the literal as text (precision); accept either an exact
;; integer or the already-written string.
(define (num n) (uir-number (if (number? n) (number->string n) n)))
(define (bool b) (uir-bool b))
(define (none) (uir-null))

;; ── Calls and access ───────────────────────────────────────────────

;; Operator application: infix in the emitted Python (`a + b`, `a < b`,
;; `a and b`). `args` must be the operand list, exactly two for infix shapes.
(define (op name args) (uir-call (sym name) args))

;; Ordinary call. Same construction as `op`; the separation names intent —
;; `op` is an operator, `call` is a procedure/builtin invocation.
(define (call name args) (uir-call (sym name) args))

;; Method call: (mcall base "append" (list x)) => base.append(x)
(define (mcall base method args)
  (uir-call (uir-get base (str method)) args))

;; Subscript: (index base (num 0)) => base[0]
(define (index base idx) (uir-get base idx))

(define (paren x) (uir-paren x))
(define (lst items) (uir-list items))

;; Left-nested string/number concatenation: (concat (list a b c)) => a + b + c.
;; emit-python renders nested `+` without parentheses, and left-nesting matches
;; Python's own left associativity, so the value and the rendered text agree.
(define (concat parts)
  (cond [(null? parts) (str "")]
        [else (foldl (λ (p acc) (op "+" (list acc p))) (car parts) (cdr parts))]))

;; Short-circuit chains. Right-nested, rendered `a and b and c` with no
;; parentheses; Python's left-assoc `and`/`or` admit the same disjunct order, so
;; the text is valid and the evaluation order is the source order.
(define (and* . xs)
  (cond [(null? xs) (bool #t)]
        [(null? (cdr xs)) (car xs)]
        [else (op "and" (list (car xs) (apply and* (cdr xs))))]))

(define (or* . xs)
  (cond [(null? xs) (bool #f)]
        [(null? (cdr xs)) (car xs)]
        [else (op "or" (list (car xs) (apply or* (cdr xs))))]))

;; ── Statements ─────────────────────────────────────────────────────

(define (assign name value) (uir-set! (sym name) value))
(define (ret value) (uir-return value))

;; Block body from statements in order; also the body shape uir-while/uir-if
;; expect when the emitter must produce an indented suite.
(define (body . stmts) (uir-block stmts))

(define (while-loop test b) (uir-while test b (uir-null)))
(define (if-else test then else) (uir-if test then else))
(define (if1 test then) (uir-if test then (uir-null)))

;; ── Functions and effect rows ──────────────────────────────────────

(define (row entries) (uir-effect-row entries))

;; One row entry: (entry "io" 'may '() #f #f).
(define (entry name modality ops budget scope)
  (uir-effect-entry (sym name) modality (map sym ops) budget
                    (and scope (sym scope))))

;; The io row main carries: `io` may be performed, any operation.
(define (io-row)
  (row (list (entry "io" 'may '() #f #f))))

(define (fn-with name params b effects)
  (uir-fn (sym name) (map sym params) b #f effects))

;; A named function with an explicit empty row: total, visibly so in the emit.
(define (pure-def name params b)
  (fn-with name params b (row '())))

;; A named function with the io row.
(define (io-def name params b)
  (fn-with name params b (io-row)))

;; ── Program ────────────────────────────────────────────────────────

;; The UIR unit: function definitions followed by the entry call.
(define (program . stmts) (uir-block stmts))
