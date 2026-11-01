#lang racket

;; racklr/lower-racket/special.rkt
;;
;; Racket core special forms → UIR. The expression and body lowerers are
;; passed in as callbacks so this module owns control-form knowledge without a
;; require cycle back into expr.rkt.
;;
;; Two rules keep the effect graph honest:
;;   - patterns and syntax data (match patterns, for binding patterns, quoted
;;     data, syntax-rules templates) are never lowered as calls;
;;   - an anonymous function is emitted as a uniquely named uir-fn and the
;;     enclosing body gets a call edge to it. A Racket lambda passed to a
;;     higher-order function runs in the enclosing call chain, so attributing
;;     its effects to the enclosing function is the safe direction (an
;;     over-approximation, never a hidden effect).

(require racket/list
         racklr/uir
         "syntax.rkt")

(provide special-form?
         lower-special-form)

;; Core forms the lowering handles structurally instead of as calls.
(define special-forms
  '(quote quasiquote unquote unquote-splicing lambda λ let let* letrec
    let-values let*-values let/cc if cond case when unless begin and or
    set! parameterize with-handlers dynamic-wind
    for for/list for/vector for/hash for/and for/or for/first for/last
    for/sum for/product for*/list for*/fold for*/hash for*/and for*/or
    for*/first for*/last for*/sum for*/product for*/vector for/fold for/foldr
    for/set for/set! for*/set for*/set! begin0
    match match* match-lambda match-lambda* match-define
    struct-copy syntax syntax-case with-syntax))

(define (special-form? h) (and (symbol? h) (memq h special-forms)))

;; The form's `(lower-special-form ctx env head args expr-fn body-fn)`:
;; expr-fn : (ctx env datum → uir?) and body-fn : (ctx env (listof datum) → uir?).
(define (lower-special-form ctx env h args expr-fn body-fn)
  (case h
    [(quote) (lower-datum (car args))]
    [(quasiquote) (lower-quasi ctx env (car args) expr-fn)]
    [(syntax syntax-case with-syntax) (lower-datum (car args))]
    [(lambda λ) (lower-lambda-form ctx env args expr-fn body-fn)]
    [(let let* letrec) (lower-let-form ctx env h args expr-fn body-fn)]
    [(let-values let*-values) (lower-let-values-form ctx env args expr-fn body-fn)]
    [(let/cc)
     (define env* (extend-env env (binding-ids (car args))))
     (body-fn ctx env* (cdr args))]
    [(if)
     (uir-if (expr-fn ctx env (first args))
             (expr-fn ctx env (second args))
             (if (pair? (cddr args)) (expr-fn ctx env (third args)) (uir-null)))]
    [(cond) (lower-cond-form ctx env args expr-fn body-fn)]
    [(case) (lower-case-form ctx env args expr-fn body-fn)]
    [(when) (uir-if (expr-fn ctx env (car args)) (body-fn ctx env (cdr args)) (uir-null))]
    [(unless) (uir-if (expr-fn ctx env (car args)) (uir-null) (body-fn ctx env (cdr args)))]
    [(begin and or begin0) (body-fn ctx env args)]
    [(set!)
     (uir-set! (bare-symbol (car args))
               (if (pair? (cdr args)) (expr-fn ctx env (cadr args)) (uir-null)))]
    [(parameterize) (lower-parameterize-form ctx env args expr-fn body-fn)]
    [(with-handlers) (lower-with-handlers-form ctx env args expr-fn body-fn)]
    [(dynamic-wind) (body-fn ctx env args)]
    [(for for/list for/vector for/hash for/and for/or for/first for/last
          for/sum for/product for*/list for*/and for*/or for*/first for*/last
          for*/sum for*/product for*/vector for/set for/set! for*/set for*/set!)
     (lower-for-form ctx env args expr-fn body-fn)]
    [(for/fold for/foldr for*/fold) (lower-for-fold-form ctx env args expr-fn body-fn)]
    [(match match*) (lower-match-form ctx env h args expr-fn body-fn)]
    [(match-define)
     (define value (and (pair? (cdr args)) (cadr args)))
     (if value (expr-fn ctx env value) (uir-null))]
    [(match-lambda match-lambda*)
     (emit-anonymous-fn ctx '() (lower-match-form ctx env 'match args expr-fn body-fn))]
    [(struct-copy) (lower-struct-copy-form ctx env args expr-fn)]
    [else (body-fn ctx env args)]))

;; ── anonymous functions ────────────────────────────────────────────

;; A nested function gets a unique symbol and an immediate call edge from the
;; enclosing body, so its effects are visible to the caller's row.
(define (emit-anonymous-fn ctx params body-uir)
  (define emitted (next-local-name ctx 'lambda))
  (uir-block
   (list (uir-fn (bare-symbol emitted) params body-uir #f #f)
         (uir-call (uir-var (bare-symbol emitted)) '()))))

;; ── quasiquote ─────────────────────────────────────────────────────

(define (lower-quasi ctx env d expr-fn)
  (cond
    [(and (pair? d) (eq? (car d) 'unquote) (pair? (cdr d))) (expr-fn ctx env (cadr d))]
    [(and (pair? d) (eq? (car d) 'unquote-splicing) (pair? (cdr d))) (expr-fn ctx env (cadr d))]
    [(pair? d) (uir-list (list (lower-quasi ctx env (car d) expr-fn)
                               (lower-quasi ctx env (cdr d) expr-fn)))]
    [else (lower-datum d)]))

;; ── lambda ─────────────────────────────────────────────────────────

(define (lower-lambda-form ctx env args expr-fn body-fn)
  (define ps (car args))
  (define env* (extend-env env (binding-ids ps)))
  (emit-anonymous-fn ctx (lower-params ps) (body-fn ctx env* (cdr args))))

;; ── let ────────────────────────────────────────────────────────────

(define (lower-let-form ctx env h args expr-fn body-fn)
  (define named (and (symbol? (car args)) (car args)))
  (define bindings (if named (second args) (car args)))
  (define body (if named (cddr args) (cdr args)))
  (if named
      (lower-named-let ctx env named bindings body expr-fn body-fn)
      (let loop ([bs bindings] [env* env] [nodes '()])
        (cond
          [(null? bs) (uir-block (append (reverse nodes) (list (body-fn ctx env* body))))]
          [else
           (define-values (ns ids) (binding-uir ctx (if (eq? h 'let) env env*) (car bs) expr-fn))
           (loop (cdr bs) (extend-env env* ids) (append (reverse ns) nodes))]))))

;; (let name ([id init] ...) body ...) is a local recursive function: model it
;; as a named uir-fn plus the call that starts it, so recursion and the
;; enclosing body's effect row both see the body.
(define (lower-named-let ctx env name bindings body expr-fn body-fn)
  (define ids (append-map (λ (b) (binding-ids (car b))) bindings))
  (define inits
    (append-map (λ (b) (if (and (pair? b) (pair? (cdr b))) (list (expr-fn ctx env (cadr b))) '()))
                bindings))
  (define emitted (next-local-name ctx name))
  (define body-env (extended-env (extend-env env ids) (list (cons name emitted))))
  (define fn (uir-fn (bare-symbol emitted) (lower-params ids) (body-fn ctx body-env body) #f #f))
  (uir-block (append inits
                     (list fn
                           (uir-call (uir-var (bare-symbol emitted)) '())))))

(define (lower-let-values-form ctx env args expr-fn body-fn)
  (define bindings (car args))
  (define body (cdr args))
  (let loop ([bs bindings] [env* env] [nodes '()])
    (cond
      [(null? bs) (uir-block (append (reverse nodes) (list (body-fn ctx env* body))))]
      [else
       (define-values (ns ids) (binding-uir ctx env* (car bs) expr-fn))
       (loop (cdr bs) (extend-env env* ids) (append (reverse ns) nodes))])))

;; ── conditionals ───────────────────────────────────────────────────

(define (lower-cond-form ctx env clauses expr-fn body-fn)
  (cond
    [(null? clauses) (uir-null)]
    [else
     (define cl (car clauses))
     (define test (car cl))
     (define rest (cdr cl))
     (cond
       [(eq? test 'else) (body-fn ctx env rest)]
       [(and (pair? rest) (eq? (car rest) '=>) (pair? (cdr rest)))
        (uir-if (expr-fn ctx env test)
                (expr-fn ctx env (cadr rest))
                (lower-cond-form ctx env (cdr clauses) expr-fn body-fn))]
       [else
        (uir-if (expr-fn ctx env test)
                (body-fn ctx env rest)
                (lower-cond-form ctx env (cdr clauses) expr-fn body-fn))])]))

(define (lower-case-form ctx env args expr-fn body-fn)
  (define key (expr-fn ctx env (car args)))
  (define clauses (cdr args))
  (define bodies
    (append-map
     (λ (cl)
       (cond
         [(and (pair? cl) (eq? (car cl) 'else)) (list (body-fn ctx env (cdr cl)))]
         [(and (pair? cl) (pair? (cdr cl)) (eq? (cadr cl) '=>))
          (list (expr-fn ctx env (if (pair? (cddr cl)) (caddr cl) (uir-null))))]
         [(pair? cl) (list (body-fn ctx env (cdr cl)))]
         [else '()]))
     clauses))
  (uir-block (cons key bodies)))

;; ── dynamic binding / handlers ─────────────────────────────────────

(define (lower-parameterize-form ctx env args expr-fn body-fn)
  (define bindings (car args))
  (define nodes
    (append-map (λ (b) (if (and (pair? b) (pair? (cdr b)))
                           (list (expr-fn ctx env (cadr b)))
                           '()))
                bindings))
  (uir-block (append nodes (list (body-fn ctx env (cdr args))))))

(define (lower-with-handlers-form ctx env args expr-fn body-fn)
  (define handlers (car args))
  (define nodes
    (append-map
     (λ (hp)
       (if (and (pair? hp) (pair? (cdr hp)))
           (list (uir-list (list (expr-fn ctx env (car hp))
                                 (expr-fn ctx env (cadr hp)))))
           '()))
     handlers))
  (uir-block (append nodes (list (body-fn ctx env (cdr args))))))

;; ── for ────────────────────────────────────────────────────────────

;; A for clause is a binding clause [id seq ...] (possibly with an inner
;; #:when guard), a keyword clause #:when / #:unless / #:break / #:final, or a
;; nested clause list for the for* variants.
(define (for-clause-ids clauses)
  (append-map
   (λ (cl)
     (cond
       [(and (pair? cl) (symbol? (car cl)) (not (keyword? (car cl)))) (binding-ids (car cl))]
       [(and (pair? cl) (pair? (car cl)) (list? (car cl))) (for-clause-ids cl)]
       [else '()]))
   clauses))

(define (lower-for-clauses ctx env clauses expr-fn)
  (append-map
   (λ (cl)
     (cond
       [(and (pair? cl) (keyword? (car cl)))
        (if (pair? (cdr cl)) (list (expr-fn ctx env (cadr cl))) '())]
       [(and (pair? cl) (pair? (car cl)) (list? (car cl)) (not (symbol? (car cl))))
        (lower-for-clauses ctx env cl expr-fn)]
       [(and (pair? cl) (symbol? (car cl)))
        (define ids (binding-ids (car cl)))
        (for/list ([item (in-list (cdr cl))] #:when (not (keyword? item)))
          (expr-fn ctx (extend-env env ids) item))]
       [else '()]))
   clauses))

(define (lower-for-form ctx env args expr-fn body-fn)
  (define clauses (car args))
  (define body (cdr args))
  (define env* (extend-env env (for-clause-ids clauses)))
  (uir-block (append (lower-for-clauses ctx env* clauses expr-fn)
                     (list (body-fn ctx env* body)))))

(define (lower-for-fold-form ctx env args expr-fn body-fn)
  (define accs (car args))
  (define clauses (cadr args))
  (define body (cddr args))
  (define acc-nodes
    (append-map (λ (b) (if (and (pair? b) (pair? (cdr b))) (list (expr-fn ctx env (cadr b))) '()))
                accs))
  (define env* (extend-env env (append (for-clause-ids clauses) (append-map binding-ids accs))))
  (uir-block (append acc-nodes
                     (lower-for-clauses ctx env* clauses expr-fn)
                     (list (body-fn ctx env* body)))))

;; ── match ──────────────────────────────────────────────────────────

;; The pattern is syntax data; clause bodies and #:when/#:do guards are
;; expressions.
(define (lower-match-clause-bodies ctx env clauses expr-fn body-fn)
  (append-map
   (λ (cl)
     (cond
       [(not (pair? cl)) '()]
       [(eq? (car cl) 'else) (list (body-fn ctx env (cdr cl)))]
       [else
        ;; A match clause body is a body context (internal definitions are
        ;; legal), so collect the non-pattern forms and lower them as a body;
        ;; #:when / #:do guards contribute their expression.
        (define forms '())
        (define mode 'body) ; body | expr | skip
        (for ([x (in-list (cdr cl))])
          (cond
            [(keyword? x) (set! mode (if (memq x '(#:do #:when)) 'expr 'skip))]
            [(eq? x '=>) (set! mode 'expr)]
            [(eq? mode 'skip) (set! mode 'body)]
            [else
             (set! forms (cons x forms))
             (set! mode 'body)]))
        (list (body-fn ctx env (reverse forms)))]))
   clauses))

(define (lower-match-form ctx env h args expr-fn body-fn)
  (define subjects
    (if (eq? h 'match*)
        (map (λ (x) (expr-fn ctx env x)) (car args))
        (list (expr-fn ctx env (car args)))))
  (uir-block (append subjects (lower-match-clause-bodies ctx env (cdr args) expr-fn body-fn))))

;; ── struct-copy ────────────────────────────────────────────────────

(define (lower-struct-copy-form ctx env args expr-fn)
  (define value (and (pair? (cdr args)) (cadr args)))
  (define field-exprs
    (append-map (λ (fe) (if (and (pair? fe) (pair? (cdr fe)))
                            (list (expr-fn ctx env (cadr fe)))
                            '()))
                (cddr args)))
  (uir-block (append (if value (list (expr-fn ctx env value)) '()) field-exprs)))
