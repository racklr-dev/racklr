#lang racket

;; Effervesce UIR → text emitter. Inverse of lower-effervesce.rkt.
;; Precedence is intentionally flat: the pass-1 grammar parses operators with
;; no precedence tiers, so infix emission without parentheses round-trips for
;; every shape the lowerer produces.

(require racklr/uir
         racklr/emit-contract)

(provide emit-effervesce)

(define operators '("+" "-" "*" "/" "%" "==" "!=" "<" "<=" ">" ">="))

(define (indent s)
  (string-join (map (λ (l) (string-append "  " l))
                    (string-split s "\n"))
               "\n"))

;; ── Top level ────────────────────────────────────────────────────────

;; Implements the emit contract (racklr/emit-contract): UIR plus an optional
;; explicit effect row in, Effervesce text out. Effervesce carries rows
;; structurally (`given` / `needs` / `provides` refs in the UIR); this target
;; renders declarations, so a function-entry row from the contract has no place
;; to be reified and is refused rather than dropped.
(define (emit-effervesce uir [row #f])
  (if (eq? row #f)
      (emit-effervesce-unit uir)
      (refuse-explicit-row 'emit-effervesce "effervesce" row)))

(define (emit-effervesce-unit uir)
  (if (uir-eff-system? uir)
      (string-append
       (format "system ~a {\n" (uir-symbol-name (uir-eff-system-name uir)))
       (string-join (map (λ (d) (indent (emit-decl d)))
                         (uir-eff-system-decls uir))
                    "\n")
       "\n}\n")
      (emit-expr uir)))

(define (emit-decl d)
  (cond
    [(uir-eff-capability? d) (emit-capability d)]
    [(uir-eff-given? d) (format "given ~a" (emit-row (uir-eff-given-row d)))]
    [(uir-eff-handler? d) (emit-handler d)]
    [(uir-eff-component? d) (emit-component d)]
    [(uir-eff-state? d) (emit-state d)]
    [(uir-eff-action? d) (emit-action d)]
    [(uir-eff-invariant? d) (format "always ~a" (emit-expr (uir-eff-invariant-expr d)))]
    [(uir-eff-policy? d)
     (format "~a ~a"
             (if (eq? (uir-eff-policy-kind d) 'never) "never" "must")
             (emit-predicate (uir-eff-policy-predicate d)))]
    [else ""]))

;; ── Declarations ─────────────────────────────────────────────────────

(define (emit-capability c)
  (string-append
   (format "capability ~a~a {"
           (uir-symbol-name (uir-eff-capability-name c))
           (emit-type-params (uir-eff-capability-type-params c)))
   "\n"
   (indent (string-join (map emit-capability-op (uir-eff-capability-ops c)) "\n"))
   "\n}"))

(define (emit-capability-op op)
  (format "~a(~a)~a"
          (uir-symbol-name (uir-eff-capability-op-name op))
          (emit-params (uir-eff-capability-op-params op))
          (emit-arrow (uir-eff-capability-op-result-type op))))

(define (emit-handler h)
  (string-append
   (format "~a via ~a {"
           (emit-effect-ref (uir-eff-handler-effect h))
           (emit-row (uir-eff-handler-row h)))
   "\n"
   (indent (string-join (map emit-op-impl (uir-eff-handler-ops h)) "\n"))
   "\n}"))

(define (emit-component c)
  (define name (uir-symbol-name (uir-eff-component-name c)))
  (define needs (uir-eff-component-needs c))
  (define provides (uir-eff-component-provides c))
  (string-append
   (format "component ~a" name)
   (if (pair? needs) (format " needs ~a" (emit-row needs)) "")
   (if (pair? provides) (format " provides ~a" (emit-row provides)) "")
   " {\n"
   (indent (string-join (map emit-op-impl (uir-eff-component-ops c)) "\n"))
   "\n}"))

(define (emit-state s)
  (format "state ~a: ~a = ~a"
          (uir-symbol-name (uir-eff-state-name s))
          (uir-symbol-name (uir-eff-state-type s))
          (emit-expr (uir-eff-state-init s))))

(define (emit-action a)
  (string-append
   (format "action ~a {"
           (uir-symbol-name (uir-eff-action-name a)))
   "\n"
   (indent (string-join (map emit-step (uir-eff-action-steps a)) "\n"))
   "\n}"))

(define (emit-step s)
  (if (uir-set!? s)
      (format "~a := ~a"
              (uir-symbol-name (uir-set!-name s))
              (emit-expr (uir-set!-value s)))
      (emit-expr s)))

;; ── Op impls (handler / component) ──────────────────────────────────

;; ops are quads: (list name params result-type body)
(define (emit-op-impl op)
  (format "~a(~a)~a = ~a"
          (uir-symbol-name (car op))
          (emit-params (cadr op))
          (emit-arrow (caddr op))
          (emit-body (cadddr op))))

(define (emit-arrow rt)
  (if rt (format " -> ~a" (uir-symbol-name rt)) ""))

(define (emit-body b)
  (if (uir-block? b) (emit-block b) (emit-expr b)))

(define (emit-block b)
  (string-append "{ "
                 (string-join (map emit-expr (uir-block-stmts b)) "; ")
                 " }"))

(define (emit-params ps)
  (string-join (map emit-param ps) ", "))

(define (emit-param p)
  (define n (uir-symbol-name (uir-typed-param-name p)))
  (define t (uir-typed-param-type p))
  (if t (format "~a: ~a" n (uir-symbol-name t)) n))

;; ── Effect rows and references ──────────────────────────────────────

(define (emit-row row)
  (string-join (map emit-effect-ref row) ", "))

(define (emit-effect-ref r)
  (define name (uir-symbol-name (uir-eff-ref-name r)))
  (define args (uir-eff-ref-type-args r))
  (if (null? args)
      name
      (format "~a<~a>" name (string-join (map uir-symbol-name args) ", "))))

(define (emit-type-params ps)
  (if (null? ps) "" (format "<~a>" (string-join (map uir-symbol-name ps) ", "))))

;; ── Expressions and predicates ──────────────────────────────────────

;; A policy predicate differs from a value expression only in the
;; `component-needs` sugar; everything else (`not`/`and`/`or`/parens) shares
;; emit-expr.
(define (emit-predicate p)
  (cond
    [(and (uir-call? p)
          (equal? (uir-symbol-name (uir-call-callee p)) "component-needs"))
     (format "component needs ~a" (emit-effect-ref (car (uir-call-args p))))]
    [(and (uir-call? p)
          (equal? (uir-symbol-name (uir-call-callee p)) "not"))
     (format "not ~a" (emit-predicate (car (uir-call-args p))))]
    [(and (uir-call? p)
          (member (uir-symbol-name (uir-call-callee p)) '("and" "or")))
     (format "~a ~a ~a"
             (emit-predicate (car (uir-call-args p)))
             (uir-symbol-name (uir-call-callee p))
             (emit-predicate (cadr (uir-call-args p))))]
    [(uir-paren? p) (format "(~a)" (emit-predicate (uir-paren-inner p)))]
    [else (emit-expr p)]))

(define (emit-expr e)
  (cond
    [(uir-var? e) (uir-symbol-name (uir-var-name e))]
    [(uir-number? e) (uir-number-value e)]
    [(uir-string? e) (format "\"~a\"" (uir-string-value e))]
    [(uir-bool? e) (if (uir-bool-value e) "true" "false")]
    [(uir-null? e) "none"]
    [(uir-get? e) (format "~a.~a" (emit-expr (uir-get-base e))
                          (uir-symbol-name (uir-get-field e)))]
    [(uir-paren? e) (format "(~a)" (emit-expr (uir-paren-inner e)))]
    [(uir-call? e) (emit-call e)]
    [else ""]))

(define (emit-call c)
  (define callee (uir-symbol-name (uir-call-callee c)))
  (define args (uir-call-args c))
  (cond
    [(equal? callee "not") (format "not ~a" (emit-expr (car args)))]
    [(member callee '("and" "or"))
     (format "~a ~a ~a" (emit-expr (car args)) callee (emit-expr (cadr args)))]
    [(member callee operators)
     (format "~a ~a ~a" (emit-expr (car args)) callee (emit-expr (cadr args)))]
    [else (format "~a(~a)" callee (string-join (map emit-expr args) ", "))]))

(module+ main
  (displayln "racklr/emit-effervesce — UIR → Effervesce text emitter loaded."))
