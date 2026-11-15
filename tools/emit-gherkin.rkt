#lang racket

;; tools/emit-gherkin.rkt <fizz-file> — render an Effervesce description as a
;; Gherkin .feature file. The effect row is the real assertion: `Then effects`
;; names exactly the domains the component may touch, so a BDD step can assert
;; side effects, not just outcomes.

(require racket/file
         racket/list
         racket/string
         racklr/uir
         effervesce/load)

(define (sym s) (uir-symbol-name s))

(define (row-names row)
  (map (λ (r) (sym (uir-eff-ref-name r))) row))

(define (emit-row row)
  (string-join (row-names row) ", "))

(define (indent s)
  (string-join (map (λ (l) (string-append "  " l))
                    (string-split s "\n"))
               "\n"))

(define (emit-decl d)
  (cond
    [(uir-eff-capability? d)
     (list 'capability (sym (uir-eff-capability-name d)))]
    [(uir-eff-given? d)
     (list 'given (emit-row (uir-eff-given-row d)))]
    [(uir-eff-handler? d)
     (list 'handler (sym (uir-eff-handler-effect d))
           (emit-row (uir-eff-handler-row d)))]
    [(uir-eff-component? d)
     (list 'component (sym (uir-eff-component-name d))
           (emit-row (uir-eff-component-needs d)))]
    [(uir-eff-state? d)
     (list 'state (sym (uir-eff-state-name d)))]
    [(uir-eff-action? d)
     (list 'action (sym (uir-eff-action-name d)))]
    [(uir-eff-invariant? d)
     (list 'invariant)]
    [(uir-eff-policy? d)
     (list 'policy (if (eq? (uir-eff-policy-kind d) 'never) 'never 'must))]
    [else '()]))

(define (emit-gherkin sys)
  (define name (sym (uir-eff-system-name sys)))
  (define decls (filter pair? (map emit-decl (uir-eff-system-decls sys))))
  (define caps (for/list ([d (in-list decls)] #:when (eq? (car d) 'capability)) (cadr d)))
  (define given (for/list ([d (in-list decls)] #:when (eq? (car d) 'given)) (cadr d)))
  (define comps (for/list ([d (in-list decls)] #:when (eq? (car d) 'component)) d))
  (define handlers (for/list ([d (in-list decls)] #:when (eq? (car d) 'handler)) d))
  (define states (for/list ([d (in-list decls)] #:when (eq? (car d) 'state)) (cadr d)))
  (define actions (for/list ([d (in-list decls)] #:when (eq? (car d) 'action)) (cadr d)))
  (define invariants (for/list ([d (in-list decls)] #:when (eq? (car d) 'invariant)) d))
  (define policies (for/list ([d (in-list decls)] #:when (eq? (car d) 'policy)) d))
  (define parts '())
  (set! parts (cons (format "Feature: ~a" name) parts))
  (when (or (pair? caps) (pair? given) (pair? states))
    (set! parts (cons "\n  Background:" parts))
    (when (pair? caps)
      (set! parts (cons (format "    Given capability ~a" (string-join caps ", ")) parts)))
    (for ([g (in-list given)])
      (set! parts (cons (format "    Given ~a" g) parts)))
    (for ([s (in-list states)])
      (set! parts (cons (format "    Given initial state ~a" s) parts))))
  (for ([h (in-list handlers)])
    (set! parts (cons (format "\n  Scenario: ~a via ~a" (cadr h) (caddr h)) parts))
    (set! parts (cons "    Then effects are interpreted by the handler row" parts)))
  (for ([c (in-list comps)])
    (define cname (cadr c))
    (define needs (caddr c))
    (set! parts (cons (format "\n  Scenario: ~a" cname) parts))
    (when (not (string=? needs ""))
      (set! parts (cons (format "    Given ~a needs ~a" cname needs) parts)))
    (set! parts (cons (format "    When ~a operates" cname) parts))
    (set! parts (cons (format "    Then effects ~a" (if (string=? needs "") "(none)" needs)) parts)))
  (for ([a (in-list actions)])
    (set! parts (cons (format "\n  Scenario: action ~a" a) parts))
    (set! parts (cons (format "    When ~a runs" a) parts)))
  (for ([i (in-list invariants)])
    (set! parts (cons "    Then always <invariant>" parts)))
  (for ([p (in-list policies)])
    (set! parts (cons (format "    Then ~a <predicate>" (if (eq? (cadr p) 'never) "never" "must")) parts)))
  (string-join (reverse parts) "\n"))

(module+ main
  (define src (file->string (car (vector->list (current-command-line-arguments)))))
  (display (emit-gherkin (load-effervesce src))))
