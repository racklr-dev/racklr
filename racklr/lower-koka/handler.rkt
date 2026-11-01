#lang racket

;; racklr/lower-koka/handler.rkt — Koka effect handlers and `with` statements.
;;
;; Modern Koka handler syntax has several shapes that all lower to one
;; `uir-handler` node:
;;
;;   handler<eff> { ... }                 a first-class handler value
;;   handle<eff>(action) { ... }          the handler applied to an action
;;   with handler<eff> { ... }            handler statement: the *rest of the
;;   with handle<eff>(action) { ... }     enclosing block is the handled body
;;   with fun op(...) ...                 single-operation with-shorthand
;;   with x <- handler                    handler binding
;;   with mask<e>                         effect mask over the rest
;;
;; `sort` records which spelling it was so the emitter can reproduce it; the
;; handler operations themselves are `uir-handler-op` nodes carrying the
;; operation kind (`fun`, `val`, `ctl`, `final ctl`, `raw ctl`, `return`,
;; `initially`, `finally`) that the effect semantics depends on.

(require racklr/tree
         racklr/uir
         "helpers.rkt"
         "type.rkt"
         "params.rkt"
         "expr.rkt"
         "hooks.rkt")

(provide lower-handlerexpr
         lower-withexpr
         lower-withstat
         install-handler-hooks!)

(define (install-handler-hooks!)
  (lower-handlerexpr-fn lower-handlerexpr)
  (lower-withexpr-fn (λ (node) (lower-withexpr node)))
  (lower-withstat-fn lower-withstat))

;; ── Handler expressions ─────────────────────────────────────────────

;; named / override flags have no uir-handler field, so they ride on an
;; `uir-decorated` wrapper (the same additive encoding the declaration
;; modifiers use).
(define (make-handler sort effect value locals ops body named? override?)
  (define h (uir-handler sort effect value locals ops body))
  (cond
    [(or named? override?)
     (uir-decorated
      (append (if named? (list (uir-symbol "named")) '())
              (if override? (list (uir-symbol "override")) '()))
      h)]
    [else h]))

(define (lower-handlerexpr node)
  (define handle? (for/or ([t (in-list (tokens node))]) (tok? t 'HANDLE)))
  (define named? (for/or ([t (in-list (tokens node))]) (tok? t 'NAMED)))
  (define override? (let ([o (find-node node 'override)])
                      (and o (for/or ([t (in-list (tokens o))]) (tok? t 'OVERRIDE)))))
  (define effect (lower-witheff (find-node node 'witheff)))
  (define action (find-node node 'ntlexpr))
  (define clauses (find-node node 'opclauses))
  (make-handler (if handle? 'handle 'handler)
                effect #f '()
                (lower-opclauses clauses)
                (and action (lower-ntlexpr action))
                (and named? #t) (and override? #t)))

;; ── witheff / opclauses ─────────────────────────────────────────────

(define (lower-witheff node)
  (define anntype (and node (find-node node 'anntype)))
  (and anntype (uir-symbol (lower-type-text anntype))))

(define (lower-opclauses node)
  (if (not node)
      '()
      (let ([xs (find-node node 'opclausexs)])
        (if (not xs)
            '()
            (filter values
                    (for/list ([g (in-list (all-list-items xs))]
                               #:when (and (node? g) (eq? (node-tag g) 'group)))
                      (let ([x (find-node g 'opclausex)])
                        (and x (lower-opclausex x)))))))))

(define (lower-opclausex node)
  (define op (find-node node 'opclause))
  (cond
    [op (lower-opclause op)]
    [else
     ;; 'finally bodyexpr | 'initially (opparam) bodyexpr
     (define tokens* (tokens node))
     (define bodyexpr (find-node node 'bodyexpr))
     (define body (lower-bodyexpr bodyexpr))
     (cond
       [(for/or ([t (in-list tokens*)]) (and (tok? t 'IDENT) (string=? (tok-value t) "finally")))
        (uir-handler-op 'finally #f #f '() body)]
       [else
        (define opp (find-node node 'opparam))
        (uir-handler-op 'initially #f #f (if opp (list (lower-opparam opp)) '()) body)])]))

(define (lower-opclause node)
  (define tokens* (tokens node))
  (cond
    [(for/or ([t (in-list tokens*)]) (tok? t 'RETURN))
     (define opp (find-node node 'opparam))
     (uir-handler-op 'return #f #f (if opp (list (lower-opparam opp)) '())
                     (lower-bodyexpr (find-node node 'bodyexpr)))]
    [(for/or ([t (in-list tokens*)]) (tok? t 'VAL))
     (define name (find-node node 'qidentifier))
     (define kt (find-node node 'ktype))
     (uir-handler-op 'val (and name (uir-symbol (identifier-text name)))
                     (and kt (uir-symbol (lower-type-text kt)))
                     '()
                     (lower-bodyexpr (find-node node 'bodyexpr)))]
    [(for/or ([t (in-list tokens*)]) (tok? t 'CTL))
     (define ctlmod (find-node node 'controlmod))
     (define control-kind
       (cond [(and ctlmod (for/or ([t (in-list (tokens ctlmod))]) (tok? t 'FINAL))) 'final-ctl]
             [(and ctlmod (for/or ([t (in-list (tokens ctlmod))]) (tok? t 'RAW))) 'raw-ctl]
             [else 'ctl]))
     (define name (find-node node 'qidentifier))
     (define opparams (find-node node 'opparams))
     (uir-handler-op control-kind
                     (and name (uir-symbol (identifier-text name)))
                     #f
                     (if opparams (lower-opparams opparams) '())
                     (lower-bodyexpr (find-node node 'bodyexpr)))]
    [else
     (define name (find-node node 'qidentifier))
     (define opparams (find-node node 'opparams))
     (uir-handler-op 'fun
                     (and name (uir-symbol (identifier-text name)))
                     #f
                     (if opparams (lower-opparams opparams) '())
                     (lower-bodyexpr (find-node node 'bodyexpr)))]))

(define (lower-bodyexpr node)
  (cond
    [(not node) (uir-null)]
    [else
     ;; bodyexpr: blockexpr | ARROW blockexpr
     (define b (find-node node 'blockexpr))
     (if b ((lower-blockexpr-fn) b) (uir-null))]))

;; ── with expressions / statements ───────────────────────────────────

(define (lower-withexpr node)
  (define ws (find-node node 'withstat))
  (define body (lower-bodyexpr (find-node node 'blockexpr)))
  (lower-withstat ws body))

(define (lower-withstat node body)
  (define named? #f)
  (define override? #f)
  (define kids (children node))
  (define binder (find-node node 'binder))
  (define basicexpr (find-node node 'basicexpr))
  (define witheff (find-node node 'witheff))
  (define opclause (find-node node 'opclause))
  (define override (find-node node 'override))
  (cond
    ;; with <basicexpr>
    [basicexpr
     (define value ((lower-basicexpr-fn) basicexpr))
     (apply-with-value value (or body (uir-block '())))]
    ;; with x <- <basicexpr>
    [(and binder basicexpr)
     (apply-with-value ((lower-basicexpr-fn) basicexpr)
                       (or body (uir-block '()))
                       (list (binder-param binder)))]
    ;; with <override> <witheff> <opclause>  (single-operation shorthand)
    [(and opclause (not binder))
     (define override? (and override
                            (for/or ([t (in-list (tokens override))]) (tok? t 'OVERRIDE))))
     (make-handler 'with (lower-witheff witheff) #f '()
                   (list (lower-opclause opclause))
                   (or body (uir-block '()))
                   #f (and override? #t))]
    ;; with x <- <witheff> <opclause>
    [else
     (define override? (and override
                            (for/or ([t (in-list (tokens override))]) (tok? t 'OVERRIDE))))
     (make-handler 'with-bind (lower-witheff witheff) #f
                   (if binder (list (binder-param binder)) '())
                   (if opclause (list (lower-opclause opclause)) '())
                   (or body (uir-block '()))
                   #f (and override? #t))]))

(define (apply-with-value value body [locals '()])
  (define sort* (if (pair? locals) 'with-bind 'with))
  (cond
    [(uir-mask? value)
     (struct-copy uir-mask value [form 'with] [body body])]
    [(uir-handler? value)
     (struct-copy uir-handler value [sort sort*] [body body]
                  [locals (append locals (uir-handler-locals value))])]
    [(and (uir-decorated? value) (uir-handler? (uir-decorated-inner value)))
     (struct-copy uir-decorated value
                  [inner (struct-copy uir-handler (uir-decorated-inner value)
                                      [sort sort*] [body body]
                                      [locals (append locals (uir-handler-locals (uir-decorated-inner value)))])])]
    [else
     (uir-handler sort* #f value locals '() body)]))

(define (binder-param node)
  (define id (or (find-descendant node 'qidentifier)
                 (find-descendant node 'identifier)
                 (find-descendant node 'wildcard)))
  (define kt (find-descendant node 'ktype))
  (uir-typed-param (if id (uir-symbol (identifier-text id)) (uir-symbol "_"))
                   (if kt (uir-symbol (lower-type-text kt)) #f)
                   #f #f))
