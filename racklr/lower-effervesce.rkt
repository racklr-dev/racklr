#lang racket

;; Effervesce source-language lowering: Effervesce.g4 → CST → UIR.
;;
;; Effervesce is a brace-delimited (not layout-sensitive) check-only language,
;; so the CST arrives with ordinary `{`/`}` structure, no INDENT/DEDENT.
;;
;;   system      → uir-eff-system
;;   capability  → uir-eff-capability (ops as uir-eff-capability-op)
;;   given       → uir-eff-given
;;   E via R     → uir-eff-handler
;;   component   → uir-eff-component
;;   state       → uir-eff-state
;;   action      → uir-eff-action
;;   always      → uir-eff-invariant
;;   never/must  → uir-eff-policy
;;
;; Every keyword lexes as an ID token (the grammar declares no keyword lexer
;; rules), so this lowerer dispatches on token *value*, not token type. Only
;; INT/STRING literals are distinguished by token type.

(require racklr/tree
         racklr/uir)

(provide lower-effervesce)

;; ── CST-walking helpers ─────────────────────────────────────────────

(define (node-children cst)
  (filter cst-node? (any-tree-children cst)))

;; The first non-empty `list` child: the wrapper the generated parser produces
;; for star/plus elements.
(define (list-wrapper node)
  (for/or ([c (any-tree-children node)])
    (and (list? c) (pair? c) c)))

(define (single-node-child node)
  (for/or ([c (any-tree-children node)])
    (and (cst-node? c) c)))

(define (find-node-child node tag)
  (for/or ([c (node-children node)])
    (and (eq? (any-tree-tag c) tag) c)))

;; The star/plus wrapper list whose elements are cst nodes of the given tag.
(define (tagged-list node tag)
  (for/or ([c (any-tree-children node)])
    (and (list? c)
         (pair? c)
         (cst-node? (first c))
         (eq? (any-tree-tag (first c)) tag)
         c)))

(define (token-like? x)
  (and (not (cst-node? x))
       (not (list? x))
       (not (null? x))
       (not (eq? x 'none))))

(define (immediate-tokens node)
  (for/list ([c (any-tree-children node)]
             #:when (token-like? c))
    c))

;; ── Entry point ─────────────────────────────────────────────────────

(define (lower-effervesce cst tv tt)
  (uir-eff-system (system-name cst tv)
                  (map (λ (d) (lower-decl d tv tt))
                       (filter cst-node? (or (list-wrapper cst) '())))))

(define (system-name cst tv)
  (uir-symbol (tv (second (immediate-tokens cst)))))

(define (lower-decl d tv tt)
  (lower-top (single-node-child d) tv tt))

(define (lower-top n tv tt)
  (case (any-tree-tag n)
    [(capabilityDecl) (lower-capability n tv tt)]
    [(givenDecl)      (lower-given n tv tt)]
    [(handlerDecl)    (lower-handler n tv tt)]
    [(componentDecl)  (lower-component n tv tt)]
    [(stateDecl)      (lower-state n tv tt)]
    [(actionDecl)     (lower-action n tv tt)]
    [(invariantDecl)  (uir-eff-invariant (lower-expr (find-node-child n 'expr) tv tt))]
    [(policyDecl)     (lower-policy n tv tt)]
    [else (uir-null)]))

;; ── Capabilities ────────────────────────────────────────────────────

(define (lower-capability n tv tt)
  (uir-eff-capability
   (uir-symbol (tv (second (immediate-tokens n))))
   (lower-type-params n tv)
   (map (λ (op) (lower-capability-op op tv tt))
        (filter cst-node? (or (list-wrapper n) '())))))

(define (lower-capability-op n tv tt)
  (uir-eff-capability-op
   (uir-symbol (tv (first (immediate-tokens n))))
   (lower-params-opt n tv tt)
   (lower-arrow-type n tv)))

;; `typeParams : '<' ID (',' ID)* '>'` → list of uir-symbol parameter names.
(define (lower-type-params n tv)
  (define tp (find-node-child n 'typeParams))
  (if (not tp)
      '()
      (cons (uir-symbol (tv (second (immediate-tokens tp))))
            (map (λ (g) (uir-symbol (tv (second (immediate-tokens g)))))
                 (filter cst-node? (or (list-wrapper tp) '()))))))

;; ── Wiring ──────────────────────────────────────────────────────────

(define (lower-given n tv tt)
  (uir-eff-given (lower-effect-row (find-node-child n 'effectRow) tv)))

(define (lower-handler n tv tt)
  (uir-eff-handler
   (lower-effect-ref (find-node-child n 'effectRef) tv)
   (let ([row (find-node-child n 'effectRow)])
     (if row (lower-effect-row row tv) '()))
   (map (λ (op) (lower-op-impl op tv tt))
        (filter cst-node? (or (list-wrapper n) '())))))

(define (lower-component n tv tt)
  (define needs-row '())
  (define provides-row '())
  (for ([cl (in-list (or (tagged-list n 'componentClause) '()))])
    (define head (tv (first (immediate-tokens cl))))
    (define row (lower-effect-row (find-node-child cl 'effectRow) tv))
    (cond [(equal? head "needs") (set! needs-row row)]
          [(equal? head "provides") (set! provides-row row)]))
  (uir-eff-component
   (uir-symbol (tv (second (immediate-tokens n))))
   needs-row
   provides-row
   (map (λ (op) (lower-op-impl op tv tt))
        (or (tagged-list n 'opImpl) '()))))

;; `opImpl : ID '(' params? ')' ('->' type_)? '=' body` →
;;   (list name params result-type body)
(define (lower-op-impl n tv tt)
  (list (uir-symbol (tv (first (immediate-tokens n))))
        (lower-params-opt n tv tt)
        (lower-arrow-type n tv)
        (lower-body (find-node-child n 'body) tv tt)))

;; ── State / actions / invariants / policies ────────────────────────

(define (lower-state n tv tt)
  (uir-eff-state
   (uir-symbol (tv (second (immediate-tokens n))))
   (uir-symbol (type-text (find-node-child n 'type_) tv))
   (lower-expr (find-node-child n 'expr) tv tt)))

(define (lower-action n tv tt)
  (uir-eff-action
   (uir-symbol (tv (second (immediate-tokens n))))
   (map (λ (s) (lower-step s tv tt))
        (filter cst-node? (or (list-wrapper n) '())))))

(define (lower-step n tv tt)
  (define toks (immediate-tokens n))
  (if (for/or ([t toks]) (equal? (tv t) ":="))
      (uir-set! (uir-symbol (tv (first toks)))
                (lower-expr (find-node-child n 'expr) tv tt))
      (lower-expr (find-node-child n 'expr) tv tt)))

(define (lower-policy n tv tt)
  (define g (find-node-child n 'group))
  (uir-eff-policy
   (if (equal? (tv (first (immediate-tokens g))) "never") 'never 'must)
   (lower-predicate (find-node-child n 'predicate) tv tt)))

;; ── Effect rows and references ──────────────────────────────────────

;; `effectRow : effectRef (',' effectRef)*` → list of uir-eff-ref.
(define (lower-effect-row n tv)
  (cons (lower-effect-ref (find-node-child n 'effectRef) tv)
        (map (λ (g) (lower-effect-ref (find-node-child g 'effectRef) tv))
             (filter cst-node? (or (list-wrapper n) '())))))

;; `effectRef : ID typeArgs?` → uir-eff-ref.
(define (lower-effect-ref n tv)
  (uir-eff-ref
   (uir-symbol (tv (first (immediate-tokens n))))
   (let ([ta (find-node-child n 'typeArgs)])
     (if ta (lower-type-args ta tv) '()))))

;; `typeArgs : '<' type_ (',' type_)* '>'` → list of uir-symbol.
(define (lower-type-args ta tv)
  (cons (uir-symbol (type-text (find-node-child ta 'type_) tv))
        (map (λ (g) (uir-symbol (type-text (find-node-child g 'type_) tv)))
             (filter cst-node? (or (list-wrapper ta) '())))))

;; ── Types ───────────────────────────────────────────────────────────

;; Reconstruct a type_ node's text (e.g. "Config" or "List<Todo>").
(define (type-text n tv)
  (define name (tv (first (immediate-tokens n))))
  (define subs (filter (λ (c) (eq? (any-tree-tag c) 'type_)) (node-children n)))
  (if (null? subs)
      name
      (string-append name "<"
                     (string-join
                      (cons (type-text (car subs) tv)
                            (map (λ (g) (type-text (find-node-child g 'type_) tv))
                                 (filter cst-node? (or (list-wrapper n) '()))))
                      ", ")
                     ">")))

;; ── Predicates (policy rules over the description) ─────────────────

(define (lower-predicate n tv tt)
  (define toks (immediate-tokens n))
  (define preds (filter (λ (c) (eq? (any-tree-tag c) 'predicate))
                        (node-children n)))
  (cond
    [(equal? (tv (first toks)) "component")
     (uir-call (uir-symbol "component-needs")
               (list (lower-effect-ref (find-node-child n 'effectRef) tv)))]
    [(equal? (tv (first toks)) "not")
     (uir-call (uir-symbol "not") (list (lower-predicate (car preds) tv tt)))]
    [(equal? (tv (first toks)) "(")
     (uir-paren (lower-predicate (car preds) tv tt))]
    [(= (length preds) 2)
     (define op (for/or ([t toks])
                  (and (member (tv t) '("and" "or")) (tv t))))
     (uir-call (uir-symbol op)
               (list (lower-predicate (first preds) tv tt)
                     (lower-predicate (second preds) tv tt)))]
    [else (uir-null)]))

;; ── Expressions ─────────────────────────────────────────────────────

(define (lower-expr n tv tt)
  (define toks (immediate-tokens n))
  (define kids (node-children n))
  (define exprs (filter (λ (c) (eq? (any-tree-tag c) 'expr)) kids))
  (define lit (find-node-child n 'literal))
  (cond
    ;; literal: `expr -> literal`
    [lit (lower-literal lit tv tt)]
    ;; call: `ID '(' args? ')'`
    [(and (>= (length toks) 2) (equal? (tv (second toks)) "("))
     (uir-call (uir-symbol (tv (first toks)))
               (lower-args-opt n tv tt))]
    ;; binary operator: two expr children plus a group carrying the op token
    [(= (length exprs) 2)
     (uir-call (uir-symbol (group-op-token n tv))
               (list (lower-expr (first exprs) tv tt)
                     (lower-expr (second exprs) tv tt)))]
    ;; field access / paren / not: one expr child, distinguished by the
    ;; first immediate token's value
    [(= (length exprs) 1)
     (define first-tok (tv (first toks)))
     (cond [(equal? first-tok ".")  (uir-get (lower-expr (car exprs) tv tt)
                                             (uir-symbol (tv (second toks))))]
           [(equal? first-tok "(")  (uir-paren (lower-expr (car exprs) tv tt))]
           [(equal? first-tok "not") (uir-call (uir-symbol "not")
                                               (list (lower-expr (car exprs) tv tt)))]
           [else (uir-null)])]
    ;; bare identifier
    [(and (= (length toks) 1) (eq? (tt (first toks)) 'ID))
     (uir-var (uir-symbol (tv (first toks))))]
    [else (uir-null)]))

(define (group-op-token n tv)
  (define g (find-node-child n 'group))
  (tv (first (immediate-tokens g))))

(define (lower-literal lit tv tt)
  (define t (first (immediate-tokens lit)))
  (define v (tv t))
  (cond [(eq? (tt t) 'INT)    (uir-number v)]
        [(eq? (tt t) 'STRING) (uir-string (strip-quotes v))]
        [(equal? v "true")    (uir-bool #t)]
        [(equal? v "false")   (uir-bool #f)]
        [(equal? v "none")    (uir-null)]
        [else (uir-var (uir-symbol v))]))

(define (strip-quotes v)
  (substring v 1 (sub1 (string-length v))))

;; ── Bodies, blocks, params, args ────────────────────────────────────

(define (lower-body n tv tt)
  (define kid (single-node-child n))
  (cond [(eq? (any-tree-tag kid) 'block) (lower-block kid tv tt)]
        [(eq? (any-tree-tag kid) 'expr)  (lower-expr kid tv tt)]
        [else (uir-null)]))

(define (lower-block n tv tt)
  (uir-block
   (cons (lower-expr (find-node-child n 'expr) tv tt)
         (map (λ (g) (lower-expr (find-node-child g 'expr) tv tt))
              (filter cst-node? (or (list-wrapper n) '()))))))

(define (lower-args-opt n tv tt)
  (define a (find-node-child n 'args))
  (if a (lower-args a tv tt) '()))

(define (lower-args n tv tt)
  (cons (lower-expr (find-node-child n 'expr) tv tt)
        (map (λ (g) (lower-expr (find-node-child g 'expr) tv tt))
             (filter cst-node? (or (list-wrapper n) '())))))

(define (lower-params-opt n tv tt)
  (define p (find-node-child n 'params))
  (if p (lower-params p tv tt) '()))

(define (lower-params n tv tt)
  (cons (lower-param (find-node-child n 'param) tv tt)
        (map (λ (g) (lower-param (find-node-child g 'param) tv tt))
             (filter cst-node? (or (list-wrapper n) '())))))

(define (lower-param n tv tt)
  (define name (uir-symbol (tv (first (immediate-tokens n)))))
  (define g (find-node-child n 'group))
  (define type (and g (let ([t (find-node-child g 'type_)])
                        (and t (uir-symbol (type-text t tv))))))
  (uir-typed-param name type #f #f))

;; The `('->' type_)?` clause, which the parser wraps in a `group` node.
(define (lower-arrow-type n tv)
  (define g (find-node-child n 'group))
  (and g (let ([t (find-node-child g 'type_)])
           (and t (uir-symbol (type-text t tv))))))

(module+ main
  (displayln "racklr/lower-effervesce — Effervesce CST → UIR lowering loaded."))
