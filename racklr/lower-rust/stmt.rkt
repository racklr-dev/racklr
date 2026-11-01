#lang racket

;; racklr/lower-rust/stmt.rkt — Rust blocks, statements, let bindings.
;;
;; Requiring this module installs `lower-block` as the expression layer's
;; block lowerer (breaking the expression↔statement cycle). Rust blocks are
;; value-producing: `lower-fn-body` turns a trailing expression into
;; `uir-return`.

(require racklr/tree
         racklr/uir
         "helpers.rkt"
         "type.rkt"
         "expr.rkt"
         "pattern.rkt")

(provide lower-block lower-statement lower-fn-body)

;; ── Blocks ──────────────────────────────────────────────────────────

(define (lower-block node tk-type tk-value [tail-returns? #f])
  (define stmts-node (find-descendant node 'statements))
  (define writer (and stmts-node (list-wrapper stmts-node)))
  (define stmt-items
    (cond
      [(not stmts-node) '()]
      [writer (filter (λ (c) (and (cst-node? c)
                                  (memq (node-tag c) '(statement))))
                      (collect-items stmts-node))]
      [else '()]))
  (define tail
    (cond
      [(not stmts-node) #f]
      [writer (for/or ([c (any-tree-children stmts-node)])
                (and (cst-node? c) (memq (node-tag c) expression-tags) c))]
      [else (for/first ([c (node-children stmts-node)]
                        #:when (memq (node-tag c) expression-tags))
              c)]))
  (define stmts (map (λ (s) (lower-statement s tk-type tk-value)) stmt-items))
  (define tail-uir (and tail (lower-expr tail tk-type tk-value)))
  (uir-block
   (cond
     [(and tail-uir tail-returns?) (append stmts (list (uir-return tail-uir)))]
     [tail-uir (append stmts (list tail-uir))]
     [else stmts])))

;; Function/method body: a trailing expression is the returned value.
(define (lower-fn-body node tk-type tk-value)
  (lower-block node tk-type tk-value #t))

;; Install the block lowerer for the expression layer.
(block-lowerer lower-block)

;; ── Statements ──────────────────────────────────────────────────────

(define (lower-statement node tk-type tk-value)
  (define payload (for/or ([c (node-children node)]) c))
  (cond
    [(not payload) (uir-null)]
    [else
     (case (node-tag payload)
       [(letStatement) (lower-let payload tk-type tk-value)]
       [(expressionStatement) (lower-expr (first-payload payload) tk-type tk-value)]
       [(macroInvocationSemi) (lower-expr payload tk-type tk-value)]
       [(item)
        ;; A macro invocation statement parses as an `item` in some contexts.
        (define mac (find-descendant payload 'macroInvocationSemi))
        (if mac (lower-expr mac tk-type tk-value) (uir-null))]
       [else (lower-expr payload tk-type tk-value)])]))

(define (first-payload node)
  (or (for/or ([c (node-children node)]) c) node))

;; `let pattern (: type)? (= expression)? ;`
(define (lower-let let-node tk-type tk-value)
  (define pattern (or (find-descendant let-node 'patternNoTopAlt)
                      (find-descendant let-node 'pattern)))
  (define name (lower-pattern-name pattern tk-type tk-value))
  ;; The optional `: type` and `= expression` arrive as groups; classify a
  ;; group by its first token so a type inside the initializer cannot be
  ;; mistaken for the binding annotation.
  (define groups (for/list ([g (node-children let-node)]
                            #:when (eq? (node-tag g) 'group))
                   g))
  (define (group-starts-with g tag)
    (define t (for/first ([tok (in-list (immediate-tokens g tk-type))]) tok))
    (and t (eq? (tk-type t) tag)))
  (define type-node
    (for/or ([g (in-list groups)] #:when (group-starts-with g 'COLON))
      (find-descendant g 'type_)))
  (define value-node
    (for/or ([g (in-list groups)] #:when (group-starts-with g 'EQ))
      (for/first ([c (node-children g)] #:when (memq (node-tag c) expression-tags)) c)))
  (define value (and value-node (lower-expr value-node tk-type tk-value)))
  (cond
    [type-node
     (uir-ann-set! (uir-var (uir-symbol name))
                   (lower-type type-node tk-type tk-value)
                   (or value (uir-null)))]
    [value (uir-set! (uir-var (uir-symbol name)) value)]
    [else (uir-set! (uir-var (uir-symbol name)) (uir-null))]))
