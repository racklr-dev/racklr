#lang racket

;; racklr/lower-koka/stmt.rkt — Koka blocks and statements → UIR.
;;
;; A Koka block is a brace-delimited statement sequence whose last statement is
;; an expression. A `with ...` statement (without `in`) applies its handler or
;; mask to the *rest of the enclosing block*, so a block lowers as a sequence
;; where each with-statement absorbs the remaining statements as its body. That
;; is the same nesting Koka's layout denotes and keeps emission structural.
;;
;; Local declarations mirror the top-level forms: `fun` → uir-fn, `val` →
;; uir-let (or uir-ann-set! when a type is written), `var` → a decorated
;; binding (`var` has no UIR node kind; the decorator carries it).

(require racklr/tree
         racklr/uir
         "helpers.rkt"
         "type.rkt"
         "pattern.rkt"
         "hooks.rkt")

(provide lower-block lower-statement-list install-stmt-hooks!)

(define (install-stmt-hooks!)
  (lower-block-fn lower-block))

(define (lower-block node)
  (uir-block (lower-statement-list (statement-nodes node))))

(define (statement-nodes node)
  (define stmts (find-node node 'statements))
  (cond
    [(not stmts) '()]
    [else
     (filter values
             (for/list ([g (in-list (all-list-items stmts))]
                        #:when (and (node? g) (eq? (node-tag g) 'group)))
               (find-node g 'statement)))]))

(define (lower-statement-list stmts)
  (cond
    [(null? stmts) '()]
    [else
     (define s (first stmts))
     (define remaining (cdr stmts))
     (define ws (find-node s 'withstat))
     (cond
       [ws
        (define body-expr (find-node s 'blockexpr))
        (define body (if body-expr
                         ((lower-blockexpr-fn) body-expr)
                         (uir-block (lower-statement-list remaining))))
        (list ((lower-withstat-fn) ws body))]
       [else (cons (lower-statement s) (lower-statement-list remaining))])]))

(define (lower-statement node)
  (define kid (find-node-in-tags node))
  (case kid
    [(decl) (lower-decl (find-node node 'decl))]
    [(returnexpr) (uir-return ((lower-expr-fn) (find-node (find-node node 'returnexpr) 'expr)))]
    [(basicexpr) ((lower-basicexpr-fn) (find-node node 'basicexpr))]
    [(withstat) ((lower-withstat-fn) (find-node node 'withstat) (uir-block '()))]
    [else (uir-null)]))

(define (find-node-in-tags node)
  (for/first ([c (in-list (children node))]
              #:when (and (node? c)
                          (memq (node-tag c) '(decl returnexpr basicexpr withstat))))
    (node-tag c)))

;; ── Local declarations ──────────────────────────────────────────────

(define (lower-decl node)
  (cond
    [(find-token node 'FUN)
     (define fundecl (find-node node 'fundecl))
     (define id (find-node fundecl 'identifier))
     (define funbody (find-node fundecl 'funbody))
     ((lower-funbody-fn) funbody (uir-symbol (identifier-text id)))]
    [(find-token node 'VAL) (lower-local-val node)]
    [(find-token node 'VAR) (lower-local-var node)]
    [else (uir-null)]))

(define (lower-local-val node)
  (define ap (find-node node 'apattern))
  (define-values (pattern annot) (lower-apattern ap))
  (define value0 ((lower-blockexpr-fn) (find-node node 'blockexpr)))
  (define value (if annot (uir-ascribe value0 (uir-symbol annot)) value0))
  (cond
    [(and annot (uir-pat-capture? pattern))
     (uir-ann-set! (uir-symbol (uir-symbol-name (uir-pat-capture-name pattern)))
                   (uir-symbol annot)
                   value0)]
    [else (uir-let (pattern-name pattern) value (uir-null))]))

(define (pattern-name pattern)
  (if (uir-pat-capture? pattern)
      (uir-symbol (uir-symbol-name (uir-pat-capture-name pattern)))
      pattern))

(define (lower-local-var node)
  (define binder (find-node node 'binder))
  (define id (and binder
                  (or (find-descendant binder 'qidentifier)
                      (find-descendant binder 'identifier)
                      (find-descendant binder 'wildcard))))
  (define kt (find-descendant binder 'ktype))
  (define value ((lower-blockexpr-fn) (find-node node 'blockexpr)))
  (define name (uir-symbol (identifier-text id)))
  (define binding
    (if kt
        (uir-ann-set! name (uir-symbol (lower-type-text kt)) value)
        (uir-let name value (uir-null))))
  (uir-decorated (list (uir-symbol "var")) binding))
