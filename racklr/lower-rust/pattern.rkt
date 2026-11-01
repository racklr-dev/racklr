#lang racket

;; racklr/lower-rust/pattern.rkt — Rust patterns → UIR pattern nodes.
;;
;; Covers identifier (capture/wildcard), literal, path, tuple, slice, or,
;; grouped, reference and struct patterns. Struct/tuple-struct patterns use
;; `uir-pat-class`; the `..` rest element is dropped because UIR has no field
;; for it (documented loss). Range patterns are represented as a `?range`
;; placeholder rather than guessed at.

(require racklr/tree
         racklr/uir
         "helpers.rkt"
         "literal.rkt")

(provide lower-pattern lower-pattern-name)

(define (lower-pattern node tk-type tk-value)
  (cond
    [(not (cst-node? node)) (uir-pat-wildcard)]
    [(eq? (node-tag node) 'pattern) (lower-pattern (single-node-child node) tk-type tk-value)]
    [(eq? (node-tag node) 'patternNoTopAlt) (lower-pattern (single-node-child node) tk-type tk-value)]
    [(eq? (node-tag node) 'patternWithoutRange) (lower-pattern (single-node-child node) tk-type tk-value)]
    [else
     (case (node-tag node)
       [(wildcardPattern) (uir-pat-wildcard)]
       [(identifierPattern)
        (define id (first-identifier node tk-type tk-value))
        (if (string=? id "_") (uir-pat-wildcard) (uir-pat-capture (uir-symbol id)))]
       [(literalPattern)
        (define t (for/first ([x (immediate-tokens node tk-type)]) x))
        (uir-pat-literal (if t (literal-uir t tk-type tk-value) (uir-symbol "?lit")))]
       [(tuplePattern slicePattern)
        (uir-pat-sequence (map (λ (p) (lower-pattern p tk-type tk-value))
                               (ordered-nodes node 'pattern)))]
       [(orPattern)
        (uir-pat-or (map (λ (p) (lower-pattern p tk-type tk-value))
                         (ordered-nodes node 'pattern)))]
       [(groupedPattern)
        (uir-pat-group (lower-pattern (find-descendant node 'pattern) tk-type tk-value))]
       [(rangePattern) (uir-pat-value (uir-symbol "?range"))]
       [(structPattern) (lower-struct-pattern node tk-type tk-value)]
       [(tupleStructPattern) (lower-tuple-struct-pattern node tk-type tk-value)]
       [(referencePattern) (lower-pattern (find-descendant node 'pattern) tk-type tk-value)]
       [(pathPattern) (uir-pat-value (uir-var (uir-symbol (path-name node tk-type tk-value))))]
       [else (uir-pat-value (uir-symbol (format "?pat-~a" (node-tag node))))])]))

;; `Foo { x, y: pat }` — class pattern; field names are kept, `..` is dropped.
(define (lower-struct-pattern node tk-type tk-value)
  (define path (or (find-descendant node 'pathInExpression) node))
  (define entries
    (for/list ([f (find-descendants node 'structPatternField)])
      (define pat (find-descendant f 'pattern))
      (define name (first-identifier (or (find-descendant f 'identifier) f) tk-type tk-value))
      (cons (uir-symbol name) (if pat (lower-pattern pat tk-type tk-value)
                                  (uir-pat-capture (uir-symbol name))))))
  (uir-pat-class (uir-symbol (path-name path tk-type tk-value)) '() entries))

;; `Foo(a, b)` / `Foo(..)` — positional class pattern.
(define (lower-tuple-struct-pattern node tk-type tk-value)
  (define path (or (find-descendant node 'pathInExpression) node))
  (uir-pat-class (uir-symbol (path-name path tk-type tk-value))
                 (map (λ (p) (lower-pattern p tk-type tk-value))
                      (ordered-nodes node 'pattern))
                 '()))

(define (lower-pattern-name node tk-type tk-value)
  (define pat (lower-pattern node tk-type tk-value))
  (cond
    [(and (uir-pat-capture? pat) (uir-symbol? (uir-pat-capture-name pat)))
     (uir-symbol-name (uir-pat-capture-name pat))]
    [(and (uir-pat-value? pat) (uir-var? (uir-pat-value-path pat)))
     (uir-symbol-name (uir-var-name (uir-pat-value-path pat)))]
    [else "_"]))
