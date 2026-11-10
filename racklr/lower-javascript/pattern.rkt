#lang racket

;; ── JavaScript binding patterns (destructuring) ─────────────────────
;; A destructuring declaration (`const {a, b: c = 1, ...rest} = o`) and the
;; binding position of `for (const [k, v] of xs)` are not expressions, so the
;; JS lowering cannot reuse objectLiteral/arrayLiteral lowering for them. Each
;; pattern lowers to a structural UIR value that emit-javascript renders back
;; into binding syntax:
;;
;;   (object-pattern ENTRY ...)   { ... }
;;   (array-pattern ENTRY ...)    [ ... ]
;;   (bind-rename KEY BINDING)    key: binding
;;   (bind-default BINDING EXPR)  binding = expr
;;   (bind-rest BINDING)          ...binding
;;   (bind-computed KEY BINDING)  [key]: binding
;;   (pattern-hole)               an elided array slot
;;
;; Every emitted form re-lowers to the same structure, so the round trip keeps
;; the binding structure instead of collapsing it to a placeholder symbol.

(require racklr/tree
         racklr/uir
         "helpers.rkt")

(provide lower-binding-pattern
         set-pattern-expression-lowerer!)

;; Expression lowering for pattern defaults and computed keys is owned by
;; expr.rkt, which requires this module; the hook breaks that cycle.
(define pattern-expression-lowerer (make-parameter #f))

(define (set-pattern-expression-lowerer! proc)
  (pattern-expression-lowerer proc))

(define (lower-pattern-expression se tk-type tk-value)
  ((pattern-expression-lowerer) se tk-type tk-value))

;; The binding target at `node`: an identifier symbol, or a nested pattern.
;; `#f` when the node is not a binding target.
(define (binding-target node tk-type tk-value)
  (cond
    [(not node) #f]
    [(and (cst-node? node) (eq? (tag-of node) 'arrayLiteral))
     (lower-array-pattern node tk-type tk-value)]
    [(and (cst-node? node) (eq? (tag-of node) 'objectLiteral))
     (lower-object-pattern node tk-type tk-value)]
    ;; A pattern nested in a singleExpression wrapper (`{a: {b}}`, `[[a]]`).
    [(and (cst-node? node) (eq? (tag-of node) 'singleExpression)
          (= (length (kids-of node)) 1))
     (binding-target (first (kids-of node)) tk-type tk-value)]
    [(and (cst-node? node) (eq? (tag-of node) 'assignable))
     (define inner (find-node-or-list node))
     (and inner (binding-target inner tk-type tk-value))]
    [(and (cst-node? node) (eq? (tag-of node) 'identifier))
     (define tok (first (kids-of node)))
     (and (tok? tok tk-type) (uir-symbol (tk-value tok)))]
    [(and (cst-node? node) (eq? (tag-of node) 'keyword))
     (define tok (first (kids-of node)))
     (and (tok? tok tk-type) (uir-symbol (tk-value tok)))]
    [else
     (define ident (find-kid node 'identifier))
     (and ident (binding-target ident tk-type tk-value))]))

;; A pattern element is a binding target, optionally with a default value
;; (`a = 1`). In the CST a default is an assignment singleExpression:
;; [singleExpression, Assign, singleExpression].
(define (lower-target-with-default node tk-type tk-value)
  (define se
    (cond [(and (cst-node? node) (eq? (tag-of node) 'singleExpression)) node]
          [(and (cst-node? node) (eq? (tag-of node) 'arrayElement))
           (find-kid node 'singleExpression)]
          [else node]))
  (cond
    [(and se (cst-node? se) (eq? (tag-of se) 'singleExpression)
          (= (length (kids-of se)) 3)
          (let ([op (second (kids-of se))])
            (and (tok? op tk-type) (eq? (tk-type op) 'Assign))))
     (define kids (kids-of se))
     (define target (binding-target (first kids) tk-type tk-value))
     (and target
          (uir-call (uir-symbol "bind-default")
                    (list target (lower-pattern-expression (third kids) tk-type tk-value))))]
    [else (binding-target se tk-type tk-value)]))

;; The key of an object-pattern entry: a computed `[expr]`, an identifier, a
;; string or a numeric property name.
(define (lower-pattern-key pname tk-type tk-value)
  (cond
    [(not pname) #f]
    [(let ([k (first (kids-of pname))])
       (and (tok? k tk-type) (eq? (tk-type k) 'OpenBracket)))
     (define expr (find-kid pname 'singleExpression))
     (and expr (cons 'computed (lower-pattern-expression expr tk-type tk-value)))]
    [else
     (define leaf (first-token-leaf pname tk-type))
     (and leaf
          (cons 'key
                (if (eq? (tk-type leaf) 'StringLiteral)
                    (uir-string (string-literal-text (tk-value leaf)))
                    (uir-symbol (tk-value leaf)))))] ))

(define (lower-object-pattern node tk-type tk-value)
  (define entries
    (for/list ([pa (in-list (rule-elements node 'propertyAssignment))])
      (define ks (kids-of pa))
      (define has-ellipsis (for/or ([k ks]) (and (tok? k tk-type) (eq? (tk-type k) 'Ellipsis))))
      (cond
        [has-ellipsis
         (define se (find-kid pa 'singleExpression))
         (define target (and se (lower-target-with-default se tk-type tk-value)))
         (and target (uir-call (uir-symbol "bind-rest") (list target)))]
        [else
         (define pname (find-kid pa 'propertyName))
         (define se (find-kid pa 'singleExpression))
         (cond
           [pname
            (define key (lower-pattern-key pname tk-type tk-value))
            (define target (and se (lower-target-with-default se tk-type tk-value)))
            (and key target
                 (if (eq? (car key) 'computed)
                     (uir-call (uir-symbol "bind-computed") (list (cdr key) target))
                     (uir-call (uir-symbol "bind-rename") (list (cdr key) target))))]
           [else (and se (lower-target-with-default se tk-type tk-value))])])))
  (uir-call (uir-symbol "object-pattern") (filter values entries)))

;; Walk an elementList's children as a comma/element stream: every arrayElement
;; contributes one slot, every Comma separator advances the slot position, and
;; elided slots become pattern-hole entries.
(define (lower-array-pattern node tk-type tk-value)
  (define grp (find-kid node 'group))
  (define el (and grp (find-kid grp 'elementList)))
  (define stream '())
  (define (walk k)
    (cond
      [(and (cst-node? k) (eq? (tag-of k) 'arrayElement))
       (set! stream (cons (cons 'element k) stream))]
      [(cst-node? k) (for ([c (kids-of k)]) (walk c))]
      [(pair? k) (for ([c k]) (walk c))]
      [(tok? k tk-type) (set! stream (cons (cons 'comma k) stream))]
      [else (void)]))
  (walk el)
  (define entries '())
  (define commas 0)
  (define saw-element? #f)
  (for ([item (in-list (reverse stream))])
    (case (car item)
      [(comma) (set! commas (+ commas 1))]
      [else
       (define holes (if saw-element? (max 0 (- commas 1)) commas))
       (for ([_ (in-range holes)]) (set! entries (cons (uir-call (uir-symbol "pattern-hole") '()) entries)))
       (set! commas 0)
       (set! saw-element? #t)
       (set! entries (cons (lower-array-element (cdr item) tk-type tk-value) entries))]))
  ;; Trailing commas: `[a,]` has no hole, `[a,,]` has one; `[,]` is one hole.
  (define trailing (if saw-element? (max 0 (- commas 1)) commas))
  (for ([_ (in-range trailing)]) (set! entries (cons (uir-call (uir-symbol "pattern-hole") '()) entries)))
  (uir-call (uir-symbol "array-pattern") (filter values (reverse entries))))

(define (lower-array-element node tk-type tk-value)
  (define is-rest (for/or ([k (kids-of node)]) (and (tok? k tk-type) (eq? (tk-type k) 'Ellipsis))))
  (define se (find-kid node 'singleExpression))
  (cond
    [is-rest
     (define target (and se (lower-target-with-default se tk-type tk-value)))
     (and target (uir-call (uir-symbol "bind-rest") (list target)))]
    [else (and se (lower-target-with-default se tk-type tk-value))]))

;; Public entry: `node` is the CST node in binding position (an `assignable`, a
;; `singleExpression`, or a for-of/for-in left-hand node). Returns a UIR value
;; or #f when the node is not a binding pattern.
(define (lower-binding-pattern node tk-type tk-value)
  (cond
    [(not node) #f]
    [(and (cst-node? node) (eq? (tag-of node) 'assignable))
     (define inner (find-node-or-list node))
     (and inner (binding-target inner tk-type tk-value))]
    [else (binding-target node tk-type tk-value)]))
