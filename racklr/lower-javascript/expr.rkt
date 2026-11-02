#lang racket

;; ── JavaScript expression lowering ──────────────────────────────────
;; Expression section of the JavaScript CST → UIR lowering, extracted from
;; lower-javascript (B75). Statements/entry live in stmt.rkt; the shared
;; CST helpers in helpers.rkt. fn-body-lowerer is set by stmt.rkt to break
;; the statement↔expression cycle (expressions lower function bodies).

(require racklr/tree
         racklr/uir
         "helpers.rkt")

(provide lower-expression-sequence
         lower-single-expression
         fn-body-lowerer
         yield-stmt-lowerer
         fn-decl-lowerer)

(define fn-body-lowerer
  (make-parameter (λ (node tk-type tk-value)
                    (error "fn-body-lowerer not set — require lower-javascript/stmt.rkt"))))

(define yield-stmt-lowerer
  (make-parameter (λ (node tk-type tk-value)
                    (error "yield-stmt-lowerer not set — require lower-javascript/stmt.rkt"))))

(define fn-decl-lowerer
  (make-parameter (λ (node tk-type tk-value)
                    (error "fn-decl-lowerer not set — require lower-javascript/stmt.rkt"))))

;; ── Expressions ──────────────────────────────────────────────────────

(define (lower-expression-sequence node tk-type tk-value)
  (cond [(not node) (uir-null)]
        [(cst-node? node)
         (case (tag-of node)
           [(expressionSequence)
            (define kid (first (cst-kids node)))
            (lower-single-expression kid tk-type tk-value)]
           [(singleExpression) (lower-single-expression node tk-type tk-value)]
           [else (uir-null)])]
        [else (uir-null)]))

(define (lower-single-expression node tk-type tk-value)
  (define kids (kids-of node))
  (define (unary-prefix? k0) (and (tok? k0 tk-type) (cst-node? (second kids))))
  (define (unary-postfix? k0 k1) (and (cst-node? k0) (tok? k1 tk-type)))
  (cond [(and (pair? kids)
              (tok? (first kids) tk-type)
              (eq? (tk-type (first kids)) 'Class))
         ;; Class expression: Class (identifier)? classTail. The generated
         ;; parser routes a top-level `class Foo {}` through expressionStatement
         ;; rather than classDeclaration, so it lands here.
         (lower-class-expr node tk-type tk-value)]
        [(= (length kids) 1)
         (lower-expr-atom (first kids) tk-type tk-value)]
         [(= (length kids) 2)
          (cond [(and (tok? (first kids) tk-type) (eq? (tk-type (first kids)) 'Await))
                 ;; Await expression: await + expr
                 (uir-await (lower-single-expression (second kids) tk-type tk-value))]
                [(unary-prefix? (first kids))
                ;; Unary prefix: op + expr
                (define raw (tk-value (first kids)))
                (define op-sym
                  (if (set-member? (set "++" "--") raw)
                      (string-append "prefix" raw)
                      raw))
                (uir-call (uir-symbol op-sym)
                          (list (lower-single-expression (second kids) tk-type tk-value)))]
               [(unary-postfix? (first kids) (second kids))
                ;; Unary postfix: expr + op (++, --)
                (define op-sym
                  (string-append "postfix" (tk-value (second kids))))
                (uir-call (uir-symbol op-sym)
                          (list (lower-single-expression (first kids) tk-type tk-value)))]
               [else
                ;; Function call: callee + arguments
                (define callee (lower-single-expression (first kids) tk-type tk-value))
                (define args-node (second kids))
                (if (and (cst-node? args-node) (eq? (tag-of args-node) 'arguments))
                    (uir-call callee (lower-arguments args-node tk-type tk-value))
                    (uir-null))])]
         [(= (length kids) 3)
          (cond [(and (tok? (first kids) tk-type) (eq? (tk-type (first kids)) 'OpenParen)
                      (tok? (third kids) tk-type) (eq? (tk-type (third kids)) 'CloseParen))
                 ;; Parenthesized expression: ( expressionSequence )
                 (uir-paren (lower-expression-sequence (second kids) tk-type tk-value))]
                [(and (tok? (first kids) tk-type) (eq? (tk-type (first kids)) 'New))
                 (cond
                  ;; new.target
                  [(and (tok? (second kids) tk-type) (eq? (tk-type (second kids)) 'Dot))
                   (define target (lower-single-expression (third kids) tk-type tk-value))
                   (uir-call (uir-symbol "dot") (list (uir-symbol "new") target))]
                  ;; new Foo(args)
                  [else
                   (define class-name (lower-single-expression (second kids) tk-type tk-value))
                   (define args-node (third kids))
                   (uir-new class-name
                            (if (cst-node? args-node)
                                (lower-arguments args-node tk-type tk-value)
                                '()))])]
               [else
                ;; Binary infix: left + op + right
                (define left (lower-single-expression (first kids) tk-type tk-value))
                (define op-elt (second kids))
                (define right (lower-single-expression (third kids) tk-type tk-value))
                (define op-tok
                  (cond [(cst-node? op-elt) (first (kids-of op-elt))]
                        [(tok? op-elt tk-type) op-elt]
                        [else #f]))
                (if (and op-tok (tok? op-tok tk-type))
                    (uir-call (uir-symbol (tk-value op-tok))
                              (list left right))
                    (uir-null))])]
        [(= (length kids) 5)
         (cond [(and (tok? (second kids) tk-type) (eq? (tk-type (second kids)) 'QuestionMark))
                ;; Ternary: test ? consequent : alternate
                (uir-if (lower-single-expression (first kids) tk-type tk-value)
                        (lower-single-expression (third kids) tk-type tk-value)
                        (lower-single-expression (fifth kids) tk-type tk-value))]
               [else
                ;; Member access: object + (Dot/OpenBracket) + property/key
                (define obj (lower-single-expression (first kids) tk-type tk-value))
                (define op-elt (third kids))
                (cond [(and (tok? op-elt tk-type) (eq? (tk-type op-elt) 'Dot))
                       (uir-call (uir-symbol "dot")
                                 (list obj (uir-symbol (lower-identifier-name (list-ref kids 4) tk-type tk-value))))]
                      [(and (tok? op-elt tk-type) (eq? (tk-type op-elt) 'OpenBracket))
                       (uir-call (uir-symbol "index")
                                 (list obj (lower-expression-sequence (list-ref kids 3) tk-type tk-value)))]
                      [else (uir-null)])])]
        [else (uir-null)]))

;; Class expression: Class (identifier)? classTail. Anonymous classes
;; (`class {}`) have no identifier child and get a placeholder name.
(define (lower-class-expr node tk-type tk-value)
  (define ident-node (find-kid node 'identifier))
  (define class-name
    (if ident-node
        (uir-symbol (tk-value (first (kids-of ident-node))))
        (uir-symbol "?")))
  (define tail (find-kid node 'classTail))
  (define super (uir-null))
  (when tail
    (define grp (find-kid tail 'group))
    (when grp
      (define se (find-kid grp 'singleExpression))
      (when se
        (set! super (lower-single-expression se tk-type tk-value)))))
  (uir-class class-name (if (uir-null? super) '() (list super)) '() '()))

(define (lower-expr-atom value tk-type tk-value)
  (cond [(cst-node? value)
         (case (tag-of value)
           [(literal) (lower-literal value tk-type tk-value)]
           [(identifier) (lower-identifier value tk-type tk-value)]
           [(singleExpression) (lower-single-expression value tk-type tk-value)]
            [(objectLiteral) (lower-object-literal value tk-type tk-value)]
            [(arrayLiteral) (lower-array-literal value tk-type tk-value)]
            [(anonymousFunction) (lower-arrow-fn value tk-type tk-value)]
            [(yieldStatement) ((yield-stmt-lowerer) value tk-type tk-value)]
            [else (uir-null)])]
        [(tok? value tk-type)
         (uir-symbol (tk-value value))]
        [else (uir-null)]))

(define (lower-identifier node tk-type tk-value)
  (define tok (first (kids-of node)))
  (uir-var (uir-symbol (tk-value tok))))

(define (lower-literal node tk-type tk-value)
  (define kid (first (kids-of node)))
  (cond [(cst-node? kid)
         (case (tag-of kid)
           [(numericLiteral)
            (uir-number (tk-value (first (kids-of kid))))]
           [(stringLiteral)
            (uir-string (tk-value (first (kids-of kid))))]
           [(regularExpressionLiteral)
            (uir-call (uir-symbol "regex") (list (uir-string (tk-value (first (kids-of kid))))))]
           [else (uir-null)])]
        [(tok? kid tk-type)
         (case (tk-type kid)
           [(BooleanLiteral)
            (uir-bool (string=? (tk-value kid) "true"))]
            [(NullLiteral) (uir-null)]
            [(RegularExpressionLiteral)
             (uir-call (uir-symbol "regex") (list (uir-string (tk-value kid))))]
            [(StringLiteral)
             (uir-string (string-literal-text (tk-value kid)))]
           [else (uir-null)])]
        [else (uir-null)]))

(define (lower-arguments node tk-type tk-value)
  (define kids (kids-of node))
  (define arg-group (second kids))
  (cond [(and (cst-node? arg-group) (eq? (tag-of arg-group) 'group))
         (define grp-kids (kids-of arg-group))
         (define arg-nodes
           (let loop ([ks grp-kids] [acc '()])
             (cond [(null? ks) acc]
                   [(and (cst-node? (car ks)) (eq? (tag-of (car ks)) 'argument))
                    (loop (cdr ks) (append acc (list (car ks))))]
                   [(pair? (car ks))
                    (define tail-args
                      (for/list ([g (car ks)] #:when (cst-node? g))
                        (find-kid g 'argument)))
                    (loop (cdr ks) (append acc tail-args))]
                   [else (loop (cdr ks) acc)])))
          (map (λ (a)
                 (define first-kid (first (kids-of a)))
                 (cond [(and (tok? first-kid tk-type) (eq? (tk-type first-kid) 'Ellipsis))
                        ;; Spread argument: ...expr
                        (define grp (find-kid a 'group))
                        (define se (and grp (first (cst-kids grp))))
                        (if se
                            (uir-spread (lower-single-expression se tk-type tk-value))
                            (uir-null))]
                       [else
                        (define grp (first (cst-kids a)))
                        (define se (and (cst-node? grp) (eq? (tag-of grp) 'group)
                                        (first (cst-kids grp))))
                        (if se
                            (lower-single-expression se tk-type tk-value)
                            (uir-null))]))
               arg-nodes)]
        [else '()]))

(define (lower-identifier-name node tk-type tk-value)
  (define ident (or (find-kid node 'identifier)
                    (let ([in (find-kid node 'identifierName)])
                      (and in (find-kid in 'identifier)))))
  (if ident
      (tk-value (first (kids-of ident)))
      "?"))

;; A StringLiteral token's value keeps its quotes; a lowered string (and a
;; string-literal property name) keeps the unquoted text.
(define (string-literal-text raw)
  (define len (string-length raw))
  (if (and (>= len 2)
           (let ([first (string-ref raw 0)]
                 [last (string-ref raw (sub1 len))])
             (and (char=? first last)
                  (or (char=? first #\") (char=? first #\')))))
      (substring raw 1 (sub1 len))
      raw))

;; The first token under node: identifier, keyword, string, numeric, etc.
(define (first-token-leaf node tk-type)
  (cond
    [(tok? node tk-type) node]
    [(cst-node? node)
     (for/first ([k (in-list (kids-of node))]
                 #:when (or (tok? k tk-type) (cst-node? k)))
       (first-token-leaf k tk-type))]
    [(pair? node)
     (for/first ([k (in-list node)]
                 #:when (or (tok? k tk-type) (cst-node? k)))
       (first-token-leaf k tk-type))]
    [else #f]))

;; Text of a non-computed propertyName — identifierName, StringLiteral, or
;; numericLiteral. The UIR keeps the key text; emit-javascript quotes it when
;; the text is not a valid JS identifier (e.g. 'accent-1', 28, '5xl').
(define (lower-property-name node tk-type tk-value)
  (define t (first-token-leaf node tk-type))
  (define raw (and t (tk-value t)))
  (cond
    [(not raw) "?"]
    [(eq? (tk-type t) 'StringLiteral) (string-literal-text raw)]
    [else raw]))

(define (lower-getter-setter-name node tk-type tk-value)
  (define cen (find-kid node (quote classElementName)))
  (define pname (and cen (find-kid cen (quote propertyName))))
  (cond
    [pname (lower-property-name pname tk-type tk-value)]
    [else
     (let ([ident (find-kid node (quote identifier))])
       (if ident (tk-value (first (kids-of ident))) "?"))]))

(define (lower-object-literal node tk-type tk-value)
  (define grp (find-kid node (quote group)))
  (define entries (quote ()))
  (define (extract-prop k)
    (define ks (kids-of k))
    ;; Check for spread: { ...expr }
    (define is-spread (and (>= (length ks) 2)
                           (tok? (first ks) tk-type)
                           (eq? (tk-type (first ks)) 'Ellipsis)))
    (define pname (and (not is-spread) (find-kid k (quote propertyName))))
    (define se (and (not is-spread) (find-kid k (quote singleExpression))))
    (define fb (and (not is-spread) (find-kid k (quote functionBody))))
    (define getter (and (not is-spread) (find-kid k (quote getter))))
    (define setter (and (not is-spread) (find-kid k (quote setter))))
    ;; Check for computed property: {[expr]: val}
    ;; The grammar matches this as propertyName('[' singleExpression ']') : singleExpression
    ;; So pname is non-#f but wraps a computed key.
    (define is-computed
      (and pname
           (not getter) (not setter)
           (>= (length (kids-of pname)) 1)
           (tok? (first (kids-of pname)) tk-type)
           (string=? (tk-value (first (kids-of pname))) "[")))
    ;; Check for shorthand: {x}
    (define is-shorthand
      (and (not is-spread) (not is-computed) (not pname) (not getter) (not setter) (not fb) se))
    (cond
      [is-spread
       (define spread-se (find-kid k (quote singleExpression)))
       (when spread-se
         (set! entries (cons (cons (uir-string "...")
                                   (uir-spread (lower-single-expression spread-se tk-type tk-value)))
                             entries)))]
      [is-computed
       ;; pname = propertyName wrapping '[' singleExpression ']'
       (define pname-ks (kids-of pname))
       (define key-expr-node (second pname-ks))
       (when (and (cst-node? key-expr-node) se)
         (define key-uir (lower-single-expression key-expr-node tk-type tk-value))
         (define val-uir (lower-single-expression se tk-type tk-value))
         (set! entries (cons (cons key-uir val-uir) entries)))]
      [is-shorthand
       ;; Shorthand {x} is {x: x}. Extract identifier name from CST.
       (define ident-node
         (or (find-kid se (quote identifier))
             (find-kid se (quote identifierName))))
       (define name
         (if ident-node
             (uir-string (tk-value (first (kids-of ident-node))))
             (uir-string "?")))
       (define shorthand-uir (lower-single-expression se tk-type tk-value))
       (set! entries (cons (cons name shorthand-uir) entries))]
      [getter
       (define getter-name (lower-getter-setter-name getter tk-type tk-value))
       (define body ((fn-body-lowerer) fb tk-type tk-value))
       (set! entries (cons (cons (uir-string (string-append "get " getter-name))
                                 (uir-fn #f (quote ()) body #f #f))
                           entries))]
      [setter
       (define setter-name (lower-getter-setter-name setter tk-type tk-value))
       (define params (list (uir-symbol "v")))
       (define body ((fn-body-lowerer) fb tk-type tk-value))
       (set! entries (cons (cons (uir-string (string-append "set " setter-name))
                                 (uir-fn #f params body #f #f))
                           entries))]
      [(and pname fb (not se))
       (define key (uir-string (lower-property-name pname tk-type tk-value)))
       (define body ((fn-body-lowerer) fb tk-type tk-value))
       (set! entries (cons (cons key (uir-fn #f (quote ()) body #f #f)) entries))]
      [(and pname se)
       (let ([key (uir-string (lower-property-name pname tk-type tk-value))]
             [val (lower-single-expression se tk-type tk-value)])
         (set! entries (cons (cons key val) entries)))]))
  (when grp
    (let loop ([ks (kids-of grp)])
      (cond [(null? ks) (void)]
            [(and (cst-node? (car ks)) (eq? (tag-of (car ks)) (quote propertyAssignment)))
             (extract-prop (car ks))
             (loop (cdr ks))]
            [(pair? (car ks))
             (for ([g (car ks)] #:when (cst-node? g))
               (define pa (find-kid g (quote propertyAssignment)))
               (when pa (extract-prop pa)))
             (loop (cdr ks))]
            [else (loop (cdr ks))])))
  (uir-record (reverse entries)))

(define (lower-array-literal node tk-type tk-value)
  (define grp (find-kid node (quote group)))
  (define elist (and grp (find-kid grp (quote elementList))))
  (define items (quote ()))
  (define (extract-array-element k)
    (define kids (kids-of k))
    (cond [(and (>= (length kids) 2)
                (tok? (first kids) tk-type)
                (eq? (tk-type (first kids)) (quote Ellipsis)))
           ;; Spread element: ...expr
           (define se (find-kid k (quote singleExpression)))
           (when se
             (set! items (cons (uir-spread (lower-single-expression se tk-type tk-value))
                               items)))]
          [else
           (define se (find-kid k (quote singleExpression)))
           (when se
             (set! items (cons (lower-single-expression se tk-type tk-value) items)))]))
  (when elist
    (for ([k (kids-of elist)])
      (cond
        ((cst-node? k)
         (case (tag-of k)
           ((arrayElement) (extract-array-element k))))
        ((pair? k)
         (for ([g k] #:when (cst-node? g))
           (define ae (find-kid g (quote arrayElement)))
           (when ae (extract-array-element ae)))))))
  (uir-list (reverse items)))

(define (lower-arrow-fn node tk-type tk-value)
  ;; The generated parser routes a statement-level `function foo() {}` through
  ;; anonymousFunction → functionDeclaration (# NamedFunction), so a named
  ;; function declaration reaches us here rather than through lower-statement.
  (cond [(find-kid node 'functionDeclaration)
         => (λ (fd) ((fn-decl-lowerer) fd tk-type tk-value))]
        [(find-kid node 'arrowFunctionParameters)
         (lower-arrow-fn-impl node tk-type tk-value)]
        [else (lower-function-expr node tk-type tk-value)]))

(define (lower-arrow-fn-impl node tk-type tk-value)
  (define params-node (find-kid node (quote arrowFunctionParameters)))
  (define body-node (find-kid node (quote arrowFunctionBody)))
  (define params (quote ()))
  (define (extract-param k)
    (define assignable (find-kid k (quote assignable)))
    (when assignable
      (let ([ident (find-kid assignable (quote identifier))])
        (when ident
          (set! params (cons (uir-symbol (tk-value (first (kids-of ident)))) params))))))
  (define (extract-rest-param k)
    (define se (find-kid k (quote singleExpression)))
    (when se
      (let ([ident (find-kid se (quote identifier))])
        (if ident
            (set! params (cons (uir-spread (uir-symbol (tk-value (first (kids-of ident)))))
                              params))
            (set! params (cons (uir-spread (lower-single-expression se tk-type tk-value))
                              params))))))
  ;; Params arrive as formalParameterArg nodes directly, or wrapped in a
  ;; repeated group/list (`(_req, res) =>` puts `res` in a group inside the
  ;; parsed repetition). Walk every shape uniformly so no parameter is lost.
  (define (handle-param-node k)
    (cond [(and (cst-node? k) (eq? (tag-of k) (quote formalParameterArg)))
           (extract-param k)]
          [(and (cst-node? k) (eq? (tag-of k) (quote lastFormalParameterArg)))
           (extract-rest-param k)]
          [(and (cst-node? k) (eq? (tag-of k) (quote group)))
           (for ([g (kids-of k)] #:when (cst-node? g)) (handle-param-node g))]
          [(pair? k)
           (for ([g k] #:when (cst-node? g)) (handle-param-node g))]
          [else (void)]))
  (when params-node
    (define fpl (find-kid params-node (quote formalParameterList)))
    (when fpl
      (for ([k (kids-of fpl)]) (handle-param-node k))))
  (define body
    (cond
      [(not body-node) (uir-null)]
      [(find-kid body-node (quote functionBody)) =>
       (λ (fb) ((fn-body-lowerer) fb tk-type tk-value))]
      [else
       (let ([se (first (cst-kids body-node))])
         (lower-single-expression se tk-type tk-value))]))
  (uir-call (uir-symbol "=>") (list (uir-list (reverse params)) body)))

(define (lower-function-expr node tk-type tk-value)
  ;; Regular function expression: anonymousFunction with Function_ token
  (define params (quote ()))
  (define body (uir-null))
  (define kids (kids-of node))
  ;; kids: [Function_, OpenParen, (params or CloseParen), ...]
  (define (extract-param k)
    (define assignable (find-kid k (quote assignable)))
    (when assignable
      (define ident (find-kid assignable (quote identifier)))
      (when ident
        (set! params (cons (uir-symbol (tk-value (first (kids-of ident)))) params)))))
  (define (extract-rest-param k)
    (define se (find-kid k (quote singleExpression)))
    (when se
      (let ([ident (find-kid se (quote identifier))])
        (if ident
            (set! params (cons (uir-spread (uir-symbol (tk-value (first (kids-of ident)))))
                              params))
            (set! params (cons (uir-spread (lower-single-expression se tk-type tk-value))
                              params))))))
  (define fpl (find-kid node (quote formalParameterList)))
  (when fpl
    (let loop ([ks (kids-of fpl)])
      (cond [(null? ks) (void)]
            [(and (cst-node? (car ks)) (eq? (tag-of (car ks)) (quote formalParameterArg)))
             (extract-param (car ks))
             (loop (cdr ks))]
            [(and (cst-node? (car ks)) (eq? (tag-of (car ks)) (quote lastFormalParameterArg)))
             (extract-rest-param (car ks))
             (loop (cdr ks))]
            [(and (cst-node? (car ks)) (eq? (tag-of (car ks)) (quote group)))
             (for ([g (kids-of (car ks))] #:when (cst-node? g))
               (cond [(eq? (tag-of g) (quote formalParameterArg))
                      (extract-param g)]
                     [(eq? (tag-of g) (quote lastFormalParameterArg))
                      (extract-rest-param g)]))
             (loop (cdr ks))]
            [(pair? (car ks))
             (for ([g (car ks)] #:when (cst-node? g))
               (when (eq? (tag-of g) (quote formalParameterArg))
                 (extract-param g))
               (when (eq? (tag-of g) (quote lastFormalParameterArg))
                 (extract-rest-param g)))
             (loop (cdr ks))]
            [else (loop (cdr ks))])))
  (define body-node (find-kid node (quote functionBody)))
  (when body-node
    (set! body ((fn-body-lowerer) body-node tk-type tk-value)))
  (uir-call (uir-symbol "function") (list (uir-list (reverse params)) body)))
