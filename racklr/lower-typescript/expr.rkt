#lang racket

(require racklr/tree racklr/uir
         "helpers.rkt")

(provide lower-expression-sequence lower-single-expression lower-expr-atom
         lower-identifier lower-literal lower-arguments lower-identifier-name
         lower-object-literal lower-array-literal
         arrow-fn-lowerer yield-stmt-lowerer fn-body-lowerer)

;; ── Hook parameters set by the core lowering module ───────────────
(define arrow-fn-lowerer
  (make-parameter (λ (node tk-type tk-value)
    (error "arrow-fn-lowerer not set"))))
(define yield-stmt-lowerer
  (make-parameter (λ (node tk-type tk-value)
    (error "yield-stmt-lowerer not set"))))
(define fn-body-lowerer
  (make-parameter (λ (node tk-type tk-value)
    (error "fn-body-lowerer not set"))))

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
  (cond [(= (length kids) 1)
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
                 ;; TypeScript non-null assertion: expr! → just expr
                 (if (eq? (tk-type (second kids)) 'Not)
                     (lower-single-expression (first kids) tk-type tk-value)
                     (let ([op-sym (string-append "postfix" (tk-value (second kids)))])
                       (uir-call (uir-symbol op-sym)
                                 (list (lower-single-expression (first kids) tk-type tk-value)))))]
                [(and (cst-node? (first kids))
                      (not (tok? (second kids) tk-type))
                      (not (cst-node? (second kids))))
                 ;; IdentifierAsExpression: identifierName + null/none trailing context
                 (lower-expr-atom (first kids) tk-type tk-value)]
                [else
                 ;; Detect TS infix: left + (operator . right) nested in right node
                 (define left-kid (first kids))
                 (define right-kid (second kids))
                 (cond [(and (cst-node? right-kid) (eq? (tag-of right-kid) 'singleExpression)
                             (not (null? (kids-of right-kid)))
                             (tok? (first (kids-of right-kid)) tk-type)
                             ;; Infix: 2 kids (op token, right operand). Call: 3+ kids (paren, args, paren).
                             (= (length (kids-of right-kid)) 2)
                             (cst-node? (second (kids-of right-kid))))
                        (define op (tk-value (first (kids-of right-kid))))
                        (define rhs (second (kids-of right-kid)))
                        (uir-call (uir-symbol op)
                                  (list (lower-single-expression left-kid tk-type tk-value)
                                        (lower-single-expression rhs tk-type tk-value)))]
                         [else
                          ;; Function call: callee + arguments
                          (define callee-raw (lower-single-expression (first kids) tk-type tk-value))
                          ;; If callee is an optional-chain ternary (a?.b), restructure
                          ;; so the call goes inside the false branch: a == null ? void 0 : a.b(args)
                          (define (optchain-call callee args)
                            (if (and (uir-if? callee)
                                     (uir-call? (uir-if-else callee)))
                                (uir-if (uir-if-test callee)
                                        (uir-if-then callee)
                                        (uir-call (uir-if-else callee) args))
                                (uir-call callee args)))
                           (define (openparen-call-args paren-node)
                             (define args-es (second (kids-of paren-node)))
                             (let loop ([ks (kids-of args-es)] [acc '()])
                               (cond [(null? ks) (reverse acc)]
                                     [(cst-node? (car ks))
                                      (loop (cdr ks)
                                            (cons (lower-single-expression (car ks) tk-type tk-value) acc))]
                                     [(pair? (car ks))
                                      ;; Comma-separated remaining args wrapped in groups
                                      (define more-args
                                        (for/list ([g (car ks)] #:when (cst-node? g))
                                          (define inner (second (kids-of g)))
                                          (lower-single-expression inner tk-type tk-value)))
                                      (loop (cdr ks) (append (reverse more-args) acc))]
                                     [else (loop (cdr ks) acc)])))
                           (define (lower-call-args callee args-node)
                             (cond
                               [(and (cst-node? args-node) (eq? (tag-of args-node) 'arguments))
                                (optchain-call callee (lower-arguments args-node tk-type tk-value))]
                               [(and (cst-node? args-node) (eq? (tag-of args-node) 'singleExpression)
                                     (>= (length (kids-of args-node)) 3)
                                     (tok? (first (kids-of args-node)) tk-type)
                                     (eq? (tk-type (first (kids-of args-node))) 'OpenParen))
                                (optchain-call callee (openparen-call-args args-node))]
                               [(and (cst-node? args-node) (eq? (tag-of args-node) 'singleExpression)
                                     (= (length (kids-of args-node)) 2)
                                     (cst-node? (first (kids-of args-node)))
                                     (eq? (tag-of (first (kids-of args-node))) 'singleExpression)
                                     (cst-node? (second (kids-of args-node)))
                                     (eq? (tag-of (second (kids-of args-node))) 'arguments))
                                ;; Curried call f(inner)(outer): the inner suffix
                                ;; holds the rest of the chain, the outer is an
                                ;; `arguments` node.
                                (define inner-call (lower-call-args callee (first (kids-of args-node))))
                                (optchain-call inner-call
                                               (lower-arguments (second (kids-of args-node)) tk-type tk-value))]
                               [else (uir-null)]))
                           (define args-node (second kids))
                          ;; Check for 'as' cast parsed as IdentifierExpression:
                          ;; singleExpression(identifierName(identifier(As)), singleExpression(type))
                          (define is-as-cast?
                            (and (cst-node? args-node) (eq? (tag-of args-node) 'singleExpression)
                                 (= (length (kids-of args-node)) 2)
                                 (cst-node? (first (kids-of args-node)))
                                 (eq? (tag-of (first (kids-of args-node))) 'identifierName)
                                 (let ([id-tok (first (kids-of (first (kids-of (first (kids-of args-node))))))])
                                   (and (tok? id-tok tk-type) (eq? (tk-type id-tok) 'As)))))
                           (cond [is-as-cast? callee-raw]  ;; strip 'as type' cast, keep left side
                                 [(and (cst-node? args-node) (eq? (tag-of args-node) 'singleExpression)
                                      (= (length (kids-of args-node)) 2)
                                      (cst-node? (first (kids-of args-node)))
                                      (eq? (tag-of (first (kids-of args-node))) 'typeArguments))
                                 ;; call with type arguments: callee + singleExpression(typeArguments, expressionSequence?)
                                 ;; Strip type args, extract call args from expressionSequence
                                 (define inner-kids (kids-of args-node))
                                 (define es (second inner-kids))
                                 (define arg-exprs
                                   (if (and (cst-node? es) (eq? (tag-of es) 'expressionSequence))
                                       (let loop ([ks (kids-of es)] [acc '()])
                                         (cond [(null? ks) (reverse acc)]
                                               [(cst-node? (car ks))
                                                (loop (cdr ks)
                                                      (cons (lower-single-expression (car ks) tk-type tk-value) acc))]
                                               [(pair? (car ks))
                                                (define more-args
                                                  (for/list ([g (car ks)] #:when (cst-node? g))
                                                    (define inner (second (kids-of g)))
                                                    (lower-single-expression inner tk-type tk-value)))
                                                (loop (cdr ks) (append (reverse more-args) acc))]
                                               [else (loop (cdr ks) acc)]))
                                       '()))
                                 (optchain-call callee-raw arg-exprs)]
                                [else (lower-call-args callee-raw args-node)])])])]
        [(= (length kids) 3)
         (cond [(and (tok? (first kids) tk-type) (eq? (tk-type (first kids)) 'New))
                (cond
                  ;; new.target
                  [(and (tok? (second kids) tk-type) (eq? (tk-type (second kids)) 'Dot))
                   (define target (lower-single-expression (third kids) tk-type tk-value))
                   (uir-call (uir-symbol "dot") (list (uir-symbol "new") target))]
                  ;; new Foo(args) — but parser may nest call inside second kid
                  [else
                   (define snd (second kids))
                   (define-values (class-name args)
                     (cond
                       ;; Case: second kid is singleExpression wrapping [classExpr, arguments]
                       ;; e.g. new Date() → [New, singleExpression([Date, arguments]), 'none]
                        [(and (cst-node? snd) (eq? (tag-of snd) 'singleExpression)
                              (= (length (kids-of snd)) 2)
                              (let ([snd-kids (kids-of snd)])
                                (and (cst-node? (first snd-kids))
                                     (cst-node? (second snd-kids))
                                     (eq? (tag-of (second snd-kids)) 'arguments))))
                         (define snd-kids (kids-of snd))
                         (values (lower-single-expression (first snd-kids) tk-type tk-value)
                                 (lower-arguments (second snd-kids) tk-type tk-value))]
                        ;; Case: second kid is a parenthesized singleExpression:
                        ;; e.g. new Error("x") → [New, singleExpression([identifierName(Error),
                        ;;   singleExpression(OpenParen expressionSequence CloseParen)]), 'none]
                        [(and (cst-node? snd) (eq? (tag-of snd) 'singleExpression)
                              (= (length (kids-of snd)) 2)
                              (let ([snd-kids (kids-of snd)])
                                (and (cst-node? (first snd-kids))
                                     (cst-node? (second snd-kids))
                                     (eq? (tag-of (second snd-kids)) 'singleExpression)
                                     (not (null? (kids-of (second snd-kids))))
                                     (tok? (first (kids-of (second snd-kids))) tk-type)
                                     (eq? (tk-type (first (kids-of (second snd-kids)))) 'OpenParen))))
                         (define snd-kids (kids-of snd))
                         (define args-es (second (kids-of (second snd-kids))))
                         (define arg-exprs
                           (let loop ([ks (kids-of args-es)] [acc '()])
                             (cond [(null? ks) (reverse acc)]
                                   [(cst-node? (car ks))
                                    (loop (cdr ks) (cons (lower-single-expression (car ks) tk-type tk-value) acc))]
                                   [(pair? (car ks))
                                    (define more-args
                                      (for/list ([g (car ks)] #:when (cst-node? g))
                                        (define inner (second (kids-of g)))
                                        (lower-single-expression inner tk-type tk-value)))
                                    (loop (cdr ks) (append (reverse more-args) acc))]
                                   [else (loop (cdr ks) acc)])))
                         (values (lower-single-expression (first snd-kids) tk-type tk-value)
                                 arg-exprs)]
                        [else
                        (values (lower-single-expression snd tk-type tk-value)
                                (let ([args-node (third kids)])
                                  (if (cst-node? args-node)
                                      (lower-arguments args-node tk-type tk-value)
                                      '())))]))
                   (uir-new class-name args)])]
                               [else
                ;; Check for parenthesized expression: (expr) → OpenParen, expressionSequence, CloseParen
                (cond [(and (tok? (first kids) tk-type) (eq? (tk-type (first kids)) 'OpenParen)
                            (cst-node? (second kids)) (eq? (tag-of (second kids)) 'expressionSequence))
                       (define es-kids (kids-of (second kids)))
                       (cond [(and (pair? es-kids) (not (null? es-kids)) (cst-node? (first es-kids)))
                              (lower-single-expression (first es-kids) tk-type tk-value)]
                             [else (uir-null)])]
                      [else
                       ;; Binary infix: left + op + right
                       (define left (lower-single-expression (first kids) tk-type tk-value))
                       (define op-elt (second kids))
                       (define right (lower-single-expression (third kids) tk-type tk-value))
                       ;; Extract operator token: handle nested singleExpression -> identifierName -> identifier -> token
                       (define op-tok
                         (let loop ([n op-elt])
                           (cond [(tok? n tk-type) n]
                                 [(cst-node? n)
                                  (let ([k (first (kids-of n))])
                                    (if (tok? k tk-type) k (loop k)))]
                                 [else #f])))
                        ;; TypeScript as-expression: left as type -> just left
                        ;; Optional chaining: left?.right -> null check ternary
                        (cond [(and op-tok (tok? op-tok tk-type) (eq? (tk-type op-tok) (quote As)))
                               left]
                              [(and op-tok (tok? op-tok tk-type) (eq? (tk-type op-tok) (quote QuestionMarkDot)))
                               (uir-if (uir-call (uir-symbol "==") (list left (uir-null)))
                                       (uir-call (uir-symbol "void") (list (uir-number "0")))
                                       (uir-call (uir-symbol "dot") (list left right)))]
                              [(and op-tok (tok? op-tok tk-type))
                               (uir-call (uir-symbol (tk-value op-tok)) (list left right))]
                              [else (uir-null)])])])]        [(>= (length kids) 5)
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
                       [else
                        ;; TS: extra wrapper nodes. Try scanning for the op token.
                        (define kid-count (length kids))
                        (define op-idx
                          (for/or ([i (in-range 1 (sub1 kid-count))]
                                   [k (in-list (drop kids 1))]
                                   #:when (and (tok? k tk-type)
                                               (member (tk-type k) '(Dot OpenBracket))))
                            i))
                        (if op-idx
                            (cond [(eq? (tk-type (list-ref kids op-idx)) 'Dot)
                                   (define left (lower-single-expression (list-ref kids (sub1 op-idx)) tk-type tk-value))
                                   (define right-node (list-ref kids (add1 op-idx)))
                                   (define right
                                     (if (cst-node? right-node)
                                         (uir-symbol (lower-identifier-name right-node tk-type tk-value))
                                         (uir-symbol "?")))
                                   (uir-call (uir-symbol "dot") (list left right))]
                                  [(eq? (tk-type (list-ref kids op-idx)) 'OpenBracket)
                                   (define left (lower-single-expression (list-ref kids (sub1 op-idx)) tk-type tk-value))
                                   (define seq (list-ref kids (add1 op-idx)))
                                   (uir-call (uir-symbol "index") (list left (lower-expression-sequence seq tk-type tk-value)))]
                                  [else (uir-null)])
                            (uir-null))])])]
        [else (uir-null)]))

(define (lower-expr-atom value tk-type tk-value)
  (cond [(cst-node? value)
         (case (tag-of value)
           [(literal) (lower-literal value tk-type tk-value)]
            [(identifier) (lower-identifier value tk-type tk-value)]
            [(identifierName)
             ;; TS: identifierName -> identifier | reservedWord | ...
             (define inner (first (kids-of value)))
             (cond [(tok? inner tk-type)
                    (case (tk-type inner)
                      [(BooleanLiteral) (uir-bool (string=? (tk-value inner) "true"))]
                      [(NullLiteral) (uir-null)]
                      [else (uir-var (uir-symbol (tk-value inner)))])]
                   [(cst-node? inner)
                    (case (tag-of inner)
                      [(reservedWord)
                       (define tok (first (kids-of inner)))
                       (cond [(tok? tok tk-type)
                              (case (tk-type tok)
                                [(BooleanLiteral) (uir-bool (string=? (tk-value tok) "true"))]
                                [(NullLiteral) (uir-null)]
                                [else (uir-var (uir-symbol (tk-value tok)))])]
                             [else (uir-null)])]
                      [(identifier)
                       (define tok (first (kids-of inner)))
                       (if (tok? tok tk-type)
                           (uir-var (uir-symbol (tk-value tok)))
                           (uir-null))]
                      [else (uir-null)])]
                   [else (uir-null)])]
           [(singleExpression) (lower-single-expression value tk-type tk-value)]
            [(objectLiteral) (lower-object-literal value tk-type tk-value)]
            [(arrayLiteral) (lower-array-literal value tk-type tk-value)]
            [(anonymousFunction) ((arrow-fn-lowerer) value tk-type tk-value)]
            [(yieldStatement) ((yield-stmt-lowerer) value tk-type tk-value)]
            [else (uir-null)])]
        [(tok? value tk-type)
         (uir-symbol (tk-value value))]
        [else (uir-null)]))

(define (lower-identifier node tk-type tk-value)
  (define tok (first (kids-of node)))
  (uir-var (uir-symbol (tk-value tok))))

(define (lower-template-literal node tk-type tk-value)
  ;; Collect parts: alternating static strings and expressions
  ;; Then left-fold: "a" + expr + "b" + expr + "c"
  (define kids (kids-of node))
  ;; kids: BackTick + templateStringAtom* + BackTick
  ;; templateStringAtom* appears as a list (not individual nodes)
  (define parts '())
  (define current-str "")
  ;; Flatten: handle both individual nodes and lists of templateStringAtom
  (define all-atoms '())
  (for ([k kids])
    (cond [(pair? k)
           ;; List of templateStringAtom nodes from * repetition
           (set! all-atoms (append all-atoms k))]
          [(or (and (tok? k tk-type) (eq? (tk-type k) 'TemplateStringAtom))
               (and (tok? k tk-type) (eq? (tk-type k) 'TemplateStringEscapeAtom))
               (cst-node? k))
           (set! all-atoms (append all-atoms (list k)))]))
  (for ([k all-atoms])
    (cond [(and (tok? k tk-type) (eq? (tk-type k) 'TemplateStringAtom))
           (set! current-str (string-append current-str (tk-value k)))]
          [(and (tok? k tk-type) (eq? (tk-type k) 'TemplateStringEscapeAtom))
           (define val (tk-value k))
           (if (>= (string-length val) 2)
               (set! current-str (string-append current-str (string-ref val 1) ""))
               (set! current-str (string-append current-str val)))]
          [(cst-node? k)
           ;; templateStringAtom: first kid is either static text or TemplateStringStartExpression
           (define atom-kids (kids-of k))
           (cond [(and (pair? atom-kids)
                       (tok? (first atom-kids) tk-type)
                       (eq? (tk-type (first atom-kids)) 'TemplateStringAtom))
                  (set! current-str (string-append current-str (tk-value (first atom-kids))))]
                 [(and (pair? atom-kids)
                       (tok? (first atom-kids) tk-type)
                       (eq? (tk-type (first atom-kids)) 'TemplateStringEscapeAtom))
                  (define val (tk-value (first atom-kids)))
                  (if (>= (string-length val) 2)
                      (set! current-str (string-append current-str (string-ref val 1) ""))
                      (set! current-str (string-append current-str val)))]
                 [else
                  (set! parts (cons (uir-string current-str) parts))
                   (set! current-str "")
                   (define expr-kid (for/or ([ak atom-kids]
                                             #:when (and (cst-node? ak)
                                                         (eq? (tag-of ak) 'singleExpression)))
                                     ak))
                   (if expr-kid
                       (set! parts (cons (uir-paren (lower-single-expression expr-kid tk-type tk-value)) parts))
                       (void))])]
          [else (void)]))
  ;; Push remaining static string at end
  (set! parts (cons (uir-string current-str) parts))
  ;; Reverse to correct order (parts were built prepending)
  (set! parts (reverse parts))
  ;; Left-fold with +
  (let loop ([remaining parts])
    (if (null? remaining)
        (uir-string "")
        (let ([r (car remaining)])
          (if (null? (cdr remaining))
              r
              (uir-call (uir-symbol "+") (list r (loop (cdr remaining)))))))))

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
           [(templateStringLiteral)
            (lower-template-literal kid tk-type tk-value)]
           [else (uir-null)])]
        [(tok? kid tk-type)
         (case (tk-type kid)
           [(BooleanLiteral)
            (uir-bool (string=? (tk-value kid) "true"))]
            [(NullLiteral) (uir-null)]
            [(RegularExpressionLiteral)
             (uir-call (uir-symbol "regex") (list (uir-string (tk-value kid))))]
            [(StringLiteral)
            (define raw (tk-value kid))
            (define len (string-length raw))
            (if (and (> len 1)
                     (or (char=? (string-ref raw 0) (string-ref raw (sub1 len)))
                         (eqv? (string-ref raw 0) #\"))
                     (or (char=? (string-ref raw 0) #\")
                         (char=? (string-ref raw 0) #\')))
                (uir-string (substring raw 1 (sub1 len)))
                (uir-string raw))]
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
                    [(and (cst-node? (car ks))
                          (member (tag-of (car ks)) '(argument argumentList)))
                     (if (eq? (tag-of (car ks)) 'argument)
                         (loop (cdr ks) (append acc (list (car ks))))
                         ;; TS wraps arguments in argumentList; dive into its kids
                         (loop (append (kids-of (car ks)) (cdr ks)) acc))]
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




(define (lower-getter-setter-name node tk-type tk-value)
  (define cen (find-kid node (quote classElementName)))
  (if cen
      (lower-identifier-name (find-kid cen (quote propertyName)) tk-type tk-value)
      (let ([ident (find-kid node (quote identifier))])
        (if ident (tk-value (first (kids-of ident))) "?"))))

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
    (define iok (and (not is-spread) (find-kid k (quote identifierOrKeyWord))))
    ;; Check for shorthand: {x}
    (define is-shorthand
      (and (not is-spread) (not is-computed) (not pname) iok (not getter) (not setter) (not fb)))
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
        ;; Shorthand {x} is {x: x}. Extract identifier name from iok.
        (define ident-node (find-kid iok (quote identifier)))
        (define name
          (if ident-node
              (uir-string (tk-value (first (kids-of ident-node))))
              (uir-string "?")))
        (define shorthand-uir (lower-single-expression iok tk-type tk-value))
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
       (define key (uir-string (lower-identifier-name pname tk-type tk-value)))
       (define body ((fn-body-lowerer) fb tk-type tk-value))
       (set! entries (cons (cons key (uir-fn #f (quote ()) body #f #f)) entries))]
      [(and pname se)
       (let ([key (uir-string (lower-identifier-name pname tk-type tk-value))]
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
    ;; Look for the group child that wraps singleExpression/identifier
    (define grp (find-kid k (quote group)))
    (cond [(and (>= (length kids) 2)
                (tok? (first kids) tk-type)
                (eq? (tk-type (first kids)) (quote Ellipsis)))
           ;; Spread element: ...expr — find singleExpression inside group
           (define se (and grp (find-kid grp (quote singleExpression))))
           (when se
             (set! items (cons (uir-spread (lower-single-expression se tk-type tk-value))
                               items)))]
          [else
           ;; Regular element — find singleExpression or identifier inside group
           (define se (and grp (find-kid grp (quote singleExpression))))
           (when se
             (set! items (cons (lower-single-expression se tk-type tk-value) items)))
           (when (not se)
             (define id (and grp (find-kid grp (quote identifier))))
             (when id
               (define id-tok (first (kids-of id)))
               (set! items (cons (uir-symbol (tk-value id-tok)) items))))]))
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

