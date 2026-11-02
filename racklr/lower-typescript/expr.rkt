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
          (cond [(folded-postfix-suffix? (first kids) (second kids))
                 ;; The generated parser folds a postfix chain after a leading
                 ;; identifier into the trailing singleExpression of the
                 ;; IdentifierExpression alternative; reattach it to the base.
                 (lower-folded-suffix (first kids) (second kids) tk-type tk-value)]
                [(and (tok? (first kids) tk-type) (eq? (tk-type (first kids)) 'Await))
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

;; ── Folded postfix suffix ────────────────────────────────────────────
;;
;; The generated TypeScript parser folds the postfix part of an expression
;; that follows a leading identifier into the trailing `singleExpression?` of
;; the IdentifierExpression alternative (`identifierName singleExpression?`)
;; instead of producing the MemberIndexExpression the grammar declares. So
;; `a[0]` arrives as
;;   singleExpression(identifierName(a), singleExpression(arrayLiteral))
;; and `a[0].b` as
;;   singleExpression(identifierName(a), singleExpression(chain ...)).
;; Without reattaching that suffix the subscript is dropped, which both loses
;; the expression and emits `const x` declarations that no JS parser accepts.
;;
;; Rather than lower each operator itself, the reconstruction builds a
;; synthetic CST in the shapes the grammar's member/call/binary alternatives
;; produce and hands that back to `lower-single-expression`, so precedence,
;; recursion and the uir-null fallback stay in one place. Shapes the
;; reconstruction cannot rebuild (optional chaining, multi-element brackets)
;; return #f and lower to uir-null exactly as they did before.

(define (folded-postfix-suffix? base suffix)
  (and (cst-node? base)
       (cst-node? suffix)
       (eq? (tag-of suffix) 'singleExpression)
       (folded-suffix-start? suffix)))

(define (folded-suffix-children n)
  (filter (λ (k) (not (eq? k 'none))) (kids-of n)))

(define (folded-suffix-start? n)
  (and (cst-node? n)
       (eq? (tag-of n) 'singleExpression)
       (let ([ks (folded-suffix-children n)])
         (and (pair? ks)
              (let ([k (first ks)])
                (and (cst-node? k)
                     (or (eq? (tag-of k) 'arrayLiteral)
                         (and (eq? (tag-of k) 'singleExpression)
                              (folded-suffix-start? k)))))))))

;; ── Synthetic CST nodes for the normal lowering path ────────────────
;;
;; Each builder mirrors the CST the generated parser produces for the same
;; construct written without the folded IdentifierExpression. No token is
;; fabricated: every call takes the token or node the folded suffix already
;; carries.

(define (make-member-dot-node obj dot-token name-node)
  (node 'singleExpression (list obj 'none dot-token 'none name-node 'none)))

(define (make-member-index-node obj open-token expr-seq close-token)
  (node 'singleExpression (list obj 'none open-token expr-seq close-token 'none)))

(define (make-call-node obj call-node)
  (node 'singleExpression (list obj call-node)))

(define (make-binary-node left op-token right-node)
  (node 'singleExpression (list left op-token right-node)))

(define (make-ternary-node test q-token conseq colon-token alt-node)
  (node 'singleExpression (list test q-token conseq colon-token alt-node)))

(define (make-postfix-node expr op-token)
  (node 'singleExpression (list expr op-token)))

(define (single-expression-node? n)
  (and (cst-node? n) (eq? (tag-of n) 'singleExpression)))

(define (identifier-name-node? n)
  (and (cst-node? n) (eq? (tag-of n) 'identifierName)))

;; Operators the JavaScript emitter renders as infix syntax. A folded operator
;; outside this set is left unreconstructed rather than emitted as a call.
(define infix-operator-values
  (set "+" "-" "*" "/" "%" "**" "=" "==" "===" "!=" "!=="
       "<" ">" "<=" ">=" "&&" "||"
       "+=" "-=" "*=" "/=" "**="))

;; First token held inside an operator node (a group, or an assignmentOperator
;; wrapper) such as `+` in `group(Plus)` or `+=` in `assignmentOperator`.
(define (contained-op-token n tk-type)
  (let loop ([n n])
    (cond [(tok? n tk-type) n]
          [(cst-node? n)
           (and (pair? (kids-of n)) (loop (first (kids-of n))))]
          [else #f])))

;; The operator token for a folded binary/assignment step, or #f. The parser
;; carries some operators as bare tokens (`&&`) and others wrapped in a group
;; node (`+`), so both forms are accepted.
(define (folded-binary-op k tk-type tk-value)
  (define t (cond [(cst-node? k) (contained-op-token k tk-type)]
                  [(tok? k tk-type) k]
                  [else #f]))
  (and t (set-member? infix-operator-values (tk-value t)) t))

;; The expression and bracket tokens of a `[expr]` subscript, which the parser
;; represents as an arrayLiteral. Returns #f unless the bracket holds exactly
;; one expression; multi-element brackets (the comma operator) are a known gap.
(define (folded-index-parts array-lit tk-type)
  (define grp (find-kid array-lit 'group))
  (define elist (and grp (find-kid grp 'elementList)))
  (define elements
    (if elist
        (let loop ([ks (kids-of elist)] [acc '()])
          (cond [(null? ks) (reverse acc)]
                [(and (cst-node? (first ks)) (eq? (tag-of (first ks)) 'arrayElement))
                 (loop (cdr ks) (cons (first ks) acc))]
                [(pair? (first ks))
                 (loop (cdr ks)
                       (append (reverse
                                (filter-map
                                 (λ (g) (and (cst-node? g) (find-kid g 'arrayElement)))
                                 (first ks)))
                               acc))]
                [else (loop (cdr ks) acc)]))
        '()))
  (define open-token
    (and grp
         (for/or ([k (kids-of grp)]
                  #:when (and (tok? k tk-type) (eq? (tk-type k) 'OpenBracket)))
           k)))
  (define close-token
    (and grp
         (for/or ([k (kids-of grp)]
                  #:when (and (tok? k tk-type) (eq? (tk-type k) 'CloseBracket)))
           k)))
  (and open-token close-token (= (length elements) 1)
       (let* ([ae (first elements)]
              [ae-group (find-kid ae 'group)]
              [expr-node (and ae-group (find-kid ae-group 'singleExpression))])
         (and expr-node (list open-token expr-node close-token)))))

(define (as-expression-sequence n)
  (cond [(and (cst-node? n) (eq? (tag-of n) 'expressionSequence)) n]
        [(cst-node? n) (node 'expressionSequence (list n))]
        [else #f]))

;; Rebuild the expression described by the folded suffix, or #f when a part of
;; the suffix cannot be rebuilt. `ks` are the suffix children in source order.
(define (rebuild-folded-ops base ks tk-type tk-value)
  (cond
    [(null? ks) base]
    [else
     (define k (first ks))
     (define rest (cdr ks))
     (cond
       ;; Closing delimiters were already consumed by the operation before them.
       [(and (tok? k tk-type)
             (member (tk-type k) '(CloseBracket CloseParen CloseBrace)))
        (rebuild-folded-ops base rest tk-type tk-value)]
       ;; a[expr] — the parser represents the bracket suffix as an arrayLiteral
       [(and (cst-node? k) (eq? (tag-of k) 'arrayLiteral))
        (define parts (folded-index-parts k tk-type))
        (and parts
             (rebuild-folded-ops
              (make-member-index-node base
                                      (first parts)
                                      (as-expression-sequence (second parts))
                                      (third parts))
              rest tk-type tk-value))]
       ;; Nested folded chain: a[0].b carries its remaining ops in a child
       [(and (cst-node? k) (eq? (tag-of k) 'singleExpression))
        (define inner (rebuild-folded-ops base (folded-suffix-children k) tk-type tk-value))
        (and inner (rebuild-folded-ops inner rest tk-type tk-value))]
       ;; .prop
       [(and (tok? k tk-type) (eq? (tk-type k) 'Dot))
        (define name-node (and (pair? rest) (first rest)))
        (and (identifier-name-node? name-node)
             (rebuild-folded-ops (make-member-dot-node base k name-node)
                                 (cdr rest) tk-type tk-value))]
       ;; [expr] kept as a bracket token pair
       [(and (tok? k tk-type) (eq? (tk-type k) 'OpenBracket))
        (define seq (and (pair? rest) (first rest)))
        (define close (and (pair? rest) (pair? (cdr rest)) (second rest)))
        (and (tok? close tk-type) (eq? (tk-type close) 'CloseBracket)
             (rebuild-folded-ops
              (make-member-index-node base k (as-expression-sequence seq) close)
              (cddr rest) tk-type tk-value))]
       ;; (args) call, carried as an open/seq/close token triple
       [(and (tok? k tk-type) (eq? (tk-type k) 'OpenParen))
        (define seq (and (pair? rest) (first rest)))
        (define close (and (pair? rest) (pair? (cdr rest)) (second rest)))
        (and (cst-node? seq) (tok? close tk-type) (eq? (tk-type close) 'CloseParen)
             (rebuild-folded-ops
              (make-call-node base (node 'singleExpression (list k seq close)))
              (cddr rest) tk-type tk-value))]
       ;; (args) call, carried as an arguments node
       [(and (cst-node? k) (eq? (tag-of k) 'arguments))
        (rebuild-folded-ops (make-call-node base k) rest tk-type tk-value)]
       ;; test ? conseq : alt
       [(and (tok? k tk-type) (eq? (tk-type k) 'QuestionMark))
        (define conseq (and (pair? rest) (first rest)))
        (define colon (and (pair? rest) (pair? (cdr rest)) (second rest)))
        (define alt (and (pair? rest) (pair? (cdr rest)) (pair? (cddr rest)) (third rest)))
        (and (single-expression-node? conseq)
             (tok? colon tk-type) (eq? (tk-type colon) 'Colon)
             (single-expression-node? alt)
             (rebuild-folded-ops
              (make-ternary-node base k conseq colon alt)
              (cdddr rest) tk-type tk-value))]
       ;; postfix ++ / --
       [(and (tok? k tk-type) (member (tk-type k) '(PlusPlus MinusMinus)))
        (rebuild-folded-ops (make-postfix-node base k) rest tk-type tk-value)]
       ;; binary / assignment operator followed by its right operand
       [(and (folded-binary-op k tk-type tk-value)
             (pair? rest) (single-expression-node? (first rest)))
        (define op-token (folded-binary-op k tk-type tk-value))
        (rebuild-folded-ops
         (make-binary-node base op-token (first rest))
         (cdr rest) tk-type tk-value)]
       [else #f])]))

(define (lower-folded-suffix base suffix tk-type tk-value)
  (define rebuilt (rebuild-folded-ops base (folded-suffix-children suffix) tk-type tk-value))
  (if rebuilt
      (lower-single-expression rebuilt tk-type tk-value)
      (uir-null)))

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

