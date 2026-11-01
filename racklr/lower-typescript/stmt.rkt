#lang racket

(require racklr/tree
         racklr/uir
         racklr/effects/jsdoc-annot
         "helpers.rkt"
         "expr.rkt")

(provide lower-program)

;; ── JavaScript CST → UIR lowering pass ───────────────────────────────
;;
;; lower-program takes: cst, tk-type, tk-value
;;   cst      — CST node for the top-level program rule (sourceElements)
;;   tk-type  — token type-accessor from the parser
;;   tk-value — token value-accessor from the parser

;; ── Entry ────────────────────────────────────────────────────────────

(define (lower-program cst tk-type tk-value [tokens '()])
  (define se (find-node-or-list cst))
  (cond
    [(not se) (uir-null)]
    [(null? tokens) (lower-source-elements se tk-type tk-value)]
    [else
     ;; With comment-preserving tokens supplied, attach JSDoc @effects rows to
     ;; the top-level declarations that follow them.
     (define elements (top-level-elements se))
     (define annotations (jsdoc-annotations tokens tk-type tk-value))
     (define-values (_remaining stmts)
       (for/fold ([remaining annotations] [out '()])
                 ([e (in-list elements)])
         (define stmt (lower-source-elements e tk-type tk-value))
         (define-values (row rest) (take-annotation remaining (min-token-offset e)))
         (values rest (cons (if row (attach-jsdoc-row stmt row) stmt) out))))
     (uir-block (filter-map (λ (s) s) (reverse stmts)))]))

(define (top-level-elements se)
  (cond
    [(cst-node? se)
     (if (eq? (cst-node-tag se) 'sourceElements)
         (or (find-list se) '())
         (list se))]
    [(pair? se) se]
    [else (list se)]))

(define (lower-source-elements v tk-type tk-value)
  (cond [(pair? v)
         (uir-block (filter-map (λ (e) (lower-source-elements e tk-type tk-value)) v))]
        [(cst-node? v)
         (case (tag-of v)
           [(sourceElements)
            (define lst (find-list v))
            (if lst
                (lower-source-elements lst tk-type tk-value)
                (uir-null))]
            [else
             ;; It's a sourceElement, statement, or other wrapper
             ;; Check for export token: the grammar parses `export` as a bare
             ;; keyword token inside sourceElement, not as an exportStatement node.
             (define has-export
               (for/or ([k (kids-of v)] #:when (and (tok? k tk-type) (eq? (tk-type k) 'Export))) #t))
             (if has-export
                 (lower-export-source-element v tk-type tk-value)
                 (let ([kid (first (cst-kids v))])
                   (if kid
                       (lower-statement kid tk-type tk-value)
                       (uir-null))))])]
        [else (uir-null)]))

(define (lower-source-element node tk-type tk-value)
  (define kid (first (cst-kids node)))
  (if kid
      (lower-statement kid tk-type tk-value)
      (uir-null)))

;; ── Export via sourceElement (grammar parses export as bare keyword) ──

(define (lower-export-source-element node tk-type tk-value)
  ;; sourceElement children: [Export token, statement, maybe eos]
  (define stmt-node (find-kid node 'statement))
  (unless stmt-node (uir-null))
  (define inner-stmt (first (cst-kids stmt-node)))
  (unless inner-stmt (uir-null))

  ;; Check for export default: look for a Default keyword in the expression
  (define (has-default-keyword? n)
    (for/or ([k (kids-of n)])
      (or (and (tok? k tk-type) (eq? (tk-type k) 'Default))
          (and (cst-node? k) (has-default-keyword? k)))))

  ;; Get the expressionStatement (may be inner-stmt itself or a child)
  (define es
    (if (eq? (tag-of inner-stmt) 'expressionStatement)
        inner-stmt
        (find-kid inner-stmt 'expressionStatement)))

  ;; Check for export { x, y } — inner statement is a block
  (cond
    ;; Type-only declarations have no runtime representation: erase them
    ;; instead of emitting `export null;`.
    [(memq (tag-of inner-stmt) '(typeAliasDeclaration interfaceDeclaration))
     #f]
    [(eq? (tag-of inner-stmt) 'block)
     (lower-export-named-block inner-stmt tk-type tk-value)]

    ;; export default expr — first singleExpression wraps [identifierName(=default), singleExpression(expr)]
    [(and es
          (let ([es-seq (find-kid es 'expressionSequence)])
            (and es-seq
                 (let ([se (find-kid es-seq 'singleExpression)])
                   (and se (has-default-keyword? se))))))
     (define es-seq (find-kid es 'expressionSequence))
     (define se (find-kid es-seq 'singleExpression))
     ;; singleExpression children: [identifierName(=default), singleExpression(expr)]
      (define se-kids (filter cst-node? (kids-of se)))
      (if (>= (length se-kids) 2)
          (let ([expr-node (second se-kids)])
            ;; export default function Name() {...} — preserve the name so the
            ;; emitted declaration stays a valid JS functionDeclaration.
            (define af (and (eq? (tag-of expr-node) 'singleExpression)
                            (find-kid expr-node 'anonymousFunction)))
            (define fd (and af (find-kid af 'functionDeclaration)))
            (define fd-ident (and fd (find-kid fd 'identifier)))
            (uir-call (uir-symbol "export")
                      (list (uir-symbol "default")
                            (if fd-ident
                                (lower-fn-decl fd tk-type tk-value)
                                (lower-single-expression expr-node tk-type tk-value)))))
          (uir-null))]

    ;; export const/let/var/function/class declaration
    [else
     (define lowered (lower-statement inner-stmt tk-type tk-value))
     (uir-call (uir-symbol "export") (list (uir-symbol "decl") lowered))]))

(define (lower-export-named-block block-node tk-type tk-value)
  ;; block: [OpenBrace, statementList, CloseBrace]
  ;; statementList: list of statement → expressionStatement → expressionSequence
  ;; Extract identifiers for export { name1, name2 }
  (define stmt-list-node (find-kid block-node 'statementList))
  (unless stmt-list-node (uir-null))
  (define stmt-list (find-list stmt-list-node))
  (unless stmt-list (uir-null))
  (define names '())
  (for ([stmt (in-list stmt-list)]
        #:when (cst-node? stmt))
    (define es (or (eq? (tag-of stmt) 'expressionStatement)
                   (find-kid stmt 'expressionStatement)))
    ;; Handle both: stmt IS expressionStatement, or stmt CONTAINS one
    (define es-node (if (eq? (tag-of stmt) 'expressionStatement)
                        stmt
                        (find-kid stmt 'expressionStatement)))
    (when es-node
      (define es-seq (find-kid es-node 'expressionSequence))
      (when es-seq
        ;; First singleExpression is usually a named export
        (define first-se (find-kid es-seq 'singleExpression))
        (when first-se
          (define name (extract-identifier-name first-se tk-type tk-value))
          (when name (set! names (cons (cons (uir-string name) (uir-symbol name)) names))))
        ;; Check for comma-separated list of more names
        (define tail (find-list es-seq))
        (when tail
          (for ([g (in-list tail)]
                #:when (cst-node? g))
            (define se (find-kid g 'singleExpression))
            (when se
              (define name (extract-identifier-name se tk-type tk-value))
              (when name (set! names (cons (cons (uir-string name) (uir-symbol name)) names)))))))))
  (if (null? names)
      (uir-null)
      (uir-call (uir-symbol "export") (list (uir-record (reverse names))))))

;; Helpers for export lowering



;; ── Statements ───────────────────────────────────────────────────────

(define (lower-statement node tk-type tk-value)
  (case (tag-of node)
    [(statement)
     (define inner (first (cst-kids node)))
     (if inner
         (lower-statement inner tk-type tk-value)
         (uir-null))]
    [(expressionStatement) (lower-expr-stmt node tk-type tk-value)]
    [(variableStatement) (lower-var-stmt node tk-type tk-value)]
    [(functionDeclaration) (lower-fn-decl node tk-type tk-value)]
    [(returnStatement) (lower-return-stmt node tk-type tk-value)]
    [(ifStatement) (lower-if-stmt node tk-type tk-value)]
    [(iterationStatement) (lower-iter-stmt node tk-type tk-value)]
    [(throwStatement) (lower-throw-stmt node tk-type tk-value)]
    [(breakStatement) (uir-call (uir-symbol "break") '())]
    [(continueStatement) (uir-call (uir-symbol "continue") '())]
    [(tryStatement) (lower-try-stmt node tk-type tk-value)]
    [(classDeclaration) (lower-class-decl node tk-type tk-value)]
    [(switchStatement) (lower-switch-stmt node tk-type tk-value)]
    [(importStatement) (lower-import-stmt node tk-type tk-value)]
    [(exportStatement) (lower-export-stmt node tk-type tk-value)]
    [(withStatement) (lower-with-stmt node tk-type tk-value)]
    [(debuggerStatement) (uir-call (uir-symbol "debugger") '())]
    [(yieldStatement) (lower-yield-stmt node tk-type tk-value)]
    [(labelledStatement) (lower-labelled-stmt node tk-type tk-value)]
    [(block) (lower-block node tk-type tk-value)]
    [(emptyStatement_) (uir-null)]
    [(enumDeclaration) (lower-enum-decl node tk-type tk-value)]
    [(namespaceDeclaration) (lower-namespace-decl node tk-type tk-value)]
    [(abstractDeclaration) (lower-abstract-decl node tk-type tk-value)]
    [(interfaceDeclaration) #f]
    [(typeAliasDeclaration) #f]
    [else (uir-null)]))

(define (lower-expr-stmt node tk-type tk-value)
  (define es (find-kid node 'expressionSequence))
  (define se (and es (find-kid es 'singleExpression)))
  ;; ASI: with no trailing semicolon the grammar parses `return <expr>` as an
  ;; expressionStatement whose singleExpression is [identifierName(return),
  ;; singleExpression(expr)] — the keyword becomes a "callee". Recover the
  ;; intended returnStatement here.
  (define (return-callee? k)
    (and (cst-node? k) (eq? (tag-of k) 'identifierName)
         (let* ([rw (find-kid k 'reservedWord)]
                [kw (and rw (find-kid rw 'keyword))]
                [tok (and kw (first (kids-of kw)))])
           (and (tok? tok tk-type) (eq? (tk-type tok) 'Return)))))
  (define node-kids (and se (filter cst-node? (kids-of se))))
  (if (and node-kids (= (length node-kids) 2)
           (return-callee? (first node-kids)))
      (uir-return (lower-single-expression (second node-kids) tk-type tk-value))
      (lower-expression-sequence es tk-type tk-value)))

(define (lower-var-stmt node tk-type tk-value)
  (define vdl (find-kid node 'variableDeclarationList))
  (unless vdl (uir-null))
  (define vm (find-kid node 'varModifier))
  (define kind (if vm (extract-var-kind vm tk-type) "var"))
  ;; Collect all variableDeclaration children: first as direct node,
  ;; then any more from the tail list (each wrapped in a group node).
  (define decls
    (let loop ([ks (kids-of vdl)] [acc '()])
      (cond [(null? ks) (reverse acc)]
            [(and (cst-node? (car ks)) (eq? (tag-of (car ks)) 'variableDeclaration))
             (loop (cdr ks) (cons (car ks) acc))]
            [(pair? (car ks))
             ;; Tail list of (group ...) nodes, each containing Comma + variableDeclaration
             (define tail-decls
               (for/list ([g (car ks)] #:when (cst-node? g))
                 (find-kid g 'variableDeclaration)))
             (loop (cdr ks) (append (reverse acc) tail-decls))]
            [else (loop (cdr ks) acc)])))
  (define uir-decls (map (λ (d) (uir-call (uir-symbol kind) (list (lower-var-decl d tk-type tk-value)))) decls))
  (if (= (length uir-decls) 1)
      (car uir-decls)
      (uir-block uir-decls)))


(define (lower-var-decl node tk-type tk-value)
  ;; TypeScript variableDeclaration:
  ;;   (identifierOrKeyWord | arrayLiteral | objectLiteral) typeAnnotation? singleExpression? ('=' typeParameters? singleExpression)?
  ;; CST: group(identifierOrKeyWord->identifier OR arrayLiteral OR objectLiteral), typeAnnotation?, none?, group(= rhs)?
  (define kids (kids-of node))
  ;; Find identifier group (first group child)
  (define id-group
    (for/or ([k kids] #:when (and (cst-node? k) (eq? (tag-of k) 'group))) k))
  
  ;; Check for array destructuring: const [a, b] = expr
  (define arr-lit (and id-group (find-kid id-group 'arrayLiteral)))
  (define obj-lit (and id-group (find-kid id-group 'objectLiteral)))
  
  ;; Find initializer group (second group child)
  (define init-group
    (let loop ([ks kids] [found-first? #f])
      (cond [(null? ks) #f]
            [(and (cst-node? (car ks)) (eq? (tag-of (car ks)) 'group))
             (if found-first? (car ks) (loop (cdr ks) #t))]
            [else (loop (cdr ks) found-first?)])))
  (define rhs
    (if init-group
        (let ([se (find-kid init-group 'singleExpression)])
          (if se
              (lower-single-expression se tk-type tk-value)
              (uir-null)))
        (uir-null)))

  (cond
    ;; Array destructuring: const [a, b] = expr
    [arr-lit
     (define names (extract-array-binding-names arr-lit tk-type tk-value))
     (if (uir-null? rhs)
         (uir-call (uir-symbol "array-bind") (list (uir-list names) (uir-null)))
         (uir-call (uir-symbol "array-bind") (list (uir-list names) rhs)))]

    ;; Object destructuring: const { x, y } = expr
    [obj-lit
     (define names (extract-object-binding-names obj-lit tk-type tk-value))
     (if (uir-null? rhs)
         (uir-call (uir-symbol "object-bind") (list (uir-list names) (uir-null)))
         (uir-call (uir-symbol "object-bind") (list (uir-list names) rhs)))]

    ;; Simple identifier binding
    [else
     (define var-name
       (if id-group
           (let* ([iok (find-kid id-group 'identifierOrKeyWord)]
                  [ident (and iok (find-kid iok 'identifier))]
                  [tok (and ident (first (kids-of ident)))])
             (if (and tok (tok? tok tk-type))
                 (uir-symbol (tk-value tok))
                 (uir-symbol "?")))
           (uir-symbol "?")))
     (if (uir-null? rhs)
         (uir-var var-name)
         (uir-set! var-name rhs))]))

;; Extract identifier names from an arrayLiteral CST node.
;; Returns list of uir-symbols.
(define (extract-array-binding-names node tk-type tk-value)
  ;; node is arrayLiteral; inside is group(OpenBracket, elementList, CloseBracket)
  (define inner-group (find-kid node 'group))
  (define el (and inner-group (find-kid inner-group 'elementList)))
  (unless el '())
  (define names '())
  ;; elementList children: first arrayElement as direct node, rest in a list of (group Comma arrayElement)
  (let loop ([ks (kids-of el)])
    (cond [(null? ks) (void)]
          [(and (cst-node? (car ks)) (eq? (tag-of (car ks)) 'arrayElement))
           (define name (extract-array-element-name (car ks) tk-type tk-value))
           (when name (set! names (cons name names)))
           (loop (cdr ks))]
          [(pair? (car ks))
           (for ([g (car ks)] #:when (cst-node? g))
             (define ae (find-kid g 'arrayElement))
             (when ae
               (define name (extract-array-element-name ae tk-type tk-value))
               (when name (set! names (cons name names)))))
           (loop (cdr ks))]
          [else (loop (cdr ks))]))
  (reverse names))

;; Extract a single identifier from an arrayElement CST node.
(define (extract-array-element-name node tk-type tk-value)
  ;; arrayElement -> (group singleExpression)?
  (define grp (find-kid node 'group))
  (and grp
       (let* ([se (find-kid grp 'singleExpression)]
              [ident (and se (or (find-kid se 'identifier)
                                 (let ([in (find-kid se 'identifierName)])
                                   (and in (find-kid in 'identifier)))))])
         (and ident
              (let ([tok (first (kids-of ident))])
                (and (tok? tok tk-type) (eq? (tk-type tok) 'Identifier)
                     (uir-symbol (tk-value tok))))))))

;; Extract binding names from an objectLiteral CST node for destructuring.
(define (extract-object-binding-names node tk-type tk-value)
  (define inner-group (find-kid node 'group))
  (unless inner-group '())
  (define names '())
  (let loop ([ks (kids-of inner-group)])
    (cond [(null? ks) (void)]
          [(and (cst-node? (car ks)) (eq? (tag-of (car ks)) 'propertyAssignment))
           (define name (extract-prop-binding-name (car ks) tk-type tk-value))
           (when name (set! names (cons name names)))
           (loop (cdr ks))]
          [(pair? (car ks))
           (for ([g (car ks)] #:when (cst-node? g))
             (define pa (find-kid g 'propertyAssignment))
             (when pa
               (define name (extract-prop-binding-name pa tk-type tk-value))
               (when name (set! names (cons name names)))))
           (loop (cdr ks))]
          [else (loop (cdr ks))]))
  (reverse names))

(define (extract-prop-binding-name node tk-type tk-value)
  ;; Forms:
  ;;   1. Shorthand: propertyAssignment(identifierOrKeyWord(identifier NAME))
  ;;   2. Renamed:   propertyAssignment(propertyName(...), group(Colon:), singleExpression(..., identifier NEWNAME))
  ;;   3. Default:   propertyAssignment(propertyName(identifierName(identifier NAME)), group(Assign=), singleExpression DEFAULT)
  ;; Priority: check singleExpression for renamed binding, then propertyName/identifierOrKeyWord for shorthand/default
  (define (find-ident-rec n)
    (or (find-kid n 'identifier)
        (let ([in (find-kid n 'identifierName)])
          (and in (or (find-ident-rec in)
                      (find-kid in 'identifier))))
        (let ([se (find-kid n 'singleExpression)])
          (and se (find-ident-rec se)))))
  (define (make-sym n)
    (and n
         (let ([tok (first (kids-of n))])
           (and (tok? tok tk-type) (eq? (tk-type tok) 'Identifier)
                (uir-symbol (tk-value tok))))))
  (or
   ;; Renamed: binding name from right-hand singleExpression
   (let ([se (find-kid node 'singleExpression)])
     (and se (make-sym (find-ident-rec se))))
   ;; Shorthand/default: binding name from propertyName or identifierOrKeyWord
   (let* ([prop (or (find-kid node 'propertyName)
                    (find-kid node 'identifierOrKeyWord))]
          [ident (and prop (find-ident-rec prop))])
     (make-sym ident))))

(define (lower-fn-decl node tk-type tk-value)
  (define kids (kids-of node))
  ;; Detect async/generator: check all tokens
  (define is-async (for/or ([k kids]) (and (tok? k tk-type) (eq? (tk-type k) 'Async))))
  (define is-generator (for/or ([k kids]) (and (tok? k tk-type) (eq? (tk-type k) 'Multiply))))
  (define ident-node (find-kid node 'identifier))
  (define name
    (if ident-node
        (uir-symbol (tk-value (first (kids-of ident-node))))
        (uir-symbol "?")))
  ;; TypeScript: function body is inside callSignature -> group -> group -> functionBody
  ;; or the declaration ends with SemiColon (bodyless)
  (define body
    (let* ([outer-group (find-kid node 'group)]
           [inner-group (and outer-group (find-kid outer-group 'group))]
           [fb-node (and inner-group (find-kid inner-group 'functionBody))])
      (if fb-node
          (lower-fn-body fb-node tk-type tk-value)
          (uir-block '()))))
  ;; TypeScript: parameters are in callSignature -> parameterList
  (define cs (find-kid node 'callSignature))
  (define pl (and cs (find-kid cs 'parameterList)))
  (define-values (params param-extractions)
    (if pl
        (lower-formal-params pl tk-type tk-value)
        (values '() '())))
  ;; If destructured params were found, prepend their extraction bindings to body
  (define eff-body
    (if (null? param-extractions)
        body
        (uir-block (append param-extractions (uir-block-stmts body)))))
  (define fn-uir (uir-fn #f params eff-body #f #f))
  (cond [is-async (uir-set! name (uir-call (uir-symbol "async-fn") (list fn-uir)))]
        [is-generator (uir-set! name (uir-call (uir-symbol "gen-fn") (list fn-uir)))]
        [else (uir-set! name fn-uir)]))

(define (lower-fn-body node tk-type tk-value)
  (define se (find-node-or-list node))
  (if se (lower-source-elements se tk-type tk-value) (uir-block '())))

(define (lower-formal-params fpl tk-type tk-value)
  (define params '())
  (define extractions '())
  (define counter 0)
  (define (extract-name k)
    ;; Try various paths to find the identifier name
    ;; k could be parameter, requiredParameter, optionalParameter, restParameter, formalParameterArg
    (define (find-ident n)
      (and n
           (or (find-kid n 'identifierOrPattern)
               (find-kid n 'identifierName)
               (find-kid n 'identifier)
               (and (member (tag-of n) '(parameter formalParameterArg restParameter))
                    (or (find-ident (find-kid n 'requiredParameter))
                        (find-ident (find-kid n 'optionalParameter))
                        (find-ident (find-kid n 'restParameter)))))))
    (define io (find-ident k))
    (if (and io (not (find-kid io 'bindingPattern)))
        ;; Simple param (not destructured) - extract token name directly
        (let* ([ident (or (find-kid io 'identifier)
                          (let ([in (find-kid io 'identifierName)])
                            (and in (find-kid in 'identifier)))
                          (let ([iok (find-kid io 'identifierOrKeyWord)])
                            (and iok (find-kid iok 'identifier)))
                          io)]
               [tok (and ident (first (kids-of ident)))])
          (when (and tok (tok? tok tk-type))
            (set! params (cons (uir-symbol (tk-value tok)) params))))
        ;; identifierOrPattern containing bindingPattern (destructuring)
        (let ([bp (and io (find-kid io 'bindingPattern))])
          (when bp
            (define synthetic-name (uir-symbol (format "_p~a" counter)))
            (set! counter (add1 counter))
            (set! params (cons synthetic-name params))
            ;; Collect bindings from the pattern
            (extract-destructured-bindings bp synthetic-name tk-type tk-value)))))

  ;; Helper: extract bindings from bindingPattern -> objectLiteral or arrayLiteral
  (define (extract-destructured-bindings bp synthetic tk-type tk-value)
    ;; bindingPattern often wraps in a group: bindingPattern → group → objectLiteral
    (define inner (let ([g (find-kid bp 'group)])
                    (and g g)))
    (define ol (and inner
                   (or (find-kid inner 'objectLiteral)
                       (find-kid inner 'arrayLiteral))))
    ;; Also try direct (no group wrapper)
    (define ol2 (or ol (find-kid bp 'objectLiteral) (find-kid bp 'arrayLiteral)))
    (when ol2
      (define grp (find-kid ol2 'group))
      (when grp
        (let loop ([ks (kids-of grp)])
          (cond [(null? ks) (void)]
                [(and (cst-node? (car ks)) (eq? (tag-of (car ks)) 'propertyAssignment))
                 (extract-destructured-prop (car ks) synthetic tk-type tk-value)
                 (loop (cdr ks))]
                [(and (cst-node? (car ks)) (eq? (tag-of (car ks)) 'singleExpression))
                 ;; array destructuring element
                 (define ident (extract-ident-from (car ks) tk-type tk-value))
                 (when ident
                   (set! extractions
                         (cons (uir-set! (uir-symbol ident)
                                         (uir-get synthetic (uir-number (number->string (length extractions)))))
                               extractions)))
                 (loop (cdr ks))]
                [(pair? (car ks))
                 (let ([flat-items '()])
                   (for ([g (car ks)] #:when (cst-node? g))
                     (if (eq? (tag-of g) 'group)
                         (set! flat-items (append flat-items (kids-of g)))
                         (set! flat-items (append flat-items (list g)))))
                   (loop (append flat-items (cdr ks))))]
                [(and (cst-node? (car ks)) (eq? (tag-of (car ks)) 'group))
                 ;; Unwrap nested group (comma-separated tail)
                 (loop (append (kids-of (car ks)) (cdr ks)))]
                [(and (cst-node? (car ks)) (eq? (tag-of (car ks)) 'elementList))
                 (loop (append (kids-of (car ks)) (cdr ks)))]
                [(and (cst-node? (car ks)) (eq? (tag-of (car ks)) 'arrayElement))
                 (define ae-name (extract-array-element-name (car ks) tk-type tk-value))
                 (when ae-name
                   (set! extractions
                         (cons (uir-set! ae-name
                                         (uir-get synthetic (uir-number (number->string (length extractions)))))
                               extractions)))
                 (loop (cdr ks))]
                [else (loop (cdr ks))])))))

  (define (extract-destructured-prop pa synthetic tk-type tk-value)
    ;; propertyAssignment in binding pattern: key [Colon value]?
    ;; Simple: identifierOrKeyWord(name) -> (uir-set! name (uir-get _p0 'name))
    ;; Renamed: identifierOrKeyWord(localName) Colon singleExpression(remoteName)
    (define ks (kids-of pa))
    ;; Find key (first non-token cst-node)
    (define key-node (for/or ([k ks] #:when (cst-node? k)) k))
    (when key-node
      (define local-name (extract-ident-from key-node tk-type tk-value))
      (when local-name
        ;; Check if there's a colon (renamed binding)
        (define colon-idx (for/or ([i (in-naturals)] [k ks])
                            (and (tok? k tk-type) (eq? (tk-type k) 'Colon) i)))
        (define source-prop
          (if colon-idx
              ;; Find the singleExpression after the colon
              (let loop ([i (add1 colon-idx)])
                (if (>= i (length ks))
                    local-name
                    (let ([k (list-ref ks i)])
                      (if (and (cst-node? k) (eq? (tag-of k) 'singleExpression))
                          (extract-ident-from k tk-type tk-value)
                          (loop (add1 i))))))
              local-name))
        (set! extractions
              (cons (uir-set! (uir-symbol local-name)
                              (uir-get synthetic (uir-string source-prop)))
                    extractions)))))

  (define (extract-ident-from node tk-type tk-value)
    ;; Extract an identifier string from a node (identifierOrKeyWord, identifierName, or singleExpression)
    (define ident-node
      (or (find-kid node 'identifier)
          (let ([in (find-kid node 'identifierName)])
            (and in (find-kid in 'identifier)))
          ;; If node itself is identifierOrKeyWord, look inside it
          (and (eq? (tag-of node) 'identifierOrKeyWord)
               (or (find-kid node 'identifier)
                   (let ([in (find-kid node 'identifierName)])
                     (and in (find-kid in 'identifier)))))
          (let ([iok (find-kid node 'identifierOrKeyWord)])
            (and iok (or (find-kid iok 'identifier)
                         (let ([in (find-kid iok 'identifierName)])
                           (and in (find-kid in 'identifier))))))))
    (and ident-node
         (let ([tok (first (kids-of ident-node))])
           (and (tok? tok tk-type) (tk-value tok)))))

  (let loop ([ks (kids-of fpl)])
    (cond [(null? ks) (void)]
          [(and (cst-node? (car ks))
                (member (tag-of (car ks)) '(parameter formalParameterArg restParameter)))
           (extract-name (car ks)) (loop (cdr ks))]
          [(pair? (car ks))
           (for ([g (car ks)] #:when (cst-node? g))
             (when (member (tag-of g) '(parameter formalParameterArg restParameter))
               (extract-name g)))
           (loop (cdr ks))]
          [(and (cst-node? (car ks)) (eq? (tag-of (car ks)) 'group))
           ;; Unwrap group (comma-separated tail: (Comma param)+)
           (loop (append (kids-of (car ks)) (cdr ks)))]
          [else (loop (cdr ks))]))
  (values (reverse params) (reverse extractions)))

(define (lower-return-stmt node tk-type tk-value)
  (define grp (find-kid node 'group))
  (define es (and grp (find-kid grp 'expressionSequence)))
  (uir-return (if es (lower-expression-sequence es tk-type tk-value) (uir-null))))

(define (lower-block node tk-type tk-value)
  (define stmt-list (find-kid node 'statementList))
  (if stmt-list
      (let ([lst (find-list stmt-list)])
        (if lst
            (lower-source-elements lst tk-type tk-value)
            (uir-null)))
      (uir-null)))

(define (lower-if-stmt node tk-type tk-value)
  (define kids (kids-of node))
  (define test (lower-expression-sequence (third kids) tk-type tk-value))
  (define consequent (lower-statement (fifth kids) tk-type tk-value))
  (define group (find-kid node 'group))
  (define alternate
    (if group
        (lower-statement (second (kids-of group)) tk-type tk-value)
        (uir-null)))
  (uir-if test consequent alternate))

(define (lower-throw-stmt node tk-type tk-value)
  (define es (find-kid node 'expressionSequence))
  (uir-call (uir-symbol "throw") (list (if es (lower-expression-sequence es tk-type tk-value) (uir-null)))))

(define (lower-try-stmt node tk-type tk-value)
  (define kids (kids-of node))
  (define try-body (lower-block (list-ref kids 1) tk-type tk-value))
  (define group (find-kid node 'group))
  (define catch-var (uir-null))
  (define catch-body (uir-null))
  (define finally-body (uir-null))
  (when group
    (for ([k (kids-of group)])
      (when (cst-node? k)
        (case (tag-of k)
           [(catchProduction)
            (define catch-group (find-kid k 'group))
            (when catch-group
              (define ident
                (let ([assignable (find-kid catch-group 'assignable)])
                  (or (and assignable (find-kid assignable 'identifier))
                      (find-kid catch-group 'identifier))))
              (when ident
                (set! catch-var (uir-var (uir-symbol (tk-value (first (kids-of ident))))))))
            (set! catch-body (lower-block (find-kid k 'block) tk-type tk-value))]
          [(finallyProduction)
           (set! finally-body (lower-block (find-kid k 'block) tk-type tk-value))]))))
  (uir-call (uir-symbol "try") (list try-body catch-var catch-body finally-body)))

(define (lower-class-decl node tk-type tk-value)
  (define ident-node (find-kid node 'identifier))
  (define class-name
    (if ident-node
        (uir-symbol (tk-value (first (kids-of ident-node))))
        (uir-symbol "?")))
  (define tail (find-kid node 'classTail))
  (define heritage (find-kid node 'classHeritage))
  (define super (uir-null))
  (when heritage
    (define extends-clause (find-kid heritage 'classExtendsClause))
    (when extends-clause
      (define tr (find-kid extends-clause 'typeReference))
      (when tr
        (define tn (find-kid tr 'typeName))
        (when tn
          (define ident (find-kid tn 'identifier))
          (when ident
            (set! super (uir-symbol (tk-value (first (kids-of ident))))))))))
  (define methods '())
  (when tail
    (define tail-kids (kids-of tail))
    ;; tail-kids: [OpenBrace, list-of-classElement, CloseBrace]
    (when (>= (length tail-kids) 2)
      (define element-list (second tail-kids))
      (when (pair? element-list)
        (set! methods
              (for/list ([elem (in-list element-list)]
                         #:when (cst-node? elem))
                (lower-class-element elem tk-type tk-value))))))
  (uir-class class-name (if (uir-null? super) '() (list super)) '() methods))

(define (extract-param-list fpl tk-type tk-value)
  ;; Extract parameter names from formalParameterList or parameterList
  ;; Handles both formalParameterArg (TS) and parameter/requiredParameter (JS) styles
  (define params '())
  (define (find-ident-deep k)
    ;; Recursively find first identifier token
    (let loop ([n k])
      (cond [(and (cst-node? n) (eq? (tag-of n) 'identifier))
             (tk-value (first (kids-of n)))]
            [(cst-node? n)
             (for/or ([c (kids-of n)] #:when (cst-node? c))
               (loop c))]
            [else #f])))
  (define (extract-name k)
    (define name (find-ident-deep k))
    (when name
      (set! params (cons (uir-symbol name) params))))
  (for ([k (kids-of fpl)])
    (cond [(and (cst-node? k) (or (eq? (tag-of k) 'formalParameterArg)
                                  (eq? (tag-of k) 'parameter)))
           (define inner (or (find-kid k 'requiredParameter)
                            (find-kid k 'assignable)))
           (if inner (extract-name inner) (extract-name k))]
          [(and (cst-node? k) (eq? (tag-of k) 'requiredParameter))
           (extract-name k)]
          [(and (cst-node? k) (eq? (tag-of k) 'group))
           ;; Comma-separated tail: group wraps (Comma param)+
           (for ([g (kids-of k)] #:when (cst-node? g))
             (cond [(or (eq? (tag-of g) 'formalParameterArg)
                        (eq? (tag-of g) 'parameter))
                    (define inner (or (find-kid g 'requiredParameter)
                                     (find-kid g 'assignable)))
                    (if inner (extract-name inner) (extract-name g))]
                   [(eq? (tag-of g) 'requiredParameter)
                    (extract-name g)]))]
          [(pair? k)
           (for ([g (in-list k)] #:when (cst-node? g))
             (cond [(or (eq? (tag-of g) 'formalParameterArg)
                        (eq? (tag-of g) 'parameter))
                    (define inner (or (find-kid g 'requiredParameter)
                                     (find-kid g 'assignable)))
                    (if inner (extract-name inner) (extract-name g))]
                   [(eq? (tag-of g) 'requiredParameter)
                    (extract-name g)]))]))
  (reverse params))

(define (lower-class-element node tk-type tk-value)
  ;; classElement → propertyMemberDeclaration
  (define pmd (find-kid node 'propertyMemberDeclaration))
  (unless pmd (uir-null))
  ;; Method name
  (define pname-node (find-kid pmd 'propertyName))
  (define method-name
    (if pname-node
        (let ([in (find-kid pname-node 'identifierName)])
          (if in
              (let ([ident (find-kid in 'identifier)])
                (if ident
                    (uir-symbol (tk-value (first (kids-of ident))))
                    (uir-symbol "?")))
              (uir-symbol "?")))
        (uir-symbol "?")))
  ;; Parameters from callSignature
  (define cs (find-kid pmd 'callSignature))
  (define params
    (if cs
        (let ([fpl (or (find-kid cs 'formalParameterList)
                       (find-kid cs 'parameterList))])
          (if fpl
              (extract-param-list fpl tk-type tk-value)
              '()))
        '()))
  ;; Body from group → group → functionBody → sourceElements
  (define outer-group (find-kid pmd 'group))
  (define body (uir-null))
  (when outer-group
    (define inner-group (find-kid outer-group 'group))
    (when inner-group
      (define fb (find-kid inner-group 'functionBody))
      (when fb
        (define se (find-kid fb 'sourceElements))
        (when se
          (set! body (lower-source-elements se tk-type tk-value))))))
  ;; Visibility: check propertyMemberBase for static/private/public tokens  
  (define pmb (find-kid pmd 'propertyMemberBase))
  (define visibility 'public)
  (uir-method method-name params body visibility))

(define (lower-switch-stmt node tk-type tk-value)
  (define kids (kids-of node))
  (define test (lower-expression-sequence (list-ref kids 2) tk-type tk-value))
  (define case-block (list-ref kids 4))
  (define case-clauses-node (find-kid case-block 'caseClauses))
  (define default-group (find-kid case-block 'group))
  (define cases (uir-null))
  (define default-case (uir-null))
  (when case-clauses-node
    (define case-list (find-list case-clauses-node))
    (when case-list
      (set! cases (uir-block
                    (for/list ([cc case-list] #:when (cst-node? cc))
                      (lower-case-clause cc tk-type tk-value))))))
  (when default-group
    (define dc (find-kid default-group 'defaultClause))
    (when dc
      (define stmt-list (find-kid dc 'statementList))
      (when stmt-list
        (define lst (find-list stmt-list))
        (when lst
          (set! default-case (lower-source-elements lst tk-type tk-value))))))
  (uir-call (uir-symbol "switch") (list test cases default-case)))

(define (lower-case-clause node tk-type tk-value)
  (define kids (kids-of node))
  (define test (lower-expression-sequence (list-ref kids 1) tk-type tk-value))
  (define stmt-list-node (list-ref kids 3))
  (define lst (and (cst-node? stmt-list-node) (find-list stmt-list-node)))
  (define body (if lst (lower-source-elements lst tk-type tk-value) (uir-null)))
  (uir-call (uir-symbol "case") (list test body)))

(define (lower-for-init eseque tk-type tk-value)
  "lower for-init expressionSequence, handling declaration keyword patterns."
  (cond [(not (and eseque (cst-node? eseque))) (uir-null)]
        [(not (eq? (tag-of eseque) 'expressionSequence)) (uir-null)]
        [else
         (define se-kids (cst-kids eseque))
         (cond [(null? se-kids) (uir-null)]
               [else
                (define first-se (first se-kids))
                (define fsk (cst-kids first-se))
                (cond
                  ;; Declaration keyword + init: singleExpression wraps (identifierName keyword, singleExpression assignment)
                  [(and (= (length fsk) 2)
                        (eq? (tag-of (first fsk)) 'identifierName)
                        (let ([kw-val (get-keyword-value (first fsk) tk-value)])
                          (and kw-val (member kw-val '("let" "var" "const")))))
                   (let ([kw-val (get-keyword-value (first fsk) tk-value)]
                         [assn (lower-single-expression (second fsk) tk-type tk-value)])
                     (uir-call (uir-symbol kw-val) (list assn)))]
                  ;; Normal single expression init
                  [else (lower-single-expression first-se tk-type tk-value)])])]))

(define (get-keyword-value node tk-value)
  ;; Walk cst-kids down to a 'keyword node, return its token value
  (let loop ([n node])
    (cond [(not (cst-node? n)) #f]
          [(eq? (cst-node-tag n) 'keyword)
           (for/or ([k (cst-node-children n)] #:when (tk-value k))
             (tk-value k))]
          [else (for/or ([k (cst-kids n)]) (loop k))])))

(define (lower-iter-stmt node tk-type tk-value)
  (define kids (kids-of node))
  (define first-tok (and (tok? (first kids) tk-type) (tk-type (first kids))))
  (case first-tok
    [(While)
     ;; kids: [While, OpenParen, expressionSequence, CloseParen, statement]
     (define test (lower-expression-sequence (list-ref kids 2) tk-type tk-value))
     (define body (lower-statement (list-ref kids 4) tk-type tk-value))
     (uir-call (uir-symbol "while") (list test body))]
    [(Do)
     ;; kids: [Do, statement, While, OpenParen, expressionSequence, CloseParen, eos]
     (define body (lower-statement (list-ref kids 1) tk-type tk-value))
     (define test (lower-expression-sequence (list-ref kids 4) tk-type tk-value))
     (uir-call (uir-symbol "dowhile") (list body test))]
    [(For)
     (define has-in? (for/or ([k kids]) (and (tok? k tk-type) (eq? (tk-type k) 'In))))
     (define has-of? (for/or ([k kids]) (and (tok? k tk-type) (eq? (tk-type k) 'Of))))
     (cond [has-in?
            ;; for (left in right) body
            ;; left is a group node wrapping singleVariableDeclaration or expressionSequence
            (define k2 (list-ref kids 2))
            (define left-expr
              (if (cst-node? k2)
                  (case (tag-of k2)
                    [(group) (lower-for-var k2 tk-type tk-value)]
                    [(expressionSequence) (lower-expression-sequence k2 tk-type tk-value)]
                    [else (uir-null)])
                  (uir-null)))
            (define right (lower-expression-sequence (list-ref kids 4) tk-type tk-value))
            (define body (lower-statement (last kids) tk-type tk-value))
            (uir-call (uir-symbol "forin") (list left-expr right body))]
           [has-of?
            ;; for (left of right) body
            (define of-pos
              (for/or ([i (in-naturals)] [k kids] #:when (and (tok? k tk-type) (eq? (tk-type k) 'Of))) i))
            (define lk (list-ref kids (or (and of-pos (sub1 of-pos)) 2)))
            (define left-expr
              (if (cst-node? lk)
                  (case (tag-of lk)
                    [(group) (lower-for-var lk tk-type tk-value)]
                    [(expressionSequence) (lower-expression-sequence lk tk-type tk-value)]
                    [else (uir-null)])
                  (uir-null)))
            (define right (lower-expression-sequence (list-ref kids (or (and of-pos (+ of-pos 1)) 5)) tk-type tk-value))
            (define body (lower-statement (last kids) tk-type tk-value))
            (uir-call (uir-symbol "forof") (list left-expr right body))]
            [else
             ;; Regular for: [For, OpenParen, init?, SemiColon, test?, SemiColon, update?, CloseParen, statement]
             (define init (lower-for-init (list-ref kids 2) tk-type tk-value))
            (define test (lower-expression-sequence (list-ref kids 4) tk-type tk-value))
            (define update (lower-expression-sequence (list-ref kids 6) tk-type tk-value))
            (define body (lower-statement (list-ref kids 8) tk-type tk-value))
             (uir-call (uir-symbol "for") (list init test update body))])]
    [else (uir-null)]))

(define (lower-for-var group-node tk-type tk-value)
  ;; Extract variable name from a for-in/for-of group containing singleVariableDeclaration
  (define svd (find-kid group-node 'singleVariableDeclaration))
  (if svd
      (let ([vd (find-kid svd 'variableDeclaration)])
        (if vd
            (let ([assignable (find-kid vd 'assignable)])
              (define ident (and assignable (find-kid assignable 'identifier)))
              (if ident
                  (uir-var (uir-symbol (tk-value (first (kids-of ident)))))
                  (uir-symbol "?")))
            (uir-symbol "?")))
      (uir-symbol "?")))


(define (lower-arrow-fn node tk-type tk-value)
  ;; Distinguish: arrow has arrowFunctionParameters, function expr has Function_ token.
  ;; TS: anonymousFunction may wrap arrowFunctionDeclaration or functionDeclaration; check inside.
  (define afd (find-kid node (quote arrowFunctionDeclaration)))
  (define arrow-params-node
    (or (find-kid node (quote arrowFunctionParameters))
        (and afd (find-kid afd (quote arrowFunctionParameters)))))
  (if arrow-params-node
      (lower-arrow-fn-impl (or afd node) tk-type tk-value)
      ;; Unwrap functionDeclaration from anonymousFunction for named function expressions
      (let ([fd (find-kid node (quote functionDeclaration))])
        (lower-function-expr (or fd node) tk-type tk-value))))

(define (lower-arrow-fn-impl node tk-type tk-value)
  (define params-node (find-kid node (quote arrowFunctionParameters)))
  (define body-node (find-kid node (quote arrowFunctionBody)))
  (define params (quote ()))
  (define extractions (quote ()))
  (define counter 0)
  (define (extract-param k)
    (define assignable (find-kid k (quote assignable)))
    (when assignable
      (let ([ident (find-kid assignable (quote identifier))])
        (if ident
            (set! params (cons (uir-symbol (tk-value (first (kids-of ident)))) params))
            (let ([ol (or (find-kid assignable (quote objectLiteral))
                          (find-kid assignable (quote arrayLiteral)))])
              (when ol
                (define synthetic-name (uir-symbol (format "_p~a" counter)))
                (set! counter (add1 counter))
                (set! params (cons synthetic-name params))
                (extract-destructured-bindings ol synthetic-name)))))))
  (define (extract-rest-param k)
    (define se (find-kid k (quote singleExpression)))
    (when se
      (let ([ident (find-kid se (quote identifier))])
        (if ident
            (set! params (cons (uir-spread (uir-symbol (tk-value (first (kids-of ident)))))
                              params))
            (set! params (cons (uir-spread (lower-single-expression se tk-type tk-value))
                              params))))))
  ;; Helpers for destructured param extraction
  (define (extract-destructured-bindings ol synthetic-name)
    (define grp (find-kid ol (quote group)))
    (when grp
      (let loop ([ks (kids-of grp)])
        (cond [(null? ks) (void)]
              [(and (cst-node? (car ks))
                    (eq? (tag-of (car ks)) (quote propertyAssignment)))
               (extract-destructured-prop (car ks) synthetic-name)
               (loop (cdr ks))]
              [(and (cst-node? (car ks))
                    (eq? (tag-of (car ks)) (quote singleExpression)))
               (define ename (extract-ident-from-expr (car ks)))
               (when ename
                 (set! extractions
                       (cons (uir-set! (uir-symbol ename)
                                       (uir-get synthetic-name (uir-number (number->string (length extractions)))))
                             extractions)))
               (loop (cdr ks))]
              [(pair? (car ks))
               (let ([flat-items '()])
                 (for ([g (car ks)] #:when (cst-node? g))
                   (if (eq? (tag-of g) (quote group))
                       (set! flat-items (append flat-items (kids-of g)))
                       (set! flat-items (append flat-items (list g)))))
                 (loop (append flat-items (cdr ks))))]
              [(and (cst-node? (car ks))
                    (eq? (tag-of (car ks)) (quote group)))
               (loop (append (kids-of (car ks)) (cdr ks)))]
              [(and (cst-node? (car ks))
                    (eq? (tag-of (car ks)) (quote elementList)))
               (loop (append (kids-of (car ks)) (cdr ks)))]
              [(and (cst-node? (car ks))
                    (eq? (tag-of (car ks)) (quote arrayElement)))
               (define ae-name (extract-array-element-name (car ks) tk-type tk-value))
               (when ae-name
                 (set! extractions
                       (cons (uir-set! ae-name
                                       (uir-get synthetic-name (uir-number (number->string (length extractions)))))
                             extractions)))
               (loop (cdr ks))]
              [else (loop (cdr ks))]))))
  (define (extract-destructured-prop pa synthetic-name)
    (define ks (kids-of pa))
    (define key-node (for/or ([k ks] #:when (cst-node? k)) k))
    (when key-node
      (define source-prop (extract-ident-from-expr key-node))
      (when source-prop
        ;; Check for colon: either direct token or nested in a group
        (define colon-idx
          (for/or ([i (in-naturals)] [k ks])
            (or (and (tok? k tk-type) (eq? (tk-type k) 'Colon) i)
                (and (cst-node? k)
                     (eq? (tag-of k) 'group)
                     (for/or ([g (kids-of k)])
                       (and (tok? g tk-type) (eq? (tk-type g) 'Colon) i))))))
        (define local-name
          (if colon-idx
              (let loop2 ([i (add1 colon-idx)])
                (if (>= i (length ks))
                    source-prop
                    (let ([k (list-ref ks i)])
                      (if (and (cst-node? k) (eq? (tag-of k) 'singleExpression))
                          (extract-ident-from-expr k)
                          (loop2 (add1 i))))))
              source-prop))
        (set! extractions
              (cons (uir-set! (uir-symbol local-name)
                              (uir-get synthetic-name (uir-string source-prop)))
                    extractions)))))
  (define (extract-ident-from-expr node)
    (define ident-node
      (or (find-kid node 'identifier)
          (let ([in (find-kid node 'identifierName)])
            (and in (find-kid in 'identifier)))
          (and (eq? (tag-of node) 'identifierOrKeyWord)
               (or (find-kid node 'identifier)
                   (let ([in (find-kid node 'identifierName)])
                     (and in (find-kid in 'identifier)))))
          (let ([iok (find-kid node 'identifierOrKeyWord)])
            (and iok (or (find-kid iok 'identifier)
                         (let ([in (find-kid iok 'identifierName)])
                           (and in (find-kid in 'identifier))))))))
    (and ident-node
         (let ([tok (first (kids-of ident-node))])
           (and (tok? tok tk-type) (tk-value tok)))))
  (when params-node
    (define fpl (find-kid params-node (quote formalParameterList)))
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
                  (cond [(eq? (tag-of g) (quote formalParameterArg))
                         (extract-param g)]
                        [(eq? (tag-of g) (quote lastFormalParameterArg))
                         (extract-rest-param g)]
                        [(eq? (tag-of g) (quote group))
                         ;; TS wraps remaining params in group(Comma, formalParameterArg)
                         (for ([gg (kids-of g)] #:when (cst-node? gg))
                           (cond [(eq? (tag-of gg) (quote formalParameterArg))
                                  (extract-param gg)]
                                 [(eq? (tag-of gg) (quote lastFormalParameterArg))
                                  (extract-rest-param gg)]))]))
                (loop (cdr ks))]
               [else (loop (cdr ks))]))))
   (define body
     (if body-node
         (let* ([se (first (cst-kids body-node))])
           (if (cst-node? se)
               (case (tag-of se)
                 [(functionBody)
                  (let ([lst (find-node-or-list se)])
                    (if lst
                        (lower-source-elements lst tk-type tk-value)
                        (uir-block (quote ()))))]
                 [else (lower-single-expression se tk-type tk-value)])
               (lower-single-expression se tk-type tk-value)))
        (uir-null)))
   (define eff-body
     (if (null? extractions)
         body
         (let ([body-stmts (if (eq? (uir-tag body) 'block)
                              (uir-block-stmts body)
                              (list body))])
           (uir-block (append extractions body-stmts)))))
   (uir-call (uir-symbol "=>") (list (uir-list (reverse params)) eff-body)))

(define (lower-function-expr node tk-type tk-value)
  ;; Regular function expression: anonymousFunction with Function_ token,
  ;; or functionDeclaration (when unwrapped from anonymousFunction).
  (define params (quote ()))
  (define extractions (quote ()))
  (define counter 0)
  (define body (uir-null))
  (define kids (kids-of node))
  ;; kids: [Function_, OpenParen, (params or CloseParen), ...]
  (define (extract-param k)
    (define assignable (find-kid k (quote assignable)))
    (when assignable
      (let ([ident (find-kid assignable (quote identifier))])
        (if ident
            (set! params (cons (uir-symbol (tk-value (first (kids-of ident)))) params))
            (let ([ol (or (find-kid assignable (quote objectLiteral))
                          (find-kid assignable (quote arrayLiteral)))])
              (when ol
                (define synthetic-name (uir-symbol (format "_p~a" counter)))
                (set! counter (add1 counter))
                (set! params (cons synthetic-name params))
                (extract-destructured-bindings ol synthetic-name)))))))
  (define (extract-rest-param k)
    (define se (find-kid k (quote singleExpression)))
    (when se
      (let ([ident (find-kid se (quote identifier))])
        (if ident
            (set! params (cons (uir-spread (uir-symbol (tk-value (first (kids-of ident)))))
                              params))
            (set! params (cons (uir-spread (lower-single-expression se tk-type tk-value))
                              params))))))
  ;; Helpers for destructured param extraction
  (define (extract-destructured-bindings ol synthetic-name)
    (define grp (find-kid ol (quote group)))
    (when grp
      (let loop ([ks (kids-of grp)])
        (cond [(null? ks) (void)]
              [(and (cst-node? (car ks))
                    (eq? (tag-of (car ks)) (quote propertyAssignment)))
               (extract-destructured-prop (car ks) synthetic-name)
               (loop (cdr ks))]
              [(and (cst-node? (car ks))
                    (eq? (tag-of (car ks)) (quote singleExpression)))
               (define ename (extract-ident-from-expr (car ks)))
               (when ename
                 (set! extractions
                       (cons (uir-set! (uir-symbol ename)
                                       (uir-get synthetic-name (uir-number (number->string (length extractions)))))
                             extractions)))
               (loop (cdr ks))]
              [(pair? (car ks))
               (let ([flat-items '()])
                 (for ([g (car ks)] #:when (cst-node? g))
                   (if (eq? (tag-of g) (quote group))
                       (set! flat-items (append flat-items (kids-of g)))
                       (set! flat-items (append flat-items (list g)))))
                 (loop (append flat-items (cdr ks))))]
              [(and (cst-node? (car ks))
                    (eq? (tag-of (car ks)) (quote group)))
               (loop (append (kids-of (car ks)) (cdr ks)))]
              [(and (cst-node? (car ks))
                    (eq? (tag-of (car ks)) (quote elementList)))
               (loop (append (kids-of (car ks)) (cdr ks)))]
              [(and (cst-node? (car ks))
                    (eq? (tag-of (car ks)) (quote arrayElement)))
               (define ae-name (extract-array-element-name (car ks) tk-type tk-value))
               (when ae-name
                 (set! extractions
                       (cons (uir-set! ae-name
                                       (uir-get synthetic-name (uir-number (number->string (length extractions)))))
                             extractions)))
               (loop (cdr ks))]
              [else (loop (cdr ks))]))))
  (define (extract-destructured-prop pa synthetic-name)
    (define ks (kids-of pa))
    (define key-node (for/or ([k ks] #:when (cst-node? k)) k))
    (when key-node
      (define source-prop (extract-ident-from-expr key-node))
      (when source-prop
        (define colon-idx
          (for/or ([i (in-naturals)] [k ks])
            (or (and (tok? k tk-type) (eq? (tk-type k) 'Colon) i)
                (and (cst-node? k)
                     (eq? (tag-of k) 'group)
                     (for/or ([g (kids-of k)])
                       (and (tok? g tk-type) (eq? (tk-type g) 'Colon) i))))))
        (define local-name
          (if colon-idx
              (let loop2 ([i (add1 colon-idx)])
                (if (>= i (length ks))
                    source-prop
                    (let ([k (list-ref ks i)])
                      (if (and (cst-node? k) (eq? (tag-of k) 'singleExpression))
                          (extract-ident-from-expr k)
                          (loop2 (add1 i))))))
              source-prop))
        (set! extractions
              (cons (uir-set! (uir-symbol local-name)
                              (uir-get synthetic-name (uir-string source-prop)))
                    extractions)))))
  (define (extract-ident-from-expr node)
    (define ident-node
      (or (find-kid node 'identifier)
          (let ([in (find-kid node 'identifierName)])
            (and in (find-kid in 'identifier)))
          (and (eq? (tag-of node) 'identifierOrKeyWord)
               (or (find-kid node 'identifier)
                   (let ([in (find-kid node 'identifierName)])
                     (and in (find-kid in 'identifier)))))
          (let ([iok (find-kid node 'identifierOrKeyWord)])
            (and iok (or (find-kid iok 'identifier)
                         (let ([in (find-kid iok 'identifierName)])
                           (and in (find-kid in 'identifier))))))))
    (and ident-node
         (let ([tok (first (kids-of ident-node))])
           (and (tok? tok tk-type) (tk-value tok)))))
  ;; Find formalParameterList: direct child, or inside callSignature → parameterList
  (define fpl
    (or (find-kid node (quote formalParameterList))
        (let ([cs (find-kid node (quote callSignature))])
          (and cs (find-kid cs (quote parameterList))))))
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
  ;; Find functionBody: direct child, or inside group → group (functionDeclaration wrapper)
  (define body-node
    (or (find-kid node (quote functionBody))
        (let* ([outer (find-kid node (quote group))]
               [inner (and outer (find-kid outer (quote group)))])
          (and inner (find-kid inner (quote functionBody))))))
  (when body-node
    (define se (find-node-or-list body-node))
    (set! body (if se (lower-source-elements se tk-type tk-value) (uir-block (quote ())))))
  (define eff-body
    (if (null? extractions)
        body
        (let ([body-stmts (if (eq? (uir-tag body) 'block)
                             (uir-block-stmts body)
                             (list body))])
          (uir-block (append extractions body-stmts)))))
  (uir-call (uir-symbol "function") (list (uir-list (reverse params)) eff-body)))


;; ── ES Modules (import/export) ───────────────────────────────────────

(define (lower-import-stmt node tk-type tk-value)
  (define ifb (find-kid node (quote importFromBlock)))
  (unless ifb (uir-null))
  (define kids (kids-of ifb))
  (define source (uir-null))
  ;; Find importFrom node for the source (named/default/namespace)
  (define if-node (find-kid ifb (quote importFrom)))
  (when if-node
    (define src-tok (for/or ([k (kids-of if-node)] #:when (and (tok? k tk-type) (eq? (tk-type k) (quote StringLiteral)))) k))
    (when src-tok
      (define raw (tk-value src-tok))
      (set! source (uir-string (substring raw 1 (sub1 (string-length raw)))))))
  (define first-kid (first (kids-of ifb)))
  (cond [(not first-kid) (uir-null)]
        ;; Bare import: import 'm'; — source is the first kid itself
        [(and (tok? first-kid tk-type) (eq? (tk-type first-kid) (quote StringLiteral)))
         (define raw (tk-value first-kid))
         (uir-call (uir-symbol "import") (list (uir-string (substring raw 1 (sub1 (string-length raw))))))]
        ;; Named/default/namespace via group
        [else
         (define group (find-kid ifb (quote group)))
         (unless group (uir-null))
         (define inner (first (cst-kids group)))
         (unless inner (uir-null))
         (case (tag-of inner)
           ;; import { x, y } or import { x, y } from 'm'
           [(importModuleItems)
            (define bindings (quote ()))
            ;; Iterate over importModuleItems children directly
            ;; Pattern: [LIST containing group], group, ...each wrapping importAliasName
            (let loop ([ks (kids-of inner)])
              (cond [(null? ks) (void)]
                    [(and (cst-node? (car ks)) (eq? (tag-of (car ks)) (quote group)))
                     (define ian (find-kid (car ks) (quote importAliasName)))
                     (let ([b (and ian (extract-import-alias ian tk-type tk-value))])
                       (when b (set! bindings (cons b bindings))))
                     (loop (cdr ks))]
                    [(pair? (car ks))
                     (for ([g (car ks)] #:when (cst-node? g))
                       (define ian (find-kid g (quote importAliasName)))
                       (let ([b (and ian (extract-import-alias ian tk-type tk-value))])
                         (when b (set! bindings (cons b bindings)))))
                     (loop (cdr ks))]
                    [else (loop (cdr ks))]))
            (define uir-bindings (uir-record (reverse bindings)))
            (uir-call (uir-symbol "import")
                      (if (uir-null? source)
                          (list uir-bindings)
                          (list uir-bindings source)))]
           ;; import x from 'm' or import * as ns from 'm'
           [(importNamespace)
            (define ns-kids (kids-of inner))
            (cond
              ;; import * as ns from 'm' — two group children:
              ;; first group has Multiply, second group has As + identifierName
              [(and (>= (length ns-kids) 2)
                    (cst-node? (first ns-kids))
                    (eq? (tag-of (first ns-kids)) (quote group))
                    (cst-node? (second ns-kids))
                    (eq? (tag-of (second ns-kids)) (quote group)))
               (define ns-name
                 (let* ([g2 (second ns-kids)]
                        [ident (find-kid (first (cst-kids g2)) (quote identifier))])
                   (if ident
                       (uir-symbol (tk-value (first (kids-of ident))))
                       (uir-symbol "?"))))
               (uir-call (uir-symbol "import")
                         (list (uir-list (list (uir-symbol "*") ns-name)) source))]
              ;; import x from 'm'
              [else
               (define g (find-kid inner (quote group)))
               (define ident-name
                 (if g
                     (let ([ident (find-kid (first (cst-kids g)) (quote identifier))])
                       (if ident
                           (uir-symbol (tk-value (first (kids-of ident))))
                           (uir-symbol "?")))
                     (uir-symbol "?")))
               (uir-call (uir-symbol "import")
                         (list ident-name source))])]
           [else (uir-null)])]))

(define (extract-import-alias node tk-type tk-value)
  (define men (find-kid node (quote moduleExportName)))
  ;; importedBinding may be wrapped in a group node (with As token)
  (define local
    (let ([g (find-kid node (quote group))])
      (or (find-kid node (quote importedBinding))
          (and g (find-kid g (quote importedBinding))))))
  (and men
       (let* ([men-name
               (let ([in (find-kid men (quote identifier))]
                     [inn (find-kid men (quote identifierName))])
                 (cond [in (tk-value (first (kids-of in)))]
                       [inn
                        (let ([ident (find-kid inn (quote identifier))])
                          (if ident (tk-value (first (kids-of ident))) "?"))]
                       [else "?"]))]
              [local-name
               (if local
                   (let ([ident (or (find-kid local (quote identifier))
                                    (let ([in (find-kid local (quote identifierName))])
                                      (and in (find-kid in (quote identifier)))))])
                     (if ident
                         (tk-value (first (kids-of ident)))
                         ;; Fallback: importedBinding may have a direct Identifier token
                         (let ([k (first (kids-of local))])
                           (if (and (not (null? k)) (not (eq? k (quote none)))
                                    (with-handlers ([exn:fail? (lambda (_) #f)])
                                      (eq? (tk-type k) (quote Identifier))))
                               (tk-value k)
                               men-name))))
                   men-name)])
         (cons (uir-string men-name) (uir-symbol local-name)))))

(define (lower-export-stmt node tk-type tk-value)
  (define kids (kids-of node))
  (define has-default (and (>= (length kids) 2)
                           (tok? (second kids) tk-type)
                           (eq? (tk-type (second kids)) (quote Default))))
  (cond
    ;; export default expr
    [has-default
     (define se (find-kid node (quote singleExpression)))
     (if se
         (uir-call (uir-symbol "export")
                   (list (uir-symbol "default") (lower-single-expression se tk-type tk-value)))
         (uir-null))]
    [else
     (define efb (find-kid node (quote exportFromBlock)))
     (define decl (find-kid node (quote declaration)))
     (cond
       ;; export { x, y } or export { x, y } from 'm'
       [efb
         (define emi (find-kid efb (quote exportModuleItems)))
         (define bindings (quote ()))
         (when emi
           (let loop ([ks (kids-of emi)])
             (cond [(null? ks) (void)]
                   [(and (cst-node? (car ks))
                         (eq? (tag-of (car ks)) (quote exportAliasName)))
                    (let ([b (extract-export-alias (car ks) tk-type tk-value)])
                      (when b (set! bindings (cons b bindings))))
                    (loop (cdr ks))]
                   [(and (cst-node? (car ks))
                         (eq? (tag-of (car ks)) (quote group)))
                    ;; Parse-opt wraps the last item in a direct group node
                    (define ean (find-kid (car ks) (quote exportAliasName)))
                    (when ean
                      (let ([b (extract-export-alias ean tk-type tk-value)])
                        (when b (set! bindings (cons b bindings)))))
                    (loop (cdr ks))]
                   [(pair? (car ks))
                    (for ([g (car ks)] #:when (cst-node? g))
                      (define ean
                        (or (find-kid g (quote exportAliasName))
                            (and (eq? (tag-of g) (quote exportAliasName)) g)))
                      (when ean
                        (let ([b (extract-export-alias ean tk-type tk-value)])
                          (when b (set! bindings (cons b bindings))))))
                    (loop (cdr ks))]
                   [else (loop (cdr ks))])))
         (define source (uir-null))
         (define if-node (find-kid efb (quote importFrom)))
        (when if-node
          (define src-tok (for/or ([k (kids-of if-node)] #:when (and (tok? k tk-type) (eq? (tk-type k) (quote StringLiteral)))) k))
          (when src-tok
            (define raw (tk-value src-tok))
            (set! source (uir-string (substring raw 1 (sub1 (string-length raw)))))))
        (uir-call (uir-symbol "export")
                  (if (uir-null? source)
                      (list (uir-record (reverse bindings)))
                      (list (uir-record (reverse bindings)) source)))]
       ;; export const x = 1; / export function f() {} / export class Foo {}
       [decl
        (define first-decl (first (cst-kids decl)))
        (if first-decl
            (let ([inner (lower-statement first-decl tk-type tk-value)])
              (uir-call (uir-symbol "export") (list (uir-symbol "decl") inner)))
            (uir-null))]
       [else (uir-null)])]))

(define (extract-export-alias node tk-type tk-value)
  (define men (find-kid node (quote moduleExportName)))
  (define alias (find-kid node (quote As)))
  (and men
       (let* ([men-name
               (let ([in (find-kid men (quote identifier))]
                     [inn (find-kid men (quote identifierName))])
                 (cond [in (tk-value (first (kids-of in)))]
                       [inn
                        (let ([ident (find-kid inn (quote identifier))])
                          (if ident (tk-value (first (kids-of ident))) "?"))]
                       [else "?"]))]
              [export-name
               (if alias
                   (let* ([al-men (for/or ([k (kids-of node)] 
                                           #:when (and (cst-node? k) 
                                                       (eq? (tag-of k) (quote moduleExportName))
                                                       (not (eq? k men)))) k)])
                     (if al-men
                         (let ([in (find-kid al-men (quote identifier))]
                               [inn (find-kid al-men (quote identifierName))])
                           (cond [in (tk-value (first (kids-of in)))]
                                 [inn
                                  (let ([ident (find-kid inn (quote identifier))])
                                    (if ident (tk-value (first (kids-of ident))) "?"))]
                                 [else "?"]))
                         men-name))
                   men-name)])
         (cons (uir-string export-name) (uir-symbol men-name)))))

;; ── Misc statements ──────────────────────────────────────────────────

(define (lower-with-stmt node tk-type tk-value)
  (define kids (kids-of node))
  (define test (lower-expression-sequence (list-ref kids 2) tk-type tk-value))
  (define body (lower-statement (list-ref kids 4) tk-type tk-value))
  (uir-call (uir-symbol "with") (list test body)))

(define (lower-yield-stmt node tk-type tk-value)
  ;; kids: [group(Yield/YieldStar), optional group(expressionSequence), eos]
  (define kids (kids-of node))
  (define first-kid (first kids))
  ;; first kid is group containing the yield token
  (define tok-kid (and (cst-node? first-kid) (first (kids-of first-kid))))
  (define is-star (and (tok? tok-kid tk-type) (eq? (tk-type tok-kid) 'YieldStar)))
  ;; second kid is optional group with expressionSequence
  (define grp (find-kid node 'group))
  ;; But find-kid returns first matching child; we need the SECOND group
  (define expr-group
    (let loop ([ks (kids-of node)] [found-first? #f])
      (cond [(null? ks) #f]
            [(and (cst-node? (car ks)) (eq? (tag-of (car ks)) 'group))
             (if found-first? (car ks) (loop (cdr ks) #t))]
            [else (loop (cdr ks) found-first?)])))
  (define val
    (if expr-group
        (lower-expression-sequence (first (cst-kids expr-group)) tk-type tk-value)
        (uir-null)))
  (uir-yield val is-star))

(define (lower-await-expr node tk-type tk-value)
  ;; kids: [Await, singleExpression]
  (define kids (kids-of node))
  (define se (find-kid node 'singleExpression))
  (uir-await (if se (lower-single-expression se tk-type tk-value) (uir-null))))

(define (lower-labelled-stmt node tk-type tk-value)
  ;; kids: [identifier, ':' token, statement]
  (define kids (kids-of node))
  (define ident-node (first kids))
  (define label-name
    (if ident-node
        (tk-value (first (kids-of ident-node)))
        "?"))
  (define stmt-node (third kids))
  (define body (lower-statement stmt-node tk-type tk-value))
  (uir-call (uir-symbol "label") (list (uir-symbol label-name) body)))

(module+ main
  (displayln "lower-javascript loaded."))

;; ============================================================================
;; TypeScript-specific lowering functions
;; ============================================================================

(define (lower-enum-decl node tk-type tk-value)
  ;; enumDeclaration -> 'enum' (Identifier) '{' enumBody '}'
  ;; enumBody -> enumMember (',' enumMember)* ','?
  ;; Lower to: (uir-call (uir-symbol "<name>") members...)
  (define name-node (find-kid node 'identifier))
  (define name
    (if name-node
        (uir-symbol (tk-value (first (kids-of name-node))))
        (uir-symbol "?")))
  (define enum-body (find-kid node 'enumBody))
  (define members
    (if enum-body
        (lower-enum-members enum-body tk-type tk-value)
        '()))
  (uir-call name members))

(define (lower-enum-members node tk-type tk-value)
  ;; enumBody -> enumMemberList (enumMember (',' enumMember)* ','?)
  ;; enumMemberList children: first enumMember direct, rest in list of (group Comma enumMember)
  (define eml (find-kid node 'enumMemberList))
  (unless eml '())
  (define members '())
  (let loop ([ks (kids-of eml)])
    (cond [(null? ks) (void)]
          [(and (cst-node? (car ks)) (eq? (tag-of (car ks)) 'enumMember))
           (set! members (cons (lower-enum-member (car ks) tk-type tk-value) members))
           (loop (cdr ks))]
          [(pair? (car ks))
           (for ([g (car ks)] #:when (cst-node? g))
             (define em (find-kid g 'enumMember))
             (when em (set! members (cons (lower-enum-member em tk-type tk-value) members))))
           (loop (cdr ks))]
          [else (loop (cdr ks))]))
  (reverse members))

(define (lower-enum-member node tk-type tk-value)
  ;; enumMember -> identifierOrKeyWord? ('=' singleExpression)?
  ;; TS enum members may use propertyName or bare identifierOrKeyWord.
  ;; Extract the member name as a string.
  (define name
    (uir-string
     (let* ([pn (find-kid node 'propertyName)]
            [name-node (if pn
                           (or (find-kid pn 'identifierOrKeyWord)
                               (find-kid pn 'identifierName))
                           (or (find-kid node 'identifierOrKeyWord)
                               (find-kid node 'identifierName)))]
            [ident (and name-node (find-kid name-node 'identifier))]
            [tok (and ident (first (kids-of ident)))])
       (if (and tok (tok? tok tk-type))
           (tk-value tok)
           "?"))))
  (define se (find-kid node 'singleExpression))
  (define value
    (if se
        (lower-single-expression se tk-type tk-value)
        (uir-null)))
  (uir-call (uir-symbol "enum-member") (list name value)))

(define (lower-namespace-decl node tk-type tk-value)
  ;; namespaceDeclaration -> 'namespace' namespaceName '{' statementList? '}'
  ;; namespaceName -> identifier ('.' identifier)*
  ;; statementList child is a LIST of statements
  (define ns-name-node (find-kid node 'namespaceName))
  (define name
    (if ns-name-node
        (let* ([ident (or (find-kid ns-name-node 'identifier)
                          (let ([in (find-kid ns-name-node 'identifierName)])
                            (and in (find-kid in 'identifier))))]
               [tok (and ident (first (kids-of ident)))])
          (if (and tok (tok? tok tk-type))
              (uir-symbol (tk-value tok))
              (uir-symbol "?")))
        (uir-symbol "?")))
  (define body
    (let ([stmt-list (find-kid node 'statementList)])
      (if stmt-list
          (let ([lst (find-list stmt-list)])
            (if lst
                (lower-source-elements lst tk-type tk-value)
                (uir-block '())))
          (uir-block '()))))
  (uir-call (uir-symbol "namespace") (list name body)))

(define (lower-abstract-decl node tk-type tk-value)
  ;; abstractDeclaration -> 'abstract' classDeclaration | functionDeclaration
  ;; Just forward to the inner declaration (stripping 'abstract')
  (define kids (kids-of node))
  (define inner (for/or ([k kids] #:when (cst-node? k)) k))
  (if inner
      (lower-statement inner tk-type tk-value)
      (uir-null)))

;; Wire up cross-module hooks from expr.rkt (must be after all definitions)
(arrow-fn-lowerer lower-arrow-fn)
(yield-stmt-lowerer lower-yield-stmt)
(fn-body-lowerer lower-fn-body)


