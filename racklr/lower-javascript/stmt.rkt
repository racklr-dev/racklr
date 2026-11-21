#lang racket

(require racklr/tree
         racklr/uir
         "helpers.rkt"
         "expr.rkt"
         "pattern.rkt")

(provide lower-program)

;; ── JavaScript CST → UIR lowering pass ───────────────────────────────
;; lower-program takes: cst, tk-type, tk-value
;; tk-type and tk-value are the token accessors from the generated parser


;; ── Entry ────────────────────────────────────────────────────────────

(define (lower-program cst tk-type tk-value)
  (define se (find-node-or-list cst))
  (if se (lower-source-elements se tk-type tk-value) (uir-null)))

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
             (define kid (first (cst-kids v)))
             (if kid
                 (lower-statement kid tk-type tk-value)
                 (uir-null))])]
        [else (uir-null)]))

(define (lower-source-element node tk-type tk-value)
  (define kid (first (cst-kids node)))
  (if kid
      (lower-statement kid tk-type tk-value)
      (uir-null)))

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
    [else (uir-null)]))

(define (lower-expr-stmt node tk-type tk-value)
  (define es (find-kid node 'expressionSequence))
  (lower-expression-sequence es tk-type tk-value))

(define (lower-var-stmt node tk-type tk-value)
  (define vdl (find-kid node 'variableDeclarationList))
  (unless vdl (uir-null))
  (define vm (find-kid vdl 'varModifier))
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

(define (extract-var-kind vm tk-type)
  (define kid (first (kids-of vm)))
  (cond [(and (tok? kid tk-type) (eq? (tk-type kid) 'Var)) "var"]
        [(and (tok? kid tk-type) (eq? (tk-type kid) 'Const)) "const"]
        [(and (cst-node? kid) (eq? (tag-of kid) 'let_)) "let"]
        [else "var"]))

(define (lower-var-decl node tk-type tk-value)
  (define assignable (find-kid node 'assignable))
  (define ident-node
    (and assignable (find-kid assignable 'identifier)))
  (define group (find-kid node 'group))
  (define rhs
    (if group
        (let ([se (find-kid group 'singleExpression)])
          (if se
              (lower-single-expression se tk-type tk-value)
              (uir-null)))
        (uir-null)))
  (cond
    [ident-node
     (define var-name (uir-symbol (tk-value (first (kids-of ident-node)))))
     (if (uir-null? rhs)
         (uir-var var-name)
         (uir-set! var-name rhs))]
    [assignable
     ;; Destructuring declaration: `const {a, b: c = 1} = o`, `let [x, y]`.
     (define pattern (lower-binding-pattern assignable tk-type tk-value))
     (if (and pattern (not (uir-null? rhs)))
         (uir-set! pattern rhs)
         (uir-null))]
    [else (uir-null)]))

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
  (define body-node (find-kid node 'functionBody))
  (define body
    (if body-node
        (lower-fn-body body-node tk-type tk-value)
        (uir-block '())))
  (define fpl (find-kid node 'formalParameterList))
  (define params
    (if fpl
        (lower-formal-params fpl tk-type tk-value)
        '()))
  (define fn-uir (uir-fn #f params body #f #f))
  (cond [is-async (uir-set! name (uir-call (uir-symbol "async-fn") (list fn-uir)))]
        [is-generator (uir-set! name (uir-call (uir-symbol "gen-fn") (list fn-uir)))]
        [else (uir-set! name fn-uir)]))

(define (lower-fn-body node tk-type tk-value)
  (define se (find-node-or-list node))
  (if se (lower-source-elements se tk-type tk-value) (uir-block '())))

(define (lower-formal-params fpl tk-type tk-value)
  (define (param-name k)
    (define assignable (find-kid k 'assignable))
    (define ident (and assignable (find-kid assignable 'identifier)))
    (and ident (uir-symbol (tk-value (first (kids-of ident))))))
  (define (rest-param k)
    (define se (find-kid k 'singleExpression))
    (define ident (and se (find-kid se 'identifier)))
    (if ident
        (uir-spread (uir-symbol (tk-value (first (kids-of ident)))))
        (uir-spread (if se (lower-single-expression se tk-type tk-value) (uir-null)))))
  (append
   (filter-map param-name (rule-elements fpl 'formalParameterArg))
   (filter-map rest-param (rule-elements fpl 'lastFormalParameterArg))))

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

;; A statement-if is a uir-if whose branches are statements. The emitter tells
;; a statement-if from an expression ternary by block wrapping (a ternary's
;; branches are expressions), so every statement branch is wrapped: emitting
;; `test ? return x : null` instead of `if (test) { return x; }` is invalid JS.
(define (statement-branch uir)
  (if (uir-block? uir) uir (uir-block (list uir))))

(define (lower-if-stmt node tk-type tk-value)
  (define kids (kids-of node))
  (define test (lower-expression-sequence (third kids) tk-type tk-value))
  (define consequent (statement-branch (lower-statement (fifth kids) tk-type tk-value)))
  (define group (find-kid node 'group))
  (define alternate
    (if group
        (statement-branch (lower-statement (second (kids-of group)) tk-type tk-value))
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
  (define catch-present? #f)
  (define finally-body (uir-null))
  (when group
    (for ([k (kids-of group)])
      (when (cst-node? k)
        (case (tag-of k)
          [(catchProduction)
           (set! catch-present? #t)
           (define catch-group (find-kid k 'group))
           (when catch-group
             (define assignable (find-kid catch-group 'assignable))
             (when assignable
               (define ident (find-kid assignable 'identifier))
               (when ident
                 (set! catch-var (uir-var (uir-symbol (tk-value (first (kids-of ident)))))))))
           (set! catch-body (lower-block (find-kid k 'block) tk-type tk-value))]
          [(finallyProduction)
           (set! finally-body (lower-block (find-kid k 'block) tk-type tk-value))]))))
  ;; An empty catch body lowers to uir-null; keep an empty block instead so the
  ;; emitter knows the catch clause exists (a `try` needs catch or finally).
  (when (and catch-present? (uir-null? catch-body))
    (set! catch-body (uir-block '())))
  (uir-call (uir-symbol "try") (list try-body catch-var catch-body finally-body)))

(define (lower-class-decl node tk-type tk-value)
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
                    [(group) (or (lower-for-var k2 tk-type tk-value) (uir-null))]
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
                    [(group) (or (lower-for-var lk tk-type tk-value) (uir-null))]
                    [(expressionSequence) (lower-expression-sequence lk tk-type tk-value)]
                    [else (uir-null)])
                  (uir-null)))
            (define right (lower-expression-sequence (list-ref kids (or (and of-pos (+ of-pos 1)) 5)) tk-type tk-value))
            (define body (lower-statement (last kids) tk-type tk-value))
            (uir-call (uir-symbol "forof") (list left-expr right body))]
           [else
            ;; Regular for: [For, OpenParen, init?, SemiColon, test?, SemiColon, update?, CloseParen, statement]
            (define init (lower-expression-sequence (list-ref kids 2) tk-type tk-value))
            (define test (lower-expression-sequence (list-ref kids 4) tk-type tk-value))
            (define update (lower-expression-sequence (list-ref kids 6) tk-type tk-value))
            (define body (lower-statement (list-ref kids 8) tk-type tk-value))
             (uir-call (uir-symbol "for") (list init test update body))])]
    [else (uir-null)]))

(define (lower-for-var group-node tk-type tk-value)
  ;; Extract the binding from a for-in/for-of group containing
  ;; singleVariableDeclaration: an identifier or a destructuring pattern, with
  ;; the declaration kind (var/let/const) preserved so the emitted loop keeps
  ;; its binding kind and re-lowers to the same structure.
  (define svd (find-kid group-node 'singleVariableDeclaration))
  (and svd
       (let ([vd (find-kid svd 'variableDeclaration)])
         (and vd
              (let* ([assignable (find-kid vd 'assignable)]
                     [vm (find-kid svd 'varModifier)]
                     [kind (uir-symbol (if vm (extract-var-kind vm tk-type) "var"))]
                     [ident (and assignable (find-kid assignable 'identifier))]
                     [target (or (and ident (uir-symbol (tk-value (first (kids-of ident)))))
                                 (and assignable (lower-binding-pattern assignable tk-type tk-value)))])
                (and target
                     (uir-call (uir-symbol "for-binding") (list kind target))))))))

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
    ;; export default expr | export default function f() {} | ... class C {}
    [has-default
     (define se (find-kid node (quote singleExpression)))
     (define decl (find-kid node (quote declaration)))
     (cond
       [se (uir-call (uir-symbol "export")
                     (list (uir-symbol "default") (lower-single-expression se tk-type tk-value)))]
       [decl
        (define first-decl (first (cst-kids decl)))
        (if first-decl
            (uir-call (uir-symbol "export")
                      (list (uir-symbol "default") (lower-statement first-decl tk-type tk-value)))
            (uir-null))]
       [else (uir-null)])]
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

;; Wire the expression module's fn-body-lowerer to this module's lower-fn-body
;; (breaks the statement↔expression cycle; see expr.rkt).
(fn-body-lowerer lower-fn-body)
(yield-stmt-lowerer lower-yield-stmt)
(fn-decl-lowerer lower-fn-decl)

(module+ main
  (displayln "lower-javascript loaded."))
