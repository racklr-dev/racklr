#lang racket

;; racklr/lower-rust/expr.rkt — Rust expressions → UIR.
;;
;; The generated parser's left-recursive `expression` rule is rewritten into a
;; flat accumulator loop (a racklr generator property shared with Koka), so a
;; mixed-precedence chain arrives as a left-leaning tree in source order. This
;; pass re-associates chains with the Rust precedence table before building
;; UIR, so `1 + 2 * 3` lowers as `1 + (2 * 3)` and `x = 1 + 2` as `x = (1 + 2)`.
;;
;; Conventions (shared with the other front ends):
;;   value identifier → (uir-var (uir-symbol "x")); qualified paths keep the
;;   whole "a::b::c" inside ONE symbol; field access uses a uir-string field;
;;   `.0` uses a uir-number field; `a[i]` is (uir-get a i); tuple literals are
;;   (tuple ...) calls and array literals are uir-list; a struct literal is
;;   `(uir-new Foo value ...)` in written order — UIR has no typed
;;   struct-literal node, so field names are dropped (documented loss).
;;
;; `block-lowerer` is set by stmt.rkt to break the expression↔statement cycle.

(require racket/list
         racklr/tree
         racklr/uir
         "helpers.rkt"
         "type.rkt"
         "literal.rkt"
         "pattern.rkt")

(provide lower-expr
         expression-tags
         block-lowerer)

;; Tags under which nested expressions appear (the patched grammar adds
;; `expressionNoStruct` for conditions/scrutinees, per the Rust reference).
(define expression-tags '(expression expressionNoStruct))

(define block-lowerer
  (make-parameter
   (λ (node tk-type tk-value tail-returns?)
     (error 'block-lowerer "lower-rust/stmt.rkt was not required"))))

;; ── Entry dispatch ──────────────────────────────────────────────────

(define (lower-expr node tk-type tk-value)
  (case (node-tag node)
    [(expression expressionNoStruct) (lower-expression node tk-type tk-value)]
    [(literalExpression) (lower-literal node tk-type tk-value)]
    [(structExpression) (lower-struct-expression node tk-type tk-value)]
    [(expressionWithBlock) (lower-expr (single-node-child node) tk-type tk-value)]
    [(ifExpression) (lower-if node tk-type tk-value)]
    [(loopExpression) (lower-loop node tk-type tk-value)]
    [(blockExpression) ((block-lowerer) node tk-type tk-value #t)]
    [(matchExpression) (lower-match node tk-type tk-value)]
    [(matchArmExpression) (lower-expr (single-node-child node) tk-type tk-value)]
    [(closureExpression) (lower-closure node tk-type tk-value)]
    [(macroInvocation macroInvocationSemi) (lower-macro node tk-type tk-value)]
    [(pathExpression) (lower-expr (single-node-child node) tk-type tk-value)]
    [(pathInExpression) (uir-var (uir-symbol (path-name node tk-type tk-value)))]
    [(qualifiedPathInExpression)
     (uir-var (uir-symbol (path-name node tk-type tk-value)))]
    [(type_) (lower-type node tk-type tk-value)]
    [else (uir-symbol (format "?rust-~a" (node-tag node)))]))

;; ── Left-recursive expression shapes ────────────────────────────────

(define (instant-token toks tk-type t)
  (for/or ([x (in-list toks)]) (and (eq? (tk-type x) t) x)))

(define (has-token toks tk-type t) (and (instant-token toks tk-type t) #t))

(define (op-text node tk-type tk-value)
  (cond
    [(not (cst-node? node)) #f]
    [(eq? (node-tag node) 'comparisonOperator)
     (for/or ([t (immediate-tokens node tk-type)]) (tk-value t))]
    [(eq? (node-tag node) 'group)
     (for/or ([t (immediate-tokens node tk-type)]) (tk-value t))]
    [else #f]))

(define (lower-expression node tk-type tk-value)
  (define kids (node-children node))
  (define toks (immediate-tokens node tk-type))
  (define (lower k) (lower-expr k tk-type tk-value))
  (cond
    [(null? kids)
     (cond [(has-token toks tk-type 'KW_RETURN) (uir-return (uir-null))]
           [(has-token toks tk-type 'KW_BREAK) (uir-symbol "break")]
           [(has-token toks tk-type 'KW_CONTINUE) (uir-symbol "continue")]
           [else (uir-null)])]
    [(has-token toks tk-type 'KW_RETURN) (uir-return (lower (last kids)))]
    [(has-token toks tk-type 'KW_BREAK)
     (if (null? kids) (uir-symbol "break") (uir-call (uir-symbol "break") (list (lower (last kids)))))]
    [(has-token toks tk-type 'KW_CONTINUE) (uir-symbol "continue")]
    [(has-token toks tk-type 'KW_AWAIT) (uir-await (lower (first kids)))]
    [(has-token toks tk-type 'QUESTION) (uir-call (uir-symbol "?") (list (lower (first kids))))]
    [(has-token toks tk-type 'KW_AS)
     (uir-call (uir-symbol "as") (list (lower (first kids))
                                       (lower-type (second kids) tk-type tk-value)))]
    [(and (has-token toks tk-type 'DOTDOT) (>= (length kids) 1))
     (uir-call (uir-symbol "..") (map lower kids))]
    [(and (has-token toks tk-type 'DOT) (>= (length kids) 2))
     (lower-dot node kids toks tk-type tk-value)]
    [(and (has-token toks tk-type 'LSQUAREBRACKET) (= (length kids) 2))
     (lower-postfix (first kids)
                    (λ (base) (uir-get base (lower (second kids))))
                    tk-type tk-value)]
    [(and (has-token toks tk-type 'LPAREN) (= (length kids) 2)
          (eq? (node-tag (second kids)) 'callParams))
     (lower-postfix (first kids)
                    (λ (base) (uir-call base (arg-expressions (second kids) tk-type tk-value)))
                    tk-type tk-value)]
    [(and (has-token toks tk-type 'EQ) (= (length kids) 2))
     (uir-set! (lower (first kids)) (lower (second kids)))]
    [(and (= (length kids) 2) (eq? (node-tag (first kids)) 'group)
          (op-text (first kids) tk-type tk-value))
     (lower-prefix (second kids)
                   (λ (operand)
                     (uir-call (uir-symbol (op-text (first kids) tk-type tk-value))
                               (list operand)))
                   tk-type tk-value)]
    [(and (>= (length kids) 3) (eq? (node-tag (second kids)) 'compoundAssignOperator))
     (define op (for/or ([t (immediate-tokens (second kids) tk-type)]) (tk-value t)))
     (define lhs (lower (first kids)))
     (uir-set! lhs (uir-call (uir-symbol (compound-base-op op))
                             (list lhs (lower (last kids)))))]
    [(and (= (length kids) 3) (op-text (second kids) tk-type tk-value))
     (parse-precedence (lower-chain-items (raw-flatten node tk-type tk-value)
                                         tk-type tk-value)
                       1)]
    [(for/first ([c kids] #:when (eq? (node-tag c) 'arrayElements)) c)
     => (λ (elts) (uir-list (arg-expressions elts tk-type tk-value)))]
    [(for/first ([c kids] #:when (eq? (node-tag c) 'tupleElements)) c)
     => (λ (elts) (uir-call (uir-symbol "tuple")
                              (arg-expressions elts tk-type tk-value)))]
    [(and (has-token toks tk-type 'LSQUAREBRACKET) (null? kids)) (uir-list '())]
    [(and (has-token toks tk-type 'LPAREN) (null? kids)) (uir-null)]
    [(and (has-token toks tk-type 'LPAREN) (= (length kids) 1))
     (uir-paren (lower (first kids)))]
    [else (lower (last kids))]))

(define (lower-dot node kids toks tk-type tk-value)
  (define kid2 (second kids))
  (define args (and (has-token toks tk-type 'LPAREN)
                    (for/or ([c (node-children node)]
                             #:when (eq? (node-tag c) 'callParams))
                      (arg-expressions c tk-type tk-value))))
  (define postfix
    (case (node-tag kid2)
      [(identifier) (λ (base) (uir-get base (uir-string (first-identifier kid2 tk-type tk-value))))]
      [(tupleIndex)
       (λ (base) (uir-get base (uir-number
                                (or (for/or ([t (immediate-tokens kid2 tk-type)])
                                      (and (eq? (tk-type t) 'INTEGER_LITERAL) (tk-value t)))
                                    "0"))))]
      [(pathExprSegment)
       (λ (base) (uir-call (uir-get base (uir-string (first-identifier kid2 tk-type tk-value)))
                           (or args '())))]
      [else (λ (base) (uir-get base (uir-string "?")))]))
  (lower-postfix (first kids) postfix tk-type tk-value))

(define (arg-expressions node tk-type tk-value)
  (map (λ (e) (lower-expr e tk-type tk-value))
       (ordered-nodes node expression-tags)))

;; ── Binary chains: re-associate with Rust precedence ────────────────

(define precedence
  '(("=" . 1) ("||" . 2) ("&&" . 3) ("==" . 4) ("!=" . 4) ("<" . 4) (">" . 4)
    ("<=" . 4) (">=" . 4) ("|" . 5) ("^" . 6) ("&" . 7) ("<<" . 8) (">>" . 8)
    ("+" . 9) ("-" . 9) ("*" . 10) ("/" . 10) ("%" . 10) (".." . 11) ("..=" . 11)))

(define (op-level op) (or (cdr (assoc op precedence)) 12))

;; Infix chain as a flat list of nodes and operator strings, in source order.
(define (raw-flatten node tk-type tk-value)
  (define kids (node-children node))
  (define toks (immediate-tokens node tk-type))
  (define opstr (and (= (length kids) 3) (op-text (second kids) tk-type tk-value)))
  (cond
    [(and (= (length kids) 2) (has-token toks tk-type 'EQ))
     (append (raw-flatten (first kids) tk-type tk-value)
             (list "=")
             (raw-flatten (second kids) tk-type tk-value))]
    [(and opstr (not (member opstr '("=" "as"))))
     (append (raw-flatten (first kids) tk-type tk-value)
             (list opstr)
             (raw-flatten (third kids) tk-type tk-value))]
    [else (list node)]))

(define (lower-chain-items raw tk-type tk-value)
  (for/list ([item (in-list raw)])
    (if (string? item) item (lower-expr item tk-type tk-value))))

;; A postfix applies to the LAST operand of a flat chain (`self.x + self.y` is
;; parsed as `(self.x + self) . y` by the flat accumulator). Rebuild the chain
;; with the postfix on its last operand, then re-associate.
(define (lower-postfix base-node postfix tk-type tk-value)
  (define raw (raw-flatten base-node tk-type tk-value))
  (cond
    [(or (< (length raw) 2) (string? (car (reverse raw))))
     (postfix (lower-expr base-node tk-type tk-value))]
    [else
     (define head (drop-right raw 1))
     (define last-node (last raw))
     (define items
       (append (lower-chain-items head tk-type tk-value)
               (list (postfix (lower-expr last-node tk-type tk-value)))))
     (parse-precedence items 1)]))

;; A prefix applies to the FIRST operand of a flat chain.
(define (lower-prefix operand-node prefix tk-type tk-value)
  (define raw (raw-flatten operand-node tk-type tk-value))
  (cond
    [(or (< (length raw) 2) (string? (first raw)))
     (prefix (lower-expr operand-node tk-type tk-value))]
    [else
     (define rest-nodes (rest raw))
     (define items
       (append (list (prefix (lower-expr (first raw) tk-type tk-value)))
               (lower-chain-items rest-nodes tk-type tk-value)))
     (parse-precedence items 1)]))

;; items: [uir op uir op uir ...]; standard precedence climbing.
(define (parse-precedence items min-level)
  (define-values (lhs rest) (parse-expr items min-level))
  lhs)

(define (parse-expr items min-level)
  (define lhs (first items))
  (let loop ([lhs lhs] [rest (cdr items)])
    (define op (and (pair? rest) (first rest)))
    (define level (and op (op-level op)))
    (cond
      [(or (not level) (< level min-level)) (values lhs rest)]
      [else
       (define right-assoc? (string=? op "="))
       (define-values (rhs rest*)
         (parse-expr (cdr rest) (if right-assoc? level (add1 level))))
       (loop (build-binary op lhs rhs) rest*)])))

(define (build-binary op lhs rhs)
  (cond [(string=? op "=") (uir-set! lhs rhs)]
        [else (uir-call (uir-symbol op) (list lhs rhs))]))

(define (compound-base-op op)
  (cond [(and (string? op) (string-suffix? op "="))
         (substring op 0 (sub1 (string-length op)))]
        [else (or op "=")]))

(define (lower-literal node tk-type tk-value)
  (define t (for/first ([x (immediate-tokens node tk-type)]) x))
  (if t (literal-uir t tk-type tk-value) (uir-symbol "?literal")))

;; ── Struct literals ─────────────────────────────────────────────────

(define (lower-struct-expression node tk-type tk-value)
  (define payload (single-node-child node))
  (case (node-tag payload)
    [(structExprStruct)
     (define path (or (find-descendant payload 'pathInExpression) payload))
     (define args
       (for/list ([f (find-descendants payload 'structExprField)])
         (define value (find-descendant f 'expression))
         (if value (lower-expr value tk-type tk-value) (uir-null))))
     (uir-new (uir-symbol (path-name path tk-type tk-value)) args)]
    [(structExprTuple)
     (define path (or (find-descendant payload 'pathInExpression) payload))
     (uir-call (uir-var (uir-symbol (path-name path tk-type tk-value)))
               (arg-expressions payload tk-type tk-value))]
    [else (lower-expr payload tk-type tk-value)]))

;; ── Control flow ────────────────────────────────────────────────────

(define (lower-if node tk-type tk-value)
  (define kids (node-children node))
  (define test (first-expression-child node))
  (define then (for/first ([c kids] #:when (eq? (node-tag c) 'blockExpression)) c))
  (define else-node
    (for/or ([c (in-list kids)])
      (and (eq? (node-tag c) 'group)
           (or (find-descendant c 'ifExpression) (find-descendant c 'blockExpression)))))
  (uir-if (if test (lower-expr test tk-type tk-value) (uir-bool #t))
          (if then ((block-lowerer) then tk-type tk-value #t) (uir-block '()))
          (cond
            [(and else-node (eq? (node-tag else-node) 'ifExpression))
             (lower-if else-node tk-type tk-value)]
            [else-node ((block-lowerer) else-node tk-type tk-value #t)]
            [else (uir-null)])))

(define (lower-loop node tk-type tk-value)
  (define body (find-descendant node 'blockExpression))
  (define body-uir (if body ((block-lowerer) body tk-type tk-value #t) (uir-block '())))
  (cond
    [(find-descendant node 'predicateLoopExpression)
     (define p (find-descendant node 'predicateLoopExpression))
     (uir-while (lower-expr (first-expression-child p) tk-type tk-value)
                body-uir (uir-null))]
    [(find-descendant node 'iteratorLoopExpression)
     (define p (find-descendant node 'iteratorLoopExpression))
     (uir-for-each (uir-var (uir-symbol (lower-pattern-name
                                         (find-descendant p 'pattern) tk-type tk-value)))
                   (lower-expr (first-expression-child p) tk-type tk-value)
                   body-uir (uir-null))]
    [else (uir-while (uir-bool #t) body-uir (uir-null))]))

(define (first-expression-child node)
  (for/first ([c (node-children node)] #:when (memq (node-tag c) expression-tags)) c))

;; `matchArms` is right-recursive: [matchArm FATARROW body matchArms] |
;; [matchArm FATARROW expression COMMA?]. Walk it to pair arms with bodies.
(define (collect-match-arms node acc)
  (cond
    [(not (cst-node? node)) (reverse acc)]
    [else
     (define kids (node-children node))
     (define arm (for/first ([c kids] #:when (eq? (node-tag c) 'matchArm)) c))
     (define body (for/or ([c kids]
                           #:when (memq (node-tag c) '(matchArmExpression expression)))
                    c))
     (define rest (for/first ([c kids] #:when (eq? (node-tag c) 'matchArms)) c))
     (collect-match-arms rest (if arm (cons (cons arm body) acc) acc))]))

(define (lower-match node tk-type tk-value)
  (define subject (first-expression-child node))
  (define arms-node (find-descendant node 'matchArms))
  (define cases
    (for/list ([pair (in-list (if arms-node (collect-match-arms arms-node '()) '()))])
      (define arm (car pair))
      (define body-node (cdr pair))
      (define pat (find-descendant arm 'pattern))
      (define guard (find-descendant arm 'matchArmGuard))
      (uir-case (if pat (lower-pattern pat tk-type tk-value) (uir-pat-wildcard))
                (and guard (lower-expr (find-descendant guard 'expression) tk-type tk-value))
                (if body-node (lower-expr body-node tk-type tk-value) (uir-null)))))
  (uir-match (if subject (lower-expr subject tk-type tk-value) (uir-null)) cases))

(define (lower-closure node tk-type tk-value)
  (define params
    (for/list ([p (find-descendants node 'closureParam)] [i (in-naturals)])
      (define id (first-identifier p tk-type tk-value))
      (define type-node (find-descendant p 'type_))
      (uir-typed-param (uir-symbol (if (string=? id "?") (format "p~a" i) id))
                       (and type-node (lower-type type-node tk-type tk-value)) #f #f)))
  (define body (or (find-descendant node 'blockExpression)
                   (find-descendant node 'expression)))
  (uir-fn #f params
          (if (and body (eq? (node-tag body) 'blockExpression))
              ((block-lowerer) body tk-type tk-value #t)
              (uir-block (list (lower-expr body tk-type tk-value))))
          #f #f))

(define (lower-macro node tk-type tk-value)
  (define name (or (find-descendant node 'simplePath) node))
  (define args
    (for/list ([t (in-list (deep-tokens node tk-type))]
               #:when (memq (tk-type t) '(INTEGER_LITERAL FLOAT_LITERAL STRING_LITERAL
                                          RAW_STRING_LITERAL CHAR_LITERAL KW_TRUE KW_FALSE)))
      (literal-uir t tk-type tk-value)))
  (uir-call (uir-var (uir-symbol (string-append (path-name name tk-type tk-value) "!")))
            args))
