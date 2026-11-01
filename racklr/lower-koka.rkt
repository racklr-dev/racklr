#lang racket

;; Koka source-language lowering (Phase 1).
;;
;; Koka.g4 → CST → UIR. Layout-sensitive blocks arrive as NEWLINE/INDENT/DEDENT
;; tokens from the generated lexer and lower here as uir-block.
;;   - fun          → uir-fn (typed params, return type, effect row)
;;   - val          → uir-let (null body marks a statement-level binding)
;;   - type         → uir-enum (constructors as uir-enum-variant)
;;   - effect       → uir-effect-decl (operations as uir-effect-op)
;;   - with handler → uir-effect-handler (ops as (name params body) triples)
;;   - operators    → uir-call with a uir-symbol callee (precedence deferred)

(require racklr/tree
         racklr/uir)

(provide lower-koka)

;; ── CST-walking helpers ─────────────────────────────────────────────

(define (node-children cst)
  (filter cst-node? (any-tree-children cst)))

;; The first `list` child of a node: the wrapper holding repeated elements
;; produced by the generated parser's star/plus handling.
(define (list-wrapper node)
  (for/or ([c (any-tree-children node)])
    (and (list? c) (pair? c) c)))

(define (single-node-child node)
  (for/or ([c (any-tree-children node)])
    (and (cst-node? c) c)))

(define (find-node-child node tag)
  (for/or ([c (node-children node)])
    (and (eq? (any-tree-tag c) tag) c)))

(define (token-like? x)
  (and (not (cst-node? x))
       (not (list? x))
       (not (null? x))
       (not (eq? x 'none))))

(define (immediate-tokens node)
  (for/list ([c (any-tree-children node)]
             #:when (token-like? c))
    c))

(define (token-with-type node tk-type want)
  (for/or ([t (immediate-tokens node)])
    (and (eq? (tk-type t) want) t)))

;; The statement nodes held by a `(NEWLINE | stmt)*` or `(NEWLINE | stmt)+`
;; list wrapper: each item is a `group` wrapping either a `stmt` or a NEWLINE.
(define (stmt-nodes node)
  (for/list ([g (filter cst-node? (or (list-wrapper node) '()))]
             #:when (eq? (any-tree-tag g) 'group)
             [s (in-value (find-node-child g 'stmt))]
             #:when s)
    s))

;; ── Entry point ─────────────────────────────────────────────────────

(define (lower-koka cst tk-type tk-value)
  (uir-block (map (λ (s) (lower-stmt s tk-type tk-value))
                  (stmt-nodes cst))))

;; ── Statements / declarations ───────────────────────────────────────

(define (lower-stmt stmt tk-type tk-value)
  (define kid (single-node-child stmt))
  (cond [(eq? (any-tree-tag kid) 'simple_stmt)
         (lower-simple-stmt kid tk-type tk-value)]
        [(eq? (any-tree-tag kid) 'compound_stmt)
         (lower-compound-stmt kid tk-type tk-value)]
        [else (uir-null)]))

(define (lower-simple-stmt node tk-type tk-value)
  (define kid (single-node-child node))
  (if (eq? (any-tree-tag kid) 'valdecl)
      (lower-valdecl kid tk-type tk-value)
      (lower-node kid tk-type tk-value)))

(define (lower-compound-stmt node tk-type tk-value)
  (define kid (single-node-child node))
  (case (any-tree-tag kid)
    [(funcdecl)   (lower-funcdecl kid tk-type tk-value)]
    [(effectdecl) (lower-effectdecl kid tk-type tk-value)]
    [(typedecl)   (lower-typedecl kid tk-type tk-value)]
    [(importdecl) (lower-importdecl kid tk-type tk-value)]
    [(moduledecl) (lower-moduledecl kid tk-type tk-value)]
    [else (uir-null)]))

(define (lower-valdecl node tk-type tk-value)
  (uir-let (uir-symbol (tk-value (token-with-type node tk-type 'IDENT)))
           (lower-node (find-node-child node 'expr) tk-type tk-value)
           (uir-null)))

(define (lower-funcdecl node tk-type tk-value)
  (define name (tk-value (token-with-type node tk-type 'IDENT)))
  (define ps (lower-params node tk-type tk-value))
  (define body (lower-block (find-node-child node 'block) tk-type tk-value))
  (define rt (find-node-child node 'rettype))
  (define-values (ret-type effects)
    (if rt (lower-rettype rt tk-type tk-value) (values #f #f)))
  (uir-fn (uir-symbol name) ps body ret-type effects))

(define (lower-effectdecl node tk-type tk-value)
  (define name (tk-value (token-with-type node tk-type 'IDENT)))
  (define ops
    (for/list ([g (filter cst-node? (or (list-wrapper node) '()))]
               #:when (eq? (any-tree-tag g) 'group)
               [op (in-value (find-node-child g 'opdecl))]
               #:when op)
      (lower-opdecl op tk-type tk-value)))
  (uir-effect-decl (uir-symbol name) ops))

(define (lower-opdecl node tk-type tk-value)
  (uir-effect-op (uir-symbol (tk-value (token-with-type node tk-type 'IDENT)))
                 (lower-params node tk-type tk-value)
                 (let ([kt (find-node-child node 'ktype)])
                   (if kt (lower-ktype kt tk-type tk-value) #f))))

(define (lower-typedecl node tk-type tk-value)
  (define name (tk-value (token-with-type node tk-type 'IDENT)))
  (define variants
    (for/list ([g (filter cst-node? (or (list-wrapper node) '()))]
               #:when (eq? (any-tree-tag g) 'group)
               [c (in-value (find-node-child g 'constr))]
               #:when c)
      (lower-constr c tk-type tk-value)))
  (uir-enum (uir-symbol name) variants))

(define (lower-constr node tk-type tk-value)
  (define name (tk-value (token-with-type node tk-type 'TYPE_IDENT)))
  (define grp (find-node-child node 'group))
  (define ps (and grp (find-node-child grp 'params)))
  (uir-enum-variant (uir-symbol name)
                    (if ps (lower-params-node ps tk-type tk-value) '())
                    #f))

(define (lower-importdecl node tk-type tk-value)
  (uir-import (lower-qualname (find-node-child node 'qualname) tk-type tk-value) '()))

(define (lower-moduledecl node tk-type tk-value)
  (uir-module (lower-qualname (find-node-child node 'qualname) tk-type tk-value)
              '() '() (uir-null)))

;; ── Types / params ──────────────────────────────────────────────────

(define (lower-qualname node tk-type tk-value)
  (define first (tk-value (token-with-type node tk-type 'IDENT)))
  (define rest
    (for/list ([g (filter cst-node? (or (list-wrapper node) '()))]
               #:when (eq? (any-tree-tag g) 'group))
      (tk-value (token-with-type g tk-type 'IDENT))))
  (uir-symbol (string-join (filter identity (cons first rest)) ".")))

(define (lower-params node tk-type tk-value)
  (define ps (find-node-child node 'params))
  (if ps (lower-params-node ps tk-type tk-value) '()))

(define (lower-params-node ps tk-type tk-value)
  (define first (find-node-child ps 'param))
  (define rest
    (for/list ([g (filter cst-node? (or (list-wrapper ps) '()))]
               #:when (eq? (any-tree-tag g) 'group))
      (find-node-child g 'param)))
  (map (λ (p) (lower-param p tk-type tk-value))
       (filter identity (cons first rest))))

(define (lower-param p tk-type tk-value)
  (define grp (find-node-child p 'group))
  (define kt (and grp (find-node-child grp 'ktype)))
  (uir-typed-param (uir-symbol (tk-value (token-with-type p tk-type 'IDENT)))
                   (if kt (lower-ktype kt tk-type tk-value) #f)
                   #f
                   #f))

(define (lower-ktype node tk-type tk-value)
  (define toks (immediate-tokens node))
  (if (= (length toks) 2)
      (uir-symbol "()")
      (uir-symbol (tk-value (first toks)))))

(define (lower-rettype node tk-type tk-value)
  (define eff (find-node-child node 'effects))
  (define kt (find-node-child node 'ktype))
  (values (if kt (lower-ktype kt tk-type tk-value) #f)
          (if eff (lower-effects eff tk-type tk-value) #f)))

(define (lower-effects node tk-type tk-value)
  (if (token-with-type node tk-type 'TOTAL)
      (uir-effect-row '())
      (let* ([first (token-with-type node tk-type 'IDENT)]
             [rest (for/list ([g (filter cst-node? (or (list-wrapper node) '()))]
                              #:when (eq? (any-tree-tag g) 'group))
                     (tk-value (token-with-type g tk-type 'IDENT)))])
        (uir-effect-row
         (for/list ([n (filter identity (cons (and first (tk-value first)) rest))])
           (uir-effect-entry (uir-symbol n) 'may '() #f #f))))))

;; ── Blocks ──────────────────────────────────────────────────────────

(define (lower-block node tk-type tk-value)
  (uir-block (map (λ (s) (lower-stmt s tk-type tk-value))
                  (stmt-nodes node))))

;; ── Expressions ─────────────────────────────────────────────────────

(define (lower-node node tk-type tk-value)
  (case (any-tree-tag node)
    [(expr)       (lower-node (single-node-child node) tk-type tk-value)]
    [(binop_expr) (lower-binop node tk-type tk-value)]
    [(unary_expr) (lower-node (single-node-child node) tk-type tk-value)]
    [(app_expr)   (lower-app node tk-type tk-value)]
    [(primary)    (lower-primary node tk-type tk-value)]
    [(literal)    (lower-literal node tk-type tk-value)]
    [(ifexpr)     (lower-if node tk-type tk-value)]
    [(lambda)     (lower-lambda node tk-type tk-value)]
    [(withexpr)   (lower-with node tk-type tk-value)]
    [else (uir-symbol (format "?~a" (any-tree-tag node)))]))

(define (lower-binop node tk-type tk-value)
  (define kids (node-children node))
  (if (= (length kids) 1)
      (lower-node (first kids) tk-type tk-value)
      (uir-call (uir-symbol (tk-value (token-with-type node tk-type 'OP)))
                (list (lower-node (first kids) tk-type tk-value)
                      (lower-node (second kids) tk-type tk-value)))))

(define (lower-app node tk-type tk-value)
  (for/fold ([acc (lower-node (find-node-child node 'primary) tk-type tk-value)])
            ([pfx (filter cst-node? (or (list-wrapper node) '()))])
    (apply-postfix pfx acc tk-type tk-value)))

(define (apply-postfix pfx acc tk-type tk-value)
  (if (token-with-type pfx tk-type 'DOT)
      (uir-get acc (uir-symbol (tk-value (token-with-type pfx tk-type 'IDENT))))
      (uir-call acc (lower-args pfx tk-type tk-value))))

(define (lower-args postfix tk-type tk-value)
  (define a (find-node-child postfix 'args))
  (if a
      (let* ([first (find-node-child a 'expr)]
             [rest (for/list ([g (filter cst-node? (or (list-wrapper a) '()))]
                              #:when (eq? (any-tree-tag g) 'group))
                     (lower-node (find-node-child g 'expr) tk-type tk-value))])
        (cons (lower-node first tk-type tk-value) rest))
      '()))

(define (lower-primary node tk-type tk-value)
  (define kid (single-node-child node))
  (cond
    [(cst-node? kid)
     (if (eq? (any-tree-tag kid) 'group)
         (uir-paren (lower-node (find-node-child kid 'expr) tk-type tk-value))
         (lower-node kid tk-type tk-value))]
    [else
     (define t (first (immediate-tokens node)))
     (cond [(eq? (tk-type t) 'IDENT) (uir-var (uir-symbol (tk-value t)))]
           [(eq? (tk-type t) 'TYPE_IDENT) (uir-symbol (tk-value t))]
           [else (uir-symbol "?primary")])]))

(define (lower-literal node tk-type tk-value)
  (define t (first (immediate-tokens node)))
  (cond [(eq? (tk-type t) 'INT) (uir-number (tk-value t))]
        [(eq? (tk-type t) 'STRING) (uir-string (strip-quotes (tk-value t)))]
        [else (uir-symbol "?literal")]))

(define (strip-quotes s)
  (if (and (> (string-length s) 1)
           (char=? (string-ref s 0) #\")
           (char=? (string-ref s (sub1 (string-length s))) #\"))
      (substring s 1 (sub1 (string-length s)))
      s))

(define (lower-if node tk-type tk-value)
  (define test (lower-node (find-node-child node 'expr) tk-type tk-value))
  (define then-block (lower-block (find-node-child node 'block) tk-type tk-value))
  (define else-grp
    (for/or ([c (any-tree-children node)])
      (and (cst-node? c) (eq? (any-tree-tag c) 'group) c)))
  (define else
    (if (and else-grp (find-node-child else-grp 'block))
        (lower-block (find-node-child else-grp 'block) tk-type tk-value)
        (uir-null)))
  (uir-if test then-block else))

(define (lower-lambda node tk-type tk-value)
  (uir-fn #f
          (lower-params node tk-type tk-value)
          (lower-block (find-node-child node 'block) tk-type tk-value)
          #f #f))

(define (lower-with node tk-type tk-value)
  (define ops '())
  (define ret #f)
  (define body-stmts '())
  (for ([g (filter cst-node? (or (list-wrapper node) '()))])
    (define hi (and (eq? (any-tree-tag g) 'group) (find-node-child g 'handler_item)))
    (when hi
      (define kid (single-node-child hi))
      (case (any-tree-tag kid)
        [(op_clause)
         (set! ops (cons (lower-op-clause kid tk-type tk-value) ops))]
        [(return_clause)
         (set! ret (lower-node (find-node-child kid 'expr) tk-type tk-value))]
        [(stmt)
         (set! body-stmts (cons (lower-stmt kid tk-type tk-value) body-stmts))])))
  (uir-effect-handler #f (reverse ops) ret (uir-block (reverse body-stmts))))

(define (lower-op-clause node tk-type tk-value)
  (list (uir-symbol (tk-value (token-with-type node tk-type 'IDENT)))
        (lower-params node tk-type tk-value)
        (lower-node (find-node-child node 'expr) tk-type tk-value)))

(module+ main
  (displayln "racklr/lower-koka — Koka CST → UIR lowering loaded."))
