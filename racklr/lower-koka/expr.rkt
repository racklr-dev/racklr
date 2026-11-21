#lang racket

;; racklr/lower-koka/expr.rkt — Koka expressions → UIR.
;;
;; The generated parser keeps operator and application chains as
;; left-leaning trees (it rewrites direct left recursion into an accumulator).
;; Koka's own application semantics are the reference:
;;   - `x.name` and `x.name(args)` are applications of `name` with the receiver
;;     as the first argument (Syntax/Parse.hs `dotexpr`), so they lower to
;;     `uir-call`; emission renders them back in dot form.
;;   - `a[i]` is the hidden `index` application of upstream, lowered to the
;;     dedicated `uir-index` node.
;;   - a trailing block / `fn` argument appends to the application's arguments.
;;   - operators are `uir-call` with a symbol callee; `:=` is `uir-set!`.
;;   Every operator application is parenthesized on emission, so emission needs
;;   no precedence table and the flat accumulated chain re-parses identically.
;;
;; Tuple values use the tuple constructor name `(,)` / `(,,)` … (Koka's own
;; tuple binder names); `ctx e` / `hole` use upstream's desugared applications,
;; and `mask` is its own node because it carries the effect-row mask.

(require racklr/tree
         racklr/uir
         "helpers.rkt"
         "type.rkt"
         "pattern.rkt"
         "hooks.rkt"
         "fixity.rkt"
         "control.rkt")

(provide lower-expr
         lower-blockexpr
         lower-ntlexpr
         lower-basicexpr
         lower-arguments
         tuple-class-name
         ;; hooks installed by this module
         install-expr-hooks!)

(define (install-expr-hooks!)
  (lower-expr-fn lower-expr)
  (lower-blockexpr-fn lower-blockexpr)
  (lower-basicexpr-fn lower-basicexpr)
  (lower-ntlexpr-fn lower-ntlexpr))

;; ── Entry points ────────────────────────────────────────────────────

(define (single-node n)
  (for/first ([c (in-list (children n))] #:when (node? c)) c))

;; expr in expression position: a bare block is a thunk (anonymous function).
(define (lower-expr node)
  (define kid (single-node node))
  (cond
    [(not kid) (uir-null)]
    [else
     (case (node-tag kid)
       [(withexpr) ((lower-withexpr-fn) kid)]
       [(block) (uir-fn #f '() ((lower-block-fn) kid) #f #f)]
       [(returnexpr) (uir-return (lower-expr (find-node kid 'expr)))]
       [(valexpr) (lower-valexpr kid)]
       [(basicexpr) (lower-basicexpr kid)]
       [else (uir-symbol (format "?expr/~a" (node-tag kid)))])]))

;; blockexpr: a block is statement grouping here (the body position), anything
;; else is an ordinary expression. `blockexpr : expr`, so a block arrives one
;; wrapper down and must not be mistaken for a lambda.
(define (lower-blockexpr node)
  (define kid (single-node node))
  (cond
    [(not kid) (uir-null)]
    [(eq? (node-tag kid) 'block) ((lower-block-fn) kid)]
    [(eq? (node-tag kid) 'expr)
     (define inner (single-node kid))
     (if (and inner (eq? (node-tag inner) 'block))
         ((lower-block-fn) inner)
         (lower-expr kid))]
    [else (lower-expr kid)]))

(define (lower-ntlexpr node)
  (define kid (single-node node))
  (if kid (lower-op kid) (uir-null)))

(define (lower-valexpr node)
  (define ap (find-node node 'apattern))
  (define-values (pattern annot) (lower-apattern ap))
  (define value (lower-blockexpr (find-node node 'blockexpr)))
  (define value* (if annot (uir-ascribe value (uir-symbol annot)) value))
  (define body (lower-expr (find-node node 'expr)))
  (uir-let (pattern-name pattern) value* body))

;; A binding whose pattern is a plain capture uses a name symbol; a destructuring
;; pattern keeps the pattern node in the let's name slot (emitted as `val pat =`).
(define (pattern-name pattern)
  (if (uir-pat-capture? pattern)
      (uir-symbol (uir-symbol-name (uir-pat-capture-name pattern)))
      pattern))

;; ── Basic expressions ───────────────────────────────────────────────

(define (lower-basicexpr node)
  (define kid (single-node node))
  (cond
    [(not kid) (uir-null)]
    [else
     (case (node-tag kid)
       [(opexpr ntlopexpr prefixexpr ntlprefixexpr appexpr ntlappexpr) (lower-op kid)]
       [(ifexpr) (lower-if kid)]
       [(matchexpr) (lower-match kid)]
       [(handlerexpr) ((lower-handlerexpr-fn) kid)]
       [(fnexpr) ((lower-fnexpr-fn) kid)]
       [else (uir-symbol (format "?basic/~a" (node-tag kid)))])]))

;; ── Operator chains ─────────────────────────────────────────────────

(define (lower-op node)
  (case (node-tag node)
    [(opexpr ntlopexpr) (lower-op-chain node)]
    [(prefixexpr ntlprefixexpr) (lower-prefix node)]
    [(appexpr ntlappexpr) (lower-app node)]
    [else (uir-symbol (format "?op/~a" (node-tag node)))]))

;; Koka resolves operator precedence and associativity from fixity
;; declarations; the generated parser keeps a flat left-leaning chain, so the
;; chain is flattened here and re-associated with Koka's fixity table.
(define (lower-op-chain node)
  (define-values (first-operand op-rhs) (flatten-op-chain node))
  (assemble-operator-chain (cons first-operand (map cdr op-rhs))
                           (map car op-rhs)))

(define (flatten-op-chain node)
  (define kids (nodes node))
  (cond
    [(and (= (length kids) 3)
          (memq (node-tag (first kids)) '(opexpr ntlopexpr)))
     (define-values (first-operand ops) (flatten-op-chain (first kids)))
     (values first-operand
             (append ops
                     (list (cons (operator-text (second kids))
                                 (lower-op (third kids))))))]
    [(pair? kids) (values (lower-op (first kids)) '())]
    [else (values (uir-null) '())]))

(define (operator-text node)
  ;; qoperator → op → OP/LT/GT/PIPE/ASSIGN token
  (identifier-text node))

(define (lower-prefix node)
  (define kids (children node))
  (define first-kid (and (pair? kids) (car kids)))
  (cond
    [(and (token-struct? first-kid)
          (eq? (tok-type first-kid) 'OP)
          (member (tok-value first-kid) '("!" "~")))
     (uir-call (uir-symbol (tok-value first-kid))
               (list (lower-op (first (nodes node)))))]
    [(node? first-kid) (lower-op first-kid)]
    [else (uir-null)]))

;; ── Applications ────────────────────────────────────────────────────

;; Flatten the left-recursive application tree into (base atom . postfix slice).
(define (app-parts node)
  (define kids (children node))
  (define first-kid (and (pair? kids) (car kids)))
  (cond
    [(and (node? first-kid) (eq? (node-tag first-kid) 'atom))
     (values first-kid (cdr kids))]
    [(and (node? first-kid) (memq (node-tag first-kid) '(appexpr ntlappexpr)))
     (define-values (base rest) (app-parts first-kid))
     (values base (append rest (cdr kids)))]
    [else (values first-kid '())]))

(define (lower-app node)
  (define-values (base-node postfixes) (app-parts node))
  (define base-tag (and (node? base-node) (node-tag base-node)))
  ;; `ctx e` binds tighter than application postfixes in Koka
  ;; (`ctx Cons(1, hole)` is `ctx` of the whole application), so the postfix
  ;; slice applies to the context expression's own atom and the result is
  ;; wrapped once.
  (cond
    [(eq? base-tag 'ctxexpr)
     (define inner (find-node base-node 'atom))
     (define applied (apply-postfixes (lower-atom inner) postfixes))
     (uir-call (uir-symbol "@cctx-create") (list applied))]
    [(eq? base-tag 'mask)
     (define-values (effects behind) (lower-mask-spec base-node))
     (define applied (apply-postfixes (uir-null) postfixes))
     (if (and (uir-call? applied) (equal? (uir-symbol-name (uir-call-callee applied)) "@mask-arg"))
         (uir-mask effects behind 'apply (first (uir-call-args applied)))
         (uir-mask effects behind 'apply (uir-null)))]
    [else (apply-postfixes (lower-atom base-node) postfixes)]))

;; Apply the flat postfix slice (tokens/nodes in source order) to a value.
(define (apply-postfixes value slice)
  (let loop ([rest slice] [acc value])
    (cond
      [(null? rest) acc]
      [(and (>= (length rest) 3) (tok? (first rest) 'OPEN_PAREN))
       (loop (cddr rest) (make-call acc (lower-arguments (first (filter node? (cdr rest))))))]
      [(and (>= (length rest) 3) (tok? (first rest) 'OPEN_BRACK))
       ;; `a[i]`: the bracket alternative carries the `arguments` rule.
       (define args-node (first (filter node? (cdr rest))))
       (define idx (first (or (lower-arguments args-node) (list (uir-null)))))
       (loop (cddr rest) (uir-index acc idx))]
      [(and (pair? rest) (tok? (first rest) 'DOT))
       (define after (cdr rest))
       (cond
         [(and (pair? after) (tok? (first after) 'OPEN_PAREN))
          ;; x.(args): apply x itself to the arguments
          (define args (lower-arguments (first (filter node? (cdr after)))))
          (loop (cddr after) (make-call acc args))]
         [else
          (define name-node (first (filter node? after)))
          (define nm (identifier-text name-node))
          (define after-name (cdr after))
          ;; Dot application: `x.name` and `x.name(args)` keep the receiver in
          ;; a field-access callee (`uir-get`), so the UIR records which
          ;; receiver the callee came from (the effect layer classifies the
          ;; receiver chain) and the emitter reproduces the dot form exactly.
          (cond
            [(and (pair? after-name) (tok? (first after-name) 'OPEN_PAREN))
             (define args (lower-arguments (first (filter node? (cdr after-name)))))
             (loop (cddr after-name)
                   (make-call (uir-get acc (uir-symbol nm)) args))]
            [else (loop after-name (uir-get acc (uir-symbol nm)))])])]
      [(and (pair? rest) (node? (first rest)))
       ;; trailing block / fn argument: append to the application's arguments
       (define arg
         (case (node-tag (first rest))
           [(block) (uir-fn #f '() ((lower-block-fn) (first rest)) #f #f)]
           [(fnexpr) ((lower-fnexpr-fn) (first rest))]
           [else (lower-expr (first rest))]))
       (loop (cdr rest) (append-arg acc arg))]
      [else (loop (cdr rest) acc)])))

(define (make-call callee args)
  (uir-call callee args))

(define (append-arg acc arg)
  (if (uir-call? acc)
      (struct-copy uir-call acc [args (append (uir-call-args acc) (list arg))])
      (uir-call acc (list arg))))

;; ── Arguments ───────────────────────────────────────────────────────

(define (lower-arguments node)
  (when node
    (for/list ([a (in-list (rule-items node 'argument))])
      (lower-argument a))))

(define (rule-items node item-tag)
  (when node
    (define first (find-node node item-tag))
    (define rest
      (for/list ([g (in-list (all-list-items node))]
                 #:when (and (node? g) (eq? (node-tag g) 'group)))
        (find-node g item-tag)))
    (filter values (cons first rest))))

(define (lower-argument node)
  (define id (find-node-in node '(identifier qimplicit)))
  (define eq (find-token node 'EQ))
  (cond
    [(and id eq)
     (uir-named-arg (if (eq? (node-tag id) 'qimplicit)
                        (uir-implicit-ref (uir-symbol (qimplicit-name id)))
                        (uir-symbol (identifier-text id)))
                    (lower-expr (find-node node 'expr)))]
    [else
     (define ae (find-node node 'aexpr))
     (if ae (lower-aexpr ae) (uir-null))]))

(define (lower-aexpr node)
  (define expr (find-node node 'expr))
  (define annot (find-node node 'annot))
  (define type-text (and annot
                         (find-node annot 'typescheme)
                         (lower-type-text (find-node annot 'typescheme))))
  (define value (if expr (lower-expr expr) (uir-null)))
  (if type-text (uir-ascribe value (uir-symbol type-text)) value))

;; ── Atoms ───────────────────────────────────────────────────────────

(define (lower-atom node)
  (define kid (single-node node))
  (cond
    [(not kid) (uir-null)]
    [else
     (case (node-tag kid)
       [(name) (lower-name kid)]
       [(literal) (lower-literal kid)]
       [(mask) (mask-atom kid)]
       [(ctxexpr)
        (define inner (find-node kid 'atom))
        (uir-call (uir-symbol "@cctx-create") (list (lower-atom inner)))]
       [(ctxhole) (uir-call (uir-symbol "@cctx-hole-create") '())]
       [(etahole) (uir-var (uir-symbol "_"))]
       [(aexprs) (lower-parens-atom kid)]
       [(cexprs) (uir-list (lower-aexpr-list kid))]
       [else (uir-symbol (format "?atom/~a" (node-tag kid)))])]))

(define (lower-name node)
  (define kid (single-node node))
  (case (and kid (node-tag kid))
    [(qidentifier qconstructor) (uir-var (uir-symbol (identifier-text kid)))]
    [(qimplicit) (uir-implicit-ref (uir-symbol (qimplicit-name kid)))]
    [else (uir-var (uir-symbol (identifier-text node)))]))

(define (lower-parens-atom node)
  (define items (lower-aexpr-list node))
  (case (length items)
    [(0) (uir-null)]
    [(1) (first items)]
    [else (uir-call (uir-symbol (tuple-class-name (length items))) items)]))

(define (lower-aexpr-list node)
  (define first (find-node node 'aexpr))
  (define rest
    (for/list ([g (in-list (all-list-items node))]
               #:when (and (node? g) (eq? (node-tag g) 'group)))
      (find-node g 'aexpr)))
  (for/list ([a (in-list (filter values (cons first rest)))])
    (lower-aexpr a)))

(define (tuple-class-name n)
  (string-append "(" (make-string (sub1 n) #\,) ")"))

;; A bare `mask<eff>` atom: an effect-row injection with no action yet. The
;; action arrives either as an application postfix (`mask<e>(action)`) or as a
;; `with mask<e>` statement (handler.rkt rewrites form/body).
(define (mask-atom node)
  (define-values (effects behind) (lower-mask-spec node))
  (uir-mask effects behind 'apply (uir-null)))

;; `mask : (MASK | 'inject') behind LT targuments1 GT` — the mask carries a
;; comma-separated list of effect types (upstream allows `mask<a,b,div>`).
(define (lower-mask-spec node)
  (define mask-node (find-node node 'mask))
  (define src (or mask-node node))
  (define args (find-node-src src))
  (define behind (and (find-node src 'behind)
                      (for/or ([c (in-list (children (find-node src 'behind)))])
                        (and (token-struct? c) (string=? (tok-value c) "behind")))))
  (values (for/list ([text (in-list (if args (lower-type-args args) '()))])
            (uir-symbol text))
          (and behind #t)))

(define (find-node-src src)
  (or (find-node src 'targuments1) (find-node src 'targuments)))

(define (split-effect-list text)
  (if (string=? text "") '() (regexp-split #rx", " text)))

;; ── Control flow ────────────────────────────────────────────────────
