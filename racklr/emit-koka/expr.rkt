#lang racket

;; racklr/emit-koka/expr.rkt — UIR expressions → Koka 3.x source text.
;;
;; Every block is a braced one-line `{ a; b; c }`, so the layout pass never
;; inserts implicit braces or semicolons inside a declaration. Operator
;; applications are always parenthesized (`(a + b)`), so emission needs no
;; precedence table. Applications render back in Koka's own surface forms:
;;   - the tuple constructor `(,)` … → `(a, b)`
;;   - upstream's `ctx`/`hole` desugaring (`@cctx-create`) → `ctx expr` / `hole`
;;   - `mask<e>(a)` for a mask application; `with mask<e>` is emitted by the
;;     declaration pass when the mask covers a block.

(require racklr/uir
         racklr/emit-effects
         "common.rkt")

(provide emit-expr
         emit-blockexpr
         emit-ret-annotation
         emit-handler
         emit-body
         emit-stmt-strings
         install-expr-hooks!)

(define (install-expr-hooks!)
  (emit-expr-fn emit-expr)
  (emit-handler-expr-fn emit-handler))

;; ── Dispatch ────────────────────────────────────────────────────────

(define (emit-expr uir)
  (cond
    [(uir-number? uir) (uir-number-value uir)]
    [(uir-string? uir) (emit-string-value (uir-string-value uir))]
    [(uir-char? uir) (uir-char-value uir)]
    [(uir-bool? uir) (if (uir-bool-value uir) "True" "False")]
    [(uir-null? uir) "()"]
    [(uir-var? uir) (emit-type (uir-var-name uir))]
    [(uir-implicit-ref? uir) (format "?~a" (emit-name-node (uir-implicit-ref-name uir)))]
    [(uir-symbol? uir) (uir-symbol-name uir)]
    [(uir-paren? uir) (format "(~a)" (emit-expr (uir-paren-inner uir)))]
    [(uir-get? uir) (format "~a.~a"
                            (emit-atom (uir-get-base uir))
                            (emit-type (uir-get-field uir)))]
    [(uir-ascribe? uir) (format "(~a : ~a)"
                                (emit-expr (uir-ascribe-expr uir))
                                (emit-type (uir-ascribe-type uir)))]
    [(uir-index? uir) (format "~a[~a]"
                              (emit-atom (uir-index-base uir))
                              (emit-expr (uir-index-index uir)))]
    [(uir-set!? uir) (format "~a := ~a"
                             (emit-expr (uir-set!-name uir))
                             (emit-expr (uir-set!-value uir)))]
    [(uir-return? uir) (format "return ~a" (emit-operand (uir-return-value uir)))]
    [(uir-call? uir) (emit-call uir)]
    [(uir-if? uir) (emit-if uir)]
    [(uir-match? uir) (emit-match uir)]
    [(uir-block? uir) (emit-blockexpr uir)]
    [(uir-fn? uir) (let-values ([(mods tps inner) (decorators-of uir)])
                     (emit-lambda mods tps inner))]
    [(uir-effect-handler? uir) (emit-legacy-handler uir)]
    [(uir-handler? uir) (emit-handler uir)]
    [(uir-mask? uir) (emit-mask-expr uir)]
    [(uir-list? uir)
     (format "[~a]" (string-join (map emit-expr (uir-list-items uir)) ", "))]
    [(uir-record? uir) (emit-record uir)]
    [(uir-effect-row? uir) (format "<~a>" (koka-row-string uir))]
    [(uir-decorated? uir)
     ;; a decorated expression: modifiers are declaration-level, so fall through
     (emit-expr (flatten-decorated uir))]
    [else (format "?~a" (uir-tag uir))]))

;; Operand position of an infix/prefix operator is a `prefixexpr` (an
;; application/atom), so anything else — a lambda, `if`, `match`, handler,
;; block, assignment — needs parentheses to re-parse.
(define (emit-operand uir)
  (cond
    [(or (uir-number? uir) (uir-string? uir) (uir-char? uir) (uir-bool? uir)
         (uir-null? uir) (uir-var? uir) (uir-symbol? uir) (uir-implicit-ref? uir)
         (uir-call? uir) (uir-index? uir) (uir-get? uir) (uir-list? uir)
         (uir-paren? uir) (uir-ascribe? uir))
     (emit-expr uir)]
    [else (format "(~a)" (emit-expr uir))]))

(define (emit-atom uir)
  (cond
    [(or (uir-number? uir) (uir-string? uir) (uir-char? uir) (uir-bool? uir)
         (uir-null? uir) (uir-var? uir) (uir-symbol? uir))
     (emit-expr uir)]
    [(uir-index? uir) (emit-expr uir)]
    [(uir-get? uir) (emit-expr uir)]
    [(uir-call? uir) (emit-expr uir)]
    [(uir-ascribe? uir) (emit-expr uir)]
    [(uir-list? uir) (emit-expr uir)]
    [else (format "(~a)" (emit-expr uir))]))

(define (emit-blockexpr uir)
  (if (uir-block? uir)
      (block-spelling (emit-stmt-strings (uir-block-stmts uir))
                      (statement-list-expression? uir))
      (emit-expr uir)))

(define (emit-stmt-strings stmts)
  (append* (map expand-stmt stmts)))

(define (expand-stmt s)
  (define-values (_mods _tps inner) (decorators-of s))
  (cond
    [(and (uir-handler? inner)
          (memq (uir-handler-sort inner) '(with with-bind))
          (uir-handler-body inner))
     (define body (uir-handler-body inner))
     (append (list (emit-with-header s))
             (if (uir-block? body)
                 (emit-stmt-strings (uir-block-stmts body))
                 (list (emit-expr body))))]
    [(and (uir-effect-handler? inner)
          (pair? (uir-block-stmts (uir-effect-handler-body inner))))
     (append (list (format "with handler { ~a }"
                           (join-semis (map emit-handler-op
                                            (for/list ([op (in-list (uir-effect-handler-ops inner))])
                                              (uir-handler-op 'fun (first op) #f (second op) (third op)))))))
             (emit-stmt-strings (uir-block-stmts (uir-effect-handler-body inner))))]
    [(and (uir-mask? inner) (eq? (uir-mask-form inner) 'with) (uir-mask-body inner))
     (if (uir-block? (uir-mask-body inner))
         (append (list (emit-mask-header inner))
                 (emit-stmt-strings (uir-block-stmts (uir-mask-body inner))))
         ;; `with mask<e> in expr` stays one expression statement.
         (list (format "(~a in ~a)"
                       (emit-mask-header inner)
                       (emit-expr (uir-mask-body inner)))))]
    [else (list ((emit-stmt-fn) s))]))

;; ── Applications ────────────────────────────────────────────────────

;; `emit-operator?`: a bare symbol operator (the `OP` surface), not a declared
;; operator name like `(==)` (which is an ordinary function value).
(define (operator-symbol? s)
  (and (string? s)
       (positive? (string-length s))
       (not (char=? (string-ref s 0) #\())
       (regexp-match? #rx"^[-+*/=<>!&|.^~%$#:?]+$" s)))

(define (prefix-operator? s) (member s '("!" "~")))

(define (emit-call uir)
  (define callee (uir-call-callee uir))
  (define args (uir-call-args uir))
  (cond
    [(and (uir-symbol? callee) (equal? (uir-symbol-name callee) "@cctx-create"))
     (format "ctx ~a" (emit-atom (first args)))]
    [(and (uir-symbol? callee) (equal? (uir-symbol-name callee) "@cctx-hole-create"))
     "hole"]
    [(and (uir-symbol? callee) (tuple-class? (uir-symbol-name callee)))
     (format "(~a)" (string-join (map emit-expr args) ", "))]
    [(and (uir-symbol? callee)
          (prefix-operator? (uir-symbol-name callee))
          (= (length args) 1))
     (format "~a~a" (uir-symbol-name callee) (emit-operand (first args)))]
    [(and (uir-symbol? callee)
          (operator-symbol? (uir-symbol-name callee))
          (= (length args) 2))
     (format "(~a ~a ~a)"
             (emit-operand (first args))
             (uir-symbol-name callee)
             (emit-operand (second args)))]
    [else
     (format "~a(~a)"
             (emit-callee callee)
             (string-join (map emit-argument args) ", "))]))

(define (emit-callee callee)
  (cond
    [(uir-symbol? callee) (uir-symbol-name callee)]
    [(uir-var? callee) (emit-type (uir-var-name callee))]
    [(uir-get? callee) (emit-expr callee)]
    [else (emit-atom callee)]))

(define (emit-argument a)
  (cond
    [(uir-named-arg? a)
     (format "~a = ~a"
             (emit-name-node (uir-named-arg-name a))
             (emit-expr (uir-named-arg-value a)))]
    [else (emit-expr a)]))

(define (emit-record uir)
  ;; uir-record has no Koka surface form; entries render as a constructor-like
  ;; application of the record type name (documented loss: Koka records are
  ;; defined by `type`/`struct`, not literals).
  (format "record(~a)"
          (string-join
           (for/list ([kv (in-list (uir-record-entries uir))])
             (format "~a = ~a" (emit-expr (car kv)) (emit-expr (cdr kv))))
           ", ")))

;; ── Control flow ────────────────────────────────────────────────────

(define (emit-if uir)
  (define test (emit-expr (uir-if-test uir)))
  (define then (uir-if-then uir))
  (define else (uir-if-else uir))
  (cond
    [(and (uir-return? then) (uir-null? else))
     (format "(if ~a then return ~a)" test (emit-expr (uir-return-value then)))]
    [(uir-null? else)
     (format "(if ~a then ~a)" test (emit-branch then))]
    [else
     (format "(if ~a then ~a else ~a)" test (emit-branch then) (emit-branch else))]))

(define (emit-branch uir)
  (cond
    [(uir-block? uir) (emit-blockexpr uir)]
    [else (emit-expr uir)]))

(define (emit-match uir)
  (format "match ~a { ~a }"
          (emit-atom (uir-match-subject uir))
          (string-join (map emit-case (uir-match-cases uir)) "; ")))

(define (emit-case c)
  (define pattern (emit-pattern (uir-case-pattern c)))
  (define guard (uir-case-guard c))
  (define body (uir-case-body c))
  (format "~a~a -> ~a"
          pattern
          (if (or (not guard) (uir-null? guard))
              ""
              (format " | ~a" (emit-expr guard)))
          (emit-branch body)))

;; ── Lambdas ─────────────────────────────────────────────────────────

(define (emit-lambda mods tps fn)
  (define params (emit-params (uir-fn-params fn)))
  (define type-params
    (if (pair? tps)
        (format "<~a>" (string-join
                        (append* (for/list ([tp (in-list tps)])
                                   (map emit-type (uir-type-params-params tp))))
                        ", "))
        ""))
  (format "fn~a(~a)~a ~a"
          type-params params
          (emit-ret-annotation (uir-fn-effects fn) (uir-fn-return-type fn))
          (emit-body (uir-fn-body fn))))

(define (emit-body uir)
  (if (uir-block? uir)
      (block-spelling (emit-stmt-strings (uir-block-stmts uir))
                      (statement-list-expression? uir))
      (emit-expr uir)))

;; The typed effect-row position: `: <row> ret` / `: total ret` / `: ret`.
(define (emit-ret-annotation row ret)
  (cond
    [(and (uir-effect-row? row) (not (koka-row-empty? row)))
     (cond
       [ret (format " : ~a ~a" (koka-row-string row) (emit-type ret))]
       ;; no result type after the row: Koka has no typed position for it
       [else ""])]
    [(and (uir-effect-row? row) (koka-row-empty? row) ret)
     (format " : total ~a" (emit-type ret))]
    [ret (format " : ~a" (emit-type ret))]
    [else ""]))

;; ── Handlers / masks ────────────────────────────────────────────────

;; Legacy `uir-effect-handler` (effects test fixtures): `handler { ... }`.
(define (emit-legacy-handler uir)
  (string-append "handler "
                 (string-append
                  "{ "
                  (join-semis
                   (append
                    (for/list ([op (in-list (uir-effect-handler-ops uir))])
                      (format "fun ~a(~a) ~a"
                              (emit-type (first op))
                              (emit-params (second op))
                              (emit-expr (third op))))
                    (if (uir-effect-handler-return-clause uir)
                        (list (format "return(x) ~a"
                                      (emit-expr (uir-effect-handler-return-clause uir))))
                        '())
                    (map (emit-stmt-fn) (uir-block-stmts (uir-effect-handler-body uir)))))
                  " }")))

;; Handler in expression position: `handler<e> { ... }` / `handle<e>(a) { ... }`.
(define (emit-handler node)
  (define-values (mods _tps h) (decorators-of node))
  (emit-handler-expr node mods h))

(define (emit-handler-expr decorated mods h)
  (define named? (member "named" mods))
  (define override? (member "override" mods))
  (define effect (let ([e (uir-handler-effect h)])
                   (if e (format "<~a>" (emit-type e)) "")))
  (define prefix
    (string-append (if (and named? #t) "named " "")
                   (if (and override? #t) "override " "")))
  (define ops (format "{ ~a }"
                      (join-semis (map emit-handler-op (uir-handler-ops h)))))
  (case (uir-handler-sort h)
    [(handler) (format "~ahandler~a ~a" prefix effect ops)]
    [(handle) (format "~ahandle~a(~a) ~a"
                      prefix effect (emit-expr (uir-handler-body h)) ops)]
    [(with-bind)
     (format "(with ~a <- ~a in ~a)"
             (emit-param (first (uir-handler-locals h)))
             (emit-expr (uir-handler-value h))
             (emit-blockexpr (uir-handler-body h)))]
    [else
     (format "(with ~ahandler~a ~a in ~a)"
             prefix effect ops
             (emit-blockexpr (uir-handler-body h)))]))

(define (emit-handler-op op)
  (define args (emit-params (uir-handler-op-params op)))
  (define body (emit-expr (uir-handler-op-body op)))
  (case (uir-handler-op-kind op)
    [(val) (format "val ~a~a = ~a"
                   (emit-type (uir-handler-op-name op))
                   (if (uir-handler-op-type op)
                       (format " : ~a" (emit-type (uir-handler-op-type op)))
                       "")
                   body)]
    [(return) (format "return(~a) ~a" args body)]
    [(initially) (format "initially(~a) ~a" args body)]
    [(finally) (format "finally ~a" body)]
    [(ctl) (format "ctl ~a(~a) ~a" (emit-type (uir-handler-op-name op)) args body)]
    [(final-ctl) (format "final ctl ~a(~a) ~a" (emit-type (uir-handler-op-name op)) args body)]
    [(raw-ctl) (format "raw ctl ~a(~a) ~a" (emit-type (uir-handler-op-name op)) args body)]
    [else (format "fun ~a(~a) ~a" (emit-type (uir-handler-op-name op)) args body)]))

;; The statement form of a `with` handler, without its body.
(define (emit-with-header decorated)
  (define-values (mods _tps inner) (decorators-of decorated))
  (define named? (member "named" mods))
  (define override? (member "override" mods))
  (define effect (let ([e (uir-handler-effect inner)])
                   (if e (format "<~a>" (emit-type e)) "")))
  (define ops (format "{ ~a }"
                      (join-semis (map emit-handler-op (uir-handler-ops inner)))))
  (if (memq (uir-handler-sort inner) '(with-bind))
      (format "with ~a <- ~ahandler~a ~a"
              (emit-param (first (uir-handler-locals inner)))
              (if (and named? #t) "named " "")
              effect ops)
      (format "with ~a~ahandler~a ~a"
              (if (and override? #t) "override " "")
              (if (and named? #t) "named " "")
              effect ops)))

(define (emit-mask-header mask)
  (format "with mask<~a>"
          (string-join (map emit-type (uir-mask-effects mask)) ", ")))

(define (emit-mask-expr mask)
  (define effects (string-join (map emit-type (uir-mask-effects mask)) ", "))
  (define behind (if (uir-mask-behind mask) "behind" ""))
  (cond
    [(eq? (uir-mask-form mask) 'with)
     (format "(with mask~a<~a> in ~a)" behind effects
             (emit-blockexpr (uir-mask-body mask)))]
    [(and (eq? (uir-mask-form mask) 'apply) (not (uir-null? (uir-mask-body mask))))
     (format "mask~a<~a>(~a)" behind effects (emit-expr (uir-mask-body mask)))]
    [else (format "mask~a<~a>" behind effects)]))
