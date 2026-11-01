#lang racket

;; racklr/emit-koka/item.rkt — UIR declarations → Koka 3.x source text.
;;
;; Statements are one-line units; a block is the braced one-line spelling from
;; common.rkt, so the layout pass never inserts anything inside a declaration.
;; Declaration modifiers travel on `uir-decorated` (one symbol per modifier, a
;; `uir-type-params` node for explicit binders); they are re-ordered here into
;; Koka's canonical declaration order (`pub inline tail fip fun …`).

(require racklr/tree
         racklr/uir
         racklr/emit-effects
         "common.rkt"
         "expr.rkt")

(provide emit-stmt emit-block install-item-hooks!)

(define (install-item-hooks!)
  (emit-stmt-fn emit-stmt)
  (emit-block-fn emit-block))

;; ── Statements ──────────────────────────────────────────────────────

(define (emit-block node)
  (block-spelling (emit-stmt-strings (uir-block-stmts node))))

(define (emit-stmt node)
  (define-values (mods tps inner) (decorators-of node))
  (cond
    [(uir-module? inner) (emit-module inner)]
    [(uir-import? inner) (emit-import mods inner)]
    [(uir-import-alias? inner)
     (format "~aimport ~a = ~a" (pub-prefix mods)
             (emit-type (uir-import-alias-alias inner))
             (emit-type (uir-import-alias-source inner)))]
    [(uir-extern-import? inner) (emit-extern-import inner)]
    [(uir-fixity? inner) (emit-fixity mods inner)]
    [(uir-struct? inner) (emit-struct mods tps inner)]
    [(uir-enum? inner) (emit-enum mods tps inner)]
    [(uir-type-alias? inner) (emit-alias mods tps inner)]
    [(uir-effect-decl? inner) (emit-effect-decl mods tps inner)]
    [(uir-extern-value? inner) (emit-extern-value mods inner)]
    [(uir-extern-fun? inner) (emit-extern-fun mods tps inner)]
    [(uir-fn? inner)
     ;; A bare lambda in statement position is an expression, not a `fun`
     ;; declaration.
     (if (uir-fn-name inner)
         (emit-named-fn mods tps inner)
         (emit-expr inner))]
    [(uir-let? inner) (emit-let mods inner)]
    [(uir-ann-set!? inner) (emit-ann-set mods inner)]
    ;; A bare handler/mask statement without a `with` body is an expression.
    [(or (uir-handler? inner) (uir-mask? inner)) (emit-expr node)]
    [(uir-effect-handler? inner) (emit-expr node)]
    [else (emit-expr node)]))

(define (pub-prefix mods) (if (member "pub" mods) "pub " ""))

;; Canonical declaration modifier order (Koka parser order).
(define (decl-modifier-prefix mods order)
  (define parts '())
  (for ([key (in-list order)])
    (cond
      [(equal? key "fip")
       (for ([m (in-list mods)]
             #:when (regexp-match? #rx"^(fip|fbip)" m))
         (set! parts (cons m parts)))]
      [(member key mods) (set! parts (cons key parts))]
      [else (void)]))
  (if (null? parts)
      ""
      (string-append (string-join (reverse parts) " ") " ")))

(define (params-list-text ps)
  (if (pair? ps)
      (format "<~a>" (string-join (map emit-type ps) ", "))
      ""))

(define (type-params-text tps)
  (if (pair? tps)
      (format "<~a>"
              (string-join
               (append* (for/list ([tp (in-list tps)])
                          (map emit-type (uir-type-params-params tp))))
               ", "))
      ""))

;; ── Program / imports ───────────────────────────────────────────────

(define (emit-import mods node)
  (define conds (for/list ([m (in-list mods)] #:when (regexp-match? #rx"^cond:" m))
                  (substring m 5)))
  (if (null? conds)
      (format "~aimport ~a" (pub-prefix mods) (emit-type (uir-import-source node)))
      (format "~aimport ~a { ~a }"
              (pub-prefix mods)
              (emit-type (uir-import-source node))
              (string-join conds "; "))))

(define (emit-module node)
  (format "module ~a" (emit-type (uir-module-name node))))

(define (emit-extern-import node)
  (define target (uir-extern-import-target node))
  (define entries (uir-extern-import-entries node))
  ;; The layout pass turns the indented body into the extern-import block
  ;; braces (`extern import` NEWLINE INDENT target ...), so the emitted form is
  ;; `extern import` then one indented target group per line.
  (cond
    [(not target)
     (format "extern import = ~a"
             (string-join
              (for/list ([kv (in-list entries)])
                (format "~a ~s" (emit-type (car kv)) (string-value (cdr kv))))
              "; "))]
    [else
     (string-append
      "extern import"
      (string-join
       (for/list ([kv (in-list entries)])
         (format "\n  ~a { ~a = ~s }"
                 (emit-type target)
                 (emit-type (car kv))
                 (string-value (cdr kv))))
       ""))]))

(define (string-value v)
  (cond [(uir-string? v) (uir-string-value v)]
        [else (format "~a" v)]))

;; ── Fixity ──────────────────────────────────────────────────────────

(define (emit-fixity mods node)
  (format "~a~a ~a ~a"
          (pub-prefix mods)
          (symbol->string (uir-fixity-kind node))
          (uir-fixity-precedence node)
          (string-join (map emit-type (uir-fixity-names node)) ", ")))

;; ── Types / structs / effects ───────────────────────────────────────

(define (emit-enum mods tps node)
  (define prefix
    (string-append (pub-prefix mods)
                   (decl-modifier-prefix mods '("abstract" "open" "extend" "co" "div" "lazy"))
                   "type "))
  (define kinds (for/list ([m (in-list mods)] #:when (regexp-match? #rx"^::" m)) m))
  (define head (format "~a~a~a~a"
                       prefix
                       (emit-type (uir-enum-name node))
                       (type-params-text tps)
                       (if (pair? kinds) (format " ~a" (string-join kinds " ")) "")))
  (define variants (uir-enum-variants node))
  (if (null? variants)
      head
      (format "~a { ~a }" head
              (string-join (map emit-variant variants) "; "))))

(define (emit-variant variant)
  (define-values (mods tps inner) (decorators-of variant))
  (define v (if (uir-enum-variant? inner) inner variant))
  (define fields (uir-enum-variant-fields v))
  (define discriminant (uir-enum-variant-discriminant v))
  (format "~a~a~a~a~a~a~a"
          (pub-prefix mods)
          (if (member "lazy" mods) "lazy " "")
          (if (member "con" mods) "con " "")
          (emit-type (uir-enum-variant-name v))
          (type-params-text tps)
          (if (pair? fields) (format "(~a)" (emit-params fields)) "")
          (if discriminant (format " -> ~a" (emit-expr discriminant)) "")))

(define (emit-struct mods tps node)
  (format "~a~a~a~a(~a)"
          (if (eq? (uir-struct-visibility node) 'public) "pub " "")
          (decl-modifier-prefix mods '("abstract" "value" "reference"))
          "struct "
          (format "~a~a" (emit-type (uir-struct-name node))
                  (params-list-text (uir-struct-type-params node)))
          (emit-params (uir-struct-fields node))))

(define (emit-alias mods tps node)
  (format "~a~aalias ~a~a = ~a"
          (pub-prefix mods)
          ""
          (emit-type (uir-type-alias-name node))
          (params-list-text (uir-type-alias-type-params node))
          (emit-type (uir-type-alias-value node))))

(define (emit-effect-decl mods tps node)
  (define name (emit-type (uir-effect-decl-name node)))
  (define kinds (for/list ([m (in-list mods)] #:when (regexp-match? #rx"^::" m)) m))
  (define in-clause (for/first ([m (in-list mods)] #:when (regexp-match? #rx"^in " m)) m))
  (define head
    (format "~a~a~aeffect~a~a~a~a"
            (if (member "named" mods) "named " "")
            (if (member "linear" mods) "linear " "")
            (if (member "div" mods) "div " "")
            (if (equal? name "") "" (format " ~a" name))
            (type-params-text tps)
            (if (pair? kinds) (format " ~a" (string-join kinds " ")) "")
            (if in-clause (format " ~a" in-clause) "")))
  (if (member "single" mods)
      (format "~a ~a" head (emit-effect-op (first (uir-effect-decl-ops node))))
      (format "~a { ~a }" head
              (string-join (map emit-effect-op (uir-effect-decl-ops node)) "; "))))

(define (emit-effect-op op)
  (define-values (mods tps inner) (decorators-of op))
  (define o (if (uir-effect-op? inner) inner op))
  (define name (emit-type (uir-effect-op-name o)))
  (define result (if (uir-effect-op-result-type o)
                     (format " : ~a" (emit-type (uir-effect-op-result-type o)))
                     ""))
  (define params (format "(~a)" (emit-params (uir-effect-op-params o))))
  (cond
    [(member "val" mods)
     (format "~aval ~a~a~a" (pub-prefix mods) name (type-params-text tps) result)]
    [(or (member "ctl" mods) (member "final" mods) (member "raw" mods))
     (format "~a~actl ~a~a~a~a"
             (pub-prefix mods)
             (cond [(member "final" mods) "final "] [(member "raw" mods) "raw "] [else ""])
             name (type-params-text tps) params result)]
    [else
     (format "~afun ~a~a~a~a" (pub-prefix mods) name (type-params-text tps) params result)]))

;; ── Extern declarations ─────────────────────────────────────────────

(define (extern-prefix visibility)
  (if (eq? visibility 'public) "pub " ""))

(define (extern-modifier-text modifiers)
  (decl-modifier-prefix (map emit-type modifiers) '("inline" "noinline" "tail" "fip")))

(define (extern-entries-text entries)
  (cond
    [(null? entries) ""]
    [else
     (format " { ~a }"
             (string-join
              (for/list ([kv (in-list entries)])
                (define key (emit-type (car kv)))
                (define inline? (regexp-match? #rx"^inline:" key))
                (define target (if inline? (substring key 7) key))
                (format "~a~a~s"
                        (if (equal? target "") "" (format "~a " target))
                        (if inline? "inline " "")
                        (string-value (cdr kv))))
              "; "))]))

(define (emit-extern-value mods node)
  (format "~a~aextern ~a : ~a~a"
          (extern-prefix (uir-extern-value-visibility node))
          (extern-modifier-text (uir-extern-value-modifiers node))
          (emit-type (uir-extern-value-name node))
          (emit-type (uir-extern-value-type node))
          (extern-entries-text (uir-extern-value-entries node))))

(define (emit-extern-fun mods tps node)
  (format "~a~aextern ~a~a(~a)~a~a"
          (extern-prefix (uir-extern-fun-visibility node))
          (extern-modifier-text (uir-extern-fun-modifiers node))
          (emit-type (uir-extern-fun-name node))
          (params-list-text (uir-extern-fun-type-params node))
          (emit-params (uir-extern-fun-params node))
          (if (uir-extern-fun-result node)
              (format " : ~a" (emit-type (uir-extern-fun-result node)))
              "")
          (extern-entries-text (uir-extern-fun-entries node))))

;; ── Functions / bindings ────────────────────────────────────────────

(define (emit-named-fn mods tps node)
  (define name (and (uir-fn-name node) (emit-type (uir-fn-name node))))
  (define prefix
    (string-append
     (decl-modifier-prefix mods '("pub" "abstract" "inline" "noinline" "tail" "fip"))
     (if tps "" "")))
  (define row (uir-fn-effects node))
  (define ret (uir-fn-return-type node))
  (define documented (koka-row-documented row))
  (format "~a~a~afun ~a~a(~a)~a ~a"
          (if documented (string-append documented "\n") "")
          prefix
          ""
          name
          (type-params-text tps)
          (emit-params (uir-fn-params node))
          (emit-ret-annotation row ret)
          (emit-body (uir-fn-body node))))

(define (emit-let mods node)
  (define name (emit-name-node (uir-let-name node)))
  (define value (emit-expr (uir-let-value node)))
  (define body (uir-let-body node))
  (if (and body (not (uir-null? body)))
      (format "(val ~a = ~a in ~a)" name value (emit-expr body))
      (if (member "var" mods)
          (format "var ~a := ~a" name value)
          (format "val ~a = ~a" name value))))

(define (emit-ann-set mods node)
  (format "~a~a : ~a ~a ~a"
          (if (member "var" mods) "var " "val ")
          (emit-expr (uir-ann-set!-lhs node))
          (emit-type (uir-ann-set!-type node))
          (if (member "var" mods) ":=" "=")
          (emit-expr (uir-ann-set!-value node))))
