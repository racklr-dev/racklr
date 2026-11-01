#lang racket

;; Koka UIR → text emitter (Phase 1). Inverse of lower-koka.rkt.
;; Precedence is intentionally flat: the pass-1 grammar parses operators
;; left-associatively with no precedence, so infix emission without
;; parentheses round-trips for every shape the lowerer produces.

(require racklr/uir
         racklr/emit-contract)

(provide emit-koka)

(define operators '("==" "!=" "+" "-" "*" "/"))

(define (operator? s) (member s operators))

(define (indent s)
  (string-join (map (λ (l) (string-append "  " l))
                    (string-split s "\n"))
               "\n"))

(define (escape-string s)
  (list->string
   (append* (for/list ([c (string->list s)])
              (cond [(char=? c #\\) '(#\\ #\\)]
                    [(char=? c #\") '(#\\ #\")]
                    [else (list c)])))))

;; ── Top level ────────────────────────────────────────────────────────

;; Implements the emit contract (racklr/emit-contract): UIR plus an optional
;; explicit effect row in, Koka source out. Koka's native row vocabulary is the
;; function effect annotation, so an explicit row is rendered as the entry
;; function's `: <effects>` (or `: total` for an empty row).
(define (emit-koka uir [row #f])
  (define source (if row (attach-root-row 'emit-koka uir row) uir))
  (if (uir-block? source)
      (string-join (map emit-stmt (uir-block-stmts source)) "\n")
      (emit-expr source)))

(define (emit-stmt uir)
  (cond
    [(uir-let? uir)
     (format "val ~a = ~a"
             (uir-symbol-name (uir-let-name uir))
             (emit-expr (uir-let-value uir)))]
    [(uir-fn? uir) (emit-fn uir)]
    [(uir-effect-decl? uir) (emit-effect-decl uir)]
    [(uir-enum? uir) (emit-enum uir)]
    [(uir-import? uir) (format "import ~a" (uir-symbol-name (uir-import-source uir)))]
    [(uir-module? uir) (format "module ~a" (uir-symbol-name (uir-module-name uir)))]
    [else (emit-expr uir)]))

;; ── Declarations ─────────────────────────────────────────────────────

(define (emit-fn uir)
  (define name (and (uir-fn-name uir) (uir-symbol-name (uir-fn-name uir))))
  (define head (if name (format "fun ~a" name) "fn"))
  (define ret (emit-rettype (uir-fn-return-type uir) (uir-fn-effects uir)))
  (string-append (format "~a(~a)" head (emit-params (uir-fn-params uir)))
                 (if ret (format " : ~a" ret) "")
                 "\n"
                 (emit-block (uir-fn-body uir))))

(define (emit-effect-decl uir)
  (string-append (format "effect ~a" (uir-symbol-name (uir-effect-decl-name uir)))
                 "\n"
                 (indent (string-join (map emit-effect-op (uir-effect-decl-ops uir)) "\n"))))

(define (emit-effect-op op)
  (format "fun ~a(~a) : ~a"
          (uir-symbol-name (uir-effect-op-name op))
          (emit-params (uir-effect-op-params op))
          (emit-type (uir-effect-op-result-type op))))

(define (emit-enum uir)
  (string-append (format "type ~a" (uir-symbol-name (uir-enum-name uir)))
                 "\n"
                 (indent (string-join (map emit-variant (uir-enum-variants uir)) "\n"))))

(define (emit-variant v)
  (define name (uir-symbol-name (uir-enum-variant-name v)))
  (define fields (uir-enum-variant-fields v))
  (if (null? fields) name (format "~a(~a)" name (emit-params fields))))

;; ── Types ────────────────────────────────────────────────────────────

(define (emit-type t)
  (if (uir-symbol? t) (uir-symbol-name t) "?"))

(define (emit-rettype ret effects)
  (cond
    [effects (string-append (emit-effects effects)
                            (if ret (format " ~a" (emit-type ret)) ""))]
    [ret (emit-type ret)]
    [else #f]))

(define (emit-effects row)
  (define entries (uir-effect-row-entries row))
  (if (null? entries)
      "total"
      (string-append "<"
                     (string-join (map (λ (e) (uir-symbol-name (uir-effect-entry-name e))) entries)
                                  ", ")
                     ">")))

(define (emit-params ps)
  (string-join (map emit-param ps) ", "))

(define (emit-param p)
  (define name (uir-symbol-name (uir-typed-param-name p)))
  (define t (uir-typed-param-type p))
  (if t (format "~a: ~a" name (emit-type t)) name))

;; ── Blocks / control ─────────────────────────────────────────────────

(define (emit-block body)
  (if (uir-block? body)
      (indent (string-join (map emit-stmt (uir-block-stmts body)) "\n"))
      (indent (emit-expr body))))

(define (emit-if uir)
  (define then-str (emit-block (uir-if-then uir)))
  (define else (uir-if-else uir))
  (string-append (format "if ~a then" (emit-expr (uir-if-test uir)))
                 "\n" then-str
                 (if (uir-null? else)
                     ""
                     (string-append "\nelse\n" (emit-block else)))))

;; ── Expressions ──────────────────────────────────────────────────────

(define (emit-expr uir)
  (cond
    [(uir-number? uir) (uir-number-value uir)]
    [(uir-string? uir) (string-append "\"" (escape-string (uir-string-value uir)) "\"")]
    [(uir-bool? uir) (if (uir-bool-value uir) "True" "False")]
    [(uir-null? uir) "()"]
    [(uir-var? uir) (uir-symbol-name (uir-var-name uir))]
    [(uir-symbol? uir) (uir-symbol-name uir)]
    [(uir-call? uir) (emit-call uir)]
    [(uir-if? uir) (emit-if uir)]
    [(uir-fn? uir) (emit-fn uir)]
    [(uir-get? uir) (format "~a.~a" (emit-expr (uir-get-base uir)) (emit-expr (uir-get-field uir)))]
    [(uir-paren? uir) (format "(~a)" (emit-expr (uir-paren-inner uir)))]
    [(uir-effect-handler? uir) (emit-handler uir)]
    [(uir-block? uir) (emit-block uir)]
    [else (format "?~a" (uir-tag uir))]))

(define (emit-call uir)
  (define callee (uir-call-callee uir))
  (if (and (uir-symbol? callee) (operator? (uir-symbol-name callee)))
      (format "~a ~a ~a"
              (emit-expr (first (uir-call-args uir)))
              (uir-symbol-name callee)
              (emit-expr (second (uir-call-args uir))))
      (format "~a(~a)"
              (emit-expr callee)
              (string-join (map emit-expr (uir-call-args uir)) ", "))))

;; ── Effect handlers ──────────────────────────────────────────────────

(define (emit-handler uir)
  (define lines '())
  (for ([op (uir-effect-handler-ops uir)])
    (set! lines (cons (emit-handler-op op) lines)))
  (define rc (uir-effect-handler-return-clause uir))
  (when rc
    (set! lines (cons (format "return(x) -> ~a" (emit-expr rc)) lines)))
  (for ([s (uir-block-stmts (uir-effect-handler-body uir))])
    (set! lines (cons (emit-stmt s) lines)))
  (string-append "with handler\n" (indent (string-join (reverse lines) "\n"))))

(define (emit-handler-op op)
  (format "fun ~a(~a) -> ~a"
          (uir-symbol-name (car op))
          (emit-params (cadr op))
          (emit-expr (caddr op))))

(module+ main
  (displayln "racklr/emit-koka — UIR → Koka text emitter loaded."))
