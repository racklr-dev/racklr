#lang racket

;; Clojure UIR → text emitter (Phase 7). Inverse of lower-clojure.rkt.
;; Canonical reader-macro calls are re-sugared: quote → ', deref → @,
;; with-meta → ^, vector → [...], hash-set → #{...}, fn* → #(...).

(require racklr/uir
         racklr/emit-contract
         racklr/emit-effects
         racklr/effects/infer)

(provide emit-clojure)

;; ── Effect-row rendering (Clojure keyword-keyed map) ────────────────
;;
;; The row's spelling is owned by the emit contract (racklr/emit-effects): a
;; keyword-keyed map placed in the `defn` attr-map, where the reader keeps it on
;; the var (the outer-`^` shape is dropped by the reader).

;; ── Top level ────────────────────────────────────────────────────────

;; Implements the emit contract (racklr/emit-contract): UIR plus an optional
;; explicit effect row in, Clojure source out. Clojure's native row vocabulary
;; is the `:__effects__` metadata map on the entry defn, which is where the
;; reader and Jolt actually keep it.
;;
;; Inference also attaches rows to named functions before emission
;; (`#:wrap-anonymous? #f`: Clojure has its own row placement, so the JS/Python
;; guard wrapper convention does not apply).
(define (emit-clojure uir [row #f])
  (define sourced (attach-root-row 'emit-clojure uir row))
  (define annotated (annotate-effects sourced #:wrap-anonymous? #f))
  (if (uir-block? annotated)
      (string-join (map emit-form (uir-block-stmts annotated)) "\n")
      (emit-form annotated)))

;; ── Forms ────────────────────────────────────────────────────────────

(define (emit-form uir)
  (cond
    [(uir-null? uir) "nil"]
    [(uir-bool? uir) (if (uir-bool-value uir) "true" "false")]
    [(uir-number? uir) (uir-number-value uir)]
    [(uir-string? uir) (string-append "\"" (escape-clj-string (uir-string-value uir)) "\"")]
    [(uir-fstring? uir) (string-append "\"" (escape-clj-string (uir-fstring-value uir)) "\"")]
    [(uir-symbol? uir) (uir-symbol-name uir)]
    [(uir-var? uir) (uir-symbol-name (uir-var-name uir))]
    [(uir-list? uir) (string-append "(" (string-join (map emit-form (uir-list-items uir)) " ") ")")]
    [(uir-record? uir)
     (string-append "{"
                    (string-join
                     (map (λ (e) (string-append (emit-form (car e)) " " (emit-form (cdr e))))
                          (uir-record-entries uir))
                     " ")
                    "}")]
    [(uir-call? uir) (emit-call uir)]
    [(uir-fn? uir) (emit-fn uir)]
    [(uir-let? uir)
     (string-append "(let [" (emit-form (uir-let-name uir)) " "
                    (emit-form (uir-let-value uir)) "] "
                    (emit-form (uir-let-body uir)) ")")]
    [(uir-if? uir)
     (string-append "(if " (emit-form (uir-if-test uir)) " "
                    (emit-form (uir-if-then uir)) " "
                    (emit-form (uir-if-else uir)) ")")]
    [(uir-block? uir)
     (string-append "(do " (string-join (map emit-form (uir-block-stmts uir)) " ") ")")]
    [(uir-return? uir) (emit-form (uir-return-value uir))]
    [(uir-set!? uir)
     (string-append "(def " (emit-form (uir-set!-name uir)) " "
                    (emit-form (uir-set!-value uir)) ")")]
    [else (format "?~a" (uir-tag uir))]))

(define (escape-clj-string s)
  ;; Re-escape backslashes and double quotes for a Clojure string literal.
  (list->string
   (append-map (λ (ch)
                 (case ch
                   [(#\\) (list #\\ #\\)
                   ]
                   [(#\") (list #\\ #\")
                   ]
                   [else (list ch)]))
               (string->list s))))

;; ── Calls (re-sugar canonical reader macros) ─────────────────────────

(define (emit-call uir)
  (define callee (uir-call-callee uir))
  (define args (uir-call-args uir))
  (define name (and (uir-symbol? callee) (uir-symbol-name callee)))
  (cond
    [(and name (equal? name "vector")) (string-append "[" (string-join (map emit-form args) " ") "]")]
    [(and name (equal? name "hash-set")) (string-append "#{" (string-join (map emit-form args) " ") "}")]
    [(and name (equal? name "quote")) (string-append "'" (emit-form (first args)))]
    [(and name (equal? name "syntax-quote")) (string-append "`" (emit-form (first args)))]
    [(and name (equal? name "unquote")) (string-append "~" (emit-form (first args)))]
    [(and name (equal? name "unquote-splicing")) (string-append "~@" (emit-form (first args)))]
    [(and name (equal? name "deref")) (string-append "@" (emit-form (first args)))]
    [(and name (equal? name "var")) (string-append "#'" (emit-form (first args)))]
    [(and name (equal? name "regex")) (string-append "#" (emit-form (first args)))]
    [(and name (equal? name "with-meta"))
     ;; args = [target meta]; emit ^meta target
     (string-append "^" (emit-form (second args)) " " (emit-form (first args)))]
    [(and name (equal? name "fn*")) (string-append "#(" (string-join (map emit-form args) " ") ")")]
    [(and name (string-prefix? name "#"))
     ;; dispatch/discard/host-expr: emit as CALLEE form...
     (string-append name " " (string-join (map emit-form args) " "))]
    [name (string-append "(" name (if (null? args) "" (string-append " " (string-join (map emit-form args) " "))) ")")]
    [else (string-append "(" (emit-form callee) " " (string-join (map emit-form args) " ") ")")]))

;; ── Functions ────────────────────────────────────────────────────────

(define (emit-fn uir)
  (define name (uir-fn-name uir))
  (define params (uir-fn-params uir))
  (define params-str
    (string-join (map (λ (p) (cond [(uir-symbol? p) (uir-symbol-name p)]
                                    [(uir-typed-param? p) (uir-symbol-name (uir-typed-param-name p))]
                                    [else (emit-form p)]))
                      params)
                 " "))
  (define body-str (emit-body (uir-fn-body uir)))
  (cond
    [(and (uir-effect-row? (uir-fn-effects uir)) (uir-symbol? name))
     ;; attach the row in the defn attr-map slot: the outer-^ shape is
     ;; dropped by the reader, so Jolt (and Clojure) keep only this one.
     (string-append "(defn " (uir-symbol-name name)
                    " {:__effects__ "
                    (render-effect-row 'clojure (uir-fn-effects uir))
                    "}"
                    " [" params-str "] " body-str ")")]
    [(uir-symbol? name)
     (string-append "(defn " (uir-symbol-name name) " [" params-str "] " body-str ")")]
    [else
     (string-append "(fn [" params-str "] " body-str ")")]))

(define (emit-body body)
  (cond
    [(uir-block? body) (string-join (map emit-form (uir-block-stmts body)) " ")]
    [(uir-null? body) ""]
    [else (emit-form body)]))

(module+ main
  (displayln "racklr/emit-clojure — UIR → Clojure text emitter loaded."))
