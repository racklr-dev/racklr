#lang racket

;; ── The one emit contract ───────────────────────────────────────────
;;
;; Every target emitter in racklr implements the same contract:
;;
;;     render : uir? × (or/c #f uir-effect-row?) × keyword → string?
;;
;; Input:  the UIR value to render, plus the effect row that describes the
;;         emitted unit. `#f` means "the caller supplied no row": the target
;;         uses the rows the UIR already carries (and, for Python and
;;         JavaScript, effect inference, as before). A `uir-effect-row` is the
;;         caller's explicit row; it is authoritative for the unit's entry
;;         function and must never be silently dropped. Target-specific keyword
;;         options (JSON `#:indent`, HTML `#:title`/`#:lang`/`#:external-js`)
;;         are forwarded by the dispatch in racklr/emit.
;; Output: target source text.
;;
;; Each target declares how it reifies an explicit row in its native effect
;; vocabulary (`emit-target-row-syntax`):
;;
;;     guard-decorator    Python            @__withEffects({...})
;;     guard-wrapper      JavaScript, HTML  __withEffects({...}, fn)
;;     effect-annotation  Koka              fun f() : <llm, db> ... (or : total)
;;     metadata           Clojure           {:__effects__ {...}}
;;     none               JSX, DOM JS, JSON, Effervesce: no native form for a
;;                        row, so an explicit row is *refused* with an error
;;                        instead of being dropped.
;;
;; The target table checks the call shape via `make-emit-target`: a renderer
;; must accept two positional arguments (an arity-only check — it cannot prove
;; the row is used). Per-target reification is exercised by
;; racklr-test/emit-contract-test.rkt. The single dispatch function is `emit`
;; in racklr/emit.rkt.

(require racklr/uir)

(provide (except-out (struct-out emit-target) emit-target)
         make-emit-target
         row-syntaxes
         row-syntax?
         attach-root-row
         refuse-explicit-row)

;; ── Contract descriptor ─────────────────────────────────────────────

;; The row-reification strategies a target may declare.
(define row-syntaxes
  '(guard-decorator guard-wrapper effect-annotation metadata none))

(define (row-syntax? s)
  (and (memq s row-syntaxes) #t))

;; One target implementing the contract.
(struct emit-target (target        ; symbol naming the target ('python, 'koka, ...)
                     vocabulary    ; string: what this target emits
                     row-syntax    ; symbol, one of `row-syntaxes`
                     render)       ; the contract: (-> uir? row? keyword ... string?)
  #:transparent)

;; Validate and build a target descriptor. The dynamic check is arity-only: it
;; proves the renderer accepts the UIR and row arguments, not that it reifies
;; the row (a two-argument renderer could ignore its second argument).
;; Reification is checked per target by racklr-test/emit-contract-test.rkt.
(define (make-emit-target target vocabulary row-syntax render)
  (cond
    [(not (symbol? target))
     (error 'make-emit-target "target must be a symbol, got: ~a" target)]
    [(not (and (string? vocabulary) (positive? (string-length vocabulary))))
     (error 'make-emit-target "vocabulary for '~a must be a non-empty string" target)]
    [(not (row-syntax? row-syntax))
     (error 'make-emit-target "target '~a declares unknown row syntax ~a; expected one of ~a"
            target row-syntax row-syntaxes)]
    [(not (and (procedure? render)
               (procedure-arity-includes? render 2)))
     (error 'make-emit-target
            "target '~a does not implement the emit contract shape: render must accept (uir row)"
            target)]
    [else (emit-target target vocabulary row-syntax render)]))

;; ── The explicit row ────────────────────────────────────────────────

;; An explicit row is `#f` (none supplied) or a `uir-effect-row`. Anything else
;; is a caller error; `who` names the entry point in the message.
(define (check-explicit-row who row)
  (cond
    [(eq? row #f) (void)]
    [(uir-effect-row? row) (void)]
    [else
     (error who
            "effect row input must be #f or a uir-effect-row, got: ~a"
            row)]))

;; Attach an explicit effect row to the unit's entry function so the target can
;; reify it in its native vocabulary. The entry function is the value itself
;; when it is function-like, otherwise the first function-like form of a
;; `uir-block` / `uir-module` unit. Raises when the unit has no function-like
;; form: the row describes a function, and dropping it because none was found
;; is exactly the silent loss this contract forbids. `#f` (no row supplied)
;; leaves the unit unchanged.
(define (attach-root-row who uir row)
  (check-explicit-row who row)
  (if (eq? row #f)
      uir
      (let ([attached (form-with-row uir row)])
        (cond
          [attached attached]
          [(uir-block? uir)
           (struct-copy uir-block
                        uir
                        [stmts (attach-in-forms who (uir-block-stmts uir) row)])]
          [(uir-module? uir)
           (struct-copy uir-module
                        uir
                        [body (attach-in-forms who (uir-module-body uir) row)])]
          [else
           (error who
                  "explicit effect row has no function to annotate (unit is ~a)"
                  (uir-tag uir))]))))

;; Rebuild `form` with `row` in its effects slot, or #f when the form cannot
;; carry a row. Function-like forms are: a bare function, a JS-style
;; `(set! name fn)`, the `(call "async-fn"|"gen-fn" (list fn))` wrapper the
;; Python/JavaScript front ends use for named async and generator definitions,
;; and a decorated definition whose inner form is one of those. `uir-method`
;; has no effects slot in the UIR, so it does not qualify.
(define (form-with-row form row)
  (cond
    [(uir-fn? form) (struct-copy uir-fn form [effects row])]
    [(and (uir-set!? form) (form-with-row (uir-set!-value form) row))
     => (λ (value) (struct-copy uir-set! form [value value]))]
    [(and (uir-call? form)
          (uir-symbol? (uir-call-callee form))
          (member (uir-symbol-name (uir-call-callee form)) '("async-fn" "gen-fn"))
          (= (length (uir-call-args form)) 1)
          (form-with-row (first (uir-call-args form)) row))
     => (λ (inner) (struct-copy uir-call form [args (list inner)]))]
    [(and (uir-decorated? form) (form-with-row (uir-decorated-inner form) row))
     => (λ (inner) (struct-copy uir-decorated form [inner inner]))]
    [else #f]))

;; Rebuild the first form that can carry the row. The rest of the unit keeps
;; the rows it already carries; only the entry function is overridden.
(define (attach-in-forms who forms row)
  (cond
    [(null? forms)
     (error who "explicit effect row has no function to annotate in the unit")]
    [(form-with-row (car forms) row)
     => (λ (attached) (cons attached (cdr forms)))]
    [else (cons (car forms) (attach-in-forms who (cdr forms) row))]))

;; A target with no native row vocabulary refuses an explicit row. This is a
;; contract error, not a silent drop: the caller asked for a row the target
;; cannot reify, and the target says so.
(define (refuse-explicit-row who target row)
  (check-explicit-row who row)
  (error who
         "target '~a has no native effect-row vocabulary; refusing to drop the explicit row: ~a"
         target row))

(module+ main
  (displayln "racklr/emit-contract — the UIR-plus-row emit contract loaded."))
