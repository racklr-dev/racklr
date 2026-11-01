#lang racket

;; racklr/loss/declaration.rkt — the vocabulary and record types of the loss
;; model, and the rules that keep a declaration honest.
;;
;; The feature/disposition vocabulary is closed: every model must declare every
;; feature exactly once (explicitly `refused` when the pair has no such thing),
;; otherwise a projected model could silently leave a dimension unverified.
;; The pair data itself lives in racklr/loss/pairs.rkt; the public facade is
;; racklr/loss-model.rkt.

(require racket/list
         racket/string)

(provide loss-features
         loss-dispositions
         loss-normalizations
         (struct-out loss-rule)
         (struct-out loss-model)
         loss-rule?
         loss-model?
         loss-rule-feature
         loss-rule-disposition
         loss-rule-detail
         loss-model-source
         loss-model-target
         loss-model-mode
         loss-model-normalizations
         loss-model-injected
         loss-model-rules
         preserved
         approximated
         refused
         loss-rule->sexp
         loss-model->sexp
         declared-features
         missing-features
         duplicate-features
         loss-model-complete?)

;; ── The vocabulary ──────────────────────────────────────────────────
;;
;; A rule names a *feature* and a *disposition*. The verifier treats a
;; `preserved` feature as a hard obligation: a difference is a violation. An
;; `approximated` feature is allowed to differ (the detail says how), and a
;; `refused` feature is dropped on purpose. A feature that does not apply to the
;; pair is declared `refused` with a detail that says so — never omitted.

;; A feature is a projection of the canonical UIR serialization the verifier
;; knows how to extract (see racklr/loss/features.rkt).
(define loss-features
  '(definitions      ; block-scope declared names (functions, classes, bindings)
    signatures       ; function name, arity, parameter names
    classes          ; class names and method names
    literals         ; string and number literal inventory
    control-flow     ; control-flow node census (if/for/while/try/...)
    effect-rows      ; per-function effect rows in the target's native form
    types            ; type-level declarations (interfaces, aliases, enums)
    jsx              ; JSX element tree before target emission
    statement-shape  ; how statements/assignments/operators are regrouped
    runtime-guards   ; emitter-injected effect-guard support code
    imports          ; import/export shape
    formatting       ; layout, indentation, statement re-flow
    comments))       ; source comments

(define loss-dispositions '(preserved approximated refused))

;; Normalizations the verifier applies to both sides before comparing. Each is
;; a pure UIR-serialization transform; `annotate-effects` is a pre-pass on the
;; absorbed side that reproduces what the emitters do (they infer rows at emit
;; time, so the absorbed UIR the emitter actually sees carries them).
(define loss-normalizations
  '(annotate-effects      ; run effect inference before emit (what emitters do)
    drop-generated        ; remove emitter-injected declarations/imports
    drop-effect-guards    ; remove effect-guard annotations from the structure
    drop-parens           ; parentheses are not semantic
    erase-js-types))      ; erase TypeScript type syntax for the JS target

;; ── Records ─────────────────────────────────────────────────────────

(struct loss-rule (feature disposition detail) #:transparent)
(struct loss-model (source target mode normalizations injected rules) #:transparent)
;; mode: 'exact     — normalized UIRs must be structurally equal.
;;       'projected — only the `preserved` feature projections must match.
;; injected: top-level names/imports the emitter adds (dropped before comparison).

(define (rule feature disposition detail)
  (unless (memq feature loss-features)
    (error 'loss-model "unknown loss feature: ~a" feature))
  (unless (memq disposition loss-dispositions)
    (error 'loss-model "unknown disposition: ~a" disposition))
  (unless (and (string? detail) (not (string=? detail "")))
    (error 'loss-model "a loss rule needs a detail: ~a" feature))
  (loss-rule feature disposition detail))

;; Preserved is the common case; the helpers keep the declaration readable.
(define (preserved feature detail) (rule feature 'preserved detail))
(define (approximated feature detail) (rule feature 'approximated detail))
(define (refused feature detail) (rule feature 'refused detail))

;; ── Completeness ────────────────────────────────────────────────────

(define (declared-features m)
  (map loss-rule-feature (loss-model-rules m)))

(define (missing-features m)
  (for/list ([f (in-list loss-features)]
             #:unless (memq f (declared-features m)))
    f))

(define (duplicate-features m)
  (define seen (make-hash))
  (for/fold ([dups '()])
      ([f (in-list (declared-features m))])
    (if (hash-ref seen f #f) (cons f dups) (begin (hash-set! seen f #t) dups))))

(define (loss-model-complete? m)
  (and (null? (missing-features m))
       (null? (duplicate-features m))))

;; ── Canonical serialization ─────────────────────────────────────────

(define (loss-rule->sexp r)
  `(rule ,(loss-rule-feature r)
         ,(loss-rule-disposition r)
         ,(loss-rule-detail r)))

(define (loss-model->sexp m)
  `(loss-model ,(loss-model-source m)
               ,(loss-model-target m)
               ,(loss-model-mode m)
               ,(loss-model-normalizations m)
               ,(loss-model-injected m)
               ,(map loss-rule->sexp (loss-model-rules m))))
