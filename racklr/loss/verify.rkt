#lang racket

;; racklr/loss/verify.rkt — round-trip verification within the declared loss
;; model.
;;
;; Given the UIR absorbed from a source file and the UIR re-absorbed from the
;; emitted target text, this module answers one question: is the translation
;; identity within the pair's declared loss model, or did it lose something the
;; model did not declare?
;;
;; Verification has two modes (see racklr/loss-model.rkt):
;;   exact     — after the model's declared normalizations the two canonical
;;               UIR serializations must be equal; a residual difference is an
;;               undeclared loss (a violation).
;;   projected — a lossy one-way translation (TSX → JavaScript). Only features
;;               the model declares `preserved` must match; declared
;;               approximations are recorded, not failed.
;;
;; Everything language-pair-specific lives in the model data. This module is
;; pair-agnostic: annotate, normalize, project, compare.

(require racket/list
         racket/string
         racklr/uir
         racklr/effects/infer
         racklr/loss-model
         racklr/loss/sexp
         racklr/loss/normalize
         racklr/loss/features
         racklr/loss/effects
         racklr/loss/report)

(provide verify-round-trip
         loss-report-summary
         report->jsexpr
         report-within-model?
         sexp-diff
         (struct-out loss-observation)
         (struct-out loss-violation)
         (struct-out loss-report))

(define (loss-report-summary r)
  (format "~a→~a (~a): ~a observation(s), ~a violation(s)"
          (loss-model-source (loss-report-model r))
          (loss-model-target (loss-report-model r))
          (loss-model-mode (loss-report-model r))
          (length (loss-report-observations r))
          (length (loss-report-violations r))))

;; ── Orchestration ───────────────────────────────────────────────────

(define (verify-round-trip model u1 u2)
  (unless (loss-model? model)
    (error 'verify-round-trip "not a loss model: ~a" model))
  ;; A model that omits a feature would leave that dimension silently
  ;; unverified in projected mode. Refuse to verify against an incomplete
  ;; declaration instead of accepting the omission.
  (unless (loss-model-complete? model)
    (error 'verify-round-trip
           "loss model for ~a→~a is incomplete (missing=~s duplicates=~s)"
           (loss-model-source model) (loss-model-target model)
           (missing-features model) (duplicate-features model)))
  (define norms (loss-model-normalizations model))
  ;; The emitters infer effect rows at emit time, so the absorbed UIR the
  ;; emitted artifact was produced from is the annotated one.
  (define u1a (if (memq 'annotate-effects norms) (annotate-effects u1) u1))
  (define s1 (uir->sexp u1a))
  (define s2 (uir->sexp u2))
  (define user-names (bound-names s1))
  (define n1 (normalize model user-names s1))
  (define n2 (normalize model user-names s2))
  (define structural-equal? (equal? n1 n2))
  (define rules (loss-model-rules model))
  (define obs
    (for/list ([r (in-list rules)])
      (if (eq? (loss-rule-feature r) 'effect-rows)
          (observe-effect-rows r s1 s2)
          (observe-rule r n1 n2 s2 user-names))))
  (define violations (make-hash))
  (define (violate! feature declared observed detail evidence)
    (hash-set! violations feature
               (loss-violation feature declared observed detail evidence)))
  (when (and (eq? (loss-model-mode model) 'exact) (not structural-equal?))
    (violate! 'structure 'preserved 'changed
              (string-append "normalized UIRs differ: the round trip lost or changed "
                             "structure the loss model does not declare")
              (map (λ (d) (format "~a: ~s ≠ ~s"
                                  (sexp-path-diff-path d)
                                  (sexp-path-diff-before d)
                                  (sexp-path-diff-after d)))
                   (sexp-diff n1 n2))))
  (for ([o (in-list obs)])
    (define feature (loss-observation-feature o))
    (define declared (loss-observation-declared o))
    (define observed (loss-observation-observed o))
    (cond
      [(and (eq? declared 'preserved) (eq? observed 'changed))
       (violate! feature declared observed
                 "declared preserved but observed changed"
                 (feature-evidence feature n1 n2 s1 s2))]
      [(and (eq? declared 'refused) (eq? observed 'present))
       (violate! feature declared observed
                 "declared refused but the target UIR carries it — update the loss model"
                 '())]
      [else (void)]))
  (define violation-list
    (sort (hash-values violations)
          (λ (a b) (string<? (symbol->string (loss-violation-feature a))
                             (symbol->string (loss-violation-feature b))))))
  (loss-report model (null? violation-list) obs violation-list
               (if structural-equal?
                   '()
                   (map (λ (d) (format "~a: ~s ≠ ~s"
                                       (sexp-path-diff-path d)
                                       (sexp-path-diff-before d)
                                       (sexp-path-diff-after d)))
                        (sexp-diff n1 n2)))))

(define (feature-evidence feature n1 n2 s1 s2)
  (define (project sexp)
    (case feature
      [(effect-rows) (extract-effect-rows sexp)]
      [else (feature-projection feature sexp)]))
  (define p1 (project (if (eq? feature 'effect-rows) s1 n1)))
  (define p2 (project (if (eq? feature 'effect-rows) s2 n2)))
  (append (for/list ([x (in-list (or p1 '()))]
                     #:when (not (member x (or p2 '()))))
            (format "missing: ~s" x))
          (for/list ([x (in-list (or p2 '()))]
                     #:when (not (member x (or p1 '()))))
            (format "added: ~s" x))))
