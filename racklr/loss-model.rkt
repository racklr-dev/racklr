#lang racket

;; racklr/loss-model.rkt — the declared loss model for absorb→emit.
;;
;; Two languages with different concurrency or memory semantics cannot
;; round-trip perfectly. Absorb then emit is identity *within a declared loss
;; model*, not a loss the tool hides. This module is the public facade: it
;; re-exports the declaration vocabulary (racklr/loss/declaration.rkt) and the
;; pair data (racklr/loss/pairs.rkt), and adds the queries, canonical
;; serialization, and the coverage gate that keeps the declaration total.
;;
;; The model is data. The verifier (racklr/loss/verify.rkt) and the round-trip
;; harness (racklr-test/examples-roundtrip-test.rkt) read it; neither carries
;; pair-specific behavior. Changing translation behavior is a deliberate edit
;; to racklr/loss/pairs.rkt whose diff is the review.

(require racket/list
         racket/string
         racklr/loss/declaration
         racklr/loss/pairs)

(provide
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
 loss-features
 loss-dispositions
 loss-normalizations
 preserved
 approximated
 refused
 declared-features
 missing-features
 duplicate-features
 loss-model-complete?
 supported-loss-models
 supported-source-languages
 loss-model-for
 loss-model-for-language
 supported-pair?
 loss-model->sexp
 loss-models->sexp
 loss-model-canonical-string
 loss-model-coverage-check)

(define supported-loss-models declared-loss-models)

;; ── Queries ─────────────────────────────────────────────────────────

(define (supported-source-languages)
  (remove-duplicates (map loss-model-source supported-loss-models)))

;; The target language an absorbed source language is emitted into.
(define (loss-model-for-language lang)
  (findf (λ (m) (eq? (loss-model-source m) lang)) supported-loss-models))

(define (loss-model-for source target)
  (findf (λ (m) (and (eq? (loss-model-source m) source)
                     (eq? (loss-model-target m) target)))
         supported-loss-models))

(define (supported-pair? source target)
  (and (loss-model-for source target) #t))

;; ── Canonical serialization (artifact and model digest) ─────────────

(define (loss-models->sexp)
  (map loss-model->sexp supported-loss-models))

(define (loss-model-canonical-string)
  (string-join (map (λ (s) (format "~s" s)) (loss-models->sexp)) "\n"))

;; ── Integrity / coverage gate ──────────────────────────────────────
;;
;; A model that omits a feature would leave that dimension unverified in
;; projected mode, which is exactly the undeclared loss the loss model exists to
;; surface. So every model must declare every feature exactly once; a feature
;; that does not apply to the pair is declared `refused` with a detail saying
;; so. The harness and the model test both run this check, and
;; verify-round-trip re-checks it so a hand-built model cannot bypass it.

(define (loss-model-coverage-check)
  (define problems '())
  (define (note! fmt . args) (set! problems (cons (apply format fmt args) problems)))
  (for ([m (in-list supported-loss-models)])
    (unless (memq (loss-model-mode m) '(exact projected))
      (note! "~a→~a: unknown mode ~a"
             (loss-model-source m) (loss-model-target m) (loss-model-mode m)))
    (for ([n (in-list (loss-model-normalizations m))])
      (unless (memq n loss-normalizations)
        (note! "~a→~a: unknown normalization ~a"
               (loss-model-source m) (loss-model-target m) n)))
    (for ([f (in-list (missing-features m))])
      (note! "~a→~a: no rule for feature ~a (declare it preserved/approximated/refused)"
             (loss-model-source m) (loss-model-target m) f))
    (for ([f (in-list (remove-duplicates (duplicate-features m)))])
      (note! "~a→~a: duplicate rule for feature ~a"
             (loss-model-source m) (loss-model-target m) f)))
  (define-values (_seen-pairs raw-dup-pairs)
    (for/fold ([seen '()] [dups '()])
        ([m (in-list supported-loss-models)])
      (define key (cons (loss-model-source m) (loss-model-target m)))
      (values (cons key seen) (if (member key seen) (cons key dups) dups))))
  (for ([d (in-list (remove-duplicates raw-dup-pairs))])
    (note! "duplicate model for pair ~a→~a" (car d) (cdr d)))
  (if (null? problems)
      (void)
      (error 'loss-model-coverage-check "~a" (string-join (reverse problems) "\n"))))
