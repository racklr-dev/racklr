#lang racket

;; racklr/loss/report.rkt — the result records of round-trip verification.
;;
;; These are plain transparent structs so a test can assert on them and the
;; harness can serialize them into a machine-readable artifact. A loss is only
;; "surfaced" if it is one of these records; nothing about a round trip is
;; allowed to live only in a printed message.

(require racklr/loss-model)

(provide
 (struct-out loss-observation)
 (struct-out loss-violation)
 (struct-out loss-report)
 report-within-model?
 report->jsexpr)

;; observed: 'preserved 'approximated 'refused 'changed 'present 'untested
(struct loss-observation (feature declared observed detail) #:transparent)
(struct loss-violation (feature declared observed detail evidence) #:transparent)
(struct loss-report (model within-model? observations violations evidence) #:transparent)

(define (report-within-model? r) (loss-report-within-model? r))

(define (report->jsexpr r)
  (hasheq 'pair (format "~a->~a" (loss-model-source (loss-report-model r))
                        (loss-model-target (loss-report-model r)))
          'mode (loss-model-mode (loss-report-model r))
          'within_model (loss-report-within-model? r)
          'observations
          (for/list ([o (in-list (loss-report-observations r))])
            (hasheq 'feature (loss-observation-feature o)
                    'declared (loss-observation-declared o)
                    'observed (loss-observation-observed o)
                    'detail (loss-observation-detail o)))
          'violations
          (for/list ([v (in-list (loss-report-violations r))])
            (hasheq 'feature (loss-violation-feature v)
                    'declared (loss-violation-declared v)
                    'observed (loss-violation-observed v)
                    'detail (loss-violation-detail v)
                    'evidence (loss-violation-evidence v)))
          'evidence (loss-report-evidence r)))
