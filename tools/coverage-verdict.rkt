#lang racket

;; tools/coverage-verdict.rkt — the coverage half of the shared-baseline gate.
;;
;; The baseline gate answers two independent questions:
;;
;;   1. Pass/fail — does the type graph match the pinned content address?
;;   2. Coverage  — is the unknown-elimination obligation discharged?
;;
;; A run can pass (1) and still fail (2): the graph is exactly the pinned one,
;; every checker is green, and unknown effect rows remain. That run is
;; `:coverage-failed`, not green. jolt-hegel's OBSERVATIONS.md draws the same
;; line for property tests: a run that passes every property can still be
;; `:status :coverage-failed`, and coverage never masks a failure.
;;
;; The obligation is the Phase-0.3 acceptance bar (ROADMAP.md): zero unknown
;; leaves. "Unknown dropped from N to M" is an observation, never a coverage
;; verdict. The census is trusted only when the summary counters agree with the
;; named `fn-effect-row` witness rows in the graph; the witness is the proof,
;; the counter is the report.

(require racket/list
         racket/set
         racket/string)

(provide (struct-out coverage-obligation)
         p0-3-obligation
         (struct-out coverage-census)
         (struct-out coverage-verdict)
         assess-coverage
         coverage-met?
         coverage-failed?
         coverage-status-line
         coverage-census->string
         census-from-report
         census-from-atom-lines
         census-disagreements)

;; ── the obligation ──────────────────────────────────────────────────

;; Each field is the maximum number of unknown leaves allowed. The Phase-0.3
;; acceptance bar is zero: an unknown leaf is an open obligation, not a fact of
;; life. Keeping the bar as data lets a staged obligation be written down later
;; without changing the verdict semantics.
(struct coverage-obligation (unknown-functions unknown-files) #:transparent)

(define (p0-3-obligation)
  (coverage-obligation 0 0))

;; ── the observation ─────────────────────────────────────────────────

;; What one baseline run observed. This is a census, not a hit rate: both
;; numbers are counts of functions whose row still carries `unknown`.
(struct coverage-census (unknown-functions unknown-files) #:transparent)

(define (coverage-census->string census)
  (format "unknown functions: ~a, unknown files: ~a"
          (coverage-census-unknown-functions census)
          (coverage-census-unknown-files census)))

;; ── the verdict ─────────────────────────────────────────────────────

;; status is 'met or 'coverage-failed. The census and obligation travel with
;; the verdict so a caller can explain an open obligation without recomputing
;; it. There is deliberately no "partially covered" status: a hit rate is not a
;; verdict.
(struct coverage-verdict (status census obligation) #:transparent)

(define (coverage-met? verdict)
  (eq? (coverage-verdict-status verdict) 'met))

(define (coverage-failed? verdict)
  (eq? (coverage-verdict-status verdict) 'coverage-failed))

(define (within-obligation? observed allowed)
  (<= observed allowed))

(define (assess-coverage census obligation)
  (define met?
    (and (within-obligation? (coverage-census-unknown-functions census)
                             (coverage-obligation-unknown-functions obligation))
         (within-obligation? (coverage-census-unknown-files census)
                             (coverage-obligation-unknown-files obligation))))
  (coverage-verdict (if met? 'met 'coverage-failed) census obligation))

(define (coverage-status-line verdict)
  (format ":status :~a" (coverage-verdict-status verdict)))

;; ── the observation: one-line counters from the type-graph report ───

;; effervesce/tools/scoreboard.rkt prints these two counters. A missing or
;; malformed counter is an error, never a silent zero: the gate fails closed
;; rather than reading a missing obligation as discharged.
(define unknown-functions-label "unknown functions")
(define unknown-files-label "unknown files")

(define (report-counter lines label)
  (define prefix (string-append label ":"))
  (define line (findf (λ (l) (string-prefix? l prefix)) lines))
  (unless line
    (error 'census-from-report
           (string-append
            "type-graph report has no ~s counter; refusing to read a missing"
            " obligation as discharged")
           label))
  (define text (string-trim (substring line (string-length prefix))))
  (define n (string->number text))
  (unless (and n (exact-nonnegative-integer? n))
    (error 'census-from-report
           "type-graph report counter ~s is not a non-negative integer: ~s"
           label text))
  n)

(define (census-from-report report)
  (define lines (string-split report "\n"))
  (coverage-census (report-counter lines unknown-functions-label)
                   (report-counter lines unknown-files-label)))

;; ── the witness: named fn-effect-row atoms ──────────────────────────

;; Count the graph's own `fn-effect-row` atoms whose row carries an `unknown`
;; effect entry, deduplicating the files those functions live in. This is the
;; reachability evidence behind the count: it names every function, instead of
;; reporting how often an unknown happened to be hit.
(define fn-effect-row-prefix "(Fn-effect-rowLink ")
(define unknown-entry-marker "(ConceptNode unknown)")
(define fn-effect-row-file-rx #px"^\\(Fn-effect-rowLink \\(ConceptNode \"([^\"]+)::")

(define (unknown-function-row-line? line)
  (and (string-prefix? line fn-effect-row-prefix)
       (string-contains? line unknown-entry-marker)))

(define (unknown-row-file line)
  (define match (regexp-match fn-effect-row-file-rx line))
  (unless match
    (error 'census-from-atom-lines
           "fn-effect-row does not name a file::function component: ~s" line))
  (cadr match))

(define (census-from-atom-lines atom-lines)
  (define unknown-rows (filter unknown-function-row-line? atom-lines))
  (define files
    (for/fold ([files (set)]) ([line (in-list unknown-rows)])
      (set-add files (unknown-row-file line))))
  (coverage-census (length unknown-rows) (set-count files)))

;; ── observation vs witness ──────────────────────────────────────────

;; Compare the summary counters against the named witness rows. A disagreement
;; means the gate cannot say what the unknown count is, so the caller must fail
;; closed instead of reporting either number as coverage.
(define (census-disagreements observed witness)
  (define (field-disagreement field observed-n witness-n)
    (if (= observed-n witness-n)
        '()
        (list (list field observed-n witness-n))))
  (append
   (field-disagreement 'unknown-functions
                       (coverage-census-unknown-functions observed)
                       (coverage-census-unknown-functions witness))
   (field-disagreement 'unknown-files
                       (coverage-census-unknown-files observed)
                       (coverage-census-unknown-files witness))))
