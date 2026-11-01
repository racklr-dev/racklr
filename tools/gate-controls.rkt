#lang racket

;; tools/gate-controls.rkt — non-vacuity controls for the shared-baseline gate.
;;
;; A green gate that cannot produce a failure proves nothing about the gate.
;; These controls run before the gate reports a verdict: known-bad inputs must
;; be rejected, known-good inputs must pass, and an input the gate cannot read
;; must fail closed instead of becoming a silent pass.
;;
;; The discipline is jolt-hegel's: random hit rates are observations, not
;; reachability proofs, so coverage is only credible when explicit known-bad
;; controls falsify the gate. `baseline-check.rkt` refuses to print a green
;; verdict when any control here is vacuous.

(require racket/list
         racket/string
         "baseline-snapshot.rkt"
         "coverage-verdict.rkt")

(provide (struct-out gate-control)
         run-gate-controls
         gate-controls-ok?
         gate-control-failures)

(struct gate-control (name passed? detail) #:transparent)

(define (control name passed? detail)
  (gate-control name (and passed? #t) detail))

;; ── fixtures ────────────────────────────────────────────────────────

(define unknown-entry
  (string-append
   "(EffectLink (ConceptNode unknown) (ConceptNode unknown)"
   " (ConceptNode ()) (ConceptNode #f) (ConceptNode #f))"))

(define db-entry
  (string-append
   "(EffectLink (ConceptNode db) (ConceptNode may)"
   " (ConceptNode ()) (ConceptNode #f) (ConceptNode #f))"))

(define (fn-effect-row-atom file name entries)
  (string-append "(Fn-effect-rowLink (ConceptNode \"" file "::" name "\") "
                 (string-join entries " ")
                 ")"))

;; Two unknown functions in one file, plus one clean row: the census must be
;; (2 functions, 1 file), not (2 functions, 3 files) and not a percentage.
(define known-bad-atom-lines
  (list
   (fn-effect-row-atom "django/django/core/a.py" "f" (list unknown-entry))
   (fn-effect-row-atom "django/django/core/a.py" "g" (list db-entry unknown-entry))
   (fn-effect-row-atom "django/django/core/b.py" "h" (list db-entry))))

(define known-good-atom-lines
  (list
   (fn-effect-row-atom "django/django/core/a.py" "f" (list db-entry))
   (fn-effect-row-atom "django/django/core/b.py" "g" (list db-entry))))

;; The two counters the coverage census reads, in the format
;; effervesce/tools/scoreboard.rkt prints them.
(define (report-with-counters unknown-functions unknown-files)
  (string-append
   "=== scoreboard (R1) ===\n"
   (format "unknown functions:      ~a\n" unknown-functions)
   (format "unknown files:          ~a\n" unknown-files)))

;; A report with the five counters build-snapshot's header needs, for the
;; pass/fail (drift) controls.
(define snapshot-report
  (string-join
   '("files scanned:  112"
     "files parsed:   112"
     "files failed:   0"
     "named functions: 979"
     "effectful functions: 740")
   "\n"))

(define snapshot-root "/tmp/racklr-gate-control")

(define (control-snapshot atom-lines)
  (build-snapshot snapshot-report atom-lines snapshot-root "django/django/core"))

;; ── controls ────────────────────────────────────────────────────────

;; The coverage detector must reject a known-bad census. A detector that
;; cannot produce :coverage-failed cannot certify a green gate.
(define (control-coverage-rejects-known-bad)
  (define observed (census-from-report (report-with-counters 2 1)))
  (define witness (census-from-atom-lines known-bad-atom-lines))
  (define verdict (assess-coverage observed (p0-3-obligation)))
  (control "coverage detector rejects a known-bad census"
           (and (coverage-failed? verdict)
                (null? (census-disagreements observed witness)))
           (format "observed ~a, witness ~a, ~a"
                   (coverage-census->string observed)
                   (coverage-census->string witness)
                   (coverage-status-line verdict))))

;; ...and pass a known-good census, so the detector is a predicate and not a
;; constant `:coverage-failed`.
(define (control-coverage-accepts-known-good)
  (define observed (census-from-report (report-with-counters 0 0)))
  (define witness (census-from-atom-lines known-good-atom-lines))
  (define verdict (assess-coverage observed (p0-3-obligation)))
  (control "coverage detector accepts a known-good census"
           (and (coverage-met? verdict)
                (null? (census-disagreements observed witness)))
           (format "observed ~a, witness ~a, ~a"
                   (coverage-census->string observed)
                   (coverage-census->string witness)
                   (coverage-status-line verdict))))

;; The summary counters are an observation; the named rows are the witness.
;; When they disagree the gate must notice, not average them.
(define (control-witness-disagreement-is-detected)
  (define observed (census-from-report (report-with-counters 0 0)))
  (define witness (census-from-atom-lines known-bad-atom-lines))
  (control "disagreeing observation and witness are detected"
           (pair? (census-disagreements observed witness))
           (format "observed ~a, witness ~a"
                   (coverage-census->string observed)
                   (coverage-census->string witness))))

;; A report that lost its counters must raise, not read as zero unknown leaves.
(define (control-missing-counters-fail-closed)
  (define raised?
    (with-handlers ([exn:fail? (λ (e) #t)])
      (census-from-report "=== scoreboard (R1) ===\nunknown functions: 3\n")
      #f))
  (control "a report without both counters fails closed"
           raised?
           "census-from-report should raise on a missing unknown-files counter"))

;; The pass/fail half of the gate needs its own non-vacuity control: the drift
;; detector must see a graph change...
(define (control-drift-detector-sees-a-change)
  (define pinned (control-snapshot known-good-atom-lines))
  (define current
    (control-snapshot
     (append (drop-right known-good-atom-lines 1)
             (list (fn-effect-row-atom "django/django/core/b.py" "g"
                                       (list db-entry unknown-entry))))))
  (define-values (header-changes removed added)
    (diff-snapshots pinned current))
  (control "drift detector sees a changed graph"
           (or (pair? header-changes) (pair? removed) (pair? added))
           (format "header changes ~a, removed ~a, added ~a"
                   (length header-changes) (length removed) (length added))))

;; ...and still report an identical graph as a match, so it is not a constant
;; "drift".
(define (control-drift-detector-accepts-an-identical-graph)
  (define snapshot (control-snapshot known-good-atom-lines))
  (define-values (header-changes removed added)
    (diff-snapshots snapshot snapshot))
  (control "drift detector accepts an identical graph"
           (and (null? header-changes) (null? removed) (null? added))
           (format "header changes ~a, removed ~a, added ~a"
                   (length header-changes) (length removed) (length added))))

(define (run-gate-controls)
  (list (control-coverage-rejects-known-bad)
        (control-coverage-accepts-known-good)
        (control-witness-disagreement-is-detected)
        (control-missing-counters-fail-closed)
        (control-drift-detector-sees-a-change)
        (control-drift-detector-accepts-an-identical-graph)))

(define (gate-controls-ok? controls)
  (andmap gate-control-passed? controls))

(define (gate-control-failures controls)
  (filter (λ (c) (not (gate-control-passed? c))) controls))
