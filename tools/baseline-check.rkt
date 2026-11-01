#lang racket

;; tools/baseline-check.rkt — content-addressed shared baseline + coverage verdict.
;;
;; Usage (from the repository root, with the submodules initialised):
;;
;;   racket tools/baseline-check.rkt                    # check the pinned graph
;;   racket tools/baseline-check.rkt --update           # re-pin after a change
;;   racket tools/baseline-check.rkt --max-diff 80      # show more changed atoms
;;   racket tools/baseline-check.rkt django/django/core # explicit probe
;;
;; The check runs effervesce/tools/type-graph.rkt --dump, canonicalizes the
;; graph output (tools/baseline-snapshot.rkt), and compares its SHA-256 against
;; the pin in baseline/type-graph.sha256. A mismatch prints the exact atom
;; lines that moved, so a drift review is a graph diff, not a judgment over
;; remembered integers.
;;
;; Pass/fail and coverage are separate verdicts (jolt-hegel OBSERVATIONS.md):
;;
;;   baseline OK sha256:<hash>               exit 0 — the graph matches the pin
;;                                           and the unknown-elimination
;;                                           obligation (Phase-0.3 bar: zero
;;                                           unknown leaves) is discharged
;;   baseline DRIFT                          exit 1 — the graph moved
;;   baseline COVERAGE-FAILED sha256:<hash>  exit 3 — the graph matches the pin,
;;                                           every check can pass, and unknown
;;                                           leaves remain
;;   baseline GATE-FAILED                    exit 4 — a non-vacuity control was
;;                                           vacuous, or the summary counters
;;                                           disagree with the named rows
;;
;; A run that passes every check but leaves unknown leaves is not green. The
;; coverage counts are cross-checked against the named fn-effect-row witnesses
;; in the graph, and the known-bad non-vacuity controls in
;; tools/gate-controls.rkt must reject before any verdict is reported.
;;
;; Exit code 2 is "could not run".

(require racket/cmdline
         racket/file
         racket/list
         racket/port
         racket/string
         racket/system
         "baseline-snapshot.rkt"
         "coverage-verdict.rkt"
         "gate-controls.rkt")

(define default-probe-dirs '("django/django/core"))
(define type-graph-relative "effervesce/tools/type-graph.rkt")
(define snapshot-relative "baseline/type-graph.snapshot")
(define hash-relative "baseline/type-graph.sha256")
(define pinned-hash-rx #px"^sha256:[0-9a-f]{64}$")

;; ── paths ───────────────────────────────────────────────────────────

;; The checkout root is the parent of the tools/ directory holding this file,
;; so the tool works from any working directory.
(define (repo-root)
  (define here (simplify-path (path->complete-path (find-system-path 'run-file))))
  (simplify-path (build-path here 'up 'up)))

(define (root-path root rel)
  (build-path root rel))

(define (read-utf8 path)
  (bytes->string/utf-8 (file->bytes path)))

(define (write-utf8! path text)
  (define-values (dir name dir?) (split-path path))
  (when dir (make-directory* dir))
  (display-to-file (string->bytes/utf-8 text) path #:exists 'replace))

;; ── running the probe ───────────────────────────────────────────────

;; Racket resolves collections (racklr/, effervesce/) through PLTCOLLECTS in
;; this repository's gate setup: prepend the checkout root to whatever the
;; process already sees, so the child type-graph run resolves this worktree
;; first.
(define (collection-paths-string root)
  (define root*
    (regexp-replace #rx"[\\\\/]+$"
                    (path->string (simplify-path (path->complete-path root)))
                    ""))
  (define existing
    (for/list ([p (in-list (current-library-collection-paths))])
      (path->string (simplify-path (path->complete-path p)))))
  (string-join (remove-duplicates (cons root* existing) string=?) ":"))

(define (child-environment root)
  (define env (environment-variables-copy (current-environment-variables)))
  (environment-variables-set!
   env #"PLTCOLLECTS"
   (string->bytes/utf-8 (collection-paths-string root)))
  env)

(define (check-probes! root probe-dirs)
  (define type-graph (root-path root type-graph-relative))
  (unless (file-exists? type-graph)
    (error 'baseline-check "type-graph tool not found: ~a" type-graph))
  (for ([dir (in-list probe-dirs)])
    (unless (directory-exists? (root-path root dir))
      (error 'baseline-check
             (string-append
              "probe directory ~a not found under ~a;"
              " run `git submodule update --init ~a` first")
             dir root dir))))

;; Returns (values report-text atom-lines).
(define (run-probe! root probe-dirs)
  (check-probes! root probe-dirs)
  (define racket-bin (find-executable-path (find-system-path 'exec-file)))
  (define type-graph (root-path root type-graph-relative))
  (eprintf "baseline-check: running ~a on ~a ...\n"
           type-graph-relative (string-join probe-dirs " "))
  (define out (open-output-string))
  (define err (open-output-string))
  (define code
    (parameterize ([current-directory root]
                   [current-output-port out]
                   [current-error-port err]
                   [current-environment-variables (child-environment root)])
      (apply system*/exit-code
             racket-bin
             (path->string type-graph)
             (append probe-dirs (list "--dump")))))
  (define report (get-output-string out))
  (define errors (get-output-string err))
  (unless (zero? code)
    (error 'baseline-check
           "type-graph exited ~a~a~a"
           code
           (if (string=? "" (string-trim errors)) "" "\n--- stderr ---\n")
           (string-trim errors)))
  (define dump-path
    (build-path root "effervesce-out" "effect-graph.scm"))
  (unless (file-exists? dump-path)
    (error 'baseline-check
           "type-graph finished but wrote no graph dump at ~a" dump-path))
  (values report (file->lines dump-path)))

;; ── check / update ──────────────────────────────────────────────────

(define (pinned-files root)
  (define snapshot-path (root-path root snapshot-relative))
  (define hash-path (root-path root hash-relative))
  (unless (file-exists? snapshot-path)
    (error 'baseline-check
           "no pinned snapshot at ~a; run `racket tools/baseline-check.rkt --update`"
           snapshot-path))
  (unless (file-exists? hash-path)
    (error 'baseline-check
           "no pinned hash at ~a; run `racket tools/baseline-check.rkt --update`"
           hash-path))
  (define pinned-text (read-utf8 snapshot-path))
  (define pinned-hash (string-trim (read-utf8 hash-path)))
  (unless (regexp-match? pinned-hash-rx pinned-hash)
    (error 'baseline-check
           "pinned hash file ~a does not hold sha256:<64 hex>: ~s"
           hash-path pinned-hash))
  (define recomputed (snapshot-content-id pinned-text))
  (unless (string=? pinned-hash recomputed)
    (error 'baseline-check
           (string-append
            "pinned snapshot does not match its pinned hash (corrupt pin):\n"
            "  pin:        ~a\n  snapshot:   ~a")
           pinned-hash recomputed))
  (values pinned-text pinned-hash))

(define (print-update! root probes-string atom-lines report)
  (define snapshot (build-snapshot report atom-lines root probes-string))
  (define id (snapshot-content-id snapshot))
  (write-utf8! (root-path root snapshot-relative) snapshot)
  (write-utf8! (root-path root hash-relative) (string-append id "\n"))
  (printf "baseline UPDATED ~a (~a atoms, ~a files)\n"
          id
          (length atom-lines)
          (cdr (assoc "files parsed" (extract-summary report))))
  (void))

(define (print-diff-lines sign label lines max-diff)
  (unless (null? lines)
    (printf "  ~a:\n" label)
    (for ([line (in-list (take lines (min max-diff (length lines))))])
      (printf "    ~a ~a\n" sign line))
    (when (> (length lines) max-diff)
      (printf "    ... ~a more line(s) omitted (--max-diff ~a)\n"
              (- (length lines) max-diff) max-diff))))

(define (print-drift! pinned-text pinned-hash current-text current-hash max-diff)
  (define-values (header-changes removed added)
    (diff-snapshots pinned-text current-text))
  (printf "baseline DRIFT\n")
  (printf "  pinned:  ~a\n" pinned-hash)
  (printf "  current: ~a\n" current-hash)
  (unless (null? header-changes)
    (printf "  summary changes:\n")
    (for ([change (in-list header-changes)])
      (printf "    ~a: ~a -> ~a\n"
              (first change) (second change) (third change))))
  (printf "  graph: -~a atom line(s), +~a atom line(s)\n"
          (length removed) (length added))
  (print-diff-lines "-" "removed (pinned only)" removed max-diff)
  (print-diff-lines "+" "added (current only)" added max-diff)
  (printf "  re-pin only if the graph change is intended: racket tools/baseline-check.rkt --update\n")
  (void))

;; ── gate integrity and coverage verdict ─────────────────────────────

;; A non-vacuity control failing is a hard gate failure: a gate that cannot
;; prove it rejects a known-bad input must not be green.
(define (gate-failed! message details)
  (printf "baseline GATE-FAILED\n")
  (printf "  :status :gate-failed\n")
  (printf "  ~a\n" message)
  (for ([detail (in-list details)])
    (printf "  ~a\n" detail))
  (exit 4))

(define (print-coverage-detail! verdict)
  (define census (coverage-verdict-census verdict))
  (define obligation (coverage-verdict-obligation verdict))
  (printf "  unknown functions: ~a (obligation: <= ~a)\n"
          (coverage-census-unknown-functions census)
          (coverage-obligation-unknown-functions obligation))
  (printf "  unknown files: ~a (obligation: <= ~a)\n"
          (coverage-census-unknown-files census)
          (coverage-obligation-unknown-files obligation)))

;; ── main ────────────────────────────────────────────────────────────

(define (main)
  (define update? #f)
  (define max-diff 40)
  (define probe-dirs '())
  (command-line
   #:program "racket tools/baseline-check.rkt"
   #:once-each
   [("-u" "--update")
    "Re-pin the baseline from the current type-graph output"
    (set! update? #t)]
   [("--max-diff")
    n
    "Maximum changed graph lines to print (default 40)"
    (set! max-diff
          (let ([parsed (string->number n)])
            (unless (and parsed (exact-nonnegative-integer? parsed))
              (error 'baseline-check "--max-diff expects a non-negative integer: ~s" n))
            parsed))]
   #:args dirs
   (set! probe-dirs (if (null? dirs) default-probe-dirs dirs)))
  (define root (repo-root))
  (define probes-string (string-join probe-dirs " "))

  ;; Gate integrity before any verdict: the known-bad controls must reject and
  ;; the known-good controls must pass, or no verdict from this run is
  ;; trustworthy.
  (define controls (run-gate-controls))
  (unless (gate-controls-ok? controls)
    (gate-failed!
     "a non-vacuity control was vacuous; the gate cannot prove it rejects a known-bad input"
     (for/list ([control (in-list (gate-control-failures controls))])
       (format "FAILED control: ~a — ~a"
               (gate-control-name control)
               (gate-control-detail control)))))

  (define-values (report atom-lines) (run-probe! root probe-dirs))

  ;; The summary counters are the observation; the named fn-effect-row atoms are
  ;; the witness. A disagreement means the unknown count is unknowable, so the
  ;; gate fails closed instead of reporting either number as coverage.
  (define observed-census (census-from-report report))
  (define witness-census (census-from-atom-lines atom-lines))
  (define disagreements (census-disagreements observed-census witness-census))
  (unless (null? disagreements)
    (gate-failed!
     "the summary counters disagree with the named fn-effect-row witnesses"
     (for/list ([disagreement (in-list disagreements)])
       (format "~a: summary ~a, named rows ~a"
               (first disagreement)
               (second disagreement)
               (third disagreement)))))
  (define coverage (assess-coverage observed-census (p0-3-obligation)))

  (if update?
      (begin
        (print-update! root probes-string atom-lines report)
        (printf "  coverage: ~a\n" (coverage-status-line coverage))
        (printf "  ~a\n" (coverage-census->string observed-census))
        (exit 0))
      (let-values ([(pinned-text pinned-hash) (pinned-files root)])
        (define current-text (build-snapshot report atom-lines root probes-string))
        (define current-hash (snapshot-content-id current-text))
        (cond
          [(string=? current-hash pinned-hash)
           (if (coverage-met? coverage)
               (begin
                 (printf "baseline OK ~a\n" pinned-hash)
                 (printf "  :status :passed\n")
                 (printf "  coverage: met\n")
                 (print-coverage-detail! coverage)
                 (exit 0))
               (begin
                 (printf "baseline COVERAGE-FAILED ~a\n" pinned-hash)
                 (printf "  ~a\n" (coverage-status-line coverage))
                 (printf "  graph: matches the pin (the pass/fail half is green)\n")
                 (print-coverage-detail! coverage)
                 (printf "  a run is not green while unknown leaves remain\n")
                 (exit 3)))]
          [else
           (print-drift! pinned-text pinned-hash current-text current-hash max-diff)
           (printf "  :status :failed\n")
           (printf "  coverage at this revision: ~a\n"
                   (coverage-status-line coverage))
           (print-coverage-detail! coverage)
           (exit 1)]))))

(module+ main
  (with-handlers ([exn:fail? (λ (e)
                               (eprintf "baseline-check: ~a\n"
                                       (exn-message e))
                               (exit 2))])
    (main)))
