#lang racket

;; tools/self-emit/gate.rkt — the shared racklr + effervesce gate.
;;
;; P6.3 does not only compare translations; it also asks that the emitted
;; compiler passes the same gate the original tower passes. This module runs
;; that gate and reports it machine-readably:
;;
;;   suites  — the racklr and effervesce suites every layer must keep green
;;   baseline — racket tools/baseline-check.rkt, pinned to the shared
;;              type-graph hash in baseline/type-graph.sha256
;;   compile — raco make over the tree (the `raco setup` step of AGENTS.md,
;;             expressed as `raco make` because a git worktree can be shadowed
;;             by the host's linked packages in `raco setup`)
;;   absorb  — the emitted tower's own source, when supplied, is read back
;;             through the tower: parse → lower → infer, and every named
;;             function must carry a row. This is the reflexive half of the
;;             gate: the system that checks code is itself checked.
;;
;; A suite passes only when its exit code is 0 *and* its output carries no
;; rackunit FAILURE/ERROR line. A zero exit from `raco test` is not success in
;; this repository, so the marker scan is not optional.

(require racket/file
         racket/list
         racket/path
         racket/string
         racket/system
         racklr/uir
         racklr/effects/contract
         racklr/effects/infer
         "oracle.rkt")

(provide default-gate-suites
         gate-suite-name
         run-gate
         run-suite
         run-baseline
         run-compile
         absorb-tower-source
         failure-lines)

;; ── the gate definition ─────────────────────────────────────────────

;; (list subdirectory suite-file). The defaults are the shared gate the rest of
;; the layer work runs: the effect-inference and contract suites, UIR, the
;; baseline machinery, the JSON round-trip, the emit contract, and the three
;; effervesce suites.
(define (default-gate-suites)
  (append
   (for/list ([suite (in-list '("effects-infer-test.rkt"
                                "effects-imports-test.rkt"
                                "effects-contract-test.rkt"
                                "effects-python-annot-test.rkt"
                                "uir-test.rkt"
                                "baseline-check-test.rkt"
                                "lower-json-test.rkt"
                                "emit-contract-test.rkt"
                                "self-emit-verify-test.rkt"
                                "self-emit-test.rkt"
                                "self-emit-integration-test.rkt"
                                "self-seed-test.rkt"))])
     (list "racklr-test" suite))
   (for/list ([suite (in-list '("library-test.rkt"
                                "check/check-test.rkt"
                                "check/verify-test.rkt"))])
     (list "effervesce" suite))))

(define (gate-suite-name spec)
  (format "~a/~a" (first spec) (second spec)))

;; ── process environment ────────────────────────────────────────────

;; Racket resolves the racklr/effervesce collections through PLTCOLLECTS. The
;; suites and the baseline tool both run from the checkout being graded, so
;; prepend its root to whatever the process already sees.
(define (collection-path-string root)
  (define root* (path->string (simplify-path (path->complete-path root))))
  (define existing
    (for/list ([p (in-list (current-library-collection-paths))])
      (path->string (simplify-path (path->complete-path p)))))
  (string-join (remove-duplicates (cons root* existing) string=?) ":"))

(define (gate-environment root)
  (define env (environment-variables-copy (current-environment-variables)))
  (environment-variables-set! env #"PLTCOLLECTS"
                              (string->bytes/utf-8 (collection-path-string root)))
  env)

(define racket-executable
  (or (find-executable-path "racket") (find-system-path 'exec-file)))

;; rackunit prints a failing test as a line starting with FAILURE, and an
;; exception escaping a test as a line starting with ERROR. Scanning those
;; markers catches the "zero exit is not success" case.
(define fail-marker-rx #px"(?m:^(FAILURE|ERROR)\\b)")

(define (failure-lines text)
  (for/list ([line (in-list (regexp-split #rx"\n" text))]
             #:when (regexp-match? fail-marker-rx line))
    line))

;; ── suites ──────────────────────────────────────────────────────────

;; A suite is run as `racket <suite>` with the suite directory as the working
;; directory, because the effervesce suites open fixtures relative to it.
;; Returns (hash 'name 'ok? 'exit 'failures 'output).
(define (run-suite root spec)
  (define subdir (first spec))
  (define suite (second spec))
  (define out (open-output-string))
  (define err (open-output-string))
  (define code
    (parameterize ([current-directory (build-path root subdir)]
                   [current-environment-variables (gate-environment root)]
                   [current-output-port out]
                   [current-error-port err])
      (system*/exit-code racket-executable suite)))
  (define output (string-append (get-output-string out) (get-output-string err)))
  (define failures (failure-lines output))
  (hash 'name (gate-suite-name spec)
        'ok? (and (zero? code) (null? failures))
        'exit code
        'failures failures
        'output output))

;; Run every suite. Returns
;;   (hash 'ok? boolean 'results (listof result) 'failed (listof name)).
(define (run-gate root [suites (default-gate-suites)])
  (define results (for/list ([spec (in-list suites)]) (run-suite root spec)))
  (define failed
    (for/list ([r (in-list results)] #:unless (hash-ref r 'ok?)) (hash-ref r 'name)))
  (hash 'ok? (null? failed) 'results results 'failed failed))

;; ── baseline ────────────────────────────────────────────────────────

;; The content-addressed shared baseline reports two separate verdicts:
;;
;;   baseline OK sha256:<hash>              the graph matches the pin and the
;;                                          Phase-0.3 zero-unknown obligation
;;                                          is discharged
;;   baseline COVERAGE-FAILED sha256:<hash> the graph matches the pin but
;;                                          unknown leaves remain (the open
;;                                          Phase-0.3 obligation: not a drift,
;;                                          and not green either)
;;   baseline DRIFT / GATE-FAILED           a regression / untrustworthy gate
;;
;; The gate therefore reports the baseline verdict instead of collapsing it to
;; a boolean: re-pinning is never justified by coverage, and a drift is never
;; excused by green suites.
(define (baseline-verdict output)
  (cond
    [(regexp-match? #px"baseline OK sha256:" output) 'ok]
    [(regexp-match? #px"baseline COVERAGE-FAILED" output) 'coverage-failed]
    [(regexp-match? #px"baseline GATE-FAILED" output) 'gate-failed]
    [(regexp-match? #px"baseline DRIFT" output) 'drift]
    [else 'unrunnable]))

(define (baseline-number output label)
  (define match? (regexp-match (pregexp (format "~a: ([0-9]+)" label)) output))
  (and match? (string->number (second match?))))

(define (run-baseline root)
  (define out (open-output-string))
  (define err (open-output-string))
  (define code
    (parameterize ([current-directory root]
                   [current-environment-variables (gate-environment root)]
                   [current-output-port out]
                   [current-error-port err])
      (system*/exit-code racket-executable "tools/baseline-check.rkt")))
  (define output (string-append (get-output-string out) (get-output-string err)))
  (define verdict (baseline-verdict output))
  (define hash-match (regexp-match #px"baseline (?:OK|COVERAGE-FAILED) (sha256:[0-9a-f]{64})" output))
  (hash 'ok? (eq? verdict 'ok)
        'verdict verdict
        'exit code
        'hash (and hash-match (second hash-match))
        'unknown-functions (baseline-number output "unknown functions")
        'unknown-files (baseline-number output "unknown files")
        'output output))

;; ── compile ─────────────────────────────────────────────────────────

(define compile-roots '("racklr" "racklr-test" "tools" "effervesce"))

(define (racket-files-under root rel)
  (define dir (build-path root rel))
  (if (directory-exists? dir)
      (for/list ([p (in-directory dir)]
                 #:when (and (file-exists? p)
                             (regexp-match? #rx"[.]rkt$" (path->string p))))
        (path->string p))
      '()))

;; `raco make` over the tree is the compile half of the gate. `raco setup`
;; itself is avoided here because a git worktree can be shadowed by packages
;; linked into the host installation (the worktree's collection is on
;; PLTCOLLECTS, but `raco setup` resolves by package name); `raco make` on
;; explicit paths still resolves the required collections through PLTCOLLECTS.
(define (run-compile root)
  (define files (append* (for/list ([rel (in-list compile-roots)])
                           (racket-files-under root rel))))
  (define out (open-output-string))
  (define err (open-output-string))
  (define code
    (parameterize ([current-directory root]
                   [current-environment-variables (gate-environment root)]
                   [current-output-port out]
                   [current-error-port err])
      (apply system*/exit-code racket-executable "-l" "raco" "make" files)))
  (hash 'ok? (zero? code) 'exit code 'files (length files)
        'output (string-append (get-output-string out) (get-output-string err))))

;; ── absorbing the emitted tower ─────────────────────────────────────

;; Read the emitted tower's source back through the tower and check the
;; reflexivity contract: parse → lower succeed and every named function carries
;; a row. `unknown` is a tracked state here, reported rather than hidden, so a
;; tower that absorbs but has unknown leaves is distinguishable from a tower
;; that does not absorb at all.
(define (absorb-tower-source language path)
  (define-values (uir rows) (translate-file language path))
  (define fns (collect-functions uir))
  (define missing
    (for/list ([(fn _) (in-hash fns)] #:unless (hash-has-key? rows fn)) fn))
  (define unknown
    (for/sum ([(_ row) (in-hash rows)])
      (if (for/or ([e (in-list (uir-effect-row-entries row))])
            (eq? (effect-name-symbol e) 'unknown))
          1
          0)))
  (hash 'ok? (and (not (hash-empty? rows)) (null? missing))
        'language language
        'path (path->string (path->complete-path path))
        'functions (hash-count fns)
        'rows (hash-count rows)
        'missing (length missing)
        'missing-names (sort (map symbol->string missing) string<?)
        'unknown unknown))
