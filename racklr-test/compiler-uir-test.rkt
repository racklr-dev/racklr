#lang racket

;; racklr-test/compiler-uir-test.rkt — P6.1 Definition of Done, executable.
;;
;; Run from this directory (PLTCOLLECTS must resolve this checkout's racklr):
;;
;;   cd racklr-test && PLTCOLLECTS="$(pwd)/..:" racket compiler-uir-test.rkt
;;
;; The ticket's DoD is "the compiler's own modules lower to UIR with effect rows
;; on every function; infer.rkt runs over the compiler's UIR with no crash and
;; reports its own rows." This suite asserts exactly that against the corpus:
;; discovery, lowering, whole-corpus inference, annotation, determinism, the
;; report's own numbers, tracked-`unknown` honesty, and three ground-truth rows
;; traced from the source call chains.
;;
;; rackunit's top-level checks print FAILURE but leave the exit code at 0, so
;; this suite counts its own failures and exits 1 on any failure (AGENTS.md: a
;; zero exit is not success). It ends with a single PASS line only when green.

(require racket/list
         racket/match
         racket/path
         racket/string
         racklr/uir
         racklr/lower-racket
         racklr/compiler-uir
         racklr/effects/infer
         racklr/effects/contract
         racklr/effects/walk)

;; ── check harness ─────────────────────────────────────────────────

(define CHECKS 0)
(define FAILURES '())

(define (check-true! label value)
  (set! CHECKS (add1 CHECKS))
  (if value
      (void)
      (begin
        (set! FAILURES (cons label FAILURES))
        (eprintf "FAILURE: ~a\n" label))))

;; ── quality threshold ─────────────────────────────────────────────
;;
;; The share of the compiler's own functions that still carry a tracked
;; `unknown` entry. This is a ratchet, not a target: lower it as the lexicon and
;; dataflow improve; never raise it to make a regression pass.
;;
;; Observed at P6.1 integration: 714 / 2495 unknown functions = 0.286, with 533
;; unknown call sites. The remaining unknowns are higher-order calls through
;; parameters (`expr-fn`/`body-fn` callbacks, `apply`) and dynamic resolution
;; (`dynamic-require`, eval), which must stay tracked unknown rather than be
;; guessed. Limit = observed + headroom, so it fails only on a real regression.
(define UNKNOWN-FUNCTION-RATIO-LIMIT 0.40)

;; ── helpers ───────────────────────────────────────────────────────

(define (path->component root p)
  (string->symbol
   (regexp-replace #rx"[.]rkt$"
                   (path->string (find-relative-path root p))
                   "")))

(define (named-fn-nodes program)
  (define nodes '())
  (walk-uir program
            (λ (n)
              (if (and (uir-fn? n) (uir-symbol? (uir-fn-name n)))
                  (set! nodes (cons n nodes))
                  (void))))
  (reverse nodes))

(define (named-keys programs)
  (append*
   (for/list ([cp (in-list programs)])
     (for/list ([nm (in-list (program-named-functions (cdr cp)))])
       (cons (car cp) nm)))))

(define (mangled-name comp name)
  (string->symbol (format "~a::~a" comp name)))

(define (function-row rows comp name)
  (hash-ref rows (cons comp (mangled-name comp name)) #f))

(define (row-has? row eff)
  (and row
       (for/or ([e (in-list (uir-effect-row-entries row))])
         (eq? (effect-name-symbol e) eff))))

(define (histogram-of-rows rows)
  (define h (make-hash))
  (for ([(k row) (in-hash rows)])
    (for ([e (in-list (uir-effect-row-entries row))])
      (hash-update! h (effect-name-symbol e) add1 0)))
  h)

(define (histogram->string h)
  (define pairs
    (sort (hash->list h)
          (λ (a b) (if (= (cdr a) (cdr b))
                       (string<? (symbol->string (car a)) (symbol->string (car b)))
                       (> (cdr a) (cdr b))))))
  (string-join
   (for/list ([p (in-list pairs)])
     (format "~a:~a" (car p) (cdr p)))
   " "))

(define (modality-histogram rows)
  (define h (make-hash))
  (for ([(k row) (in-hash rows)])
    (for ([e (in-list (uir-effect-row-entries row))])
      (hash-update! h (uir-effect-entry-modality e) add1 0)))
  h)

;; ── phase 1: discovery ────────────────────────────────────────────

(define ROOT (repo-root))
(define PATHS (compiler-source-paths))

(define RACKLR-DIR (path->string (build-path ROOT "racklr")))
(define EFFERVESCE-DIR (path->string (build-path ROOT "effervesce")))

(define (under? dir p)
  (string-prefix? (path->string p) (string-append dir "/")))

(define racklr-count
  (for/sum ([p (in-list PATHS)]) (if (under? RACKLR-DIR p) 1 0)))
(define effervesce-count
  (for/sum ([p (in-list PATHS)]) (if (under? EFFERVESCE-DIR p) 1 0)))

(printf "discovery: ~a compiler source modules (~a racklr, ~a effervesce)\n"
        (length PATHS) racklr-count effervesce-count)

(check-true! "repo root contains racklr/ and effervesce/"
             (and (directory-exists? (build-path ROOT "racklr"))
                  (directory-exists? (build-path ROOT "effervesce"))))
(check-true! "discovery finds at least 120 compiler modules"
             (>= (length PATHS) 120))
(check-true! "every discovered source is a .rkt file"
             (andmap (λ (p) (regexp-match? #rx"[.]rkt$" (path->string p))) PATHS))
(check-true! "every discovered source is under <root>/racklr or <root>/effervesce"
             (andmap (λ (p) (or (under? RACKLR-DIR p) (under? EFFERVESCE-DIR p))) PATHS))
(check-true! "discovery excludes generated output (compiled/ directories)"
             (andmap (λ (p) (not (regexp-match? #rx"(^|/)compiled/" (path->string p)))) PATHS))
(check-true! "discovery excludes package info.rkt files"
             (andmap (λ (p) (not (equal? (path->string (file-name-from-path p)) "info.rkt")))
                     PATHS))
(check-true! "discovery covers the racklr collection (>= 80 modules)"
             (>= racklr-count 80))
(check-true! "discovery covers the effervesce collection (>= 40 modules)"
             (>= effervesce-count 40))

;; ── phase 2: every module lowers to a uir-module ──────────────────

(define PROGRAMS (compiler-programs))

(printf "lowering: ~a modules lowered to UIR\n" (length PROGRAMS))

(check-true! "lowering returns one program per discovered source"
             (equal? (length PROGRAMS) (length PATHS)))
(check-true! "every lowered program is a uir-module"
             (andmap uir-module? (map cdr PROGRAMS)))
(check-true! "every component id is a symbol"
             (andmap symbol? (map car PROGRAMS)))
(check-true! "component ids are unique and are module paths rooted at the checkout"
             (and (equal? (map car PROGRAMS) (map (λ (p) (path->component ROOT p)) PATHS))
                  (= (length (remove-duplicates (map car PROGRAMS))) (length PROGRAMS))))

(define EMIT-JSON-PATH
  (findf (λ (p) (equal? (path->string (file-name-from-path p)) "emit-json.rkt")) PATHS))
(check-true! "lower-racket-file lowers a compiler module in isolation"
             (and EMIT-JSON-PATH (uir-module? (lower-racket-file EMIT-JSON-PATH))))

;; ── phase 3: inference over the compiler's own UIR ────────────────

(define ROWS (compiler-effect-rows #:programs PROGRAMS))
(define NAMED (named-keys PROGRAMS))
(define MISSING (filter (λ (k) (not (hash-has-key? ROWS k))) NAMED))

(printf "inference: ~a rows over ~a named functions (~a row keys missing)\n"
        (hash-count ROWS) (length NAMED) (length MISSING))

(check-true! "whole-corpus inference completed without crashing (rows computed)"
             (hash? ROWS))
(check-true! "inferred rows are keyed by (component . function-name) pairs"
             (andmap (λ (k) (and (pair? k) (symbol? (car k)) (symbol? (cdr k))))
                     (hash-keys ROWS)))
(check-true! "the compiler has at least 1000 named functions"
             (>= (length NAMED) 1000))
(check-true! "every named function has an inferred row key"
             (null? MISSING))

;; Every row entry is a real effect or a tracked unknown — never a bare name.
(define bad-modalities 0)
(define bad-entry-names 0)
(define entry-count 0)
(for ([(k row) (in-hash ROWS)])
  (for ([e (in-list (uir-effect-row-entries row))])
    (set! entry-count (add1 entry-count))
    (if (memq (uir-effect-entry-modality e) '(unknown may))
        (void)
        (set! bad-modalities (add1 bad-modalities)))
    (if (uir-symbol? (uir-effect-entry-name e))
        (void)
        (set! bad-entry-names (add1 bad-entry-names)))))

(check-true! "every row entry has a tracked modality ('unknown or 'may)"
             (zero? bad-modalities))
(check-true! "every row entry names its effect with a uir-symbol"
             (zero? bad-entry-names))

;; ── phase 4: annotation writes the inferred row to every function ──

(define ANNOTATED (compiler-annotated-programs #:programs PROGRAMS #:rows ROWS))

(define slot-missing 0)
(define row-mismatch 0)
(for ([cp (in-list ANNOTATED)])
  (define comp (car cp))
  (for ([n (in-list (named-fn-nodes (cdr cp)))])
    (define nm (string->symbol (uir-symbol-name (uir-fn-name n))))
    (define attached (uir-fn-effects n))
    (define expected (hash-ref ROWS (cons comp nm) #f))
    (if (uir-effect-row? attached)
        (void)
        (set! slot-missing (add1 slot-missing)))
    (if (equal? attached expected)
        (void)
        (set! row-mismatch (add1 row-mismatch)))))

(printf "annotation: ~a programs; ~a functions missing a row slot; ~a rows differing from inference\n"
        (length ANNOTATED) slot-missing row-mismatch)

(check-true! "annotation returns one program per module"
             (equal? (length ANNOTATED) (length PROGRAMS)))
(check-true! "annotated programs are uir-modules"
             (andmap uir-module? (map cdr ANNOTATED)))
(check-true! "every named function carries a uir-effect-row in its effects slot"
             (zero? slot-missing))
(check-true! "every attached row is exactly the inferred row (unknown kept, nothing invented)"
             (zero? row-mismatch))

;; ── phase 5: determinism ──────────────────────────────────────────

(define SAMPLE-COMPS '(racklr/g4-parse racklr/emit-json racklr/tree))
(define SAMPLE (filter (λ (cp) (memq (car cp) SAMPLE-COMPS)) PROGRAMS))
(define SAMPLE-PATHS
  (filter (λ (p) (memq (path->component ROOT p) SAMPLE-COMPS)) PATHS))

(define SAMPLE-ROWS-1
  (with-handlers ([exn:fail?
                   (λ (e)
                     (eprintf "ERROR: infer-effects-all crashed: ~a\n" (exn-message e))
                     #f)])
    (infer-effects-all SAMPLE)))
(define SAMPLE-ROWS-2
  (with-handlers ([exn:fail?
                   (λ (e)
                     (eprintf "ERROR: infer-effects-all crashed: ~a\n" (exn-message e))
                     #f)])
    (infer-effects-all SAMPLE)))

(check-true! "determinism sample resolves exactly three modules"
             (equal? (length SAMPLE) 3))
(check-true! "inference over the sample runs without crash"
             (and (hash? SAMPLE-ROWS-1) (hash? SAMPLE-ROWS-2)))
(check-true! "inference rows are deterministic across two runs (sample)"
             (equal? SAMPLE-ROWS-1 SAMPLE-ROWS-2))
(check-true! "lowering is deterministic across two runs (sample)"
             (equal? (lower-racket-modules SAMPLE-PATHS #:root ROOT)
                     (lower-racket-modules SAMPLE-PATHS #:root ROOT)))

;; ── phase 6: the report reports the rows ─────────────────────────

(define REPORT (compiler-report #:programs PROGRAMS))
(define REPORT-HIST (hash-ref REPORT 'effect-histogram (hash)))
(define REPORT-MODALITIES (hash-ref REPORT 'entry-modality-histogram (hash)))
(define REPORT-UNKNOWN-FNS (hash-ref REPORT 'unknown-function-count #f))
(define REPORT-UNKNOWN-SITES (hash-ref REPORT 'unknown-site-count #f))
(define REPORT-UNKNOWN-RATIO (hash-ref REPORT 'unknown-function-ratio #f))
(define REPORT-STRING (compiler-report->string REPORT))

(printf "report: ~a modules, ~a functions, ~a with effect rows\n"
        (hash-ref REPORT 'modules 0)
        (hash-ref REPORT 'functions 0)
        (hash-ref REPORT 'functions-with-row 0))
(printf "effect histogram: ~a\n" (histogram->string REPORT-HIST))
(printf "entry modalities: ~a\n" (histogram->string REPORT-MODALITIES))
(printf "unknown: ~a functions (~a of rows), ~a call sites, ratio limit ~a\n"
        REPORT-UNKNOWN-FNS
        (if (real? REPORT-UNKNOWN-RATIO)
            (format "~a%" (real->decimal-string (* 100.0 REPORT-UNKNOWN-RATIO) 1))
            "?")
        REPORT-UNKNOWN-SITES
        UNKNOWN-FUNCTION-RATIO-LIMIT)

(check-true! "report modules count matches the lowered corpus"
             (equal? (hash-ref REPORT 'modules #f) (length PROGRAMS)))
(check-true! "report functions count matches named functions"
             (equal? (hash-ref REPORT 'functions #f) (length NAMED)))
(check-true! "report functions-with-row covers every named function (DoD)"
             (equal? (hash-ref REPORT 'functions-with-row #f) (length NAMED)))
(check-true! "report covers every module in its per-component counts"
             (equal? (hash-count (hash-ref REPORT 'per-component (hash)))
                     (length PROGRAMS)))
(check-true! "report effect histogram is non-empty"
             (> (hash-count REPORT-HIST) 0))
(check-true! "report histogram matches an independent histogram over the rows"
             (equal? REPORT-HIST (histogram-of-rows ROWS)))
(check-true! "report unknown function count is present and finite"
             (and (exact-nonnegative-integer? REPORT-UNKNOWN-FNS)
                  (<= REPORT-UNKNOWN-FNS (hash-count ROWS))))
(check-true! "report unknown site count is present and finite"
             (exact-nonnegative-integer? REPORT-UNKNOWN-SITES))
(check-true! "unknown functions are tracked in the report, not required to be zero"
             (and (exact-nonnegative-integer? REPORT-UNKNOWN-FNS)
                  (exact-nonnegative-integer? (hash-ref REPORT 'missing-row-count #f))))
(check-true! "unknown-function ratio is under the agreed quality threshold"
             (and (real? REPORT-UNKNOWN-RATIO)
                  (<= REPORT-UNKNOWN-RATIO UNKNOWN-FUNCTION-RATIO-LIMIT)))
(check-true! "report string is human-readable and mentions unknown rows"
             (and (string? REPORT-STRING)
                  (string-contains? REPORT-STRING "unknown")
                  (string-contains? REPORT-STRING "effect histogram")))

;; ── phase 7: ground truth from the source call chains ────────────

(define G4-ROW (function-row ROWS 'racklr/g4-parse "parse-g4-file"))
(define ESBUILD-ROW (function-row ROWS 'racklr/esbuild-bundle "esbuild-bundle"))
(define EMIT-JSON-ROW (function-row ROWS 'racklr/emit-json "write-json-string"))

(printf "ground truth:\n  racklr/g4-parse::parse-g4-file       ~a\n  racklr/esbuild-bundle::esbuild-bundle ~a\n  racklr/emit-json::write-json-string   ~a\n"
        (row->string G4-ROW)
        (row->string ESBUILD-ROW)
        (row->string EMIT-JSON-ROW))

;; parse-g4-file calls file->string (io) directly.
(check-true! "ground truth: racklr/g4-parse parse-g4-file contains io"
             (row-has? G4-ROW 'io))
;; esbuild-bundle runs `system` (process) and reads the output file (io).
(check-true! "ground truth: racklr/esbuild-bundle esbuild-bundle contains process"
             (row-has? ESBUILD-ROW 'process))
;; write-json-string writes to an output port with display (io).
(check-true! "ground truth: racklr/emit-json write-json-string contains io"
             (row-has? EMIT-JSON-ROW 'io))

;; ── verdict ───────────────────────────────────────────────────────

(if (null? FAILURES)
    (begin
      (printf "PASS: compiler-uir — P6.1 Definition of Done (~a checks; ~a modules, ~a named functions, ~a rows, ~a with effect rows, ~a unknown)\n"
              CHECKS
              (length PROGRAMS)
              (length NAMED)
              (hash-count ROWS)
              (hash-ref REPORT 'functions-with-row 0)
              REPORT-UNKNOWN-FNS)
      (exit 0))
    (begin
      (printf "FAIL: ~a of ~a checks failed\n" (length FAILURES) CHECKS)
      (for ([f (in-list (reverse FAILURES))])
        (eprintf "  failed: ~a\n" f))
      (exit 1)))
