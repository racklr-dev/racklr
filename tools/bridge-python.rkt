#lang racket

;; tools/bridge-python.rkt — run the Effervesce bridge over Python source.
;;
;; effervesce/check/host-python.rkt generates its Python parser from the
;; untracked vendored `grammars-v4/` tree. The repository's own Python3 front
;; end uses the tracked `grammars/python3/` grammar (README, "Modern Python 3
;; syntax"), so this tool is the racklr-side counterpart of
;; effervesce/tools/verify-dir.rkt: parse and lower every `*.py` under a
;; directory with the tracked grammar, then run the four Effervesce checkers
;; (type, policy, model, bridge) with that host code under every component the
;; spec declares (or the one named by `--component`).
;;
;; Unlike `verify-dir.rkt` it does not merge the files into one program. A
;; merged program collides on repeated top-level names — the vendored FastAPI
;; sample has five alembic migrations that each define `upgrade`/`downgrade`,
;; plus repeated `create_user`/`create_item`/`main`/`init` — and
;; `infer-effects` keys rows by function name, so a merged run would silently
;; drop the effect rows of the overwritten definitions. The bridge is run once
;; per file here and the findings are unioned.
;;
;; Usage (from the repository root):
;;
;;   racket tools/bridge-python.rkt <dir> <spec.fizz>
;;         [--component NAME] [--interpret Cap:e1,e2]...
;;
;; Interpretations map a capability name to the allowed host effect names; the
;; default is the lowercased capability name, plus the one well-known alias
;; Http -> network. Repeated `--interpret` flags override the default for that
;; capability. The exit code is 0 when every file parses and lowers cleanly and
;; every checker is green.

(require racket/file
         racket/list
         racket/path
         racket/string
         racklr/uir
         racklr/lower-python
         racklr/effects/infer
         racklr/effects/contract
         effervesce/load
         effervesce/check/bridge
         effervesce/check/verify
         "scan-python.rkt")

(provide source-run source-run? source-run-path source-run-status source-run-program
         bridge-run bridge-run?
         bridge-run-files bridge-run-components bridge-run-interpretations
         bridge-run-findings bridge-run-bridge-issues bridge-run-rows-by-file
         bridge-run-violations bridge-run-unverified bridge-run-ok?
         bridge-run-effect-histogram bridge-run-function-count
         bridge-run-unknown-function-count
         default-interpretations
         run-bridge-over-dir)

;; ── one source file through the Python3 front end ───────────────────

(struct source-run (path status program) #:transparent)

;; Repo-relative label for a scanned file, e.g. "api/routes/items.py".
(define (file-label dir path)
  (define root (simplify-path (path->complete-path dir) #f))
  (path->string (find-relative-path root (simplify-path (path->complete-path path) #f))))

;; Parse + lower one file, classifying the first pipeline stage that fails the
;; same way tools/scan-python.rkt does (INDENT-ERROR / PARSE-FAIL /
;; LOWER-ERROR / FALLBACK), but keep the UIR for the bridge.
(define (lower-source-file path parser)
  (define status 'ok)
  (define program #f)
  (define tks
    (with-handlers ([exn:fail? (λ (_e) (set! status 'INDENT-ERROR) #f)])
      ((python-parser-tokenize parser) (file->string path))))
  (when tks
    (define res
      (with-handlers ([exn:fail? (λ (_e) (set! status 'PARSE-FAIL) #f)])
        ((python-parser-parse-file-input parser) tks 0)))
    (cond
      [(and (list? res) (= (length res) 2))
       (set! program
             (with-handlers ([exn:fail? (λ (_e) (set! status 'LOWER-ERROR) #f)])
               (lower-python (second res)
                             (python-parser-token-type parser)
                             (python-parser-token-value parser))))
       (when (and (not program) (eq? status 'ok))
         (set! status 'LOWER-ERROR))]
      [else (set! status 'PARSE-FAIL)]))
  (when (and program (has-fallback? program))
    (set! status 'FALLBACK))
  (source-run path status program))

;; ── the spec: components and capability interpretations ─────────────

(define (spec-component-names sys)
  (for/list ([d (in-list (uir-eff-system-decls sys))]
             #:when (uir-eff-component? d))
    (uir-symbol-name (uir-eff-component-name d))))

(define (default-interpretations sys overrides)
  (define h (make-hash))
  (for ([d (in-list (uir-eff-system-decls sys))]
        #:when (uir-eff-capability? d))
    (define nm (uir-symbol-name (uir-eff-capability-name d)))
    (hash-set! h nm (list (string->symbol (string-downcase nm)))))
  ;; The host effect is named `network`, not `http`.
  (hash-set! h "Http" '(network))
  (for ([(cap effs) (in-hash overrides)])
    (hash-set! h cap effs))
  h)

;; ── the bridge run ──────────────────────────────────────────────────

(struct bridge-run
  (files components interpretations findings bridge-issues rows-by-file)
  #:transparent)

;; Run the four checkers over <dir> against <spec-path>. `findings` holds the
;; spec-only checkers (type, policy, model); the bridge findings are per file
;; in `bridge-issues` as (list (cons file-label (listof issue))).
;;
;; #:component restricts the run to one component (default: every component in
;; the spec); #:interpretations overrides capability -> effect names.
(define (run-bridge-over-dir dir spec-path
                             #:component [component #f]
                             #:interpretations [overrides (hash)])
  (define parser (load-python-parser
                  (build-path (repository-root) "grammars" "python3" "Python3Parser.g4")))
  (define files
    (for/list ([p (in-list (find-python-files dir))])
      (lower-source-file p parser)))
  (define parsed (filter (λ (r) (eq? (source-run-status r) 'ok)) files))
  (define sys (load-effervesce (file->string spec-path)))
  (define comps (if component (list component) (spec-component-names sys)))
  (define interps (default-interpretations sys overrides))
  ;; Type/policy/model depend on the spec only: pass no host code so `verify`
  ;; produces no bridge findings, which are reported per file below.
  (define findings (verify sys (hash) interps))
  (define rows-by-file
    (for/hash ([r (in-list parsed)])
      (values (file-label dir (source-run-path r))
              (infer-effects (source-run-program r)))))
  (define bridge-issues
    (for/list ([r (in-list parsed)])
      (cons (file-label dir (source-run-path r))
            (append*
             (for/list ([c (in-list comps)])
               (check-bridge sys (hash c (source-run-program r)) interps))))))
  (bridge-run files comps interps findings bridge-issues rows-by-file))

;; ── reporting helpers ───────────────────────────────────────────────

(define (flatten-issues run pred)
  (append*
   (for/list ([pair (in-list (bridge-run-bridge-issues run))])
     (for/list ([i (in-list (cdr pair))] #:when (pred i))
       (list (car pair) i)))))

(define (bridge-run-violations run)
  (flatten-issues run bridge-violation?))

(define (bridge-run-unverified run)
  (flatten-issues run bridge-unverified?))

(define (bridge-run-ok? run)
  (and (for/and ([r (in-list (bridge-run-files run))])
         (eq? (source-run-status r) 'ok))
       (verify-ok? (bridge-run-findings run))
       (for/and ([pair (in-list (bridge-run-bridge-issues run))])
         (bridge-ok? (cdr pair)))))

;; Effect-name symbol -> number of functions (across files) whose inferred row
;; carries that effect. Rows are per file, so repeated top-level names do not
;; collide.
(define (bridge-run-effect-histogram run)
  (define hist (make-hash))
  (for ([rows (in-hash-values (bridge-run-rows-by-file run))])
    (for ([row (in-hash-values rows)])
      (for ([e (in-list (uir-effect-row-entries row))])
        (hash-update! hist (effect-name-symbol e) add1 0))))
  hist)

(define (bridge-run-function-count run)
  (for/sum ([rows (in-hash-values (bridge-run-rows-by-file run))])
    (hash-count rows)))

(define (bridge-run-unknown-function-count run)
  (for/sum ([rows (in-hash-values (bridge-run-rows-by-file run))])
    (for/sum ([(nm row) (in-hash rows)])
      (if (member 'unknown (map effect-name-symbol (uir-effect-row-entries row))) 1 0))))

(define (effect-histogram-string hist)
  (string-join
   (for/list ([kv (in-list (sort (hash->list hist)
                                 (λ (a b)
                                   (string<? (symbol->string (car a))
                                             (symbol->string (car b))))))])
     (format "~a=~a" (car kv) (cdr kv)))
   " "))

;; ── command line ────────────────────────────────────────────────────

(define (print-bridge-issue file i)
  (cond
    [(bridge-violation? i)
     (define sites (bridge-violation-sites i))
     (printf "  ~a: [bridge:bridge-violation] component ~a performs uncovered effect ~a~a\n"
             file
             (bridge-violation-component i)
             (bridge-violation-effect i)
             (if (null? sites)
                 ""
                 (format " — via ~a" (string-join (take sites (min 4 (length sites))) ", "))))]
    [(bridge-unverified? i)
     (printf "  ~a: [bridge:bridge-unverified] component ~a has unresolvable (higher-order) calls\n"
             file (bridge-unverified-component i))]
    [else (printf "  ~a: unknown bridge issue\n" file)]))

(module+ main
  (define args (vector->list (current-command-line-arguments)))
  (define opts (make-hash))
  (define overrides (make-hash))
  (define pos '())
  (let loop ([as args])
    (cond
      [(null? as) (void)]
      [(string=? (car as) "--component")
       (hash-set! opts 'component (second as)) (loop (cddr as))]
      [(string=? (car as) "--interpret")
       (match (regexp-split #rx":" (second as))
         [(list cap effs)
          (hash-set! overrides cap (map string->symbol (regexp-split #rx"," effs)))]
         [_ (eprintf "bad --interpret ~a\n" (second as)) (exit 2)])
       (loop (cddr as))]
      [else (set! pos (cons (car as) pos)) (loop (cdr as))]))
  (set! pos (reverse pos))
  (unless (= (length pos) 2)
    (eprintf "usage: bridge-python.rkt <dir> <spec.fizz> [--component NAME] [--interpret Cap:e1,e2]...\n")
    (exit 2))
  (define dir (first pos))
  (define spec-path (second pos))
  (define run (run-bridge-over-dir dir spec-path
                                   #:component (hash-ref opts 'component #f)
                                   #:interpretations overrides))
  (define files (bridge-run-files run))
  (define ok-count
    (count (λ (r) (eq? (source-run-status r) 'ok)) files))
  (printf "bridge-python: ~a — ~a files (~a ok, ~a failed)\n"
          dir (length files) ok-count (- (length files) ok-count))
  (for ([r (in-list files)] #:unless (eq? (source-run-status r) 'ok))
    (printf "  ~a  ~a\n" (source-run-status r) (source-run-path r)))
  (define hist (bridge-run-effect-histogram run))
  (define comps (bridge-run-components run))
  (printf "functions: ~a (~a with an unresolved call)\n"
          (bridge-run-function-count run)
          (bridge-run-unknown-function-count run))
  (printf "effects: ~a\n" (effect-histogram-string hist))
  (printf "spec: ~a component~a ~a\n"
          (length comps) (if (= 1 (length comps)) "" "s")
          (string-join comps ", "))
  (printf "type/policy/model: ~a finding~a\n"
          (length (bridge-run-findings run))
          (if (= 1 (length (bridge-run-findings run))) "" "s"))
  (for ([m (in-list (verify-report (bridge-run-findings run)))])
    (printf "  ~a\n" m))
  (define violations (bridge-run-violations run))
  (define unverified (bridge-run-unverified run))
  (printf "bridge: ~a violation~a, ~a unverified file finding~a\n"
          (length violations) (if (= 1 (length violations)) "" "s")
          (length unverified) (if (= 1 (length unverified)) "" "s"))
  (for ([pair (in-list (bridge-run-bridge-issues run))])
    (for ([i (in-list (cdr pair))])
      (print-bridge-issue (car pair) i)))
  (printf "~a\n" (if (bridge-run-ok? run) "ok" "FAILED"))
  (exit (if (bridge-run-ok? run) 0 1)))
