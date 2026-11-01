#lang racket

;; tools/self-emit.rkt — P6.3 self-emission check.
;;
;; The acceptance question: does a translation through the *self-emitted* tower
;; produce the same UIR-plus-row output as the original, and does the emitted
;; compiler pass the shared gate? This tool is the oracle and the differential:
;;
;;   racket tools/self-emit.rkt languages
;;   racket tools/self-emit.rkt oracle <language> <file> [--out <file>]
;;   racket tools/self-emit.rkt goldens <language> <file>... --out-dir <dir>
;;   racket tools/self-emit.rkt verify --tower <executable> <file>...
;;   racket tools/self-emit.rkt tower          (run the P6.2 emitted compiler)
;;   racket tools/self-emit.rkt gate [--no-compile]
;;   racket tools/self-emit.rkt selftest
;;
;; The emitted-tower contract is deliberately small. An executable is invoked as
;;
;;     <tower> dump-uir <language> <source-file>
;;
;; and must print one canonical JSON document (format "racklr-self-emit/1";
;; shape in tools/self-emit/canonical.rkt) on stdout and exit 0. The command is
;; free to live in any language; the document is how it proves it translates
;; the same way the original does. docs/self-emit-verification.md states the
;; contract in full.
;;
;; Exit codes: 0 = ok / every translation matches; 1 = a mismatch or a red
;; gate; 2 = the check could not run (missing or failing emitted tower, bad
;; input, unsupported language).

(require racket/file
         racket/list
         racket/path
         racket/string
         racket/system
         "self-emit/canonical.rkt"
         "self-emit/differential.rkt"
         "self-emit/gate.rkt"
         "self-emit/oracle.rkt"
         "self-emit/tower.rkt")

(provide main)

;; ── argument parsing ────────────────────────────────────────────────

;; Returns (values positional-list flag-hash). Flags are --name value pairs;
;; --help is a bare flag.
(define (parse-arguments args)
  (let loop ([args args] [positional '()] [flags (hash)])
    (cond
      [(null? args) (values (reverse positional) flags)]
      [(member (car args) '("--help" "-h"))
       (loop (cdr args) positional (hash-set flags (car args) #t))]
      [(string-prefix? (car args) "--")
       (cond
         [(null? (cdr args))
          (error 'parse-arguments "flag ~a needs a value" (car args))]
         [else (loop (cddr args) positional
                     (hash-set flags (car args) (cadr args)))])]
      [else (loop (cdr args) (cons (car args) positional) flags)])))

(define (flag-ref flags name [default #f])
  (hash-ref flags name default))

;; ── commands ────────────────────────────────────────────────────────

(define (usage)
  (displayln
   (string-join
    '("usage: racket tools/self-emit.rkt <command> [options]"
      ""
      "  languages                            list supported source languages"
      "  oracle <language> <file> [--out f]   canonical UIR-plus-row JSON (original tower)"
      "  goldens <language> <file>... --out-dir <dir>"
      "                                       write canonical goldens for a tower to match"
      "  verify --tower <executable> <file>..."
      "                                       compare an emitted tower with the original"
      "  tower                                run the P6.2 emitted compiler and compare"
      "                                       its translations with the original contract"
      "  gate [--no-compile]                  run the shared racklr + effervesce gate"
      "  selftest                             prove the checker detects match and drift"
      ""
      "The emitted-tower contract: <tower> dump-uir <language> <source-file> prints one"
      "canonical JSON document and exits 0. See docs/self-emit-verification.md.")
    "\n")))

(define (command-oracle positional flags)
  (define rest (cdr positional))
  (unless (>= (length rest) 2)
    (error 'oracle "usage: oracle <language> <file> [--out <file>]"))
  (define language (string->symbol (first rest)))
  (define path (second rest))
  (define document (original-document language path))
  (define text (canonical-json-string document))
  (define out (flag-ref flags "--out"))
  (cond
    [out (display-to-file text out #:exists 'replace)
         (printf "wrote ~a\n" out)]
    [else (displayln text)]))

(define (command-goldens positional flags)
  (define rest (cdr positional))
  (define out-dir (flag-ref flags "--out-dir"))
  (unless (and out-dir (pair? rest) (>= (length rest) 2))
    (error 'goldens "usage: goldens <language> <file>... --out-dir <dir>"))
  (define language (string->symbol (first rest)))
  (make-directory* out-dir)
  (for ([path (in-list (rest rest))])
    (define name (path->string (file-name-from-path path)))
    (define out (build-path out-dir (string-append name ".canonical.json")))
    (display-to-file (canonical-json-string (original-document language path))
                     out #:exists 'replace)
    (printf "wrote ~a\n" out)))

(define (command-verify positional flags)
  (define tower (flag-ref flags "--tower"))
  (unless tower
    (error 'verify "usage: verify --tower <executable> <file>..."))
  (define files (cdr positional))
  (define fixtures (if (null? files) (default-fixtures) files))
  (define report (verify-translations tower fixtures))
  (print-verify-report report)
  (cond
    [(hash-ref report 'unrunnable?) 2]
    [(hash-ref report 'ok?) 0]
    [else 1]))

(define (command-gate positional flags)
  (define root (repo-root))
  (define baseline (run-baseline root))
  (define verdict (hash-ref baseline 'verdict))
  (printf "baseline: ~a~a\n"
          (case verdict
            [(ok) "OK"]
            [(coverage-failed) "COVERAGE-FAILED"]
            [(gate-failed) "GATE-FAILED"]
            [(drift) "DRIFT"]
            [else "FAILED"])
          (let ([h (hash-ref baseline 'hash)]) (if h (format " ~a" h) "")))
  (cond
    [(eq? verdict 'coverage-failed)
     (printf "  the graph matches the pin, but the Phase-0.3 unknown-elimination\n")
     (printf "  obligation is open: ~a unknown functions in ~a files\n"
             (hash-ref baseline 'unknown-functions)
             (hash-ref baseline 'unknown-files))]
    [(not (eq? verdict 'ok)) (printf "~a\n" (hash-ref baseline 'output))])
  (define gate (run-gate root))
  (for ([r (in-list (hash-ref gate 'results))])
    (printf "suite: ~a ~a (exit ~a)~a\n"
            (hash-ref r 'name)
            (if (hash-ref r 'ok?) "PASS" "FAIL")
            (hash-ref r 'exit)
            (if (hash-ref r 'ok?) "" (format "~a~a"
                                             (if (null? (hash-ref r 'failures))
                                                 "" "; failure markers: ")
                                             (string-join (hash-ref r 'failures) " | ")))))
  (define compile
    (if (flag-ref flags "--no-compile")
        (hash 'ok? #t 'skipped? #t)
        (run-compile root)))
  (unless (hash-ref compile 'skipped? #f)
    (printf "compile: ~a (~a files)~n"
            (if (hash-ref compile 'ok?) "OK" "FAILED")
            (hash-ref compile 'files))
    (unless (hash-ref compile 'ok?) (printf "~a\n" (hash-ref compile 'output))))
  (define suites-and-compile? (and (hash-ref gate 'ok?) (hash-ref compile 'ok?)))
  (cond
    [(and (eq? verdict 'ok) suites-and-compile?)
     (printf "gate: OK\n")
     0]
    [(and (eq? verdict 'coverage-failed) suites-and-compile?)
     (printf "gate: COVERAGE-FAILED (suites and compile green; graph matches the pin;\n")
     (printf "Phase-0.3 unknown-elimination obligation open)\n")
     3]
    [else
     (printf "gate: FAILED\n")
     1]))

(define (command-tower)
  (define report (run-tower-differential))
  (cond
    [(not (hash-ref report 'available?))
     (define availability (hash-ref report 'availability))
     (printf "P6.2 self-image not present in this checkout.\n")
     (for ([(module-name present?) (in-hash availability)])
       (printf "  ~a: ~a\n" module-name (if present? "present" "missing")))
     (printf "\nThe emitted compiler is built by P6.2 (issue #39); until it is merged\n")
     (printf "there is nothing to run. `verify --tower` takes an already-built tower.\n")
     2]
    [else
     (printf "emitted compiler: ~a\n" (hash-ref report 'emitted-path))
     (printf "rows native in emitted source: ~a\n"
             (if (hash-ref report 'rows-native?) "yes" "NO"))
     (printf "effect rows vs original UIR program: ~a (~a functions)\n"
             (if (hash-ref report 'rows-match?) "MATCH" "MISMATCH")
             (length (hash-ref report 'expected-rows)))
     (unless (hash-ref report 'rows-match?)
       (define emitted (hash-ref report 'emitted-rows))
       (for ([pair (in-list (hash-ref report 'expected-rows))])
         (define name (car pair))
         (unless (equal? (cdr pair) (cdr (assoc name emitted)))
           (printf "  row differs for ~a\n" name))))
     (define absorb (hash-ref report 'absorb))
     (printf "absorb emitted source: ~a (~a/~a functions carry a row; ~a unknown)\n"
             (if (hash-ref absorb 'ok?) "OK" "FAILED")
             (hash-ref absorb 'rows) (hash-ref absorb 'functions)
             (hash-ref absorb 'unknown))
     (for ([c (in-list (hash-ref report 'cases))])
       (printf "~a  ~s -> ~s~a\n"
               (if (hash-ref c 'match-compiled?) "MATCH   " "MISMATCH")
               (hash-ref c 'source)
               (hash-ref c 'output)
               (cond
                 [(not (hash-ref c 'ok?))
                  (format "  (exit failed; stderr: ~a)" (string-trim (hash-ref c 'stderr)))]
                 [(not (hash-ref c 'match-result?)) "  (compiled program result mismatch)"]
                 [else ""])))
     (printf "\n~a/~a translations match the original contract\n"
             (for/sum ([c (in-list (hash-ref report 'cases))]
                       #:when (hash-ref c 'match-compiled?))
               1)
             (length (hash-ref report 'cases)))
     (if (hash-ref report 'ok?) 0 1)]))

(define (command-selftest)
  (run-selftest))

(define (main args)
  (cond
    [(null? args) (usage) 2]
    [else
     (define command (car args))
     (define-values (positional flags) (parse-arguments (cdr args)))
     (with-handlers ([exn:fail? (λ (e) (eprintf "self-emit: ~a\n" (exn-message e)) 2)])
       (cond
         [(equal? command "languages")
          (printf "~a\n" (string-join (map symbol->string (supported-languages)) " "))
          0]
         [(equal? command "oracle") (command-oracle (cons command positional) flags) 0]
         [(equal? command "goldens") (command-goldens (cons command positional) flags) 0]
         [(equal? command "verify") (command-verify (cons command positional) flags)]
         [(equal? command "tower") (command-tower)]
         [(equal? command "gate") (command-gate (cons command positional) flags)]
         [(equal? command "selftest") (command-selftest)]
         [(equal? command "help") (usage) 0]
         [else (eprintf "self-emit: unknown command: ~a\n" command) (usage) 2]))]))

;; The generated Python parser is written as gen-tmp-*.rkt in the working
;; directory; a one-shot CLI run must not leave that file behind.
(define (run-and-clean thunk)
  (dynamic-wind void thunk (λ () (cleanup-oracle))))

(module+ main
  (exit (run-and-clean
         (λ () (main (vector->list (current-command-line-arguments)))))))
