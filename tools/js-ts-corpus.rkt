#lang racket

;; tools/js-ts-corpus.rkt — the JavaScript/TypeScript slice of the
;; cross-language capability battery: command line, summary and JSON artifact.
;;
;; For every source file under a corpus root, run the front end's real entry
;; points and classify the file by the first stage that blocks a clean
;; absorb -> emit -> re-parse round trip:
;;
;;   ok             tokenize + parse + lower + emit + re-parse all succeeded
;;   tokenize-error the generated tokenizer raised
;;   parse-fail     the generated parser returned #f or raised
;;   lower-error    the lowering hook raised on a CST it should handle
;;   fallback       lowered, but a `?`-prefixed unhandled-node symbol remains
;;   emit-error     the emitter raised
;;   emit-empty     the emitter returned a non-string or an empty string
;;   reparse-fail   the emitted text did not re-parse (or raised)
;;   timeout        a single-file pipeline exceeded --timeout-ms (watchdog);
;;                  quarantined as a processing failure, never counted clean
;;
;; Languages are selected by extension, matching the cross-language battery:
;; `.js` / `.mjs` (JavaScript), `.ts` (TypeScript), `.tsx` (TSX, parsed as
;; TypeScript after the JSX preprocess) and `.json`. TypeScript and TSX emit
;; JavaScript, so their re-parse stage runs the JavaScript parser (one-way
;; verification, matching examples-roundtrip-test); their tokenize/parse stage
;; runs the preprocess-imports/JSX preprocess the front end needs. `.cjs` is
;; deliberately not selected: it is the CommonJS configuration path tracked
;; separately, and the historical battery left it out.
;;
;; The generated parsers come from the untracked vendored `grammars-v4/` tree
;; (see README "ANTLR4 grammars"); they are generated into a private scratch
;; directory, so parallel runs in the same checkout cannot collide on
;; gen-tmp-N.rkt and the checkout stays clean.
;;
;; Usage (from the repository root, with the checkout on PLTCOLLECTS):
;;
;;   PLTCOLLECTS="$(pwd):" racket tools/js-ts-corpus.rkt <corpus-dir> ...
;;                                              [--parse-only]
;;                                              [--json <path>]
;;                                              [--timeout-ms N]
;;
;;   --parse-only  stop after parse; report tokenize-error / parse-fail /
;;                 parse-ok only (the fast grammar/tokenizer measurement)
;;   --json PATH   write the full per-file result set as JSON to PATH
;;   --timeout-ms N per-file watchdog in milliseconds (default 60000); a file
;;                 that trips it is classified `timeout` (a quarantined
;;                 processing failure, listed by name in the summary, never
;;                 folded into the ok count) and the scan continues
;;
;; Exit codes: 0 = every file in the selected mode is ok/parse-ok,
;; 1 = at least one blocker, 2 = usage error.

(require racket/list
         racket/path
         racket/string
         json
         "js-ts-corpus-lib.rkt")

(provide js-ts-corpus-main)

;; ── Summary + JSON ─────────────────────────────────────────────────

(define (print-summary corpora mode results)
  (printf "js/ts corpus: ~a\n" (string-join (map path->string corpora) " "))
  (printf "mode: ~a\n" (if mode "parse-only" "full round-trip"))
  (printf "files=~a\n" (length results))
  (define statuses (if mode parse-only-statuses full-statuses))
  (for ([s (in-list statuses)])
    (printf "  ~a=~a\n" s (count-status results s)))
  ;; Per-language totals make the JS and TS coverage separately visible.
  (for ([lang (in-list '(javascript typescript tsx json))])
    (define total (for/sum ([r (in-list results)])
                    (if (eq? lang (lang-of (scan-result-file r))) 1 0)))
    (when (> total 0)
      (printf "  [~a] files=~a ~a\n" lang total
              (string-join
               (for/list ([s (in-list statuses)])
                 (format "~a=~a" s (count-lang-status results lang s)))
               " "))))
  ;; A watchdog timeout is a processing failure, not a clean pass: name every
  ;; file that tripped the watchdog so it can never be folded into an ok count.
  (define timed-out (filter (λ (r) (eq? 'timeout (scan-result-status r))) results))
  (when (pair? timed-out)
    (printf "\nquarantined timeouts (~a; not counted as clean):\n" (length timed-out))
    (for ([r (in-list timed-out)])
      (printf "  ~a  (~a)\n" (scan-result-file r) (scan-result-detail r))))
  (define groups (failure-groups results mode))
  (define any? (for/or ([fg (in-list groups)]) (pair? (cadr fg))))
  (when any?
    (printf "\nfailure messages (status: count message; sample file):\n")
    (for ([fg (in-list groups)])
      (for ([m (in-list (cadr fg))])
        (printf "  ~a: ~a  ~s\n    e.g. ~a\n"
                (car fg) (cadr m) (car m) (caddr m))))))

(define (results->jsexpr corpora mode results)
  (hash 'corpora (map path->string corpora)
        'mode (if mode "parse-only" "full")
        'files (length results)
        'counts (for/hash ([s (in-list (if mode parse-only-statuses full-statuses))])
                  (values s (count-status results s)))
        'failures (for/list ([fg (in-list (failure-groups results mode))])
                    (hash 'status (symbol->string (car fg))
                          'messages
                          (for/list ([m (in-list (cadr fg))])
                            (hash 'message (car m)
                                  'count (cadr m)
                                  'sample (caddr m)))))
        'rows (for/list ([r (in-list results)])
                (hash 'file (path->string (scan-result-file r))
                      'lang (let ([l (lang-of (scan-result-file r))])
                              (if l (symbol->string l) null))
                      'status (symbol->string (scan-result-status r))
                      'detail (or (scan-result-detail r) null)))))

;; ── Command line ───────────────────────────────────────────────────

(define usage
  (string-append
   "usage: racket tools/js-ts-corpus.rkt <corpus-dir> ...\n"
   "                                    [--parse-only] [--json <path>]\n"
   "                                    [--timeout-ms N]\n"
   "  --parse-only  stop after parse (tokenize-error/parse-fail/parse-ok)\n"
   "  --json PATH   write the per-file result set as JSON\n"
   "  --timeout-ms N per-file watchdog in milliseconds (default 60000)\n"))

;; (list corpus-dirs parse-only? json-path timeout-ms) or #f on a usage error.
(define (parse-cli args)
  (define (flag? a) (string-prefix? a "--"))
  (define (collect-dirs rest acc)
    (cond [(null? rest) (values (reverse acc) rest)]
          [(flag? (car rest)) (values (reverse acc) rest)]
          [else (collect-dirs (cdr rest) (cons (car rest) acc))]))
  (let-values ([(corpora rest) (collect-dirs args '())])
    (cond
      [(null? corpora) #f]
      [else
       (let loop ([rest rest] [parse-only? #f] [json-path #f] [timeout-ms 60000])
         (cond
           [(null? rest) (list corpora parse-only? json-path timeout-ms)]
           [(equal? (car rest) "--parse-only") (loop (cdr rest) #t json-path timeout-ms)]
           [(equal? (car rest) "--json")
            (if (and (pair? (cdr rest)) (not (flag? (cadr rest))))
                (loop (cddr rest) parse-only? (cadr rest) timeout-ms)
                #f)]
           [(equal? (car rest) "--timeout-ms")
            (define v (and (pair? (cdr rest)) (string->number (cadr rest))))
            (if (and v (exact-positive-integer? v))
                (loop (cddr rest) parse-only? json-path v)
                #f)]
           [else #f]))])))

(define (js-ts-corpus-main args)
  (define cli (parse-cli args))
  (unless cli
    (eprintf "~a" usage)
    (exit 2))
  (define corpora (map (λ (d) (string->path d)) (first cli)))
  (define parse-only? (second cli))
  (define json-path (third cli))
  (define timeout-ms (list-ref cli 3))
  (for ([c (in-list corpora)])
    (unless (directory-exists? c)
      (eprintf "js-ts-corpus: no such corpus directory: ~a\n" c)
      (exit 2)))
  (define parsers (load-js-ts-parsers))
  (define results
    (append* (for/list ([c (in-list corpora)])
               (scan-js-ts-directory c parsers parse-only? timeout-ms))))
  (print-summary corpora parse-only? results)
  (when json-path
    (call-with-output-file json-path #:exists 'replace
      (λ (out) (write-json (results->jsexpr corpora parse-only? results) out)))
    (printf "\njson: ~a\n" json-path))
  (define counts (if parse-only? 'parse-ok 'ok))
  (exit (if (= (count-status results counts) (length results)) 0 1)))

(module+ main
  (js-ts-corpus-main (vector->list (current-command-line-arguments))))

