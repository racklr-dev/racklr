#lang racket

;; tools/koka-corpus.rkt — real-world Koka capability scorer.
;;
;; The Koka-only slice of the cross-language capability battery: for every
;; .kk file under a corpus root, run the absorb path (tokenize -> parse ->
;; lower-koka) and the round-trip path (emit-koka -> re-parse the emitted
;; text), then classify the file:
;;
;;   ok             tokenize + parse + lower + emit + re-parse all succeeded
;;   tokenize-error tokenize (including INDENT/DEDENT insertion) raised
;;   parse-fail     the generated parser returned #f or raised
;;   lower-error    lower-koka raised on a CST it should handle
;;   fallback       lowered, but a `?`-prefixed unhandled-node symbol remains
;;   emit-error     emit-koka raised
;;   emit-empty     emit-koka returned a non-string or an empty string
;;   reparse-fail   the emitted Koka text did not re-parse (or raised)
;;   timeout        a single-file pipeline exceeded --timeout-ms (watchdog)
;;
;; Usage (from the repository root, with the checkout on PLTCOLLECTS):
;;
;;   PLTCOLLECTS="$(pwd):" racket tools/koka-corpus.rkt <corpus-dir>
;;                                              [--parse-only]
;;                                              [--json <path>]
;;
;;   --parse-only  stop after parse; report tokenize-error / parse-fail /
;;                 parse-ok only (the fast grammar/tokenizer measurement)
;;   --json PATH   write the full per-file result set as JSON to PATH
;;                 (nothing is written without it, so the default run leaves
;;                 the repo clean apart from tracked code)
;;   --timeout-ms N per-file watchdog in milliseconds (default 60000); a file
;;                 that trips it is classified `timeout` and the scan continues
;;
;; Exit codes: 0 = every file in the selected mode is ok/parse-ok,
;; 1 = at least one blocker, 2 = usage error.
;;
;; The parser is generated from the tracked grammar (grammars/koka/Koka.g4)
;; via racklr/gen-test's gen-and-load-layout into a private scratch directory,
;; so parallel runs in the same checkout cannot collide on gen-tmp-N.rkt.

(require racket/file
         racket/list
         racket/path
         racket/runtime-path
         racket/string
         json
         racklr/tree
         racklr/uir
         racklr/gen-test
         racklr/lower-koka
         racklr/emit-koka)

(provide repository-root
         koka-grammar-path
         koka-parser
         koka-parser?
         load-koka-parser
         koka-corpus-files
         scan-koka-file
         scan-koka-directory
         failure-groups
         koka-corpus-main)

;; ── Paths ───────────────────────────────────────────────────────────

(define-runtime-path this-file "koka-corpus.rkt")

;; The checkout root is the parent of the tools/ directory holding this file,
;; so the tool works from any working directory.
(define (repository-root)
  (simplify-path (build-path (path-only this-file) 'up)))

(define (koka-grammar-path)
  (build-path (repository-root) "grammars" "koka" "Koka.g4"))

;; ── Parser generation ───────────────────────────────────────────────

(struct koka-parser (parse tokenize token-type token-value) #:transparent)

;; Generate the layout-aware parser from the tracked grammar. The generated
;; module is written into a fresh scratch directory and deleted once loaded,
;; so the checkout and other runs are untouched.
(define (load-koka-parser [grammar (koka-grammar-path)])
  (define scratch (make-temporary-file "racklr-koka-corpus-~a" 'directory))
  (dynamic-wind
    void
    (λ ()
      (parameterize ([current-directory scratch])
        (define-values (parse tokenize tok-type tok-value)
          (gen-and-load-layout (path->string grammar)))
        (cleanup)
        (koka-parser parse tokenize tok-type tok-value)))
    (λ ()
      (if (directory-exists? scratch)
          (delete-directory/files scratch)
          (void)))))

;; ── Classification ──────────────────────────────────────────────────

(define full-statuses
  '(ok tokenize-error parse-fail lower-error fallback emit-error emit-empty
       reparse-fail timeout))
(define parse-only-statuses '(parse-ok tokenize-error parse-fail timeout))

(struct scan-result (file status detail) #:transparent)

;; Run thunk, returning (cons #t value) or (cons #f message). All front-end
;; failures are ordinary Racket exceptions; none should escape the scan.
(define (attempt thunk)
  (with-handlers ([(λ (e) #t) (λ (e) (cons #f (exn-message e)))])
    (cons #t (thunk))))

;; Failure messages that embed a source position are normalized so equal
;; failures group together: "tokenize: ... at 12:5" and "... at 907:31"
;; are one bucket.
(define (normalize-message msg)
  (if (string? msg)
      (regexp-replace #rx"at [0-9]+:[0-9]+" msg "at L:C")
      "exception without message"))

(define (fallback-symbol? uir)
  (cond
    [(uir-symbol? uir)
     (define nm (uir-symbol-name uir))
     (and (> (string-length nm) 0) (char=? (string-ref nm 0) #\?))]
    [(struct? uir)
     (for/or ([field (in-list (cdr (vector->list (struct->vector uir))))])
       (fallback-symbol? field))]
    [(pair? uir) (or (fallback-symbol? (car uir)) (fallback-symbol? (cdr uir)))]
    [else #f]))

(define (with-timeout thunk ms)
  (define cust (make-custodian))
  (define result (box 'timeout))
  (parameterize ([current-custodian cust])
    (define th (thread (λ ()
                         (with-handlers ([exn:fail? (λ (e) (set-box! result (cons 'error e)))])
                           (set-box! result (cons 'ok (thunk)))))))
    (unless (sync/timeout (/ ms 1000) th)
      (custodian-shutdown-all cust)))
  (unbox result))

(define (lower-koka-file parser cst)
  (lower-koka cst (koka-parser-token-type parser)
              (koka-parser-token-value parser)))

;; Tokenize first so a layout/lexer failure is its own bucket, then parse.
;; The generated entry point tokenizes again internally; that is the price of
;; keeping the front end's real entry points untouched.
(define (scan-koka-file path parser [parse-only? #f] [timeout-ms 60000])
  (define src (file->string path))
  (define timed
    (with-timeout
     (λ () (scan-koka-source path parser src parse-only?))
     timeout-ms))
  (cond
    [(eq? timed 'timeout)
     (scan-result path 'timeout (format "pipeline exceeded ~a ms" timeout-ms))]
    [else (cdr timed)]))

(define (scan-koka-source path parser src parse-only?)
  (define tk (attempt (λ () ((koka-parser-tokenize parser) src))))
  (cond
    [(not (car tk))
     (scan-result path 'tokenize-error (normalize-message (cdr tk)))]
    [else
     (define p (attempt (λ () ((koka-parser-parse parser) src))))
     (cond
       [(not (car p))
        (scan-result path 'parse-fail (normalize-message (cdr p)))]
       [(not (any-tree? (cdr p)))
        (scan-result path 'parse-fail "parser returned #f")]
       [parse-only?
        (scan-result path 'parse-ok #f)]
       [else
        (define l (attempt (λ () (lower-koka-file parser (cdr p)))))
        (cond
          [(not (car l))
           (scan-result path 'lower-error (normalize-message (cdr l)))]
          [(fallback-symbol? (cdr l))
           (scan-result path 'fallback #f)]
          [else
           (define e (attempt (λ () (emit-koka (cdr l)))))
           (cond
             [(not (car e))
              (scan-result path 'emit-error (normalize-message (cdr e)))]
             [(or (not (string? (cdr e))) (equal? (cdr e) ""))
              (scan-result path 'emit-empty #f)]
             [else
              (define r (attempt (λ ()
                                   ((koka-parser-parse parser) (cdr e)))))
              (cond
                [(not (car r))
                 (scan-result path 'reparse-fail
                              (normalize-message (cdr r)))]
                [(not (any-tree? (cdr r)))
                 (scan-result path 'reparse-fail "parser returned #f")]
                [else (scan-result path 'ok #f)])])])])]))

;; ── Corpus discovery ────────────────────────────────────────────────

(define skip-dirs
  '("node_modules" ".git" "compiled" "__pycache__" ".venv" "venv" ".koka"
    "dist" "build" "vendor"))

(define (skip-dir? name)
  (or (member (path->string name) skip-dirs)
      (and (> (string-length (path->string name)) 0)
           (char=? (string-ref (path->string name) 0) #\.))))

(define (walk-kk dir)
  (append*
   (for/list ([entry (in-list (directory-list dir))])
     (define p (build-path dir entry))
     (cond
       [(directory-exists? p) (if (skip-dir? entry) '() (walk-kk p))]
       [(regexp-match? #rx"[.]kk$" (path->string entry)) (list p)]
       [else '()]))))

(define (koka-corpus-files dir)
  (sort (walk-kk dir)
        (λ (a b) (string<? (path->string a) (path->string b)))))

(define (scan-koka-directory dir parser [parse-only? #f] [timeout-ms 60000])
  (for/list ([p (in-list (koka-corpus-files dir))])
    (scan-koka-file p parser parse-only? timeout-ms)))

;; ── Summary + JSON ─────────────────────────────────────────────────

(define (count-status results status)
  (count (λ (r) (eq? (scan-result-status r) status)) results))

;; (status (message count sample-file) ...) groups, most frequent first.
(define (failure-groups results parse-only?)
  (define statuses (if parse-only?
                       '(tokenize-error parse-fail timeout)
                       '(tokenize-error parse-fail lower-error emit-error
                                        emit-empty reparse-fail fallback timeout)))
  (for/list ([status (in-list statuses)])
    (define rows (filter (λ (r) (eq? (scan-result-status r) status)) results))
    (if (null? rows)
        (list status '())
        (let* ([groups (group-by (λ (r) (or (scan-result-detail r) "")) rows)]
               [sorted (sort groups
                             (λ (a b) (> (length a) (length b))))])
          (list status
                (for/list ([g (in-list sorted)])
                  (list (or (scan-result-detail (car g)) "(no message detail)")
                        (length g)
                        (path->string (scan-result-file (car g))))))))))

(define (results->jsexpr corpus mode results)
  (hash 'corpus (if (path? corpus) (path->string corpus) corpus)
        'mode (if mode "parse-only" "full")
        'files (length results)
        'counts (for/hash ([s (in-list (if mode parse-only-statuses
                                             full-statuses))])
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
                      'status (symbol->string (scan-result-status r))
                      'detail (or (scan-result-detail r) null)))))

(define (print-failure-groups results parse-only?)
  (define groups (failure-groups results parse-only?))
  (define any? (for/or ([fg (in-list groups)]) (pair? (cadr fg))))
  (if any?
      (begin
        (printf "\nfailure messages (status: count message; sample file):\n")
        (for ([fg (in-list groups)])
          (for ([m (in-list (cadr fg))])
            (printf "  ~a: ~a  ~s\n    e.g. ~a\n"
                    (car fg) (cadr m) (car m) (caddr m)))))
      (void)))

(define (print-summary corpus mode results)
  (define statuses (if mode parse-only-statuses full-statuses))
  (printf "koka corpus: ~a\n" corpus)
  (printf "mode: ~a\n" (if mode "parse-only" "full round-trip"))
  (printf "files=~a\n" (length results))
  (for ([s (in-list statuses)])
    (printf "  ~a=~a\n" s (count-status results s)))
  (print-failure-groups results mode))

;; ── Command line ────────────────────────────────────────────────────

(define usage
  (string-append
   "usage: racket tools/koka-corpus.rkt <corpus-dir>\n"
   "                                    [--parse-only] [--json <path>]\n"
   "                                    [--timeout-ms N]\n"
   "  --parse-only  stop after parse (tokenize-error/parse-fail/parse-ok)\n"
   "  --json PATH   write the per-file result set as JSON\n"
   "  --timeout-ms N per-file watchdog in milliseconds (default 60000)\n"))

;; (list corpus-dir parse-only? json-path timeout-ms) or #f on a usage error.
(define (parse-cli args)
  (define (flag? a) (string-prefix? a "--"))
  (cond
    [(null? args) #f]
    [(flag? (car args)) #f]
    [else
     (let loop ([rest (cdr args)]
                [parse-only? #f]
                [json-path #f]
                [timeout-ms 60000])
       (cond
         [(null? rest) (list (car args) parse-only? json-path timeout-ms)]
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
         [else #f]))]))

(define (koka-corpus-main args)
  (define cli (parse-cli args))
  (unless cli
    (eprintf "~a" usage)
    (exit 2))
  (define corpus (first cli))
  (define parse-only? (second cli))
  (define json-path (third cli))
  (define timeout-ms (list-ref cli 3))
  (unless (directory-exists? corpus)
    (eprintf "koka-corpus: no such corpus directory: ~a\n" corpus)
    (exit 2))
  (define parser (load-koka-parser))
  (define results (scan-koka-directory corpus parser parse-only? timeout-ms))
  (print-summary corpus parse-only? results)
  (if json-path
      (begin
        (call-with-output-file json-path #:exists 'replace
          (λ (out)
            (write-json (results->jsexpr corpus parse-only? results) out)))
        (printf "\njson: ~a\n" json-path))
      (void))
  (define counts (if parse-only? 'parse-ok 'ok))
  (exit (if (= (count-status results counts) (length results)) 0 1)))

(module+ main
  (koka-corpus-main (vector->list (current-command-line-arguments))))
