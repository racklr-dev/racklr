#lang racket

;; tools/js-ts-corpus-lib.rkt — the JavaScript/TypeScript real-corpus
;; measurement engine: parser loading, staged absorb/emit/re-parse
;; classification, corpus discovery and failure grouping.
;;
;; tools/js-ts-corpus.rkt is the command-line front end (summary, JSON, exit
;; code); racklr-test/js-ts-real-world-test.rkt reuses the same front ends over
;; tracked fixtures. See tools/js-ts-corpus.rkt for the status taxonomy and
;; the vendored-grammar requirement.
;;
;; Statuses per file: ok, tokenize-error, parse-fail, lower-error, fallback,
;; emit-error, emit-empty, reparse-fail, timeout. Parse-only mode reports
;; parse-ok / tokenize-error / parse-fail / timeout.

(require racket/file
         racket/list
         racket/path
         racket/runtime-path
         json
         racklr/tree
         racklr/uir
         racklr/gen-test
         racklr/tsx-preprocess
         ;; JavaScript
         racklr/lower-javascript
         racklr/emit-javascript
         ;; TypeScript + TSX (both emit JavaScript)
         (prefix-in ts-lower: racklr/lower-typescript)
         ;; JSON
         (prefix-in json: racklr/gen-json-parser)
         racklr/lower-json
         racklr/emit-json)

(provide repository-root
         js-ts-grammar-path
         load-js-ts-parsers
         lang-parser
         lang-parser-parse
         lang-parser-tokenize
         lang-parser-token-type
         lang-parser-token-value
         lang-of
         full-statuses
         parse-only-statuses
         scan-result scan-result-file scan-result-status scan-result-detail
         count-status
         count-lang-status
         js-ts-corpus-files
         scan-js-ts-file
         scan-js-ts-directory
         failure-groups)

;; ── Paths ───────────────────────────────────────────────────────────

(define-runtime-path this-file "js-ts-corpus-lib.rkt")

;; The checkout root is the parent of the tools/ directory holding this file,
;; so the tool works from any working directory.
(define (repository-root)
  (simplify-path (build-path (path-only this-file) 'up)))

;; The grammars are the tracked, locally-cleaned copies the JS/TS front ends
;; are developed against (grammars/javascript, grammars/typescript,
;; grammars/jsx) — same layout as grammars/python3 and grammars/rust, so a
;; clean checkout runs the JS/TS front ends without the untracked vendored tree.
(define (js-ts-grammar-path lang)
  (define relative
    (case lang
      [(javascript) '("javascript" "JavaScriptParser.g4")]
      [(typescript) '("typescript" "TypeScriptParser.g4")]
      [(jsx) '("jsx" "JSXParser.g4")]
      [else #f]))
  (and relative (apply build-path (repository-root) "grammars" relative)))

;; ── Parser generation ───────────────────────────────────────────────

(struct lang-parser (parse tokenize token-type token-value) #:transparent)

(define language-names '(javascript typescript jsx))

;; Generate every JS/TS parser from the vendored grammars into a fresh scratch
;; directory, then delete the scratch directory once the modules are loaded.
;; Returns a hash 'javascript/'typescript/'jsx -> lang-parser. TSX reuses the
;; TypeScript parser and only borrows the JSX parser inside the preprocess.
(define (load-js-ts-parsers)
  (for ([lang (in-list language-names)])
    (define grammar (js-ts-grammar-path lang))
    (unless (file-exists? grammar)
      (error 'js-ts-corpus "missing tracked grammar ~a" grammar)))
  (define scratch (make-temporary-file "racklr-js-ts-corpus-~a" 'directory))
  (dynamic-wind
    void
    (λ ()
      (parameterize ([current-directory scratch])
        (for/hash ([lang (in-list language-names)])
          (define-values (parse tokenize tok-type tok-value)
            (gen-and-load (path->string (js-ts-grammar-path lang))))
          (cleanup)
          (values lang (lang-parser parse tokenize tok-type tok-value)))))
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
;; The parse stage result: the CST plus whatever the host's preprocess produced
;; for the lower stage (the JSX UIR map for TSX, #f for everything else).
(struct parsed (cst aux) #:transparent)

;; Run thunk, returning (cons #t value) or (cons #f message). All front-end
;; failures are ordinary Racket exceptions; none should escape the scan.
(define (attempt thunk)
  (with-handlers ([(λ (e) #t) (λ (e) (cons #f (exn-message e)))])
    (cons #t (thunk))))

;; Failure messages that embed a source position are normalized so equal
;; failures group together.
(define (normalize-message msg)
  (if (string? msg)
      (regexp-replace #rx"at [0-9]+:[0-9]+" msg "at L:C")
      "exception without message"))

;; `?`-prefixed symbols mark unhandled CST nodes; the operator encodings `?.`
;; and `??` are legitimate UIR vocabulary (the emitter renders them as infix
;; operators), so they are not fallbacks.
(define operator-symbols (set "?." "??" "?:"))

(define (fallback-symbols uir)
  (cond
    [(uir-symbol? uir)
     (define nm (uir-symbol-name uir))
     (if (and (> (string-length nm) 0)
              (char=? (string-ref nm 0) #\?)
              (not (set-member? operator-symbols nm)))
         (list nm)
         '())]
    [(struct? uir)
     (append* (for/list ([field (in-list (cdr (vector->list (struct->vector uir))))])
                (fallback-symbols field)))]
    [(pair? uir) (append (fallback-symbols (car uir))
                         (fallback-symbols (cdr uir)))]
    [else '()]))

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

;; ── Per-language absorb stages ──────────────────────────────────────

(define (source-for-tokenize lang src)
  ;; The TypeScript front end preprocesses imports before tokenizing; TSX adds
  ;; the JSX preprocess on top of that, but only in the parse stage.
  (if (memq lang '(typescript tsx)) (preprocess-imports src) src))

(define (parse-source lang parsers src)
  (case lang
    [(javascript)
     (parsed ((lang-parser-parse (hash-ref parsers 'javascript)) src) #f)]
    [(typescript)
     (parsed ((lang-parser-parse (hash-ref parsers 'typescript))
              (preprocess-imports src)) #f)]
    [(tsx)
     (define ts (hash-ref parsers 'typescript))
     (define jsx (hash-ref parsers 'jsx))
     (define cleaned (preprocess-imports src))
     (define-values (processed _jsx-map jsx-uir)
       (preprocess-tsx cleaned
                       #:jsx-parse (lang-parser-parse jsx)
                       #:jsx-lower-tk-type (lang-parser-token-type jsx)
                       #:jsx-lower-tk-value (lang-parser-token-value jsx)))
     (parsed ((lang-parser-parse ts) processed) jsx-uir)]
    [else (error 'parse-source "no parse path for ~a" lang)]))

(define (lower-parsed lang parsers parsed)
  (define p (hash-ref parsers (if (eq? lang 'tsx) 'typescript lang)))
  (case lang
    [(javascript) (lower-program (parsed-cst parsed)
                                 (lang-parser-token-type p)
                                 (lang-parser-token-value p))]
    [(typescript) (ts-lower:lower-program (parsed-cst parsed)
                                         (lang-parser-token-type p)
                                         (lang-parser-token-value p))]
    [(tsx) (lower-hooks
            (restore-jsx (ts-lower:lower-program (parsed-cst parsed)
                                                (lang-parser-token-type p)
                                                (lang-parser-token-value p))
                         (parsed-aux parsed)))]
    [else (error 'lower-parsed "no lower path for ~a" lang)]))

(define (emit-uir lang uir)
  (case lang
    [(javascript typescript tsx) (emit-javascript uir)]
    [else (error 'emit-uir "no emit path for ~a" lang)]))

(define (reparse-source lang parsers out)
  (define js (hash-ref parsers 'javascript))
  ((lang-parser-parse js) out))

;; ── JSON (hand-written front end, no generated parser) ──────────────

(define (parse-json-source src)
  (parsed (json:parse src) #f))

(define (lower-json-parsed parsed)
  (lower-json (parsed-cst parsed)))

;; ── Per-file scan ───────────────────────────────────────────────────

(define (scan-js-ts-file path lang parsers [parse-only? #f] [timeout-ms 60000])
  (define src (file->string path))
  (define timed
    (with-timeout (λ () (scan-source path lang parsers src parse-only?))
                  timeout-ms))
  (cond
    [(eq? timed 'timeout)
     (scan-result path 'timeout (format "pipeline exceeded ~a ms" timeout-ms))]
    [else (cdr timed)]))

(define (scan-source path lang parsers src parse-only?)
  (define p (hash-ref parsers (if (eq? lang 'tsx) 'typescript lang) #f))
  (define tokenize (if (eq? lang 'json) json:tokenize (lang-parser-tokenize p)))
  (define tk (attempt (λ () (tokenize (source-for-tokenize lang src)))))
  (cond
    [(not (car tk))
     (scan-result path 'tokenize-error (normalize-message (cdr tk)))]
    [else
     (define parse-attempt
       (attempt (λ () (if (eq? lang 'json)
                          (parse-json-source src)
                          (parse-source lang parsers src)))))
     (cond
       [(not (car parse-attempt))
        (scan-result path 'parse-fail (normalize-message (cdr parse-attempt)))]
       [(not (parsed-cst (cdr parse-attempt)))
        (scan-result path 'parse-fail "parser returned #f")]
       [parse-only? (scan-result path 'parse-ok #f)]
       [else
        (define lowered
          (attempt (λ () (if (eq? lang 'json)
                             (lower-json-parsed (cdr parse-attempt))
                             (lower-parsed lang parsers (cdr parse-attempt))))))
        (cond
          [(not (car lowered))
           (scan-result path 'lower-error (normalize-message (cdr lowered)))]
          [else
           (define fb (fallback-symbols (cdr lowered)))
           (cond
             [(pair? fb)
              (scan-result path 'fallback
                           (format "~a unhandled-node symbol(s)" (length fb)))]
             [else
              (define emitted
                (attempt (λ () (if (eq? lang 'json)
                                   (emit-json (cdr lowered))
                                   (emit-uir lang (cdr lowered))))))
              (cond
                [(not (car emitted))
                 (scan-result path 'emit-error (normalize-message (cdr emitted)))]
                [(or (not (string? (cdr emitted))) (equal? (cdr emitted) ""))
                 (scan-result path 'emit-empty #f)]
                [else
                 (define reparsed
                   (attempt (λ () (if (eq? lang 'json)
                                      (json:parse (cdr emitted))
                                      (reparse-source lang parsers (cdr emitted))))))
                 (cond
                   [(not (car reparsed))
                    (scan-result path 'reparse-fail (normalize-message (cdr reparsed)))]
                   [else (scan-result path 'ok #f)])])])])])]))

;; ── Corpus discovery ────────────────────────────────────────────────

(define skip-dirs
  '("node_modules" ".git" "compiled" "__pycache__" ".venv" "venv"
    "dist" "build" "vendor"))

(define (skip-dir? name)
  (member (path->string name) skip-dirs))

(define (lang-of path)
  (define p (path->string path))
  (cond [(regexp-match? #rx"[.]m?js$" p) 'javascript]
        [(regexp-match? #rx"[.]tsx$" p) 'tsx]
        [(regexp-match? #rx"[.]ts$" p) 'typescript]
        [(regexp-match? #rx"[.]json$" p) 'json]
        [else #f]))

(define (walk-corpus dir)
  (append*
   (for/list ([entry (in-list (directory-list dir))])
     (define p (build-path dir entry))
     (cond
       [(directory-exists? p) (if (skip-dir? entry) '() (walk-corpus p))]
       [(lang-of entry) (list p)]
       [else '()]))))

(define (js-ts-corpus-files dir)
  (sort (walk-corpus dir)
        (λ (a b) (string<? (path->string a) (path->string b)))))

(define (scan-js-ts-directory dir parsers [parse-only? #f] [timeout-ms 60000])
  (for/list ([p (in-list (js-ts-corpus-files dir))])
    (scan-js-ts-file p (lang-of p) parsers parse-only? timeout-ms)))


;; ── Result counting and failure grouping ────────────────────────────

(define (count-status results status)
  (count (λ (r) (eq? (scan-result-status r) status)) results))

(define (count-lang-status results lang status)
  (for/sum ([r (in-list results)]
            #:when (and (eq? status (scan-result-status r))
                        (let ([p (scan-result-file r)])
                          (eq? lang (lang-of p)))))
    1))

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
               [sorted (sort groups (λ (a b) (> (length a) (length b))))])
          (list status
                (for/list ([g (in-list sorted)])
                  (list (or (scan-result-detail (car g)) "(no message detail)")
                        (length g)
                        (path->string (scan-result-file (car g))))))))))

