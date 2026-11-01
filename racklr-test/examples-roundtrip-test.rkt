#lang racket

;; End-to-end round-trip over docs/examples/*.
;;
;; For each source file the pipeline is: parse → lower → emit → re-parse
;; the emitted text → re-lower. Every step must succeed without crashing,
;; with a non-empty emit. Round-trip languages (JSON, Python, JavaScript,
;; Clojure) are re-parsed in the same language; TS/TSX are one-way to
;; JavaScript, so their emitted output is re-parsed with the JS parser.
;;
;; Run from inside racklr-test/ (grammar and example paths are relative
;; to the working directory).

(require rackunit
         racket/file
         racket/path
         racket/string
         racket/match
         racklr/tree
         racklr/uir
         racklr/gen-test
         racklr/g4-parse
         racklr/gend-parser
         ;; JSON
         (prefix-in json: racklr/gen-json-parser)
         racklr/lower-json
         racklr/emit-json
         ;; Python
         racklr/lower-python
         racklr/emit-python
         ;; JavaScript
         racklr/lower-javascript
         racklr/emit-javascript
         ;; TypeScript + TSX
         (prefix-in ts-lower: racklr/lower-typescript)
         racklr/tsx-preprocess
         racklr/lower-jsx
         ;; Clojure
         racklr/lower-clojure
         racklr/emit-clojure)

;; ── Parsers ─────────────────────────────────────────────────────────

(define-values (py-parse py-tokenize py-tok-type py-tok-value)
  (gen-and-load-py "../grammars/python3/Python3Parser.g4"))
(define py-mod-path (*last-module-path*))  ; capture before later loads overwrite it

(define-values (js-parse js-tokenize js-tok-type js-tok-value)
  (gen-and-load "../grammars-v4/javascript/javascript-cleaned/JavaScriptParser.g4"))

(define-values (ts-parse ts-tokenize ts-tok-type ts-tok-value)
  (gen-and-load "../grammars-v4/javascript/typescript-cleaned/TypeScriptParser.g4"))

(define-values (jsx-parse jsx-tokenize jsx-tok-type jsx-tok-value)
  (gen-and-load "../grammars-v4/javascript/jsx-cleaned/JSXParser.g4"))

(define-values (clj-parse clj-tokenize clj-tok-type clj-tok-value)
  (gen-and-load "../grammars-v4/clojure/Clojure.g4"))

;; Full-file Python entry (parse-file_input, INDENT/DEDENT aware).
;; file_input requires a trailing NEWLINE to terminate a simple statement;
;; the emitter does not append one, so normalize before re-parsing.
(define (norm-py s)
  (if (regexp-match? #rx"\n$" s) s (string-append s "\n")))

(define (py-parse-file src)
  (define tks ((dynamic-require py-mod-path 'tokenize) (norm-py src)))
  (match-define (list _fp res) ((dynamic-require py-mod-path 'parse-file_input) tks 0))
  res)

;; ── Pipelines (return #t on success, raise otherwise) ───────────────

(define (assert-parsed cst)
  (when (not cst) (error "parse returned #f"))
  cst)

(define (assert-emitted out #:allow-empty? [allow? #f])
  (when (or (not (string? out)) (and (not allow?) (equal? out "")))
    (error "emit returned non-string or empty"))
  out)

(define (pipeline-json src)
  (define u1 (lower-json (json:parse src)))
  (define out (assert-emitted (emit-json u1)))
  (lower-json (json:parse out))
  #t)

(define (pipeline-py src)
  (define u1 (lower-python (assert-parsed (py-parse-file src)) py-tok-type py-tok-value))
  (define out (assert-emitted (emit-python u1)))
  (lower-python (assert-parsed (py-parse-file out)) py-tok-type py-tok-value)
  #t)

(define (pipeline-js src)
  (define u1 (lower-program (assert-parsed (js-parse src)) js-tok-type js-tok-value))
  (define out (assert-emitted (emit-javascript u1)))
  (lower-program (assert-parsed (js-parse out)) js-tok-type js-tok-value)
  #t)

(define (pipeline-clj src)
  (define u1 (lower-clojure (assert-parsed (clj-parse src)) clj-tok-type clj-tok-value))
  (define out (assert-emitted (emit-clojure u1)))
  (lower-clojure (assert-parsed (clj-parse out)) clj-tok-type clj-tok-value)
  #t)

(define (lower-ts-src src)
  (ts-lower:lower-program (assert-parsed (ts-parse (preprocess-imports src))) ts-tok-type ts-tok-value))

(define (pipeline-ts src)
  (define uir (lower-ts-src src))
  ;; A type-only module erases to empty JS, which is valid.
  (define out (assert-emitted (emit-javascript uir) #:allow-empty? #t))
  ;; one-way: emitted JS must re-parse as JavaScript
  (lower-program (assert-parsed (js-parse out)) js-tok-type js-tok-value)
  #t)

(define (tsx->uir src)
  (define cleaned (preprocess-imports src))
  (define-values (processed _jsx-map jsx-uir)
    (preprocess-tsx cleaned
                    #:jsx-parse jsx-parse
                    #:jsx-lower-tk-type jsx-tok-type
                    #:jsx-lower-tk-value jsx-tok-value))
  (define ts-uir (ts-lower:lower-program (assert-parsed (ts-parse processed)) ts-tok-type ts-tok-value))
  (lower-hooks (restore-jsx ts-uir jsx-uir)))

(define (pipeline-tsx src)
  (define out (assert-emitted (emit-javascript (tsx->uir src)) #:allow-empty? #t))
  (lower-program (assert-parsed (js-parse out)) js-tok-type js-tok-value)
  #t)

;; ── Dispatch by extension ───────────────────────────────────────────

(define (lang-of path)
  (define p (path->string path))
  (cond [(regexp-match? #rx"[.]py$" p) 'python]
        [(regexp-match? #rx"[.]clj[cs]?$" p) 'clojure]
        [(regexp-match? #rx"[.]json$" p) 'json]
        [(regexp-match? #rx"[.]m?js$" p) 'javascript]
        [(regexp-match? #rx"[.]tsx$" p) 'tsx]
        [(regexp-match? #rx"[.]ts$" p) 'typescript]
        [else #f]))

(define (pipeline-of lang)
  (case lang
    [(python) pipeline-py]
    [(clojure) pipeline-clj]
    [(json) pipeline-json]
    [(javascript) pipeline-js]
    [(tsx) pipeline-tsx]
    [(typescript) pipeline-ts]
    [else (error "no pipeline")]))

;; ── Walk examples ───────────────────────────────────────────────────

(define examples-root
  (simplify-path (build-path (current-directory) ".." "docs" "examples")))

(define (skip-dir? e)
  (member (path->string e) '("node_modules" ".git" "compiled")))

(define (walk dir)
  (append*
   (for/list ([e (directory-list dir)])
     (define p (build-path dir e))
     (cond [(directory-exists? p)
            (if (skip-dir? e) '() (walk p))]
           [else (list p)]))))

;; Config files, lockfiles, declarations, and generated artifacts are not
;; application source: they are JSONC (comments/trailing commas), ambient
;; type declarations, or build tooling. Exclude them from the round-trip.
(define (source-file? p)
  (define name (path->string (file-name-from-path p)))
  (and (not (regexp-match? #rx"[.]d[.]ts$" name))
       (not (member name '("package.json"
                           "package-lock.json"
                           "tsconfig.json"
                           "postcss.config.js"
                           "tailwind.config.js"
                           ".racklr-eval.mjs")))
       (not (regexp-match? #rx"tsconfig[.][^.]*[.]json$" name))))

;; Optional command-line args select which languages to run:
;;   racket examples-roundtrip-test.rkt python clojure
;; With no args every language runs, including the TS/TSX backlog.
(define (selected-langs)
  (define args (vector->list (current-command-line-arguments)))
  (and (pair? args) (map string->symbol args)))

(define (collect-examples allowed)
  (if (directory-exists? examples-root)
      (for/list ([p (walk examples-root)]
                 #:when (and (lang-of p)
                             (source-file? p)
                             (or (not allowed) (member (lang-of p) allowed))))
        p)
      '()))

;; ── Run ─────────────────────────────────────────────────────────────

(define (guard thunk)
  (with-handlers ([exn:fail? (λ (e) (cons #f (exn-message e)))])
    (cons #t (thunk))))

(define allowed (selected-langs))
(define examples (collect-examples allowed))
(printf "~a example file(s) found under ~a~a\n"
        (length examples) examples-root
        (if allowed (format " (languages: ~a)" allowed) ""))

(define failures 0)
(define counts (make-hasheq))
(for ([p (in-list examples)])
  (define lang (lang-of p))
  (define rel (path->string (find-relative-path examples-root p)))
  (define r (guard (λ () ((pipeline-of lang) (file->string p)))))
  (hash-update! counts lang (λ (n) (add1 n)) 0)
  (cond
    [(and (car r) (cdr r))
     (printf "PASS  ~a (~a)\n" rel lang)]
    [(car r)
     (printf "FAIL  ~a (~a): pipeline returned #f\n" rel lang)
     (set! failures (add1 failures))]
    [else
     (printf "FAIL  ~a (~a): ~a\n" rel lang (cdr r))
     (set! failures (add1 failures))]))

(printf "examples per language: ~a\n"
        (for/list ([(k v) (in-hash counts)]) (format "~a=~a" k v)))
(printf "~a passed, ~a failed of ~a\n"
        (- (length examples) failures) failures (length examples))

(exit (if (zero? failures) 0 1))
