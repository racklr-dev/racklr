#lang racket

;; End-to-end round-trip over docs/examples/*, verified within the declared
;; loss model (racklr/loss-model.rkt).
;;
;; For each source file the pipeline is: parse → lower → emit → re-parse the
;; emitted text → re-lower. Absorb-then-emit must then be identity within the
;; loss model the pair declares for that direction; a difference the model does
;; not declare is a violation. Every result, including the violations and their
;; canonical evidence, is written to loss-artifacts/round-trip.json so drift is
;; a first-class artifact instead of a silent pass.
;;
;; Same-language pairs (JSON, Python, JavaScript, Clojure, Koka) are verified
;; in `exact` mode (the normalized UIRs must be equal). TypeScript and TSX emit
;; JavaScript, so they are verified in `projected` mode: only the features their
;; model declares preserved must match.
;;
;; Run from inside racklr-test/ (grammar and example paths are relative to the
;; working directory).

(require rackunit
         json
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
         racklr/emit-clojure
         ;; Koka
         racklr/lower-koka
         racklr/emit-koka
         ;; Loss model + verification
         racklr/loss-model
         racklr/loss/verify
         racklr/loss/report
         "../tools/loss-artifact.rkt")

;; ── Parsers ─────────────────────────────────────────────────────────

(define-values (py-parse py-tokenize py-tok-type py-tok-value)
  (gen-and-load-py "../grammars/python3/Python3Parser.g4"))
(define py-mod-path (*last-module-path*))  ; capture before later loads overwrite it

(define-values (js-parse js-tokenize js-tok-type js-tok-value)
  (gen-and-load "../grammars/javascript/JavaScriptParser.g4"))

(define-values (ts-parse ts-tokenize ts-tok-type ts-tok-value)
  (gen-and-load "../grammars/typescript/TypeScriptParser.g4"))

(define-values (jsx-parse jsx-tokenize jsx-tok-type jsx-tok-value)
  (gen-and-load "../grammars/jsx/JSXParser.g4"))

(define-values (clj-parse clj-tokenize clj-tok-type clj-tok-value)
  (gen-and-load "../grammars-v4/clojure/Clojure.g4"))

(define-values (koka-parse koka-tokenize koka-tok-type koka-tok-value)
  (gen-and-load-layout "../grammars/koka/Koka.g4"))

;; Full-file Python entry (parse-file_input, INDENT/DEDENT aware).
;; file_input requires a trailing NEWLINE to terminate a simple statement;
;; the emitter does not append one, so normalize before re-parsing.
(define (norm-py s)
  (if (regexp-match? #rx"\n$" s) s (string-append s "\n")))

(define (py-parse-file src)
  (define tks ((dynamic-require py-mod-path 'tokenize) (norm-py src)))
  (match-define (list _fp res) ((dynamic-require py-mod-path 'parse-file_input) tks 0))
  res)

;; ── Pipelines ───────────────────────────────────────────────────────
;; Each returns (list absorbed emitted re-absorbed) so the verifier can compare
;; the two UIRs and the artifact keeps the emitted target text reachable.

(define (assert-parsed cst)
  (if (not cst) (error "parse returned #f") cst))

(define (assert-emitted out)
  (if (or (not (string? out)) (equal? out ""))
      (error "emit returned non-string or empty")
      out))

(define (pipeline-json src)
  (define u1 (lower-json (json:parse src)))
  (define out (assert-emitted (emit-json u1)))
  (list u1 out (lower-json (json:parse out))))

(define (pipeline-py src)
  (define u1 (lower-python (assert-parsed (py-parse-file src)) py-tok-type py-tok-value))
  (define out (assert-emitted (emit-python u1)))
  (list u1 out (lower-python (assert-parsed (py-parse-file out)) py-tok-type py-tok-value)))

(define (pipeline-js src)
  (define u1 (lower-program (assert-parsed (js-parse src)) js-tok-type js-tok-value))
  (define out (assert-emitted (emit-javascript u1)))
  (list u1 out (lower-program (assert-parsed (js-parse out)) js-tok-type js-tok-value)))

(define (pipeline-clj src)
  (define u1 (lower-clojure (assert-parsed (clj-parse src)) clj-tok-type clj-tok-value))
  (define out (assert-emitted (emit-clojure u1)))
  (list u1 out (lower-clojure (assert-parsed (clj-parse out)) clj-tok-type clj-tok-value)))

(define (pipeline-koka src)
  (define u1 (lower-koka (assert-parsed (koka-parse src)) koka-tok-type koka-tok-value))
  (define out (assert-emitted (emit-koka u1)))
  (list u1 out (lower-koka (assert-parsed (koka-parse out)) koka-tok-type koka-tok-value)))

(define (lower-ts-src src)
  (ts-lower:lower-program (assert-parsed (ts-parse (preprocess-imports src)))
                          ts-tok-type ts-tok-value))

(define (pipeline-ts src)
  (define uir (lower-ts-src src))
  ;; Type-only declarations have no runtime form, but the emitter documents
  ;; each erased one as a comment, so no TypeScript module may emit empty.
  (define out (assert-emitted (emit-javascript uir)))
  ;; one-way: emitted JS must re-parse as JavaScript
  (list uir out (lower-program (assert-parsed (js-parse out)) js-tok-type js-tok-value)))

(define (tsx->uir src)
  (define cleaned (preprocess-imports src))
  (define-values (processed _jsx-map jsx-uir)
    (preprocess-tsx cleaned
                    #:jsx-parse jsx-parse
                    #:jsx-lower-tk-type jsx-tok-type
                    #:jsx-lower-tk-value jsx-tok-value))
  (lower-hooks (restore-jsx (ts-lower:lower-program (assert-parsed (ts-parse processed))
                                                     ts-tok-type ts-tok-value)
                            jsx-uir)))

(define (pipeline-tsx src)
  (define uir (tsx->uir src))
  ;; As for TypeScript: erased type declarations are documented, never empty.
  (define out (assert-emitted (emit-javascript uir)))
  (list uir out (lower-program (assert-parsed (js-parse out)) js-tok-type js-tok-value)))

;; ── Dispatch by extension ───────────────────────────────────────────

(define (lang-of path)
  (define p (path->string path))
  (cond [(regexp-match? #rx"[.]py$" p) 'python]
        [(regexp-match? #rx"[.]clj[cs]?$" p) 'clojure]
        [(regexp-match? #rx"[.]json$" p) 'json]
        [(regexp-match? #rx"[.]m?js$" p) 'javascript]
        [(regexp-match? #rx"[.]tsx$" p) 'tsx]
        [(regexp-match? #rx"[.]ts$" p) 'typescript]
        [(regexp-match? #rx"[.]kk$" p) 'koka]
        [else #f]))

(define (pipeline-of lang)
  (case lang
    [(python) pipeline-py]
    [(clojure) pipeline-clj]
    [(json) pipeline-json]
    [(javascript) pipeline-js]
    [(tsx) pipeline-tsx]
    [(typescript) pipeline-ts]
    [(koka) pipeline-koka]
    [else (error "no pipeline")]))

;; Every supported source language resolves to its declared target pair.
(define (model-of lang)
  (loss-model-for-language lang))

;; ── Walk examples ───────────────────────────────────────────────────

(define repo-root
  (let ([run-file (find-system-path 'run-file)])
    (simplify-path
     (if (path? run-file)
         (build-path run-file 'up 'up)  ; racklr-test/<this file>
         (build-path (current-directory) 'up)))))

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
;; With no args every language runs.
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

;; A model that cannot verify every pipeline it claims would make the artifact
;; lie about coverage, so fail loudly instead of skipping.
(loss-model-coverage-check)
(for ([lang (in-list (supported-source-languages))])
  (unless (model-of lang)
    (error 'examples-roundtrip-test "declared source language has no model: ~a" lang))
  (unless (with-handlers ([exn:fail? (λ (e) #f)]) (pipeline-of lang))
    (error 'examples-roundtrip-test "declared pair has no pipeline: ~a" lang)))

(define allowed (selected-langs))
(define examples (collect-examples allowed))
(printf "~a example file(s) found under ~a~a\n"
        (length examples) examples-root
        (if allowed (format " (languages: ~a)" allowed) ""))

(define failures 0)
(define violations-total 0)
(define counts (make-hasheq))
(define results '())

(define (print-violation rel lang v)
  (printf "    ~a declared=~a observed=~a: ~a\n"
          (loss-violation-feature v) (loss-violation-declared v)
          (loss-violation-observed v) (loss-violation-detail v))
  (for ([e (in-list (take (loss-violation-evidence v)
                          (min 3 (length (loss-violation-evidence v)))))])
    (printf "      evidence: ~a\n" e)))

(for ([p (in-list examples)])
  (define lang (lang-of p))
  (define rel (path->string (find-relative-path examples-root p)))
  (define model (model-of lang))
  (define r (guard (λ ()
                     (define vals ((pipeline-of lang) (file->string p)))
                     (verify-round-trip model (first vals) (third vals)))))
  (hash-update! counts lang (λ (n) (add1 n)) 0)
  (cond
    [(not model)
     (printf "FAIL  ~a (~a): no declared loss model\n" rel lang)
     (set! failures (add1 failures))
     (set! results (cons (hasheq 'file rel 'language lang
                                 'status "no-model") results))]
    [(car r)
     (define report (cdr r))
     (define vs (loss-report-violations report))
     (set! results
           (cons (hasheq 'file rel
                         'language lang
                         'status (if (null? vs) "within-model" "violation")
                         'report (report->jsexpr report))
                 results))
     (if (null? vs)
         (printf "PASS  ~a (~a, ~a)\n" rel lang (loss-model-mode model))
         (begin
           (set! failures (add1 failures))
           (set! violations-total (+ violations-total (length vs)))
           (printf "VIOLATION  ~a (~a): ~a undeclared loss(es)\n" rel lang (length vs))
           (for ([v (in-list vs)]) (print-violation rel lang v))))]
    [else
     (printf "FAIL  ~a (~a): ~a\n" rel lang (cdr r))
     (set! failures (add1 failures))
     (set! results (cons (hasheq 'file rel 'language lang
                                 'status "pipeline-error"
                                 'detail (cdr r))
                         results))]))

;; ── First-class artifact ────────────────────────────────────────────

(define pair-coverage
  (for/list ([m (in-list supported-loss-models)])
    (define lang (loss-model-source m))
    (hasheq 'pair (format "~a->~a" (loss-model-source m) (loss-model-target m))
            'mode (loss-model-mode m)
            'files (hash-ref counts lang 0)
            'declared
            (for/list ([rule (in-list (loss-model-rules m))])
              (hasheq 'feature (loss-rule-feature rule)
                      'disposition (loss-rule-disposition rule)
                      'detail (loss-rule-detail rule))))))

(define artifact-file (artifact-path repo-root))
(void
 (write-loss-artifact!
  artifact-file
 (hasheq 'tool "racklr-test/examples-roundtrip-test.rkt"
         'model_digest (loss-model-digest)
         'summary (hasheq 'examples (length examples)
                          'passed (- (length examples) failures)
                          'failed failures
                          'violations violations-total)
         'pairs pair-coverage
          'files (reverse results))))

(printf "examples per language: ~a\n"
        (for/list ([(k v) (in-hash counts)]) (format "~a=~a" k v)))
(printf "loss artifact: ~a\n" artifact-file)
(printf "~a passed, ~a failed of ~a (~a undeclared loss(es))\n"
        (- (length examples) failures) failures (length examples) violations-total)

(exit (if (zero? failures) 0 1))
