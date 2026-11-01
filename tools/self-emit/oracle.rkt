#lang racket

;; tools/self-emit/oracle.rkt — the original tower's translation output.
;;
;; One translation is parse → lower → infer, and its result is the canonical
;; UIR-plus-row document that canonical.rkt defines. This module is the
;; "original" side of the P6.3 differential: the self-emitted tower must
;; produce exactly this document for the same source text.
;;
;; Supported source languages are the ones whose grammars are tracked in this
;; repository, so the oracle needs no vendored grammars-v4 checkout:
;;
;;   python — grammars/python3/Python3Parser.g4, INDENT/DEDENT aware
;;   json   — the static racklr/gen-json-parser.rkt parser
;;
;; JavaScript/TypeScript/TSX/Clojure/Koka grammars live under grammars-v4/ or
;; are not yet a stable lowering path; adding one means adding a `translate`
;; arm here, not changing the comparison format.

(require racket/file
         racket/path
         racket/string
         racklr/uir
         racklr/effects/infer
         racklr/lower-python
         racklr/lower-json
         racklr/gen-json-parser
         racklr/gen-test)

(provide supported-languages
         supported-language?
         translate-source
         translate-file
         cleanup-oracle
         repo-root)

;; ── repository root ─────────────────────────────────────────────────

;; The oracle reads grammars shared with the suite runner, so it must know
;; which checkout it lives in. Walk up from the working directory: the suites
;; run from racklr-test/ and the tool runs from the repository root, and both
;; are one or zero levels below the marker file.
(define (repo-root [start (current-directory)])
  (let loop ([dir (simplify-path (path->complete-path start))])
    (cond
      [(file-exists? (build-path dir "grammars" "python3" "Python3Parser.g4")) dir]
      [else
       (define parent (simplify-path (build-path dir 'up)))
       (if (equal? parent dir)
           (error 'repo-root
                  "cannot find the repository root from ~a: no grammars/python3/Python3Parser.g4 above it"
                  start)
           (loop parent))])))

;; ── Python front end ────────────────────────────────────────────────

;; The Python parser is generated from the tracked modern-Python grammar on
;; first use and cached for the life of the process. `gen-and-load-py` writes
;; its generated module into the working directory, so generation happens with
;; the working directory pinned to the repository root.
(define python-front-end #f)

(define (load-python-front-end)
  (unless python-front-end
    (define root (repo-root))
    (define grammar (build-path root "grammars" "python3" "Python3Parser.g4"))
    (define-values (_parse tokenize token-type token-value)
      (parameterize ([current-directory root])
        (gen-and-load-py (path->string grammar))))
    (set! python-front-end
          (hash 'parse-file-input
                (dynamic-require (*last-module-path*) 'parse-file_input)
                'tokenize tokenize
                'token-type token-type
                'token-value token-value)))
  python-front-end)

;; `file_input` requires a trailing NEWLINE to terminate a simple statement;
;; an emitter output does not append one, so normalize before parsing.
(define (normalize-python-source text)
  (if (regexp-match? #rx"\n$" text) text (string-append text "\n")))

(define (python->uir source)
  (define fe (load-python-front-end))
  (define tokens ((hash-ref fe 'tokenize) (normalize-python-source source)))
  (match-define (list _position cst) ((hash-ref fe 'parse-file-input) tokens 0))
  (unless cst
    (error 'translate-source "python: parser rejected the source text"))
  (lower-python cst (hash-ref fe 'token-type) (hash-ref fe 'token-value)))

;; ── JSON front end ──────────────────────────────────────────────────

(define (json->uir source)
  (define cst (parse source))
  (unless cst
    (error 'translate-source "json: parser rejected the source text"))
  (lower-json cst))

;; ── language dispatch ───────────────────────────────────────────────

(define front-ends
  (hash 'python python->uir
        'json json->uir))

(define (supported-languages) (sort (hash-keys front-ends) symbol<?))

(define (supported-language? language)
  (and (symbol? language) (hash-has-key? front-ends language)))

;; source text → (values uir rows), the original tower's translation output.
(define (translate-source language source)
  (unless (supported-language? language)
    (error 'translate-source
           "unsupported source language: ~a (supported: ~a)"
           language
           (string-join (map symbol->string (supported-languages)) ", ")))
  (define uir ((hash-ref front-ends language) source))
  (values uir (infer-effects uir)))

;; file path + language → (values uir rows).
(define (translate-file language path)
  (translate-source language (file->string path)))

;; Drop the generated parser module this process wrote for the Python front
;; end. A long-lived caller keeps the loaded module; only the temp source file
;; on disk is removed.
(define (cleanup-oracle)
  (cleanup))
