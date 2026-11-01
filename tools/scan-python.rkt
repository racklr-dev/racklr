#lang racket

;; tools/scan-python.rkt — full-tree Python front-end scan.
;;
;; Usage (from the repository root):
;;
;;   racket tools/scan-python.rkt django/django              # aggregate only
;;   racket tools/scan-python.rkt django/django --verbose    # plus failures/effects
;;
;; Generates the Python3 parser from the tracked grammar the repository owns
;; (grammars/python3/Python3Parser.g4 — no vendored grammars-v4/ tree needed),
;; then walks every *.py file under the directory and reports which pipeline
;; stage fails:
;;
;;   INDENT-ERROR  tokenize / indent insertion raised (layout mismatch)
;;   PARSE-FAIL    parse-file_input returned #f (grammar gap)
;;   LOWER-ERROR   lower-python raised on a CST it should handle
;;   FALLBACK      lowered, but contains `?`-prefixed unhandled-node symbols
;;   ok            lowered cleanly; effects=N counts non-empty effect rows
;;
;; This is the measurement behind docs/django-frontend-catalogue.md and the
;; Django acceptance bar in ROADMAP.md ("907 of 907 files parse with zero
;; parse failures"). Same buckets and output shape as
;; effervesce/tools/scan-python.rkt, so the two measurements are comparable.
;;
;; Exit codes: 0 = every file lowered cleanly, 1 = at least one blocker.

(require racket/file
         racket/list
         racket/path
         racket/runtime-path
         racket/string
         racklr/uir
         racklr/g4-parse
         racklr/gend-parser
         racklr/lower-python
         racklr/effects/infer
         racklr/effects/contract)

(provide repository-root
         python-parser
         python-parser?
         python-parser-tokenize
         python-parser-parse-file-input
         python-parser-token-type
         python-parser-token-value
         load-python-parser
         find-python-files
         scan-result
         scan-result?
         scan-result-path
         scan-result-status
         scan-result-effects
         scan-result-error
         has-fallback?
         scan-python-file
         scan-python-directory)

(define-runtime-path this-file "scan-python.rkt")
(define grammar-relative (build-path "grammars" "python3" "Python3Parser.g4"))
(define statuses '(ok INDENT-ERROR PARSE-FAIL LOWER-ERROR FALLBACK))

;; The checkout root is the parent of the tools/ directory holding this file,
;; so the tool works from any working directory.
(define (repository-root)
  (simplify-path (build-path (path-only this-file) 'up)))

;; ── parser generation ───────────────────────────────────────────────

(struct python-parser (tokenize parse-file-input token-type token-value) #:transparent)

;; Generate from the tracked grammar into a temp module and load the four
;; entry points the scanner needs. One generation per process.
(define (load-python-parser grammar-path)
  (define cst (parse-g4-file grammar-path))
  (define module-src (generate-parser-module cst
                                             #:source-path grammar-path
                                             #:indent-tokens? #t))
  (define tmp-path (make-temporary-file "racklr-scan-python-~a.rkt"))
  (display-to-file module-src tmp-path #:exists 'replace)
  (define parser
    (python-parser (dynamic-require tmp-path 'tokenize)
                   (dynamic-require tmp-path 'parse-file_input)
                   (dynamic-require tmp-path 'token-type)
                   (dynamic-require tmp-path 'token-value)))
  (delete-file tmp-path)
  parser)

;; ── scan ────────────────────────────────────────────────────────────

(struct scan-result (path status effects error) #:transparent)

;; Does the UIR contain a `?`-prefixed fallback symbol (an unhandled CST node)?
(define (has-fallback? n)
  (cond
    [(uir-symbol? n)
     (define nm (uir-symbol-name n))
     (and (> (string-length nm) 0) (char=? (string-ref nm 0) #\?))]
    [(struct? n)
     (for/or ([f (in-list (cdr (vector->list (struct->vector n))))])
       (has-fallback? f))]
    [(pair? n) (or (has-fallback? (car n)) (has-fallback? (cdr n)))]
    [else #f]))

(define (nonempty-effect-rows program)
  (for/list ([(nm row) (in-hash (infer-effects program))]
             #:when (pair? (uir-effect-row-entries row)))
    (cons nm (map effect-name-symbol (uir-effect-row-entries row)))))

(define (scan-python-file path parser)
  (define src (file->string path))
  (define status 'ok)
  (define effects '())
  (define err #f)
  (define tks
    (with-handlers ([exn:fail? (λ (e) (set! status 'INDENT-ERROR) (set! err (exn-message e)) #f)])
      ((python-parser-tokenize parser) src)))
  (if tks
      (let ([res ((python-parser-parse-file-input parser) tks 0)])
        (if (and (list? res) (= (length res) 2))
            (let ([uir (with-handlers ([exn:fail? (λ (e) (set! status 'LOWER-ERROR) (set! err (exn-message e)) #f)])
                         (lower-python (second res)
                                       (python-parser-token-type parser)
                                       (python-parser-token-value parser)))])
              (if uir
                  (begin
                    (if (has-fallback? uir) (set! status 'FALLBACK) (void))
                    (set! effects (nonempty-effect-rows uir)))
                  (void)))
            (set! status 'PARSE-FAIL)))
      (void))
  (scan-result path status effects err))

(define (find-python-files dir)
  (define out '())
  (let loop ([d dir])
    (for ([e (in-list (directory-list d))])
      (define p (build-path d e))
      (cond
        [(directory-exists? p) (loop p)]
        [(regexp-match? #rx"[.]py$" (path->string e)) (set! out (cons p out))]
        [else (void)])))
  (sort (reverse out) (λ (a b) (string<? (path->string a) (path->string b)))))

(define (scan-python-directory dir parser)
  (for/list ([f (in-list (find-python-files dir))])
    (scan-python-file f parser)))

(define (count-status rows status)
  (count (λ (r) (eq? (scan-result-status r) status)) rows))

;; ── command line ────────────────────────────────────────────────────

(define (print-row row verbose?)
  (printf "~a  ~a~a\n"
          (scan-result-status row)
          (scan-result-path row)
          (if (and (scan-result-error row) verbose?)
              (format "  (~a)" (scan-result-error row))
              ""))
  (if (and verbose? (pair? (scan-result-effects row)))
      (for ([e (in-list (scan-result-effects row))])
        (printf "    ~a -> ~a\n" (car e) (map symbol->string (cdr e))))
      (void)))

(module+ main
  (define args (vector->list (current-command-line-arguments)))
  (define dir (if (pair? args) (car args) #f))
  (define verbose? (and (member "--verbose" args) #t))
  (unless (and dir (not (string-prefix? dir "--")))
    (eprintf "usage: scan-python.rkt <dir> [--verbose]\n")
    (exit 2))
  (define parser (load-python-parser (build-path (repository-root) grammar-relative)))
  (define rows (scan-python-directory dir parser))
  (printf "files=~a\n" (length rows))
  (for ([s (in-list statuses)])
    (printf "  ~a=~a\n" s (count-status rows s)))
  (for ([r (in-list rows)]) (print-row r verbose?))
  (exit (if (= (count-status rows 'ok) (length rows)) 0 1)))
