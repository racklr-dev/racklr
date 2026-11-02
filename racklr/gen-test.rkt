#lang racket

(require rackunit
         racklr/tree
         racklr/g4-parse
         racklr/gend-parser
         (only-in racklr/gend-parser/lexer extract-option)
         (only-in racklr/gend-parser/language-policy
                  language-policy-by-name
                  default-language-policy))

(provide check-parse check-parse-sexp check-reject gen-and-load gen-and-load-py gen-and-load-layout gen-and-load-comments cleanup *last-module-path*)

;; ── Temp File Tracking ──────────────────────────────────────────────

(define *last-module-path* (make-parameter #f))

(define tmp-files '())

(define (cleanup)
  "Remove all temp files created by gen-and-load."
  (for ([f (reverse tmp-files)])
    (when (file-exists? f)
      (delete-file f)))
  (set! tmp-files '()))

;; ── Generator → Module Loading ──────────────────────────────────────

(define gen-counter 0)

;; A layout grammar may name its generated-code policy with
;; `options { layout = <name>; }`; the name is resolved from the policy data
;; module, so the loader stays host-neutral and callers need no extra argument.
(define (grammar-layout-policy grammar-cst)
  (define rules (any-tree-children (second (any-tree-children grammar-cst))))
  (define options
    (for/first ([r (in-list rules)] #:when (eq? (any-tree-tag r) 'options)) r))
  (define layout-name (and options (extract-option options "layout")))
  (cond
    [(not layout-name) default-language-policy]
    [else
     (or (language-policy-by-name (string->symbol layout-name))
         (error 'grammar-layout-policy
                "grammar requests unknown layout policy ~a (options { layout = ~a; })"
                layout-name layout-name))]))

(define (gen-and-load grammar-path)
  "Parse grammar file, generate parser module, write to temp file in pwd, load it.
Returns (values parse tokenize token-type token-value)."
  (define grammar-cst (parse-g4-file grammar-path))
  (define module-src (generate-parser-module grammar-cst #:source-path grammar-path))
  (set! gen-counter (+ gen-counter 1))
  (define tmp-path
    (build-path (current-directory)
                (format "gen-tmp-~a.rkt" gen-counter)))
  (display-to-file module-src tmp-path #:exists 'replace)
  (set! tmp-files (cons tmp-path tmp-files))
  (values (dynamic-require tmp-path 'parse)
          (dynamic-require tmp-path 'tokenize)
          (dynamic-require tmp-path 'token-type)
          (dynamic-require tmp-path 'token-value)))

(define (gen-and-load-py grammar-path)
  "Like gen-and-load but enables Python INDENT/DEDENT token insertion."
  (define grammar-cst (parse-g4-file grammar-path))
  (define module-src (generate-parser-module grammar-cst
                                             #:source-path grammar-path
                                             #:indent-tokens? #t))
  (set! gen-counter (+ gen-counter 1))
  (define tmp-path
    (build-path (current-directory)
                (format "gen-tmp-~a.rkt" gen-counter)))
  (display-to-file module-src tmp-path #:exists 'replace)
  (set! tmp-files (cons tmp-path tmp-files))
  (*last-module-path* tmp-path)
  (values (dynamic-require tmp-path 'parse)
          (dynamic-require tmp-path 'tokenize)
          (dynamic-require tmp-path 'token-type)
          (dynamic-require tmp-path 'token-value)))

(define (gen-and-load-layout grammar-path)
  "Like gen-and-load but enables a layout token post-pass (used by Koka and
  other indentation-sensitive languages). The grammar's
  `options { layout = <name>; }` selects the policy; without it the default
  Python NEWLINE/INDENT/DEDENT layout is used."
  (define grammar-cst (parse-g4-file grammar-path))
  (define module-src (generate-parser-module grammar-cst
                                             #:source-path grammar-path
                                             #:indent-tokens? #t
                                             #:policy (grammar-layout-policy grammar-cst)))
  (set! gen-counter (+ gen-counter 1))
  (define tmp-path
    (build-path (current-directory)
                (format "gen-tmp-~a.rkt" gen-counter)))
  (display-to-file module-src tmp-path #:exists 'replace)
  (set! tmp-files (cons tmp-path tmp-files))
  (*last-module-path* tmp-path)
  (values (dynamic-require tmp-path 'parse)
          (dynamic-require tmp-path 'tokenize)
          (dynamic-require tmp-path 'token-type)
          (dynamic-require tmp-path 'token-value)))

(define (gen-and-load-comments grammar-path)
  "Like gen-and-load but keeps channel(HIDDEN) comment tokens in the token
  stream (the parse entry still filters them, so the CST is unchanged)."
  (define grammar-cst (parse-g4-file grammar-path))
  (define module-src (generate-parser-module grammar-cst
                                             #:source-path grammar-path
                                             #:preserve-comments? #t))
  (set! gen-counter (+ gen-counter 1))
  (define tmp-path
    (build-path (current-directory)
                (format "gen-tmp-~a.rkt" gen-counter)))
  (display-to-file module-src tmp-path #:exists 'replace)
  (set! tmp-files (cons tmp-path tmp-files))
  (*last-module-path* tmp-path)
  (values (dynamic-require tmp-path 'parse)
          (dynamic-require tmp-path 'tokenize)
          (dynamic-require tmp-path 'token-type)
          (dynamic-require tmp-path 'token-value)))

;; ── Test Helpers ────────────────────────────────────────────────────

(define (check-parse parse-fn input-str)
  "Assert input-str parses successfully. Returns the CST."
  (define result (parse-fn input-str))
  (check-true (any-tree? result)
              (format "parse ~s: expected a tree, got ~v" input-str result))
  result)

(define (check-parse-sexp parse-fn input-str expected-sexp)
  "Assert input-str parses and its tree->sexp matches expected-sexp."
  (define cst (check-parse parse-fn input-str))
  (define actual (tree->sexp cst))
  (check-equal? actual expected-sexp
                (format "parse ~s sexp mismatch" input-str)))

(define (check-reject parse-fn input-str)
  "Assert input-str fails to parse (returns #f or raises an error)."
  (define result
    (with-handlers ([exn:fail? (lambda (e) #f)])
      (parse-fn input-str)))
  (check-false result
               (format "parse ~s: expected #f, got ~v" input-str result)))
