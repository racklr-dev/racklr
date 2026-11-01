#lang racket

;; Multi-line triple-quoted strings (docs/django-frontend-catalogue.md §6).
;;
;; A triple-quoted string may span lines: the tracked grammars/python3 lexer
;; matches the whole literal as one STRING token, so the catalogue's PARSE-FAIL
;; reproducers (module, class and function docstrings) parse. These tests pin
;; that down.
;;
;; They also pin the scanner's position bookkeeping across a multi-line token.
;; The generated lexer used to advance only the column over a token's embedded
;; newlines, so every token after a multi-line string was reported on the
;; string's opening line; a docstring pushed the rest of the file onto line 1.
;;
;; Parse through file_input, the entry the lowering pipeline uses: the
;; single_input entry point is known to be broken for compound statements
;; (LOOP_PLAN issue #1).
;;
;; Run: cd racklr-test && racket python-multiline-strings-test.rkt

(require rackunit
         rackunit/text-ui
         racket/list
         racklr/tree
         racklr/gen-test
         racklr/uir
         racklr/lower-python
         racklr/emit-python)

;; ── Load the tracked Python 3 parser ───────────────────────────────

(define-values (_py-parse py-tokenize py-tok-type py-tok-value)
  (gen-and-load-py "../grammars/python3/Python3Parser.g4"))

(define parser-module (*last-module-path*))
(define py-token-start (dynamic-require parser-module 'token-start))
(define py-token-end (dynamic-require parser-module 'token-end))

(define (py-parse-file src)
  (define parsed ((dynamic-require parser-module 'parse-file_input)
                  (py-tokenize src) 0))
  (and (pair? parsed) (cadr parsed)))

(define (token-types src) (map py-tok-type (py-tokenize src)))
(define (tokens-of-type src type)
  (for/list ([t (py-tokenize src)] #:when (eq? (py-tok-type t) type)) t))

;; The single STRING token of `src` (#f if there is none or more than one).
(define (string-token src)
  (match (tokens-of-type src 'STRING)
    [(list t) t]
    [_ #f]))

(define (pos-line p) (source-pos-line p))
(define (pos-col p) (source-pos-col p))
(define (pos-offset p) (source-pos-offset p))

;; ── Lexing ──────────────────────────────────────────────────────────

(define multiline-strings-tests
  (test-suite
   "multi-line triple-quoted strings"

   (test-case
    "a triple-quoted string containing a newline is one STRING token"
    (check-equal? (token-types "x = \"\"\"a\nb\"\"\"\n")
                  '(NAME ASSIGN STRING NEWLINE EOF))
    (check-false (memq 'UNKNOWN_CHAR (token-types "x = \"\"\"a\nb\"\"\"\n")))
    (check-equal? (py-tok-value (string-token "x = \"\"\"a\nb\"\"\"\n"))
                  "\"\"\"a\nb\"\"\""
                  "the STRING token keeps its embedded newlines"))

   (test-case
    "string content that looks like code stays inside the token"
    (check-equal? (token-types "x = \"\"\"line one\nreturn\n# not a comment\n\"\"\"\n")
                  '(NAME ASSIGN STRING NEWLINE EOF)))

   (test-case
    "single-quoted and prefixed triple-quoted strings span lines too"
    (check-equal? (token-types "x = '''a\nb'''\n")
                  '(NAME ASSIGN STRING NEWLINE EOF))
    (check-equal? (token-types "x = r\"\"\"a\nb\"\"\"\n")
                  '(NAME ASSIGN STRING NEWLINE EOF))
    (check-equal? (token-types "x = f\"\"\"a\n{b}\"\"\"\n")
                  '(NAME ASSIGN STRING NEWLINE EOF)))

   (test-case
    "CRLF line endings inside a triple-quoted string"
    (define src "x = \"\"\"a\r\nb\"\"\"\r\ny = 1\r\n")
    (check-equal? (token-types src) '(NAME ASSIGN STRING NEWLINE NAME ASSIGN NUMBER NEWLINE EOF))
    (check-equal? (py-tok-value (string-token src)) "\"\"\"a\r\nb\"\"\"")
    (define y-token (second (tokens-of-type src 'NAME)))
    (check-equal? (pos-line (py-token-start y-token)) 3 "the line after a CRLF string")
    (check-equal? (pos-col (py-token-start y-token)) 1))

   ;; ── Parsing: the catalogue's reproducers ─────────────────────────

   (test-case
    "catalogue reproducers parse"
    (check-true (any-tree? (py-parse-file "x = \"\"\"a\nb\"\"\"\n")))
    (check-true (any-tree?
                 (py-parse-file "def f():\n    \"\"\"a\n    b\"\"\"\n    return 1\n\n"))
                "multi-line function docstring")
    (check-true (any-tree?
                 (py-parse-file "class C:\n    \"\"\"a\n    b\"\"\"\n\n    pass\n"))
                "multi-line class docstring")
    (check-true (any-tree?
                 (py-parse-file "\"\"\"module\n\ndocstring\"\"\"\n\nx = 1\n"))
                "multi-line module docstring")
    (check-true (any-tree? (py-parse-file "x = \"\"\"abc\"\"\"\n"))
                "single-line triple-quoted string still parses"))

   ;; ── Token positions across a multi-line token ────────────────────

   (test-case
    "the STRING token spans its real lines"
    (define src "x = \"\"\"a\nb\"\"\"\ny = 1\n")
    (define tok (string-token src))
    (check-equal? (pos-line (py-token-start tok)) 1)
    (check-equal? (pos-col (py-token-start tok)) 5)
    (check-equal? (pos-offset (py-token-start tok)) 4)
    (check-equal? (pos-line (py-token-end tok)) 2)
    (check-equal? (pos-col (py-token-end tok)) 5)
    (check-equal? (pos-offset (py-token-end tok)) 13))

   (test-case
    "a form feed inside a literal is not a line break"
    (define src "x = \"\"\"a\fb\"\"\"\ny = 1\n")
    (check-equal? (token-types src) '(NAME ASSIGN STRING NEWLINE NAME ASSIGN NUMBER NEWLINE EOF))
    (check-equal? (pos-line (py-token-start (second (tokens-of-type src 'NAME)))) 2))

   (test-case
    "tokens after a multi-line string are on their real lines"
    (define src "x = \"\"\"a\nb\"\"\"\ny = 1\n")
    (define names (tokens-of-type src 'NAME))
    (define x-token (first names))
    (define y-token (second names))
    (check-equal? (list (pos-line (py-token-start x-token))
                        (pos-col (py-token-start x-token)))
                  '(1 1))
    (check-equal? (list (pos-line (py-token-start y-token))
                        (pos-col (py-token-start y-token))
                        (pos-offset (py-token-start y-token)))
                  '(3 1 14))
    (define newline-token (first (tokens-of-type src 'NEWLINE)))
    (check-equal? (list (pos-line (py-token-start newline-token))
                        (pos-line (py-token-end newline-token)))
                  '(2 3))
    (define eof-token (first (tokens-of-type src 'EOF)))
    (check-equal? (pos-line (py-token-start eof-token)) 4))

   (test-case
    "a statement after a multi-line docstring keeps its line"
    (define src "def f():\n    \"\"\"a\n    b\"\"\"\n    return 1\n\n")
    (define string-tok (string-token src))
    (check-equal? (list (pos-line (py-token-start string-tok))
                        (pos-col (py-token-start string-tok))
                        (pos-offset (py-token-start string-tok)))
                  '(2 5 13))
    (check-equal? (list (pos-line (py-token-end string-tok))
                        (pos-col (py-token-end string-tok))
                        (pos-offset (py-token-end string-tok)))
                  '(3 9 26))
    (define return-token (first (tokens-of-type src 'RETURN)))
    (check-equal? (list (pos-line (py-token-start return-token))
                        (pos-col (py-token-start return-token))
                        (pos-offset (py-token-start return-token)))
                  '(4 5 31)))

   ;; ── Lowering and emission ────────────────────────────────────────

   (test-case
    "the catalogue reproducers lower and re-emit as parseable Python"
    (for ([src (in-list '("x = \"\"\"a\nb\"\"\"\n"
                          "def f():\n    \"\"\"a\n    b\"\"\"\n    return 1\n\n"
                          "class C:\n    \"\"\"a\n    b\"\"\"\n\n    pass\n"))])
      (define cst (py-parse-file src))
      (check-true (any-tree? cst) (format "parse ~s" src))
      (define lowered (lower-python cst py-tok-type py-tok-value))
      (check-true (uir? lowered) (format "lower ~s" src))
      (define emitted (emit-python lowered))
      (check-true (any-tree? (py-parse-file (string-append emitted "\n")))
                  (format "emitted Python re-parses: ~s -> ~s" src emitted))))))

;; ── Run and clean up ────────────────────────────────────────────────

(define failure-count (run-tests multiline-strings-tests))
(cleanup)
(exit (if (zero? failure-count) 0 1))
