#lang racket

;; racklr-test/scan-python-test.rkt — the gate for tools/scan-python.rkt.
;;
;; Run from racklr-test/:  racket scan-python-test.rkt
;;
;; The full-corpus measurement is the tool itself:
;;
;;   racket tools/scan-python.rkt django/django
;;
;; These tests pin the bucket classification on a small fixture tree so a
;; regression in the scanner, or in the pipeline stages it distinguishes,
;; fails locally instead of only on the next full Django scan.

(require rackunit
         racket/file
         racket/path
         "../tools/scan-python.rkt"
         racklr/uir)

;; ── fixtures ────────────────────────────────────────────────────────

(define parser
  (load-python-parser
   (build-path (repository-root) "grammars" "python3" "Python3Parser.g4")))

(define (make-fixture-dir)
  (define d (build-path (find-system-path 'temp-dir) "racklr-scan-python-test"))
  (if (directory-exists? d) (delete-directory/files d) (void))
  (make-directory d)
  (display-to-file "x = 1\n" (build-path d "ok.py"))
  (display-to-file "def f(:\n    pass\n" (build-path d "parse-fail.py"))
  (display-to-file "def f():\n    if x:\n        a()\n   b()\n"
                   (build-path d "indent-error.py"))
  d)

(define fixture-dir (make-fixture-dir))

(define (status-of rows file-name)
  (define row
    (for/first ([r (in-list rows)]
                #:when (string=? (path->string (file-name-from-path (scan-result-path r)))
                                 file-name))
      r))
  (and row (scan-result-status row)))

;; ── discovery ───────────────────────────────────────────────────────

(define (test-find-python-files)
  (define names
    (map (λ (p) (path->string (file-name-from-path p)))
         (find-python-files fixture-dir)))
  (check-equal? names (list "indent-error.py" "ok.py" "parse-fail.py")
                "find-python-files sorts and keeps only *.py files"))

;; ── bucket classification ───────────────────────────────────────────

(define (test-scan-classification)
  (define rows (scan-python-directory fixture-dir parser))
  (check-equal? (length rows) 3 "one result per Python file")
  (check-eq? (status-of rows "ok.py") 'ok "a clean file lowers ok")
  (check-eq? (status-of rows "parse-fail.py") 'PARSE-FAIL
             "an unparsable file is PARSE-FAIL")
  (check-eq? (status-of rows "indent-error.py") 'INDENT-ERROR
             "a bad layout raises at tokenize, not parse"))

;; ── fallback detection ──────────────────────────────────────────────

(define (test-has-fallback)
  (check-true (has-fallback? (uir-block (list (uir-symbol "?expr"))))
              "a ?-prefixed symbol marks a fallback node")
  (check-false (has-fallback? (uir-block (list (uir-symbol "x"))))
               "a plain symbol is not a fallback")
  (check-false (has-fallback? (uir-block (list)))
               "an empty block is not a fallback"))

;; ── run ─────────────────────────────────────────────────────────────

(test-find-python-files)
(test-scan-classification)
(test-has-fallback)
(delete-directory/files fixture-dir)
(displayln "All scan-python tests passed.")
