#lang racket

(require rackunit
         racklr/tree
         racklr/uir
         racklr/gen-test
         racklr/lower-typescript
         racklr/emit-javascript
         racklr/emit-html)

;; ── Load the TypeScript parser ──────────────────────────────────────

(define-values (ts-parse ts-tokenize tok-type tok-value)
  (gen-and-load "../grammars/typescript/TypeScriptParser.g4"))

;; ── Test helpers ────────────────────────────────────────────────────

(define (lower-and-emit input)
  (define cst (ts-parse input))
  (unless cst (error 'test "parse failed for: ~s" input))
  (define uir (lower-program cst tok-type tok-value))
  (values uir (emit-html uir)))

;; ── Basic HTML structure ────────────────────────────────────────────

(let ([html (call-with-values
             (λ () (lower-and-emit "let x = 42;"))
             (λ (uir h) h))])
  ;; HTML5 doctype
  (check-true (string-contains? html "<!DOCTYPE html>"))
  ;; lang attribute
  (check-true (string-contains? html "<html lang=\"en\">"))
  ;; charset
  (check-true (string-contains? html "<meta charset=\"UTF-8\">"))
  ;; viewport
  (check-true (string-contains? html "viewport"))
  ;; title
  (check-true (string-contains? html "<title>Generated Page</title>"))
  ;; Script tag
  (check-true (string-contains? html "<script type=\"module\">"))
  ;; Emitted JS content
  (check-true (string-contains? html "let x = 42")))

;; ── Custom title ────────────────────────────────────────────────────

(let* ([cst (ts-parse "const hello = 'world';")]
       [uir (lower-program cst tok-type tok-value)]
       [html (emit-html uir #:title "My TypeScript App")])
  (check-true (string-contains? html "<title>My TypeScript App</title>"))
  (check-true (string-contains? html "hello"))
  (check-true (string-contains? html "\"world\"")))

;; ── HTML-escaped title ──────────────────────────────────────────────

(let* ([cst (ts-parse "1;")]
       [uir (lower-program cst tok-type tok-value)]
       [html (emit-html uir #:title "A < B & C > \"D\"")])
  (check-true (string-contains? html "<title>A &lt; B &amp; C &gt; &quot;D&quot;</title>"))
  (check-false (string-contains? html "<title>A < B")))

;; ── External JS (no inline) ─────────────────────────────────────────

(let* ([cst (ts-parse "let z = 99;")]
       [uir (lower-program cst tok-type tok-value)]
       [html (emit-html uir #:external-js "main.js")])
  (check-true (string-contains? html "<script type=\"module\" src=\"main.js\"></script>"))
  (check-false (string-contains? html "let z = 99")))

;; ── CSS-free, JS-only page is valid HTML5 ───────────────────────────

(let ([html (call-with-values
             (λ () (lower-and-emit "1 + 2;"))
             (λ (uir h) h))])
  (check-true (string-contains? html "</html>"))
  (check-true (string-contains? html "<body>"))
  (check-true (string-contains? html "</body>")))

;; ── Complex TS constructs emit into HTML correctly ──────────────────

(let ([html (call-with-values
             (λ () (lower-and-emit "function greet(name: string): string { return 'Hi ' + name; }"))
             (λ (uir h) h))])
  (check-true (string-contains? html "function greet"))
  (check-true (string-contains? html "return"))
  (check-false (string-contains? html ": string")))
