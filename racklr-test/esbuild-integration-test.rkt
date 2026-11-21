#lang racket

(require racket/string
         racket/file
         rackunit
         racklr/tree
         racklr/uir
         racklr/gen-test
         racklr/tsx-preprocess
         racklr/esbuild-bundle
         racklr/emit-javascript
         racklr/lower-jsx
         (prefix-in ts-lower: racklr/lower-typescript))

;; ── esbuild → racklr integration test ─────────────────────────────────

;; Load parsers
(define-values (ts-parse ts-tokenize ts-tok-type ts-tok-value)
  (gen-and-load "../grammars/typescript/TypeScriptParser.g4"))

(define-values (jsx-parse jsx-tokenize jsx-tok-type jsx-tok-value)
  (gen-and-load "../grammars/jsx/JSXParser.g4"))

;; Pipeline: TSX source → UIR
(define (tsx->uir source)
  (define-values (processed jsx-map jsx-uir)
    (preprocess-tsx source
                    #:jsx-parse jsx-parse
                    #:jsx-lower-tk-type jsx-tok-type
                    #:jsx-lower-tk-value jsx-tok-value))
  (define ts-cst (ts-parse processed))
  (define ts-uir (ts-lower:lower-program ts-cst ts-tok-type ts-tok-value))
  (restore-jsx ts-uir jsx-uir))

;; Pipeline: TSX source → JS
(define (tsx->js source)
  (define uir (tsx->uir source))
  (emit-javascript uir))

;; Pipeline: TSX source → HTML
(define (tsx->html source #:title [title "TSX App"])
  (define js (tsx->js source))
  (string-append
   "<!DOCTYPE html>\n"
   "<html lang=\"en\">\n"
   "<head>\n"
   "  <meta charset=\"UTF-8\">\n"
   "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n"
   (format "  <title>~a</title>\n" title)
   "</head>\n"
   "<body>\n"
   "  <script>\n"
   js "\n"
   "  </script>\n"
   "</body>\n"
   "</html>\n"))

;; ── Fixture Helpers ────────────────────────────────────────────────────

(define fixture-dir "/tmp/racklr-esbuild-test")

(define nav-source
  (string-join
   '("interface NavProps { title: string; links: string[] }"
     "export function NavBar({ title, links }: NavProps) {"
     "  return <nav className=\"navbar\">"
     "    <h2>{title}</h2>"
     "    <ul>{links.map(l => <li>{l}</li>)}</ul>"
     "  </nav>;"
     "}")
   "\n"))

(define hero-source
  (string-join
   '("import { NavBar } from \"./NavBar\";"
     "const links = [\"Home\", \"About\", \"Contact\"];"
     "export function Hero() {"
     "  return <div><NavBar title=\"MyApp\" links={links} /><main>Welcome!</main></div>;"
     "}")
   "\n"))

(define index-source
  (string-join
   '("import { Hero } from \"./Hero\";"
     "function App() { return <div><Hero /></div>; }"
     "console.log(App());")
   "\n"))

(define (setup-fixtures!)
  (when (directory-exists? fixture-dir)
    (for ([f (directory-list fixture-dir)])
      (delete-file (build-path fixture-dir f))))
  (make-directory* fixture-dir)
  (display-to-file nav-source (build-path fixture-dir "NavBar.tsx") #:exists 'replace)
  (display-to-file hero-source (build-path fixture-dir "Hero.tsx") #:exists 'replace)
  (display-to-file index-source (build-path fixture-dir "index.tsx") #:exists 'replace))

(define (cleanup-fixtures!)
  (when (directory-exists? fixture-dir)
    (for ([f (directory-list fixture-dir)])
      (delete-file (build-path fixture-dir f)))
    (delete-directory fixture-dir)))

;; ── Tests ─────────────────────────────────────────────────────────────

;; Test 1: esbuild bundles multi-file TSX with JSX preserved
(setup-fixtures!)
(define bundled
  (esbuild-bundle #:entry "index.tsx"
                  #:working-dir fixture-dir
                  #:external '("react")))

(printf "=== esbuild output ===\n~a\n=== end ===\n" bundled)
(check-true (string-contains? bundled "function NavBar"))
(check-true (string-contains? bundled "<nav"))
(check-true (string-contains? bundled "className=\"navbar\""))
(check-true (string-contains? bundled "function Hero"))
(check-true (string-contains? bundled "function App"))
(check-true (string-contains? bundled "<Hero />"))

;; No pipeline test on bundled output: the IIFE wrapper from esbuild
;; bundling is not valid TS source. Multi-file resolution is tested
;; via tsx-integration-test.rkt (B6).

(cleanup-fixtures!)
