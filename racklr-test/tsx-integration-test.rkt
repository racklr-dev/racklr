#lang racket

(require racket/string
         racket/file
         rackunit
         racklr/tree
         racklr/uir
         racklr/gen-test
         racklr/tsx-preprocess
         racklr/emit-javascript
         racklr/lower-jsx
         racklr/esbuild-resolve
         (prefix-in ts-lower: racklr/lower-typescript))

;; ── TSX → HTML + Vanilla JS integration test ──────────────────────

;; Load parsers
(define-values (ts-parse ts-tokenize ts-tok-type ts-tok-value)
  (gen-and-load "../grammars/typescript/TypeScriptParser.g4"))

(define-values (jsx-parse jsx-tokenize jsx-tok-type jsx-tok-value)
  (gen-and-load "../grammars/jsx/JSXParser.g4"))

(define (tsx->uir source)
  (define cleaned (preprocess-imports source))
  (define-values (processed jsx-map jsx-uir)
    (preprocess-tsx cleaned
                    #:jsx-parse jsx-parse
                    #:jsx-lower-tk-type jsx-tok-type
                    #:jsx-lower-tk-value jsx-tok-value))
  
  (define ts-cst (ts-parse processed))
  (define ts-uir (ts-lower:lower-program ts-cst ts-tok-type ts-tok-value))
  (define with-jsx (restore-jsx ts-uir jsx-uir))
  (lower-hooks with-jsx))

(define (tsx->js source)
  (define uir (tsx->uir source))
  (emit-javascript uir))

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

;; ── Multi-file support ─────────────────────────────────────────────

(define (tsx-app->js files #:entry [entry "app.tsx"])
  (define bundled (resolve-imports files #:entry entry))
  (tsx->js bundled))

;; ── Tests ──────────────────────────────────────────────────────────

(define test1 "const el = <div>hello</div>;")
(define result1 (tsx->js test1))
(printf "Test 1 result: ~a\n" result1)
(check-true (string-contains? result1 "document.createElement"))
(check-true (string-contains? result1 "createTextNode"))

(define test2 "function App() { return <div className=\"app\"><h1>Title</h1></div>; }")
(define result2 (tsx->js test2))
(printf "Test 2 result: ~a\n" result2)
(check-true (string-contains? result2 "document.createElement(\"div\")"))
(check-true (string-contains? result2 "setAttribute(\"className\",\"app\")"))
(check-true (string-contains? result2 "document.createElement(\"h1\")"))

(define test3
  "const app = <div class=\"container\"><p>Hello, world!</p></div>;")
(define result3 (tsx->html test3 #:title "My App"))
(printf "Test 3 result:\n~a\n" result3)
(check-true (string-contains? result3 "<!DOCTYPE html>"))
(check-true (string-contains? result3 "<title>My App</title>"))
(check-true (string-contains? result3 "createTextNode(\"Hello, world!\")"))

;; ── B7+B8: Component calls + props ──────────────────────────────────
;; A component (uppercase tag) should emit as a function call, not document.createElement.
;; Props should become an object literal argument, not setAttribute.

(define test4 "const Header = ({title}) => <h1>{title}</h1>;
const app = <Header title=\"My App\" />;")
(define result4 (tsx->js test4))
(printf "Test 4 result: ~a\n" result4)
(check-true (string-contains? result4 "Header({"))
(check-true (string-contains? result4 "title: \"My App\""))
(check-false (string-contains? result4 "document.createElement(\"Header\")"))
(check-false (string-contains? result4 "setAttribute"))

;; ── B9: Expression children ─────────────────────────────────────────
;; Expression children like {title} must be wrapped in createTextNode.
;; NB: Test 4 already shows Header's <h1>{title}</h1> emits _el.appendChild(title)
;;     without createTextNode — that's the bug.
(check-true (string-contains? result4 "createTextNode(title)"))
(check-false (string-contains? result4 "_el.appendChild(title)"))

;; ── B10: React hooks (useState, useEffect) ──────────────────────────
;; useState destructuring → closure-based state
;; useEffect → direct callback call
;; React imports are stripped

(define test5
  "import { useState } from \"react\";
const [count, setCount] = useState(0);
const msg = <div>{count}</div>;")
(define result5 (tsx->js test5))
(printf "Test 5 result: ~a\n" result5)
(check-false (string-contains? result5 "import"))
(check-false (string-contains? result5 "react"))
(check-false (string-contains? result5 "useState"))
(check-true (string-contains? result5 "var count"))
(check-true (string-contains? result5 "var setCount = function"))
(check-true (string-contains? result5 "document.createElement(\"div\")"))
(check-true (string-contains? result5 "createTextNode(count)"))

;; ── B11: Event handlers (onClick → addEventListener) ────────────────
;; React-style event attributes become addEventListener for HTML elements.

(define test6 "<button onClick={() => alert('hi')}>Click</button>")
(define result6 (tsx->js test6))
(printf "Test 6 result: ~a\n" result6)
(check-true (string-contains? result6 "addEventListener(\"click\""))
(check-false (string-contains? result6 "setAttribute(\"onClick\")"))
(check-true (string-contains? result6 "createTextNode(\"Click\")"))

;; ── B12: Conditional rendering ──────────────────────────────────────
;; {cond && <JSX/>} → cond ? <emission> : null
;; {cond ? <A/> : <B/>} → cond ? A_emission : B_emission

(define test-b12a
  "const show = true;
const el = <div>{show && <span>hello</span>}</div>;")
(define result-b12a (tsx->js test-b12a))
(printf "B12a result: ~a\n" result-b12a)
(check-true (string-contains? result-b12a "document.createElement(\"div\")"))
(check-true (string-contains? result-b12a "document.createElement(\"span\")"))
(check-true (string-contains? result-b12a "createTextNode(\"hello\")"))
(check-true (string-contains? result-b12a "? (function()"))
(check-not-false (string-contains? result-b12a ": null"))
(check-false (string-contains? result-b12a "createTextNode(show"))
(check-false (string-contains? result-b12a "createTextNode(show ?"))

(define test-b12b
  "const cond = true;
const Page = () => <div>{cond ? <h1>Yes</h1> : <h2>No</h2>}</div>;")
(define result-b12b (tsx->js test-b12b))
(printf "B12b result: ~a\n" result-b12b)
(check-true (string-contains? result-b12b "document.createElement(\"div\")"))
(check-true (string-contains? result-b12b "document.createElement(\"h1\")"))
(check-true (string-contains? result-b12b "document.createElement(\"h2\")"))
(check-true (string-contains? result-b12b "createTextNode(\"Yes\")"))
(check-true (string-contains? result-b12b "createTextNode(\"No\")"))
(check-true (string-contains? result-b12b "? (function()"))
(check-true (string-contains? result-b12b ": (function()"))
;; No createTextNode wrapping the complex ternaries
(check-false (string-contains? result-b12b "createTextNode(cond"))

;; ── B13: Style objects ──────────────────────────────────────────────

(define test7 "<div style={{color: 'red'}}>Red text</div>")
(define result7 (tsx->js test7))
(printf "Test 7 result: ~a\n" result7)
(check-true (string-contains? result7 "Object.assign(_el.style"))
(check-false (string-contains? result7 "setAttribute(\"style\""))

;; ── B51: Object shorthand in JSX expression ──────────────────────

(define test-b51a "<Comp props={{x, y: 1}} />")
(define result-b51a (tsx->js test-b51a))
(printf "B51a result: ~a\n" result-b51a)
(check-true (string-contains? result-b51a "{x, y: 1}"))

(define test-b51b "<Comp config={{x}} />")
(define result-b51b (tsx->js test-b51b))
(printf "B51b result: ~a\n" result-b51b)
(check-true (string-contains? result-b51b "{x}"))

;; ── B52: Destructured function params ────────────────────────────────

;; Arrow function with destructured params
(define test-b52a "const f = ({x, y}) => x + y;")
(define result-b52a (tsx->js test-b52a))
(printf "B52a result: ~a\n" result-b52a)
(check-true (string-contains? result-b52a "_p0.x"))
(check-true (string-contains? result-b52a "_p0.y"))
(check-true (string-contains? result-b52a "=>"))

;; Arrow function with renamed destructured param
(define test-b52b "const f = ({x: a, y: b}) => a + b;")
(define result-b52b (tsx->js test-b52b))
(printf "B52b result: ~a\n" result-b52b)
(check-true (string-contains? result-b52b "_p0.x"))
(check-true (string-contains? result-b52b "_p0.y"))

;; Function expression with destructured params
(define test-b52c "const f = function({x, y}) { return x + y; };")
(define result-b52c (tsx->js test-b52c))
(printf "B52c result: ~a\n" result-b52c)
(check-true (string-contains? result-b52c "_p0.x"))
(check-true (string-contains? result-b52c "_p0.y"))

;; Arrow with array destructuring
(define test-b52d "const f = ([a, b]) => a + b;")
(define result-b52d (tsx->js test-b52d))
(printf "B52d result: ~a\n" result-b52d)
(check-true (string-contains? result-b52d "_p0[0]"))
(check-true (string-contains? result-b52d "_p0[1]"))

;; ── B53: Optional chaining ?. lowered as function call ────────────────

;; Basic optional property access: a?.b → a == null ? void 0 : a.b
(define test-b53a "const x = a?.b;")
(define result-b53a (tsx->js test-b53a))
(printf "B53a result: ~a\n" result-b53a)
(check-true (string-contains? result-b53a "a == null"))
(check-true (string-contains? result-b53a "a.b"))
(check-false (string-contains? result-b53a "?.("))

;; Chained optional property access: a?.b?.c
(define test-b53b "const x = a?.b?.c;")
(define result-b53b (tsx->js test-b53b))
(printf "B53b result: ~a\n" result-b53b)
(check-true (string-contains? result-b53b "a == null"))
(check-true (string-contains? result-b53b "a.b"))

;; Optional method call: a?.b()
(define test-b53c "const x = a?.b();")
(define result-b53c (tsx->js test-b53c))
(printf "B53c result: ~a\n" result-b53c)
(check-true (string-contains? result-b53c "a == null"))
(check-true (string-contains? result-b53c "a.b()"))

;; ── Multi-file tests ───────────────────────────────────────────────

(define multi-files
  (hash "app.tsx"  "import { Button } from \"./components/Button\";
const App = () => <div><Button /></div>;
export { App };"
        "components/Button.tsx"  "export const Button = () => <button>Click</button>;"))

(define multi-result (tsx-app->js multi-files #:entry "app.tsx"))
(printf "Multi-file test result:\n~a\n" multi-result)
(check-true (string-contains? multi-result "document.createElement(\"div\")"))
(check-true (string-contains? multi-result "document.createElement(\"button\")"))
(check-true (string-contains? multi-result "createTextNode(\"Click\")"))

;; ── B64: Nested component resolution ──────────────────────────────

;; B64a: Two-level component tree via resolve-imports
(define b64a-files
  (hash "pages/index.tsx"
        "import Container from \"../components/container\";
         export default function Page() { return <Container><span>hello</span></Container>; }"
        "components/container.tsx"
        "import Header from \"./header\";
         export default function Container(props: any) { return <div><Header /><main>{props.children}</main></div>; }"
        "components/header.tsx"
        "export default function Header() { return <h1>Site Title</h1>; }"))
(define b64a-result (tsx-app->js b64a-files #:entry "pages/index.tsx"))
(check-true (string-contains? b64a-result "document.createElement(\"h1\")")
            "B64a: nested component lowered to h1")
(check-true (string-contains? b64a-result "document.createElement(\"span\")")
            "B64a: page JSX preserved")
(check-true (string-contains? b64a-result "document.createElement(\"main\")")
            "B64a: container wraps content in main")

;; ── B65: npm polyfills ────────────────────────────────────────────

;; B65a: classnames import stripped, polyfill `cn` call preserved
(let ([js (tsx->js
           "import cn from 'classnames';
            export default function Btn() { return cn('base', 'active'); }")])
  (check-false (string-contains? js "classnames") "B65a: classnames import stripped")
  (check-true (string-contains? js "cn(") "B65a: cn polyfill call preserved"))

;; B65b: date-fns import stripped, polyfill `format` call preserved
(let ([js (tsx->js
           "import { format } from 'date-fns';
            export default function DateLabel(d: any) { return format(d, 'MMM'); }")])
  (check-false (string-contains? js "date-fns") "B65b: date-fns import stripped")
  (check-true (string-contains? js "format(") "B65b: format polyfill call preserved"))

;; ── B14: Multi-page routing ────────────────────────────────────

(require racklr/emit-router)

(define emit-pages
  (make-emit-pages-html ts-parse ts-tokenize ts-tok-type ts-tok-value
                        jsx-parse jsx-tokenize jsx-tok-type jsx-tok-value))

;; ── JSDoc @effects through the production emit-pages pipeline ────────
;; page->js must tokenize with comments preserved and pass those tokens to
;; lower-program so a JSDoc @effects row attaches to the declaration that
;; follows it, then emit-javascript renders the __withEffects guard.
(define-values (ts-parse-c ts-tokenize-c ts-tok-type-c ts-tok-value-c)
  (gen-and-load-comments "../grammars/typescript/TypeScriptParser.g4"))

(define emit-pages-jsdoc
  (make-emit-pages-html ts-parse-c ts-tokenize-c ts-tok-type-c ts-tok-value-c
                        jsx-parse jsx-tokenize jsx-tok-type jsx-tok-value))

(define jsdoc-html
  (emit-pages-jsdoc
   (hash "/" "/** @effects { llm: { max: 5 } } */\nfunction f() { return llm.query('hi'); }")
   #:title "JSDoc"))

(check-true (string-contains? jsdoc-html "__withEffects") "JSDoc: __withEffects guard emitted through emit-pages")
(check-true (string-contains? jsdoc-html "\"llm\"") "JSDoc: effect name in emitted row")
(check-true (string-contains? jsdoc-html "\"max\": 5") "JSDoc: budget in emitted row")

;; ── B66: _app.tsx / _document.tsx handling ───────────────────────────

;; B66a: _app.tsx wraps all pages via #:layout
(let* ([b66-dir "/tmp/b66-test"]
       [pages-dir (build-path b66-dir "pages")])
  (when (directory-exists? b66-dir) (delete-directory/files b66-dir))
  (make-directory* pages-dir)
  ;; _app.tsx: wraps pages with a shell div
  (display-to-file
   "export default function MyApp({ children }: any) { return <div className=\"app-shell\"><header>Site</header><main>{children}</main></div>; }"
   (build-path pages-dir "_app.tsx") #:exists 'replace)
  ;; Page
  (display-to-file
   "export default function Home() { return <h1>Home Page</h1>; }"
   (build-path pages-dir "index.tsx") #:exists 'replace)
  (define b66-pages (discover-pages pages-dir))
  (define b66-app (find-app-tsx pages-dir))
  (define b66-html (emit-pages b66-pages #:title "B66" #:layout b66-app))
  (check-true (string-contains? b66-html "app-shell") "B66a: _app.tsx layout wrapper rendered")
  (check-true (string-contains? b66-html "Home Page") "B66a: page content inside layout")
  (delete-directory/files b66-dir))

(define pages-b14
  (hash "/"       "export default () => <h1>Home</h1>;"
        "/about"  "export default () => <p>About page content</p>;"))

(define pages-b14-html (emit-pages pages-b14 #:title "B14 Test"))
(printf "B14 HTML (~a chars)\n" (string-length pages-b14-html))
(check-true (string-contains? pages-b14-html "<div id=\"_app\">"))
(check-true (string-contains? pages-b14-html "var _pages = {"))
(check-true (string-contains? pages-b14-html "\"/\":"))
(check-true (string-contains? pages-b14-html "\"/about\":"))
(check-true (string-contains? pages-b14-html "function _mount"))
(check-true (string-contains? pages-b14-html "hashchange"))
(check-true (string-contains? pages-b14-html "document.getElementById(\"_app\")"))
(check-true (string-contains? pages-b14-html "createElement(\"h1\")"))
(check-true (string-contains? pages-b14-html "createElement(\"p\")"))
;; No createTextNode wrapping the page function value
(check-false (string-contains? pages-b14-html "createTextNode(Home"))

;; ── B14b: Two self-contained pages with navigation ──────────────

(define pages-b14b
  (hash "/"       "export default () => <div><h1>Home</h1><a href=\"#/about\">About</a></div>;"
        "/about"  "export default () => <div><h1>About</h1><a href=\"#/\">Home</a></div>;"))

(define pages-b14b-html
  (emit-pages pages-b14b #:title "B14b Test"))

(printf "B14b HTML (~a chars)\n" (string-length pages-b14b-html))
(check-true (string-contains? pages-b14b-html "createElement(\"h1\")"))
(check-true (string-contains? pages-b14b-html "createTextNode(\"Home\")"))
(check-true (string-contains? pages-b14b-html "createTextNode(\"About\")"))
(check-true (string-contains? pages-b14b-html "setAttribute(\"href\""))
(check-true (string-contains? pages-b14b-html "\"#/about\""))
(check-true (string-contains? pages-b14b-html "\"#/\""))

;; ── B15: SSR / data fetching ──────────────────────────────────

(define pages-b15
  (hash "/" "export async function getStaticProps() { return { props: { title: \"Home\" } }; }
export default () => <h1>Home Page</h1>;"
        "/about" "export function getServerSideProps() { return { props: {} }; }
export default () => <p>About page</p>;"))

(define pages-b15-html (emit-pages pages-b15 #:title "B15 Test"))
(printf "B15 HTML (~a chars)\n" (string-length pages-b15-html))
;; getStaticProps and getServerSideProps must be stripped
(check-false (string-contains? pages-b15-html "getStaticProps"))
(check-false (string-contains? pages-b15-html "getServerSideProps"))
;; But the page components still render
(check-true (string-contains? pages-b15-html "createElement(\"h1\")"))
(check-true (string-contains? pages-b15-html "createElement(\"p\")"))
(check-true (string-contains? pages-b15-html "createTextNode(\"Home Page\")"))
(check-true (string-contains? pages-b15-html "createTextNode(\"About page\")"))

;; ── B17: getStaticProps data inlining ───────────────────────────

(define pages-b17
  (hash "/" "export async function getStaticProps() { return { props: { title: \"Home\" } }; }
export default (props) => <h1>{props.title}</h1>;"
        "/about" "export function getStaticProps() { return { props: { heading: \"About Us\" } }; }
export default (props) => <p>{props.heading}</p>;"))

(define pages-b17-html (emit-pages pages-b17 #:title "B17 Test"))
(printf "B17 HTML (~a chars)\n" (string-length pages-b17-html))
;; getStaticProps must be stripped from output
(check-false (string-contains? pages-b17-html "getStaticProps"))
;; _pageData must be present with the extracted props
(check-true (string-contains? pages-b17-html "var _pageData"))
(check-true (string-contains? pages-b17-html "title: \"Home\""))
(check-true (string-contains? pages-b17-html "heading: \"About Us\""))
;; pageFn must be called with pageData
(check-true (string-contains? pages-b17-html "pageFn(pageData)"))
;; The page component references props.title as a variable
(check-true (string-contains? pages-b17-html "createTextNode(props.title)"))
(check-true (string-contains? pages-b17-html "createTextNode(props.heading)"))
;; Page components still render DOM elements
(check-true (string-contains? pages-b17-html "createElement(\"h1\")"))
(check-true (string-contains? pages-b17-html "createElement(\"p\")"))

;; B18 helper alias
(define check-string-contains? (lambda (s sub) (check-true (string-contains? s sub))))

;; ── B18: Pages directory discovery ─────────────────────────────────

;; Test discover-pages with a temp directory
(define b18-tmp-dir (make-temporary-file "racklr-b18-~a" 'directory))
(display-to-file "export default () => <h1>Index Page</h1>;" (build-path b18-tmp-dir "index.tsx"))
(display-to-file "export default () => <p>About</p>;" (build-path b18-tmp-dir "about.tsx"))
;; Non-.tsx files should be ignored
(display-to-file "ignored" (build-path b18-tmp-dir "readme.md"))
(display-to-file "ignored" (build-path b18-tmp-dir ".hidden.tsx"))

(define b18-discovered (discover-pages b18-tmp-dir))
(check-equal? (hash-count b18-discovered) 2 "should discover exactly 2 pages")
(check-true (hash-has-key? b18-discovered "/") "should map index.tsx to /")
(check-true (hash-has-key? b18-discovered "/about") "should map about.tsx to /about")
(check-false (hash-has-key? b18-discovered "/readme") "should ignore .md files")
(check-false (hash-has-key? b18-discovered "/.hidden") "should ignore dotfiles")
(check-string-contains? (car (hash-ref b18-discovered "/")) "<h1>Index Page</h1>")
(check-string-contains? (car (hash-ref b18-discovered "/about")) "<p>About</p>")

;; End-to-end: discover + emit
(define b18-html (emit-pages b18-discovered #:title "B18 Test"))
(check-true (string-contains? b18-html "var _pages = {"))
(check-true (string-contains? b18-html "\"/\":"))
(check-true (string-contains? b18-html "\"/about\":"))

;; Cleanup
(delete-file (build-path b18-tmp-dir "index.tsx"))
(delete-file (build-path b18-tmp-dir "about.tsx"))
(delete-file (build-path b18-tmp-dir "readme.md"))
(delete-file (build-path b18-tmp-dir ".hidden.tsx"))
(delete-directory b18-tmp-dir)

;; ── B19: Shared layout wrapper ─────────────────────────────────────

(define b19-pages
  (hash "/" "export default () => <h1>Home Page</h1>;"
        "/about" "export default () => <p>About page content</p>;"))

(define b19-layout
  "export default ({ children, title }) => <div class=\"layout\"><header>Site</header><main>{children}</main><footer>Foot</footer></div>;")

(define b19-html-no-layout (emit-pages b19-pages #:title "No Layout"))
(check-true (string-contains? b19-html-no-layout "createElement(\"h1\")"))
(check-false (string-contains? b19-html-no-layout "Site"))

(define b19-html (emit-pages b19-pages #:title "With Layout" #:layout b19-layout))
;; Layout wrapper creates a div.layout, header, main, footer
(check-true (string-contains? b19-html "createElement(\"div\")"))
(check-true (string-contains? b19-html "createTextNode(\"Site\")"))
(check-true (string-contains? b19-html "createTextNode(\"Foot\")"))
;; _layout function must be present
(check-true (string-contains? b19-html "var _layout = "))
(check-false (string-contains? b19-html "var _layout = null"))
;; Page content still rendered
(check-true (string-contains? b19-html "createTextNode(\"Home Page\")"))
(check-true (string-contains? b19-html "createTextNode(\"About page content\")"))
;; _mount uses layout wrapping
(check-true (string-contains? b19-html "Object.assign({}, pageData || {}, { children: el })"))

;; ── B55: Template literals ──────────────────────────────────────────

(define b55a (tsx->js "const x = `hello ${name} world`;"))
(check-true (string-contains? b55a "\"hello \" + (name) + \" world\""))

(define b55b (tsx->js "const x = `hello world`;"))
(check-true (string-contains? b55b "\"hello world\""))

(define b55c (tsx->js "const x = `${greeting}, ${name}!`;"))
(check-true (string-contains? b55c "\"\" + (greeting) + \", \" + (name) + \"!\""))

;; Expression precedence preserved
(define b55d (tsx->js "const x = `result: ${a ? b : c}`;"))
(check-true (string-contains? b55d "(a ? b : c)"))

(define b55e (tsx->js "const x = `count: ${x + y}`;"))
(check-true (string-contains? b55e "(x + y)"))

(define b55f (tsx->js "const x = `items: ${arr.join(\",\")}`;"))
(check-true (string-contains? b55f "(arr.join(\",\"))"))

(define b55g (tsx->js "const x = `${a > b}`;"))
(check-true (string-contains? b55g "(a > b)"))

(define b55h (tsx->js "const x = `${a && b}`;"))
(check-true (string-contains? b55h "(a && b)"))

;; ── B57: Conditional JSX in expression text ──────────────────────────

;; Simple single-line && conditional
(define b57a (tsx->js "const el = <div>{show && <span>hi</span>}</div>;"))
(check-true (string-contains? b57a "createElement(\"span\""))

;; Multi-line && with paren-wrapped JSX (blog-starter style)
(define b57b-src "
const el = (
  <div>
    {heroPost && (
      <HeroPost
        title=\"hello\"
      />
    )}
  </div>
);
")
(define b57b (tsx->js b57b-src))
;; HeroPost is uppercase → component call, not createElement
(check-true (string-contains? b57b "HeroPost({") "HeroPost component in conditional")

;; Ternary: cond ? <A/> : <B/>
(define b57c (tsx->js "const el = <div>{flag ? <A/> : <B/>}</div>;"))
(check-true (string-contains? b57c "A({})") "ternary then branch")
(check-true (string-contains? b57c "B({})") "ternary else branch")

;; Bare JSX in expression
(define b57d (tsx->js "const el = <div>{<span>hi</span>}</div>;"))
(check-true (string-contains? b57d "createElement(\"span\"") "bare JSX in expression")

;; ── B58: useRouter shim ──────────────────────────────────────────────

;; Basic useRouter() call → shim object with isFallback, pathname, query
(define b58a (tsx->js "const router = useRouter();"))
(check-true (string-contains? b58a "isFallback") "useRouter: isFallback shim")
(check-true (string-contains? b58a "false") "useRouter: isFallback = false")
(check-true (string-contains? b58a "window.location.pathname") "useRouter: pathname shim")
(check-true (string-contains? b58a "query") "useRouter: query shim")

;; useRouter with property access
(define b58b (tsx->js "const router = useRouter(); const x = router.isFallback;"))
(check-true (string-contains? b58b "isFallback") "useRouter: prop access")

;; useRouter inside a component
(define b58c-src "
const App = () => {
  const router = useRouter();
  return <div class={router.isFallback ? 'loading' : 'ready'}>hi</div>;
};
")
(define b58c (tsx->js b58c-src))
(check-true (string-contains? b58c "isFallback") "useRouter: in component")
(check-true (string-contains? b58c "window.location.pathname")
            "useRouter: pathname in component")

;; ── B59: next/error ErrorPage stub ────────────────────────────────────

;; Basic ErrorPage → div with status code text
(define b59a (tsx->js "const page = <ErrorPage statusCode={404} />;"))
(check-true (string-contains? b59a "Error 404") "ErrorPage: shows status code")

;; ErrorPage with inline expression statusCode
(define b59b (tsx->js "const page = <ErrorPage statusCode={500} />;"))
(check-true (string-contains? b59b "Error 500") "ErrorPage: status 500")

;; ErrorPage import is stripped (next is in known-externals)
(define b59c (tsx->js "import ErrorPage from 'next/error'; const page = <ErrorPage statusCode={404} />;"))
(check-true (string-contains? b59c "Error 404") "ErrorPage: works with import")

;; ── B60: next/head Head content injection into HTML <head> ──────────

;; Head with title should inject into HTML <head>, not just body JS
(define b60a-pages
  (hash "/" "import Head from 'next/head';
export default () => (<div><Head><title>My Custom Title</title></Head><p>Hello</p></div>);"))
(define b60a-html (emit-pages b60a-pages #:title "Default Title"))
(check-true (string-contains? b60a-html "My Custom Title") "B60: title in HTML")
;; Verify title is NOT emitted as DOM JS (no document.createElement in body)
(check-false (string-contains? b60a-html "createElement(\"title\"") "B60: title not in body JS")
;; The body should still have the Hello paragraph
(check-true (string-contains? b60a-html "createTextNode(\"Hello\")") "B60: body content preserved")

;; Head with meta tags
(define b60b-pages
  (hash "/" (string-append
             "import Head from 'next/head';"
             "export default () => (<div><Head><meta name=\"description\" content=\"My desc\" /></Head><p>Hi</p></div>);")))
(define b60b-html (emit-pages b60b-pages #:title "B60b"))
(check-true (string-contains? b60b-html "description") "B60: meta tag in HTML")

;; ── B61: Node.js builtins evaluated via node at build time ──────────

;; getStaticProps using process.cwd() — evaluated via node
(define b61a-pages
  (hash "/" (string-append
             "export function getStaticProps() {"
             "  return { props: { cwd: process.cwd() } };"
             "}"
             "export default (props) => <p>{props.cwd}</p>;")))
(define b61a-html (emit-pages b61a-pages #:title "B61"))
(check-true (string-contains? b61a-html (path->string (current-directory)))
            "B61: process.cwd() evaluated via node")

;; getStaticProps returns data inlined as _pageData
(check-true (string-contains? b61a-html "_pageData")
            "B61: _pageData present")

;; fs.readdirSync with literal path
(let ()
  (define test-dir (build-path (current-directory) "racklr-test" "b61-tmp"))
  (make-directory* test-dir)
  (with-output-to-file (build-path test-dir "a.md") #:exists 'replace
    (lambda () (display "# A\n")))
  (with-output-to-file (build-path test-dir "b.md") #:exists 'replace
    (lambda () (display "# B\n")))
  (define b61b-pages
    (hash "/" (string-append
               "export function getStaticProps() {"
               "  return { props: { filenames: fs.readdirSync('racklr-test/b61-tmp') } };"
               "}"
               "export default (props) => <p>{props.filenames.length}</p>;")))
  (define b61b-html (emit-pages b61b-pages #:title "B61b"))
  (check-true (string-contains? b61b-html "a.md") "B61: fs.readdirSync finds a.md")
  (check-true (string-contains? b61b-html "b.md") "B61: fs.readdirSync finds b.md")
  (delete-directory/files test-dir))

;; fs.readFileSync with frontmatter parsing (b61c)
(let ()
  (define test-dir (build-path (current-directory) "racklr-test" "b61-tmp"))
  (make-directory* test-dir)
  (with-output-to-file (build-path test-dir "post.md") #:exists 'replace
    (lambda ()
      (display "---\n")
      (display "title: \"Hello World\"\n")
      (display "date: \"2024-01-01\"\n")
      (display "---\n")
      (display "# Post Content\n")))
  (define b61c-pages
    (hash "/" (string-append
               "export function getStaticProps() {"
               "  const data = fs.readFileSync('racklr-test/b61-tmp/post.md', 'utf8');"
               "  return { props: data };"
               "}"
               "export default (props) => <p>{props.title} - {props.date}</p>;")))
  (define b61c-html (emit-pages b61c-pages #:title "B61c"))
  (check-true (string-contains? b61c-html "Hello World") "B61c: fs.readFileSync frontmatter title")
  (check-true (string-contains? b61c-html "2024-01-01") "B61c: fs.readFileSync frontmatter date")
  (delete-directory/files test-dir))

;; getStaticPaths — literal paths (b61d)
(define b61d-pages
  (hash "/:slug" (string-append
                  "export function getStaticPaths() {"
                  "  return {"
                  "    paths: [{ params: { slug: 'hello' } }, { params: { slug: 'world' } }],"
                  "    fallback: false"
                  "  };"
                  "}"
                  "export default (props) => <p>slug: {props.slug}</p>;")))
(define b61d-html (emit-pages b61d-pages #:title "B61d"))
(check-true (string-contains? b61d-html "hello") "B61d: getStaticPaths generates hello page")
(check-true (string-contains? b61d-html "world") "B61d: getStaticPaths generates world page")

;; ── B62: Cross-file import evaluation via Node.js ─────────────────────
;; getStaticProps that imports a helper function from another file.
;; Uses Node --experimental-strip-types to evaluate the chain.
(let ([test-dir "/tmp/b62-tmp"])
  (when (directory-exists? test-dir)
    (delete-directory/files test-dir))
  (make-directory test-dir)
  (make-directory (build-path test-dir "lib"))
  ;; package.json: required for Node to treat .ts files as ES modules
  (with-output-to-file (build-path test-dir "package.json") #:exists 'replace
    (lambda () (display "{\"type\":\"module\"}")))
  ;; lib/data.ts: helper that returns blog post data
  (with-output-to-file (build-path test-dir "lib" "data.ts") #:exists 'replace
    (lambda ()
      (display "export function getItems() { return [{title:'B62 Title',slug:'b62-slug'}]; }")))
  ;; page.ts: imports getItems from ./lib/data, uses in getStaticProps
  (with-output-to-file (build-path test-dir "page.ts") #:exists 'replace
    (lambda ()
      (display "import { getItems } from './lib/data';\n")
      (display "export const getStaticProps = () => {\n")
      (display "  const items = getItems();\n")
      (display "  return { props: { items } };\n")
      (display "};\n")
      (display "export default () => null;\n")))
  (define b62-src (file->string (build-path test-dir "page.ts")))
  (define b62-html
    (emit-pages (hash "/" b62-src) #:title "B62" #:project-root test-dir))
  (check-true (string-contains? b62-html "B62 Title") "B62: cross-file Node eval embeds imported data")
  (check-true (string-contains? b62-html "b62-slug") "B62: cross-file Node eval embeds slug")
  (delete-directory/files test-dir))

;; ── B67: npm deps in lib/api.ts via Node (gray-matter, remark, remark-html) ─

;; B67a: getStaticProps imports helper that imports from node_modules
(let ([test-dir "/tmp/b67-tmp"])
  (when (directory-exists? test-dir)
    (delete-directory/files test-dir))
  (make-directory test-dir)
  (make-directory* (build-path test-dir "node_modules" "test-lib"))
  (make-directory (build-path test-dir "lib"))
  ;; Fake npm package
  (display-to-file
   "exports.greet = function(name) { return 'Hello, ' + name + '!'; };"
   (build-path test-dir "node_modules" "test-lib" "index.js")
   #:exists 'replace)
  (display-to-file "{\"main\":\"index.js\"}"
   (build-path test-dir "node_modules" "test-lib" "package.json")
   #:exists 'replace)
  ;; package.json for ES module mode
  (display-to-file "{\"type\":\"module\"}"
   (build-path test-dir "package.json") #:exists 'replace)
  ;; Helper that imports from npm package (mimics lib/api.ts)
  (display-to-file
   "import { greet } from 'test-lib';\nexport function getGreeting(name: string) { return greet(name); }\n"
   (build-path test-dir "lib" "api.ts") #:exists 'replace)
  ;; Page imports helper, uses in getStaticProps (relative import triggers Node eval)
  (display-to-file
   "import { getGreeting } from './lib/api';\n"
   (build-path test-dir "page.ts") #:exists 'replace)
  (display-to-file
   "export const getStaticProps = () => { return { props: { msg: getGreeting('World') } }; };\n"
   (build-path test-dir "page.ts") #:exists 'append)
  (display-to-file
   "export default function Page(p: any) { return <div>{p.msg}</div>; }\n"
   (build-path test-dir "page.ts") #:exists 'append)
  (define b67-src (file->string (build-path test-dir "page.ts")))
  (define b67-html
    (emit-pages (hash "/" b67-src) #:title "B67" #:project-root test-dir))
  (check-true (string-contains? b67-html "Hello, World!") "B67: npm package import evaluated via Node")
  (delete-directory/files test-dir))

;; ── B63: Insert ; between newline-separated type/interface members ──
;; The ANTLR TS grammar requires `;`/`,` separators and a trailing `;` after
;; `type X = {...}` (its `eos` rule is `SemiColon | EOF`), while standard TS
;; allows bare newlines. preprocess-imports must *insert* separators.

;; B63a: insert ; between newline-separated type members
(let ([processed (preprocess-imports
                  "type Props = {\n  x: string\n  y: number\n}")])
  (check-true (string-contains? processed "x: string;") "B63a: ; inserted after type member")
  (check-true (string-contains? processed "y: number;") "B63a: ; inserted after second type member")
  (check-true (string-contains? processed "};") "B63a: ; inserted after type alias"))

;; B63b: insert ; between newline-separated interface members
(let ([processed (preprocess-imports
                  "interface I {\n  name: string\n  age: number\n}")])
  (check-true (string-contains? processed "name: string;") "B63b: ; inserted after interface member")
  (check-true (string-contains? processed "age: number;") "B63b: ; inserted after second interface member"))

;; B63c: normalize double-space after {
(let ([processed (preprocess-imports
                  "type Props = {  x: string; y: number }")])
  (check-true (string-contains? processed "{ x:") "B63c: normalize double-space after {"))

;; B63e: prose mentioning "interface" before a commented-out import must not
;; corrupt the comment (the scanner falsely treats "interface below ... {"
;; as a type block and inserts ; inside the commented import).
(let ([processed (preprocess-imports
                  "// example, to import the interface below do:\n//\n// import { User } from 'path/to/interfaces';\n\nexport type User = {\n  id: number\n  name: string\n}")])
  (check-true (string-contains? processed "// import { User } from 'path/to/interfaces';")
              "B63e: commented-out import left intact")
  (check-true (string-contains? processed "id: number;") "B63e: real type member still gets ;")
  (check-true (string-contains? processed "name: string;") "B63e: second member still gets ;"))

;; B63d: full pipeline with type members carrying semicolons
(let ([js (tsx->js
           "type Props = {
              title: string;
              count: number;
            }
            function Page(props: Props) {
              return <div>{props.title}{props.count}</div>;
            }")])
  (check-true (string-contains? js "function Page") "B63d: type with semicolons lowers correctly")
  (check-true (string-contains? js "props.title") "B63d: props access works"))

;; ── B112: ambient `declare module` declarations are erased ──────────
;; The vendored TS grammar has no moduleDeclaration rule, so the usual body of
;; a .d.ts file fails to parse. Ambient declarations have no runtime form, so
;; preprocess-imports strips them before parsing.

;; B112a: bare ambient module declaration (the reported remark-html.d.ts case)
(let ([processed (preprocess-imports "declare module 'remark-html'")])
  (check-false (string-contains? processed "declare")
               "B112a: bare ambient module stripped")
  (check-equal? (string-trim processed) "" "B112a: nothing else survives"))

;; B112b: terminated declaration with surrounding runtime code
(let ([processed (preprocess-imports
                  "declare module 'remark-html';\nexport const x = 1;")])
  (check-false (string-contains? processed "declare")
               "B112b: terminated ambient module stripped")
  (check-true (string-contains? processed "export const x = 1;")
              "B112b: surrounding code kept"))

;; B112c: ambient module with a body; code after the body must survive
(let ([processed (preprocess-imports
                  (string-append
                   "declare module 'styled-components' {\n"
                   "  export default function styled(tag: string): any;\n"
                   "}\n"
                   "export const x = 1;"))])
  (check-false (string-contains? processed "declare")
               "B112c: ambient module body stripped")
  (check-true (string-contains? processed "export const x = 1;")
              "B112c: code after the body kept"))

;; B112d: declaration text inside comments and strings is not touched
(let* ([source (string-append
                "/*\n"
                "declare module 'commented-out';\n"
                "*/\n"
                "const s = \"; declare module 'in-a-string';\";\n"
                "export const x = 1;")]
       [processed (preprocess-imports source)])
  (check-equal? processed source
                "B112d: comment/string occurrences of declare module left intact"))

;; B112e: the reported .d.ts lowers and emits instead of throwing
(define remark-html-dts
  (build-path (current-directory) ".." "docs" "examples" "test-apps"
              "blog-starter" "@types" "remark-html.d.ts"))
(let* ([cleaned (preprocess-imports (file->string remark-html-dts))]
       [cst (ts-parse cleaned)]
       [js (emit-javascript (ts-lower:lower-program cst ts-tok-type ts-tok-value))])
  (check-true (string? js) "B112e: remark-html.d.ts lowers to UIR and emits"))

;; ── B68: next/image <Image> → <img> with loading (eager/lazy) ───────

;; B68a: <Image> without priority → <img loading="lazy">, props pass through
(let ([js (tsx->js
           "const img = <Image src=\"/a.jpg\" width={1300} height={630} alt=\"A\" />;")])
  (check-true (string-contains? js "createElement(\"img\")") "B68a: Image lowers to img")
  (check-true (string-contains? js "setAttribute(\"src\",\"/a.jpg\")") "B68a: src passes through")
  (check-true (string-contains? js "setAttribute(\"width\",1300)") "B68a: width passes through")
  (check-true (string-contains? js "setAttribute(\"height\",630)") "B68a: height passes through")
  (check-true (string-contains? js "setAttribute(\"alt\",\"A\")") "B68a: alt passes through")
  (check-true (string-contains? js "setAttribute(\"loading\",\"lazy\")") "B68a: default loading is lazy"))

;; B68b: <Image priority> → <img loading="eager">, priority prop dropped
(let ([js (tsx->js
           "const img = <Image src=\"/a.jpg\" width={100} height={50} priority />;")])
  (check-true (string-contains? js "setAttribute(\"loading\",\"eager\")") "B68b: priority maps to eager")
  (check-false (string-contains? js "priority") "B68b: priority prop dropped"))

;; Remove gen-tmp-*.rkt parser artifacts so they don't pollute `raco test racklr-test/*.rkt`.
(cleanup)
