#lang racket

(require rackunit
         racket/file
         racklr/tree
         racklr/uir
         racklr/gen-test
         racklr/lower-javascript
         racklr/emit-javascript)

;; ── Load the JavaScript parser ──────────────────────────────────────

(define-values (js-parse js-tokenize tok-type tok-value)
  (gen-and-load "../grammars/javascript/JavaScriptParser.g4"))

;; ── Test helper ─────────────────────────────────────────────────────

(define (check-js input)
  (define cst (js-parse input))
  (when (not cst)
    (error 'check-js "parse failed for: ~s" input))
  (define uir (lower-program cst tok-type tok-value))
  (printf "~s\n  UIR: ~a\n" input (uir->sexp uir))
  (define emitted (emit-javascript uir))
  (printf "  JS:  ~a\n" emitted)
  (values uir emitted))

;; ── Lowering + Emit Tests ───────────────────────────────────────────

;; Variable declaration with init
(let-values ([(uir emitted) (check-js "var x = 1;")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "x"))
  (check-true (string-contains? emitted "1")))

;; Variable declaration without init
(let-values ([(uir emitted) (check-js "var x;")])
  (check-equal? (uir-tag uir) 'block))

;; Expression statement: number literal
(let-values ([(uir emitted) (check-js "42;")])
  (check-equal? (uir-tag uir) 'block))

;; Expression statement: boolean
(let-values ([(uir emitted) (check-js "true;")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "true")))

;; Expression statement: null
(let-values ([(uir emitted) (check-js "null;")])
  (check-equal? (uir-tag uir) 'block))

;; Assignment
(let-values ([(uir emitted) (check-js "x = 42;")])
  (check-equal? (uir-tag uir) 'block))

;; Binary expression
(let-values ([(uir emitted) (check-js "1 + 2;")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "+")))

;; Function declaration
(let-values ([(uir emitted) (check-js "function foo() { return 42; }")])
  (check-equal? (uir-tag uir) 'block))

;; Return statement
(let-values ([(uir emitted) (check-js "function f() { return 1; }")])
  (check-equal? (uir-tag uir) 'block))

;; If statement
(let-values ([(uir emitted) (check-js "if (true) { 1; } else { 2; }")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "if")))

;; Function call: no args
(let-values ([(uir emitted) (check-js "foo();")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "foo()")))

;; Function call: with args
(let-values ([(uir emitted) (check-js "foo(1, 2);")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "foo(1, 2)")))

;; Dot member access
(let-values ([(uir emitted) (check-js "a.b;")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "a.b")))

;; Bracket member access
(let-values ([(uir emitted) (check-js "a[b];")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "a[b]")))

;; String literal (double-quoted)
(let-values ([(uir emitted) (check-js "\"hello\";")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "hello")))

;; String literal (single-quoted)
(let-values ([(uir emitted) (check-js "'world';")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "world")))

;; Ternary expression
(let-values ([(uir emitted) (check-js "x ? 1 : 2;")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "?"))
  (check-true (string-contains? emitted ":"))
  (check-true (string-contains? emitted "x")))

;; Logical AND
(let-values ([(uir emitted) (check-js "x && y;")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "&&")))

;; Logical OR
(let-values ([(uir emitted) (check-js "x || y;")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "||")))

;; Unary not
(let-values ([(uir emitted) (check-js "!x;")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "!")))

;; Unary minus
(let-values ([(uir emitted) (check-js "-x;")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "-")))

;; Prefix increment
(let-values ([(uir emitted) (check-js "++x;")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "++x")))

;; Postfix increment
(let-values ([(uir emitted) (check-js "x++;")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "x++")))

;; typeof
(let-values ([(uir emitted) (check-js "typeof x;")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "typeof")))

;; void
(let-values ([(uir emitted) (check-js "void 0;")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "void")))

;; delete
(let-values ([(uir emitted) (check-js "delete x;")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "delete")))

;; this
(let-values ([(uir emitted) (check-js "this;")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "this")))

;; new expression
(let-values ([(uir emitted) (check-js "new Foo();")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "new")))

;; Comparison not-equal
(let-values ([(uir emitted) (check-js "x != y;")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "!=")))

;; While loop
(let-values ([(uir emitted) (check-js "while (true) { x = 1; }")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "while")))

;; For loop
(let-values ([(uir emitted) (check-js "for (var i = 0; i < 10; i = i + 1) {}")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "for")))

;; For...in loop
(let-values ([(uir emitted) (check-js "for (var x in y) {}")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "for"))
  (check-true (string-contains? emitted "in")))

;; For...of loop
(let-values ([(uir emitted) (check-js "for (var x of y) {}")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "for"))
  (check-true (string-contains? emitted "of")))

;; Do-while loop
(let-values ([(uir emitted) (check-js "do {} while (true);")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "do"))
  (check-true (string-contains? emitted "while")))

;; Throw statement
(let-values ([(uir emitted) (check-js "throw x;")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "throw")))

;; Break statement
(let-values ([(uir emitted) (check-js "break;")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "break")))

;; Continue statement
(let-values ([(uir emitted) (check-js "continue;")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "continue")))

;; Try-catch statement
(let-values ([(uir emitted) (check-js "try { x = 1; } catch (e) { x = 2; }")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "try"))
  (check-true (string-contains? emitted "catch")))

;; Let declaration
(let-values ([(uir emitted) (check-js "let x = 1;")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "let"))
  (check-true (string-contains? emitted "x"))
  (check-true (string-contains? emitted "1")))

;; Let without init
(let-values ([(uir emitted) (check-js "let x;")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "let"))
  (check-false (string-contains? emitted "=")))

;; Const declaration
(let-values ([(uir emitted) (check-js "const x = 1;")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "const"))
  (check-true (string-contains? emitted "x"))
  (check-true (string-contains? emitted "1")))

;; Class declaration (no extends)
(let-values ([(uir emitted) (check-js "class Foo {}")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "class"))
  (check-true (string-contains? emitted "Foo"))
  (check-false (string-contains? emitted "extends")))

;; Class declaration (with extends)
(let-values ([(uir emitted) (check-js "class Bar extends Foo {}")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "class"))
  (check-true (string-contains? emitted "Bar"))
  (check-true (string-contains? emitted "extends"))
  (check-true (string-contains? emitted "Foo")))

;; Object literal: single property
(let-values ([(uir emitted) (check-js "var x = {a: 1};")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "a"))
  (check-true (string-contains? emitted "1")))

;; Object literal: multiple properties
(let-values ([(uir emitted) (check-js "var x = {a: 1, b: 2};")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "a"))
  (check-true (string-contains? emitted "b"))
  (check-true (string-contains? emitted "1"))
  (check-true (string-contains? emitted "2")))

;; Object literal: empty
(let-values ([(uir emitted) (check-js "var x = {};")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "{")))

;; ── B112: non-identifier object keys keep their key text ────────────
;; Keys such as 'accent-1', '5xl', and 28 are legal JS property names but not
;; identifiers; lowering used to drop them to `?` and emit invalid `?:` pairs.
(let-values ([(uir emitted)
              (check-js "var x = { 'accent-1': 1, 28: '7rem', '5xl': '2.5rem', plain: 3 };")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "\"accent-1\": 1"))
  (check-true (string-contains? emitted "\"28\": \"7rem\""))
  (check-true (string-contains? emitted "\"5xl\": \"2.5rem\""))
  (check-true (string-contains? emitted "plain: 3"))
  (check-false (string-contains? emitted "?"))
  (check-true (and (js-parse emitted) #t) "B112: emitted quoted keys re-parse"))

;; Object literal: reserved-word key (identifierName → reservedWord)
(let-values ([(uir emitted) (check-js "var x = { default: 1 };")])
  (check-true (string-contains? emitted "default: 1")))

;; Array literal: multiple items
(let-values ([(uir emitted) (check-js "var x = [1, 2, 3];")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "["))
  (check-true (string-contains? emitted "1"))
  (check-true (string-contains? emitted "2"))
  (check-true (string-contains? emitted "3")))

;; Array literal: empty
(let-values ([(uir emitted) (check-js "var x = [];")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "[")))

;; Arrow function: no params
(let-values ([(uir emitted) (check-js "var f = () => 42;")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "=>"))
  (check-true (string-contains? emitted "42")))

;; Arrow function: single param
(let-values ([(uir emitted) (check-js "var f = (x) => x + 1;")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "=>"))
  (check-true (string-contains? emitted "x"))
  (check-true (string-contains? emitted "+")))

;; Arrow function: multiple params
(let-values ([(uir emitted) (check-js "var f = (a, b) => a + b;")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "=>"))
  (check-true (string-contains? emitted "a"))
  (check-true (string-contains? emitted "b"))
  (check-true (string-contains? emitted "+")))

;; Arrow function: block body (previously crashed: the { ... } body was fed
;; to the single-expression lowerer as the OpenBrace token)
(let-values ([(uir emitted) (check-js "var f = (x) => { return x; };")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "=>"))
  (check-true (string-contains? emitted "return")))

;; Parenthesized IIFE (previously crashed: the lowerer recursed into the
;; OpenParen token of `( function(){...} )` and violated cst-node-children)
(let-values ([(uir emitted) (check-js "(function(){ return 1; })();")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "function"))
  (check-true (string-contains? emitted "return")))

;; Function expression: no params
(let-values ([(uir emitted) (check-js "var f = function() { return 1; };")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "function"))
  (check-true (string-contains? emitted "return"))
  (check-true (string-contains? emitted "1")))

;; Function expression: with params
(let-values ([(uir emitted) (check-js "var f = function(x, y) { return x + y; };")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "function"))
  (check-true (string-contains? emitted "x"))
  (check-true (string-contains? emitted "y"))
  (check-true (string-contains? emitted "+")))

;; Object method shorthand
(let-values ([(uir emitted) (check-js "var x = { foo() { return 1; } };")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "foo()"))
  (check-true (string-contains? emitted "return"))
  (check-not-false (string-contains? emitted "1")))

;; Object getter
(let-values ([(uir emitted) (check-js "var x = { get bar() { return 42; } };")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "get"))
  (check-true (string-contains? emitted "bar"))
  (check-true (string-contains? emitted "42")))

;; Object setter
(let-values ([(uir emitted) (check-js "var x = { set baz(v) { } };")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "set"))
  (check-true (string-contains? emitted "baz"))
  (check-true (string-contains? emitted "v")))

;; Mixed object: properties + method
(let-values ([(uir emitted) (check-js "var x = { a: 1, foo() { return 2; }, b: 3 };")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "a"))
  (check-true (string-contains? emitted "foo()"))
  (check-true (string-contains? emitted "b"))
  (check-true (string-contains? emitted "2")))

;; Spread in array
(let-values ([(uir emitted) (check-js "var x = [...arr];")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "...arr")))

;; Spread in function call
(let-values ([(uir emitted) (check-js "foo(...args);")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "...args")))

;; Spread in object
(let-values ([(uir emitted) (check-js "var x = {...obj};")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "...obj")))

;; Mixed spread in array
(let-values ([(uir emitted) (check-js "var x = [1, ...arr, 2];")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "1"))
  (check-true (string-contains? emitted "...arr"))
  (check-true (string-contains? emitted "2")))

;; Rest parameter in arrow function
(let-values ([(uir emitted) (check-js "var f = (a, ...rest) => rest;")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "...rest"))
  (check-true (string-contains? emitted "=>")))

;; Rest parameter in function expression
(let-values ([(uir emitted) (check-js "var f = function(a, ...rest) { return rest; };")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "...rest"))
  (check-true (string-contains? emitted "function")))

;; ES Modules: import named
(let-values ([(uir emitted) (check-js "import { x } from 'm';")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "import"))
  (check-true (string-contains? emitted "x")))

;; ES Modules: import default
(let-values ([(uir emitted) (check-js "import x from 'm';")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "import"))
  (check-true (string-contains? emitted "x")))

;; ES Modules: import namespace
(let-values ([(uir emitted) (check-js "import * as ns from 'm';")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "import"))
  (check-true (string-contains? emitted "*"))
  (check-true (string-contains? emitted "ns")))

;; ES Modules: import bare
(let-values ([(uir emitted) (check-js "import 'm';")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "import")))

;; ES Modules: export default
(let-values ([(uir emitted) (check-js "export default 42;")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "export"))
  (check-true (string-contains? emitted "default"))
  (check-true (string-contains? emitted "42")))

;; Debugger statement
(let-values ([(uir emitted) (check-js "debugger;")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "debugger")))

;; With statement
(let-values ([(uir emitted) (check-js "with (x) { y = 1; }")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "with"))
  (check-true (string-contains? emitted "x")))

;; Regex literal
(let-values ([(uir emitted) (check-js "var r = /foo/;")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "/foo/")))

;; new.target
(let-values ([(uir emitted) (check-js "var x = new.target;")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "new.target")))

;; Import alias (multi-name)
(let-values ([(uir emitted) (check-js "import { x, y } from 'm';")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "x"))
  (check-true (string-contains? emitted "y")))

;; Import alias (as)
(let-values ([(uir emitted) (check-js "import { x as y } from 'm';")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "x"))
  (check-true (string-contains? emitted "y")))

;; Async function declaration
(let-values ([(uir emitted) (check-js "async function f() { await 1; }")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "async"))
  (check-true (string-contains? emitted "await"))
  (check-true (string-contains? emitted "1")))

;; Generator function declaration
(let-values ([(uir emitted) (check-js "function* g() { yield 1; }")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "function*"))
  (check-true (string-contains? emitted "yield 1")))

;; ── Computed property name ─────────────────────────────────────────

(let-values ([(uir emitted) (check-js "var x = {[y]: 1};")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "[y]"))
  (check-true (string-contains? emitted "1")))

;; ── Property shorthand ─────────────────────────────────────────────

(let-values ([(uir emitted) (check-js "var x = {y};")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "y")))

(let-values ([(uir emitted) (check-js "var x = {y, z};")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "y"))
  (check-true (string-contains? emitted "z")))

;; ── Labeled statement ──────────────────────────────────────────────

(let-values ([(uir emitted) (check-js "label: 1;")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "label"))
  (check-true (string-contains? emitted "1")))

(let-values ([(uir emitted) (check-js "label: for(;;) {}")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "label"))
  (check-true (string-contains? emitted "for")))

;; ── Named exports ──────────────────────────────────────────────────

(let-values ([(uir emitted) (check-js "export const x = 1;")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "export"))
  (check-true (string-contains? emitted "const"))
  (check-true (string-contains? emitted "x"))
  (check-true (string-contains? emitted "1")))

(let-values ([(uir emitted) (check-js "export function f() { return 1; }")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "export"))
  (check-true (string-contains? emitted "function"))
  (check-true (string-contains? emitted "f"))
  (check-true (string-contains? emitted "return")))

(let-values ([(uir emitted) (check-js "export { x, y };")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "export"))
  (check-true (string-contains? emitted "x"))
  (check-true (string-contains? emitted "y")))

;; ── B112: the reported tailwind.config.js round-trips ───────────────
;; The config's nested objects use keys the JS grammar only accepts quoted
;; ('accent-1', '5xl') or as numbers (28); those used to lower to `?` and the
;; emitted text then failed to re-parse.
(define tailwind-config
  (build-path (current-directory) ".." "docs" "examples" "test-apps"
              "blog-starter" "tailwind.config.js"))
(let* ([src (file->string tailwind-config)]
       [cst (js-parse src)]
       [uir (and cst (lower-program cst tok-type tok-value))]
       [emitted (and uir (emit-javascript uir))])
  (check-true (and cst #t) "B112: tailwind.config.js parses")
  (check-true (and emitted #t) "B112: tailwind.config.js lowers")
  (check-false (string-contains? emitted "?:") "B112: no placeholder keys survive")
  (check-true (and (js-parse emitted) #t) "B112: emitted tailwind config re-parses"))

;; ── Issue #122: statement-if, repetition parameters and destructuring ──
;; A statement-if must emit as `if (...) { ... }`: the emitter distinguishes a
;; statement-if from an expression ternary by block branches, so emitting
;; `test ? return x : null` was invalid JavaScript.
(let-values ([(uir emitted) (check-js "function f(value) { if (!value) return null; if (value.bad) throw value.bad; }")])
  (check-true (string-contains? emitted "if (! value) { return null; }"))
  (check-true (string-contains? emitted "if (value.bad) { throw value.bad; }"))
  (check-false (string-contains? emitted "? return")))

;; Parameters after the first live in a repetition group; they were dropped.
(let-values ([(uir emitted) (check-js "run(function (req, res, next) { next(); });")])
  (check-true (string-contains? emitted "function(req, res, next)")))

;; A reserved word used as a property name keeps its name (`p.catch`).
(let-values ([(uir emitted) (check-js "promise.catch(onError).finally(onDone);")])
  (check-true (string-contains? emitted "promise.catch(onError).finally(onDone)")))

;; Destructuring declarations keep their binding structure instead of a `?`
;; placeholder, and re-lower to the same structural UIR after emission.
(define (check-destructuring src anchor)
  (define-values (uir emitted) (check-js src))
  (check-true (string-contains? emitted anchor) (format "~s keeps ~s" src anchor))
  (check-true (and (js-parse emitted) #t) (format "~s re-parses" src))
  (check-equal? (uir->sexp (lower-program (js-parse emitted) tok-type tok-value))
                (uir->sexp uir)
                (format "~s round-trips structurally" src)))

(check-destructuring "const { a, b: c, d = 1, ...rest } = o;"
                     "const {a, b: c, d = 1, ...rest} = o;")
(check-destructuring "const [x, , y = 2, ...z] = xs;" "const [x, , y = 2, ...z] = xs;")
(check-destructuring "const { outer: { inner } } = o;" "const {outer: {inner}} = o;")
(check-destructuring "for (const [k, v] of entries) { g(k, v); }"
                     "for (const [k, v] of entries)")
(check-destructuring "for (const { name } of users) { g(name); }"
                     "for (const {name} of users)")

;; Optional call and nullish coalescing keep a first-class encoding.
(let-values ([(uir emitted) (check-js "a?.(); const n = x ?? y; const m = a?.b;")])
  (check-true (string-contains? emitted "a?.()"))
  (check-true (string-contains? emitted "x ?? y"))
  (check-true (string-contains? emitted "a ?. b")))

(cleanup)
(displayln "All JavaScript lowering tests passed.")
