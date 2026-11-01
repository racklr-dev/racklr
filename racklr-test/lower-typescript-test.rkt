#lang racket

(require rackunit
         racklr/tree
         racklr/uir
         racklr/gen-test
         racklr/lower-typescript
         racklr/emit-javascript)

;; ── Load the TypeScript parser ──────────────────────────────────────

(define-values (ts-parse ts-tokenize tok-type tok-value)
  (gen-and-load "../grammars-v4/javascript/typescript-cleaned/TypeScriptParser.g4"))

;; ── Test helper ─────────────────────────────────────────────────────

(define (check-ts input)
  (define cst (ts-parse input))
  (when (not cst)
    (error 'check-ts "parse failed for: ~s" input))
  (define uir (lower-program cst tok-type tok-value))
  (printf "TS: ~s\n  UIR: ~a\n" input (uir->sexp uir))
  (define emitted (emit-javascript uir))
  (printf "  JS:  ~a\n" emitted)
  (values uir emitted))

;; ── Lowering + Emit Tests: Type Annotation Stripping ────────────────

;; Variable with type annotation — type stripped, JS output should have no ':'
(let-values ([(uir emitted) (check-ts "let x: number = 42;")])
  (check-equal? (uir-tag uir) 'block)
  (check-false (string-contains? emitted "number"))
  (check-true (string-contains? emitted "let"))
  (check-true (string-contains? emitted "42")))

;; Variable without type annotation — works normally
(let-values ([(uir emitted) (check-ts "let y = 42;")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "42")))

;; Const with string type annotation
(let-values ([(uir emitted) (check-ts "const name: string = \"hello\";")])
  (check-equal? (uir-tag uir) 'block)
  (check-false (string-contains? emitted "string")))

;; Var with boolean type annotation
(let-values ([(uir emitted) (check-ts "var flag: boolean = true;")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "true")))

;; Let with any type annotation
(let-values ([(uir emitted) (check-ts "let x: any = 42;")])
  (check-equal? (uir-tag uir) 'block))

;; ── Functions with Type Annotations ──────────────────────────────────

(let-values ([(uir emitted) (check-ts "function greet(name: string): string { return \"hello \" + name; }")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "function"))
  (check-true (string-contains? emitted "greet"))
  (check-true (string-contains? emitted "return"))
  (check-false (string-contains? emitted ": string")))

(let-values ([(uir emitted) (check-ts "function log(msg: string): void { }")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "function"))
  (check-true (string-contains? emitted "log")))

;; Arrow function with type annotations
(let-values ([(uir emitted) (check-ts "const add = (a: number, b: number) => a + b;")])
  (check-equal? (uir-tag uir) 'block))

;; ── Generics (type parameter stripping) ─────────────────────────────

(let-values ([(uir emitted) (check-ts "function identity<T>(arg: T): T { return arg; }")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "function"))
  (check-true (string-contains? emitted "identity")))

;; ── Interface — should be elided (no runtime equivalent) ────────────

(let-values ([(uir emitted) (check-ts "interface Point { x: number; y: number; }")])
  (check-equal? (uir-tag uir) 'block)
  (check-false (string-contains? emitted "interface"))
  (check-false (string-contains? emitted "Point")))

;; ── Type Alias — should be elided ───────────────────────────────────

(let-values ([(uir emitted) (check-ts "type ID = string | number;")])
  (check-equal? (uir-tag uir) 'block)
  (check-false (string-contains? emitted "type"))
  (check-false (string-contains? emitted "ID")))

;; ── Enum — lowered to a call ────────────────────────────────────────

(let-values ([(uir emitted) (check-ts "enum Color { Red, Green, Blue }")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "Color")))

;; ── Namespace — lowered to a call ───────────────────────────────────

(let-values ([(uir emitted) (check-ts "namespace Utils { export function f(): void {} }")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "Utils")))

;; ── As expression (type cast) — cast stripped ──────────────────────

(let-values ([(uir emitted) (check-ts "let x = 42 as number;")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "42"))
  (check-false (string-contains? emitted "as"))
  (check-false (string-contains? emitted "number")))

;; ── Non-null assertion — stripped ───────────────────────────────────

(let-values ([(uir emitted) (check-ts "let x = foo!;")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "foo"))
  (check-false (string-contains? emitted "!")))

;; ── Expression statements ───────────────────────────────────────────

(let-values ([(uir emitted) (check-ts "1 + 2;")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "+")))

(let-values ([(uir emitted) (check-ts "console.log(\"hello\");")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "console")))

;; ── New expression ───────────────────────────────────────────────────

(let-values ([(uir emitted) (check-ts "new Date();")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "new Date()"))
  (check-false (string-contains? emitted "new Date()()")))

;; ── Curried calls (f(1)(2)) must not drop the RHS ──────────────────

(let-values ([(uir emitted) (check-ts "const c = f(1)(2);")])
  (check-true (string-contains? emitted "f(1)(2)")
              "curried call with non-empty inner args is preserved"))

(let-values ([(uir emitted) (check-ts "const r = withEffects({read: [], write: [\"db\"], max: 5})(fn);")])
  (check-true (string-contains? emitted "withEffects(")
              "curried withEffects callee survives lowering")
  (check-true (string-contains? emitted "fn")
              "curried withEffects keeps its function argument"))

(let-values ([(uir emitted) (check-ts "const c = f(1)(2)(3);")])
  (check-true (string-contains? emitted "f(1)(2)(3)")
              "deeper curried call chain is preserved"))

;; ── If statement ────────────────────────────────────────────────────

(let-values ([(uir emitted) (check-ts "if (true) { 1; } else { 2; }")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "if")))

;; ── Class declaration ────────────────────────────────────────────────

(let-values ([(uir emitted) (check-ts "class Greeter { greet() { return 1; } }")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "class Greeter"))
  (check-true (string-contains? emitted "greet()"))
  (check-true (string-contains? emitted "return 1")))

(let-values ([(uir emitted) (check-ts "class Foo extends Bar { baz(x) { return x; } }")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "class Foo"))
  (check-true (string-contains? emitted "extends Bar"))
  (check-true (string-contains? emitted "baz(x)")))

;; ── drg-b876 regressions ────────────────────────────────────────────

;; new X(args) must not gain a spurious trailing call
(let-values ([(uir emitted) (check-ts "new Error(\"x\");")])
  (check-true (string-contains? emitted "new Error(\"x\")"))
  (check-false (string-contains? emitted "new Error(\"x\")()")))

;; typed catch binding must lower to a bare catch (err)
(let-values ([(uir emitted) (check-ts "try { a(); } catch (err: any) { b(); }")])
  (check-true (string-contains? emitted "catch (err)"))
  (check-false (string-contains? emitted "catch (null)")))

;; export default function must preserve its name
(let-values ([(uir emitted) (check-ts "export default function Post() { return 1; }")])
  (check-true (string-contains? emitted "export default function Post()"))
  (check-false (string-contains? emitted "export default function()")))

;; return with no trailing semicolon (parenthesized or object) must not
;; misparse as an expression statement with a `null` callee
(let-values ([(uir emitted) (check-ts "function f() { return (\n x\n ) }")])
  (check-true (string-contains? emitted "return x"))
  (check-false (string-contains? emitted "null(")))

(let-values ([(uir emitted) (check-ts "function f() { return {\n a: 1\n } }")])
  (check-true (string-contains? emitted "return { a: 1 }"))
  (check-false (string-contains? emitted "null(")))

;; ── drg-4788: subscript expressions ─────────────────────────────────

;; A subscript initializer must survive lowering. Dropping the rhs emits an
;; initializer-less `const`, which is not valid JavaScript and loses the
;; element access.
(let-values ([(uir emitted) (check-ts "const first = posts[0];")])
  (check-true (string-contains? emitted "posts[0]"))
  (check-false (string-contains? emitted "const first;")))

;; Subscript on a member chain.
(let-values ([(uir emitted) (check-ts "const title = posts[0].title;")])
  (check-true (string-contains? emitted "posts[0].title")))

;; Chained subscripts.
(let-values ([(uir emitted) (check-ts "const cell = grid[i][j];")])
  (check-true (string-contains? emitted "grid[i][j]")))

;; Subscript in an arithmetic expression.
(let-values ([(uir emitted) (check-ts "const next = posts[0] + 1;")])
  (check-true (string-contains? emitted "posts[0] + 1")))

;; Subscript as a call target and as a call argument.
(let-values ([(uir emitted) (check-ts "posts[0]();")])
  (check-true (string-contains? emitted "posts[0]()")))
(let-values ([(uir emitted) (check-ts "render(posts[0]);")])
  (check-true (string-contains? emitted "render(posts[0])")))

;; Subscript as a return value.
(let-values ([(uir emitted) (check-ts "function first(rows) { return rows[0]; }")])
  (check-true (string-contains? emitted "return rows[0]")))

;; Subscript assignment and compound assignment.
(let-values ([(uir emitted) (check-ts "posts[0] = 1;")])
  (check-true (string-contains? emitted "posts[0] = 1")))
(let-values ([(uir emitted) (check-ts "posts[0] += 1;")])
  (check-true (string-contains? emitted "posts[0] += 1")))

;; ── Clean up temp files ─────────────────────────────────────────────

(cleanup)
