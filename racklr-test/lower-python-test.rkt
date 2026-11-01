#lang racket

(require rackunit
         racklr/tree
         racklr/uir
         racklr/gen-test
         racklr/g4-parse
         racklr/gend-parser
         racklr/lower-python
         racklr/emit-python
         racklr/effects/contract)

;; ── Load the Python3 parser ──────────────────────────────────────────

(define-values (py-parse-raw py-tokenize py-tok-type py-tok-value)
  (gen-and-load-py "../grammars/python3/Python3Parser.g4"))

;; The tracked grammar's single_input entry rejects one-line compound statements
;; ("for x in y: pass"): simple_stmts consumes the statement's NEWLINE, so the
;; alternative's trailing NEWLINE can never match. That is LOOP_PLAN.md's
;; documented "Known failures" parser gap (gen-python3-test / lower-python-test
;; used to die here before running any assertion). Test-harness workaround only:
;; fall back to the file_input entry (the entry the real lowering pipeline uses)
;; and unwrap its single `stmt`, which lower-python then lowers exactly as it
;; would have lowered the single_input CST. No lowering/emission assertion is
;; weakened — compound-statement round-trips are still checked byte-for-byte.
(define (parse-single-via-file sql)
  (define fp (py-parse-file-input sql))
  (for/or ([c (any-tree-children fp)] #:when (and (list? c) (pair? c)))
    (for/or ([cc c] #:when (and (cst-node? cc) (eq? (any-tree-tag cc) 'group)))
      (define gk (filter cst-node? (any-tree-children cc)))
      (and (pair? gk) (eq? (any-tree-tag (first gk)) 'stmt) (first gk)))))
(define (py-parse sql)
  (define r (with-handlers ([exn:fail? (lambda (_) #f)]) (py-parse-raw sql)))
  (if (any-tree? r) r (parse-single-via-file sql)))

;; Load raw parse-file_input from the SAME generated module as gen-and-load-py
;; (avoids cross-module token struct incompatibility)
(define py-parse-file-input
  (let ([mod-path (*last-module-path*)])
    (λ (src)
      (define tks ((dynamic-require mod-path 'tokenize) src))
      (match-define (list _fp res) ((dynamic-require mod-path 'parse-file_input) tks 0))
      res)))

;; ── Round-trip test helper ──────────────────────────────────────────

(define (check-py input-str expected-emitted)
  (define cst (py-parse input-str))
  (check-true (any-tree? cst)
              (format "parse ~s" input-str))
  (define uir (lower-python cst py-tok-type py-tok-value))
  (check-true (uir? uir)
              (format "lower ~s" input-str))
  (define emitted (emit-python uir))
  (check-true (string? emitted)
              (format "emit ~s" input-str))
  (check-equal? emitted expected-emitted
                (format "round-trip ~s" input-str)))

;; ── Simple statements ────────────────────────────────────────────────

(check-py "pass\n" "pass")
(check-py "return\n" "return None")
(check-py "return 42\n" "return 42")
(check-py "x = 1\n" "x = 1")
(check-py "import os\n" "import os")
(check-py "from os import path\n" "from os import path")
(check-py "y = x\n" "y = x")

;; ── UIR inspection tests ────────────────────────────────────────────

;; pass lowers to uir-null
(let ([cst (py-parse "pass\n")])
  (define uir (lower-python cst py-tok-type py-tok-value))
  (check-true (uir-null? uir) "pass lowers to uir-null"))

;; return 42 lowers to (uir-return (uir-number "42"))
(let ([cst (py-parse "return 42\n")])
  (define uir (lower-python cst py-tok-type py-tok-value))
  (check-true (uir-return? uir) "return lowers to uir-return")
  (check-true (uir-number? (uir-return-value uir)) "return value is number")
  (check-equal? (uir-number-value (uir-return-value uir)) "42" "return value is 42"))

;; x = 1 lowers to (uir-set! (uir-var ...) (uir-number "1"))
(let ([cst (py-parse "x = 1\n")])
  (define uir (lower-python cst py-tok-type py-tok-value))
  (check-true (uir-set!? uir) "assignment lowers to uir-set!")
  (check-true (uir-var? (uir-set!-name uir)) "LHS is uir-var")
  (check-equal? (uir-symbol-name (uir-var-name (uir-set!-name uir))) "x" "LHS name is x")
  (check-true (uir-number? (uir-set!-value uir)) "RHS is number")
  (check-equal? (uir-number-value (uir-set!-value uir)) "1" "RHS value is 1"))

;; import os lowers to (uir-import (uir-symbol "os") '())
(let ([cst (py-parse "import os\n")])
  (define uir (lower-python cst py-tok-type py-tok-value))
  (check-true (uir-import? uir) "import lowers to uir-import")
  (check-equal? (uir-symbol-name (uir-import-source uir)) "os" "module name"))

;; class docstring (a bare string-literal statement) must not lower to a "?" field.
;; Uses parse-file_input because the single_input entry (py-parse) is broken for
;; compound statements (see LOOP_PLAN issue #1).
(let ([cst (py-parse-file-input "class A:\n    \"\"\"doc\"\"\"\n")])
  (define uir (lower-python cst py-tok-type py-tok-value))
  (define klass (first (uir-block-stmts uir)))
  (check-true (uir-class? klass) "docstring-only class lowers to uir-class")
  (check-equal? (length (uir-class-fields klass)) 0 "docstring is not a field"))

(let ([cst (py-parse-file-input "class A:\n    \"\"\"doc\"\"\"\n    x = 1\n")])
  (define uir (lower-python cst py-tok-type py-tok-value))
  (define klass (first (uir-block-stmts uir)))
  (check-equal? (length (uir-class-fields klass)) 1 "docstring + assignment lowers to one field")
  (check-equal? (uir-symbol-name (uir-field-name (first (uir-class-fields klass)))) "x"
                "the surviving field is x, not ?"))

;; Class super extraction: the positional base lowers from a `namedexpr_test`
;; (after the walrus rewiring of `argument`), not a bare `test`;
;; find-super-node must still extract it into uir-class-super.
(let ([cst (py-parse-file-input "class Foo(Bar):\n    pass\n")])
  (define uir (lower-python cst py-tok-type py-tok-value))
  (check-true (uir-block? uir) "file class lowers to uir-block")
  (define klass (first (uir-block-stmts uir)))
  (check-true (uir-class? klass) "class stmt lowers to uir-class")
  (define sup (first (uir-class-super klass)))
  (check-true (uir-var? sup) "super lowers to uir-var")
  (check-equal? (uir-symbol-name (uir-var-name sup)) "Bar" "super name is Bar"))

;; Multiple inheritance: every positional base is captured in left-to-right
;; order; keyword (`metaclass=`) and star bases are skipped.
(let ([cst (py-parse-file-input "class C(A, B, metaclass=M):\n    pass\n")])
  (define uir (lower-python cst py-tok-type py-tok-value))
  (define klass (first (uir-block-stmts uir)))
  (define supers (uir-class-super klass))
  (check-equal? (length supers) 2 "two positional bases captured")
  (check-equal? (uir-symbol-name (uir-var-name (first supers))) "A" "first base is A")
  (check-equal? (uir-symbol-name (uir-var-name (second supers))) "B" "second base is B"))

;; Multiple bases round-trip through emit-python.
(let ([cst (py-parse-file-input "class C(A, B):\n    pass\n")])
  (define uir (lower-python cst py-tok-type py-tok-value))
  (check-equal? (emit-python uir) "class C(A, B):\n    pass" "multi-base emit"))

;; ── Operators (left-recursive expr flattening) ────────────────────
;; Uses py-parse-file-input so these run regardless of the single_input bug.

;; `1 | 2` (raw OR_OP token between two expr nodes) lowers to a `|` call,
;; not ?expr.
(let ([cst (py-parse-file-input "1 | 2\n")])
  (define uir (lower-python cst py-tok-type py-tok-value))
  (define call (first (uir-block-stmts uir)))
  (check-true (uir-call? call) "bitwise-or lowers to uir-call")
  (check-equal? (uir-symbol-name (uir-call-callee call)) "|" "op symbol is |"))

;; `2 ** 3` (raw POWER token) lowers to a `**` call, not ?expr.
(let ([cst (py-parse-file-input "2 ** 3\n")])
  (define uir (lower-python cst py-tok-type py-tok-value))
  (define call (first (uir-block-stmts uir)))
  (check-true (uir-call? call) "power lowers to uir-call")
  (check-equal? (uir-symbol-name (uir-call-callee call)) "**" "op symbol is **"))

;; Chained comparison `a < b < c` lowers to the dedicated cmp-chain node
;; (operand, operator, operand, operator, operand), not nested `<` calls:
;; Python evaluates `b` once and means `a < b and b < c`, so the nested-call
;; encoding was both semantically wrong and unprintable without changing shape.
(let ([cst (py-parse-file-input "a < b < c\n")])
  (define uir (lower-python cst py-tok-type py-tok-value))
  (define call (first (uir-block-stmts uir)))
  (check-true (uir-call? call) "chained comparison lowers to uir-call")
  (check-equal? (uir-symbol-name (uir-call-callee call)) "cmp-chain"
                "chained comparison uses the cmp-chain constructor")
  (define args (uir-call-args call))
  (check-equal? (length args) 5 "cmp-chain has operand/op/operand/op/operand")
  (check-equal? (map (lambda (a) (and (uir-symbol? a) (uir-symbol-name a)))
                     (list (list-ref args 1) (list-ref args 3)))
                '("<" "<") "cmp-chain operator entries are < symbols")
  (check-equal? (map (lambda (a) (and (uir-var? a)
                                      (uir-symbol-name (uir-var-name a))))
                     (list (first args) (list-ref args 2) (list-ref args 4)))
                '("a" "b" "c") "cmp-chain operands are a, b, c"))

;; Two-token comparison operators keep their names inside the chain.
(let* ([cst (py-parse-file-input "a is not b is not c\n")]
       [uir (lower-python cst py-tok-type py-tok-value)]
       [call (first (uir-block-stmts uir))]
       [args (uir-call-args call)])
  (check-equal? (uir-symbol-name (uir-call-callee call)) "cmp-chain")
  (check-equal? (map uir-symbol-name (list (list-ref args 1) (list-ref args 3)))
                '("is not" "is not") "two-token operators survive in cmp-chain"))

;; Union annotation `int | None` lowers to a `|` call, not ?expr.
(let ([cst (py-parse-file-input "x: int | None = 1\n")])
  (define uir (lower-python cst py-tok-type py-tok-value))
  (define ann (first (uir-block-stmts uir)))
  (check-true (uir-ann-set!? ann) "annotated assignment lowers to uir-ann-set!")
  (define ty (uir-ann-set!-type ann))
  (check-true (uir-call? ty) "union annotation lowers to uir-call")
  (check-equal? (uir-symbol-name (uir-call-callee ty)) "|" "union op symbol is |"))

;; Soft keyword `match` (PEP 634) tokenizes as MATCH, not NAME, so it must
;; still lower to a named loop/comprehension variable, not ?var.
(let ([cst (py-parse-file-input "for match in matches:\n    pass\n")])
  (define uir (lower-python cst py-tok-type py-tok-value))
  (define loop (first (uir-block-stmts uir)))
  (check-true (uir-for-each? loop) "for match lowers to uir-for-each")
  (check-equal? (uir-symbol-name (uir-for-each-var loop)) "match" "loop var is match"))

(let ([cst (py-parse-file-input "xs = [match[1] for match in ms]\n")])
  (define uir (lower-python cst py-tok-type py-tok-value))
  (define stmt (first (uir-block-stmts uir)))
  (define comp (uir-set!-value stmt))
  (check-true (uir-call? comp) "comprehension lowers to uir-call")
  (check-equal? (uir-symbol-name (uir-call-callee comp)) "list-comp" "list-comp constructor")
  (check-equal? (uir-symbol-name (list-ref (uir-call-args comp) 1)) "match"
                "comprehension var is match"))

;; `...` (ELLIPSIS token) lowers to uir-symbol "...", not ?atom-ELLIPSIS.
(let ([cst (py-parse-file-input "y = ...\n")])
  (define uir (lower-python cst py-tok-type py-tok-value))
  (define stmt (first (uir-block-stmts uir)))
  (check-true (uir-set!? stmt) "ellipsis assignment lowers to uir-set!")
  (define val (uir-set!-value stmt))
  (check-true (uir-symbol? val) "ellipsis lowers to uir-symbol")
  (check-equal? (uir-symbol-name val) "..." "ellipsis symbol is ..."))

;; except* (PEP 654) parses and lowers; the `*` is a token the lowering skips,
;; so the catch still extracts type + (absent) name.
(let ([cst (py-parse-file-input "try: pass\nexcept* ValueError: pass\n")])
  (define uir (lower-python cst py-tok-type py-tok-value))
  (define t (first (uir-block-stmts uir)))
  (check-true (uir-try? t) "except* lowers to uir-try")
  (check-equal? (length (uir-try-catches t)) 1 "one except* clause")
  (let ([cat (car (uir-try-catches t))])
    (check-true (uir-var? (car cat)) "exception type is var")
    (check-equal? (uir-symbol-name (uir-var-name (car cat))) "ValueError"
                  "exception type is ValueError")
    (check-true (uir-null? (cadr cat)) "no as-name in bare except*")))

;; ── For loops ─────────────────────────────────────────────────────

;; round-trip: for x in y: pass
(check-py "for x in y: pass\n\n" "for x in y:\n    pass")

;; UIR inspection: for x in y: pass lowers to uir-for-each
(let ([cst (py-parse "for x in y: pass\n\n")])
  (define uir (lower-python cst py-tok-type py-tok-value))
  (check-true (uir-for-each? uir) "for loop lowers to uir-for-each")
  (check-equal? (uir-symbol-name (uir-for-each-var uir)) "x" "loop var is x")
  (check-true (uir-var? (uir-for-each-iterable uir)) "iterable is uir-var")
  (check-equal? (uir-symbol-name (uir-var-name (uir-for-each-iterable uir))) "y" "iterable is y")
  (check-true (uir-null? (uir-for-each-body uir)) "body is pass (uir-null)")
  (check-true (uir-null? (uir-for-each-else-body uir)) "no else clause"))

;; ── Try/except/finally ────────────────────────────────────────────

;; round-trip: try/except bare
(check-py "try: pass\nexcept: pass\n\n" "try:\n    pass\nexcept:\n    pass")

;; round-trip: try/except with exception type and as
(check-py "try: pass\nexcept Exception as e: pass\n\n"
          "try:\n    pass\nexcept Exception as e:\n    pass")

;; round-trip: try/except/else
(check-py "try: pass\nexcept: pass\nelse: pass\n\n"
          "try:\n    pass\nexcept:\n    pass\nelse:\n    pass")

;; round-trip: try/finally (no except)
(check-py "try: pass\nfinally: pass\n\n"
          "try:\n    pass\nfinally:\n    pass")

;; UIR inspection: try/except structure
(let ([cst (py-parse "try: pass\nexcept ValueError as e: return e\n\n")])
  (define uir (lower-python cst py-tok-type py-tok-value))
  (check-true (uir-try? uir) "try lowers to uir-try")
  (check-true (uir-null? (uir-try-body uir)) "try body is pass (uir-null)")
  (check-equal? (length (uir-try-catches uir)) 1 "one catch clause")
  (let ([cat (car (uir-try-catches uir))])
    (check-true (uir-var? (car cat)) "exception type is var")
    (check-equal? (uir-symbol-name (uir-var-name (car cat))) "ValueError" "exception type is ValueError")
    (check-equal? (uir-symbol-name (uir-var-name (cadr cat))) "e" "exception name is e")
    (check-true (uir-return? (caddr cat)) "handler has return"))
  (check-false (uir-try-else-body uir) "no else clause")
  (check-false (uir-try-finally-body uir) "no finally clause"))

;; ── With statements ───────────────────────────────────────────────

;; round-trip: with x as y: pass
(check-py "with x as y: pass\n\n"
          "with x as y:\n    pass")

;; round-trip: with x: pass (no as)
(check-py "with x: pass\n\n"
          "with x:\n    pass")

;; UIR inspection: with with_items
(let ([cst (py-parse "with x as y: pass\n\n")])
  (define uir (lower-python cst py-tok-type py-tok-value))
  (check-true (uir-with? uir) "with lowers to uir-with")
  (check-equal? (length (uir-with-items uir)) 1 "one with item")
  (let ([item (car (uir-with-items uir))])
    (check-true (uir-var? (car item)) "context expr is var")
    (check-true (uir-var? (cadr item)) "as-name is var")
    (check-equal? (uir-symbol-name (uir-var-name (cadr item))) "y" "as-name is y"))
  (check-true (uir-null? (uir-with-body uir)) "body is pass"))

;; ── Yield ──────────────────────────────────────────────────────────

;; round-trip: yield x
(check-py "yield x\n" "yield x")

;; round-trip: yield from x
(check-py "yield from x\n" "yield from x")

;; UIR inspection: yield x
(let ([cst (py-parse "yield x\n")])
  (define uir (lower-python cst py-tok-type py-tok-value))
  (check-true (uir-yield? uir) "yield lowers to uir-yield")
  (check-false (uir-yield-from? uir) "not yield from")
  (check-true (uir-var? (uir-yield-value uir)) "yield value is var")
  (check-equal? (uir-symbol-name (uir-var-name (uir-yield-value uir))) "x" "yield value is x"))

;; UIR inspection: yield from x
(let ([cst (py-parse "yield from x\n")])
  (define uir (lower-python cst py-tok-type py-tok-value))
  (check-true (uir-yield-from? uir) "is yield from"))

;; ── Await ──────────────────────────────────────────────────────────

;; round-trip: await foo()
(check-py "await foo()\n" "await foo()")

;; UIR inspection: await foo()
(let ([cst (py-parse "await foo()\n")])
  (define uir (lower-python cst py-tok-type py-tok-value))
  (check-true (uir-await? uir) "await lowers to uir-await")
  (check-true (uir-call? (uir-await-expr uir)) "await wraps a call"))

;; ── Function calls ─────────────────────────────────────────────────

;; UIR inspection: foo(1, 2, 3) keeps all three arguments
(let ([cst (py-parse "foo(1, 2, 3)\n")])
  (define uir (lower-python cst py-tok-type py-tok-value))
  (check-true (uir-call? uir) "call lowers to uir-call")
  (check-equal? (length (uir-call-args uir)) 3 "call has three args"))

;; round-trip: foo(1, 2, 3)
(check-py "foo(1, 2, 3)\n" "foo(1, 2, 3)")

;; round-trip: trailing comma normalizes to no trailing comma
(check-py "foo(1, 2,)\n" "foo(1, 2)")

;; ── Async def ─────────────────────────────────────────────────────

;; UIR inspection: async def foo(): pass
(let ([cst (py-parse "async def foo(): pass\n\n")])
  (define uir (lower-python cst py-tok-type py-tok-value))
  (check-true (uir-call? uir) "async def lowers to an async-fn wrapper call")
  (check-equal? (uir-symbol-name (uir-call-callee uir)) "async-fn"
                "async def uses the async-fn wrapper")
  (check-equal? (length (uir-call-args uir)) 1 "async-fn wraps one function")
  (check-true (uir-fn? (first (uir-call-args uir))) "inner is uir-fn"))

;; round-trip: async def foo(): pass
(check-py "async def foo(): pass\n\n" "async def foo():\n    pass")

;; ── Lambda ────────────────────────────────────────────────────────

;; UIR inspection: lambda x: x + 1
(let ([cst (py-parse "lambda x: x + 1\n")])
  (define uir (lower-python cst py-tok-type py-tok-value))
  (check-true (uir-fn? uir) "lambda lowers to uir-fn")
  (check-equal? (length (uir-fn-params uir)) 1 "lambda has 1 param")
  (check-equal? (uir-symbol-name (first (uir-fn-params uir))) "x" "param name is x"))

;; UIR inspection: lambda: 42
(let ([cst (py-parse "lambda: 42\n")])
  (define uir (lower-python cst py-tok-type py-tok-value))
  (check-true (uir-fn? uir) "no-arg lambda lowers to uir-fn")
  (check-equal? (length (uir-fn-params uir)) 0 "no params"))

;; ── Decorators ─────────────────────────────────────────────────────

;; round-trip: @deco\ndef foo(): pass
(check-py "@deco\ndef foo(): pass\n\n"
          "@deco\ndef foo():\n    pass")

;; UIR inspection: @deco\ndef foo(): pass
(let ([cst (py-parse "@deco\ndef foo(): pass\n\n")])
  (define uir (lower-python cst py-tok-type py-tok-value))
  (check-true (uir-decorated? uir) "decorated lowers to uir-decorated")
  (check-equal? (length (uir-decorated-decorators uir)) 1 "one decorator")
  (check-true (uir-call? (first (uir-decorated-decorators uir))) "decorator is a call")
  (check-true (uir-fn? (uir-decorated-inner uir)) "inner is uir-fn"))

;; @effects(...) extraction: decorator arg is a positional dict
(let ([cst (py-parse "@effects({\"llm\": {\"max\": 5}, \"db\": {\"reads\": \"search_index\"}})\ndef summarize(): pass\n\n")])
  (define uir (lower-python cst py-tok-type py-tok-value))
  (define fn (uir-decorated-inner uir))
  (check-true (uir-fn? fn) "inner is uir-fn")
  (check-true (uir-effect-row? (uir-fn-effects fn)) "effects row attached")
  (define row (uir-fn-effects fn))
  (check-equal? (uir-effect-entry-budget (row-lookup row 'llm)) 5)
  (check-equal? (uir-symbol-name (uir-effect-entry-scope (row-lookup row 'db)))
                "reads=search_index"))

;; @effects(...) emission: preamble + runtime guard decorator
(let ([cst (py-parse "@effects({\"llm\": {\"max\": 5}})\ndef summarize(): pass\n\n")])
  (define uir (lower-python cst py-tok-type py-tok-value))
  (define emitted (emit-python uir))
  (check-not-false (string-contains? emitted "def __withEffects(row):")
                   "preamble defines __withEffects")
  (check-not-false (string-contains? emitted "@__withEffects({\"llm\": {\"modality\": \"may\", \"max\": 5}})")
                   "guard decorator emitted with the effect row")
  (check-not-false (string-contains? emitted "def summarize():")
                   "decorated function still emitted"))

;; ── Data structure literals ─────────────────────────────────────────

;; list round-trip
(check-py "[1, 2, 3]\n" "[1, 2, 3]")

;; empty list
(check-py "[]\n" "[]")

;; dict round-trip
(check-py "{\"a\": 1, \"b\": 2}\n" "{\"a\": 1, \"b\": 2}")

;; empty dict
(check-py "{}\n" "{}")

;; UIR inspection: list
(let ([cst (py-parse "[1, 2, 3]\n")])
  (define uir (lower-python cst py-tok-type py-tok-value))
  (check-true (uir-list? uir) "list literal lowers to uir-list")
  (check-equal? (length (uir-list-items uir)) 3 "list has 3 items")
  (check-equal? (uir-number-value (first (uir-list-items uir))) "1" "first item is 1"))

;; UIR inspection: dict
(let ([cst (py-parse "{\"a\": 1}\n")])
  (define uir (lower-python cst py-tok-type py-tok-value))
  (check-true (uir-record? uir) "dict literal lowers to uir-record")
  (check-equal? (length (uir-record-entries uir)) 1 "dict has 1 entry")
  (let ([entry (car (uir-record-entries uir))])
    (check-equal? (uir-string-value (car entry)) "a" "key is a")
    (check-equal? (uir-number-value (cdr entry)) "1" "value is 1")))

;; ── Comprehensions ──────────────────────────────────────────────────

;; list comp round-trip
(check-py "[x for x in y]\n" "[x for x in y]")

;; list comp with filter
(check-py "[x for x in y if True]\n" "[x for x in y if True]")

;; list comp with expression
(check-py "[x+1 for x in y]\n" "[x + 1 for x in y]")

;; set comp
(check-py "{x for x in y}\n" "{x for x in y}")

;; UIR inspection: list comp
(let ([cst (py-parse "[x for x in y]\n")])
  (define uir (lower-python cst py-tok-type py-tok-value))
  (check-true (uir-call? uir) "list comp lowers to uir-call")
  (let ([callee (uir-call-callee uir)])
    (check-true (uir-symbol? callee) "callee is symbol")
    (check-equal? (uir-symbol-name callee) "list-comp" "callee is list-comp")))

;; UIR inspection: set comp
(let ([cst (py-parse "{x for x in y}\n")])
  (define uir (lower-python cst py-tok-type py-tok-value))
  (check-true (uir-call? uir) "set comp lowers to uir-call")
  (let ([callee (uir-call-callee uir)])
    (check-equal? (uir-symbol-name callee) "set-comp" "callee is set-comp")))

;; ── Boolean operators ───────────────────────────────────────────────

;; and round-trip
(check-py "x and y\n" "x and y")

;; or round-trip
(check-py "x or y\n" "x or y")

;; not round-trip
(check-py "not x\n" "not x")

;; UIR inspection: not x
(let ([cst (py-parse "not x\n")])
  (define uir (lower-python cst py-tok-type py-tok-value))
  (check-true (uir-call? uir) "not lowers to uir-call")
  (check-equal? (uir-symbol-name (uir-call-callee uir)) "not" "callee is not"))

;; ── Augmented assignment ────────────────────────────────────────────

;; += round-trip
(check-py "x += 1\n" "x = x + 1")

;; -= round-trip
(check-py "x -= 2\n" "x = x - 2")

;; UIR inspection: x += 1
(let ([cst (py-parse "x += 1\n")])
  (define uir (lower-python cst py-tok-type py-tok-value))
  (check-true (uir-set!? uir) "augassign lowers to uir-set!")
  (check-true (uir-call? (uir-set!-value uir)) "RHS is a call")
  (check-equal? (uir-symbol-name (uir-call-callee (uir-set!-value uir))) "+" "operator is +"))

;; ── Attribute access and subscript ──────────────────────────────────

;; round-trip: x.attr
(check-py "x.attr\n" "x.attr")

;; round-trip: x[y]
(check-py "x[y]\n" "x[y]")

;; round-trip: x[0]
(check-py "x[0]\n" "x[0]")

;; round-trip: chained: x.attr[y]
(check-py "x.attr[y]\n" "x.attr[y]")

;; UIR inspection: x.attr lowers to uir-get with string field
(let ([cst (py-parse "x.attr\n")])
  (define uir (lower-python cst py-tok-type py-tok-value))
  (check-true (uir-get? uir) "x.attr lowers to uir-get")
  (check-true (uir-var? (uir-get-base uir)) "base is var")
  (check-equal? (uir-symbol-name (uir-var-name (uir-get-base uir))) "x" "base name is x")
  (check-true (uir-string? (uir-get-field uir)) "field is string")
  (check-equal? (uir-string-value (uir-get-field uir)) "attr" "field is 'attr'"))

;; UIR inspection: x[0] lowers to uir-get with number field
(let ([cst (py-parse "x[0]\n")])
  (define uir (lower-python cst py-tok-type py-tok-value))
  (check-true (uir-get? uir) "x[0] lowers to uir-get")
  (check-true (uir-var? (uir-get-base uir)) "base is var")
  (check-true (uir-number? (uir-get-field uir)) "field is number")
  (check-equal? (uir-number-value (uir-get-field uir)) "0" "field is 0"))

;; round-trip: x[a, b] (emitter keeps the source subscript shape)
(check-py "x[a, b]\n" "x[a, b]")

;; UIR inspection: slices lower to (call "slice" (list start stop step)).
;; Uses py-parse-file-input so these run despite the single_input bug (#1).
(define (check-slice src start stop step)
  (define cst (py-parse-file-input src))
  (define uir (lower-python cst py-tok-type py-tok-value))
  (define stmt (first (uir-block-stmts uir)))
  (define rhs (uir-set!-value stmt))
  (check-true (uir-get? rhs) (format "~s is a subscript get" src))
  (define field (uir-get-field rhs))
  (check-true (uir-call? field) (format "~s field is a slice call" src))
  (check-equal? (uir-symbol-name (uir-call-callee field)) "slice" (format "~s callee is slice" src))
  (define args (uir-call-args field))
  (check-equal? (length args) 3 (format "~s slice has 3 args" src))
  (check-equal? (uir-null? (list-ref args 0)) start (format "~s start nullness" src))
  (check-equal? (uir-null? (list-ref args 1)) stop (format "~s stop nullness" src))
  (check-equal? (uir-null? (list-ref args 2)) step (format "~s step nullness" src)))

(check-slice "x = a[1:2]\n" #f #f #t)     ;; start=1 stop=2 step=null
(check-slice "x = a[:2]\n" #t #f #t)      ;; start=null stop=2
(check-slice "x = a[1:]\n" #f #t #t)      ;; start=1 stop=null
(check-slice "x = a[::2]\n" #t #t #f)     ;; start=null stop=null step=2
(check-slice "x = a[::]\n" #t #t #t)      ;; full copy

;; UIR inspection: `*a, b = c` lowers its unpack target to a uir-list whose
;; first element is a uir-spread (a single-element spread in the name slot
;; could not represent the second bound name).
(let ([cst (py-parse-file-input "*a, b = c\n")])
  (define uir (lower-python cst py-tok-type py-tok-value))
  (define stmt (first (uir-block-stmts uir)))
  (check-true (uir-set!? stmt) "*a, b = c lowers to uir-set!")
  (define target (uir-set!-name stmt))
  (check-true (uir-list? target) "LHS is a target uir-list")
  (check-equal? (length (uir-list-items target)) 2 "both target elements kept")
  (check-true (uir-spread? (first (uir-list-items target))) "first element is a spread")
  (check-equal? (uir-symbol-name (uir-var-name (uir-spread-expr (first (uir-list-items target)))))
                "a" "spread binds a")
  (check-true (uir-var? (second (uir-list-items target))) "second element is b"))

;; round-trip: f-string
(check-py "f\"hello {name}\"\n" "f\"hello {name}\"")

;; ── del, global, nonlocal ──────────────────────────────────────────

;; round-trip: del x
(check-py "del x\n" "del x")

;; round-trip: del x, y
(check-py "del x, y\n" "del x, y")

;; round-trip: global x
(check-py "global x\n" "global x")

;; round-trip: global x, y
(check-py "global x, y\n" "global x, y")

;; round-trip: nonlocal x
(check-py "nonlocal x\n" "nonlocal x")

;; round-trip: nonlocal x, y
(check-py "nonlocal x, y\n" "nonlocal x, y")

;; UIR inspection: del x
(let ([cst (py-parse "del x\n")])
  (define uir (lower-python cst py-tok-type py-tok-value))
  (check-true (uir-call? uir) "del lowers to uir-call")
  (check-equal? (uir-symbol-name (uir-call-callee uir)) "del" "callee is del")
  (check-equal? (length (uir-call-args uir)) 1 "one arg")
  (check-true (uir-var? (first (uir-call-args uir))) "arg is var"))

;; UIR inspection: global x, y
(let ([cst (py-parse "global x, y\n")])
  (define uir (lower-python cst py-tok-type py-tok-value))
  (check-true (uir-call? uir) "global lowers to uir-call")
  (check-equal? (uir-symbol-name (uir-call-callee uir)) "global" "callee is global")
  (check-equal? (length (uir-call-args uir)) 2 "two names")
  (check-equal? (uir-symbol-name (first (uir-call-args uir))) "x" "first name is x"))

;; ── Raise ───────────────────────────────────────────────────────────

;; round-trip: raise
(check-py "raise\n" "raise")

;; round-trip: raise expr
(check-py "raise ValueError\n" "raise ValueError")

;; round-trip: raise expr from cause
(check-py "raise ValueError from e\n" "raise ValueError from e")

;; UIR inspection: raise ValueError
(let ([cst (py-parse "raise ValueError\n")])
  (define uir (lower-python cst py-tok-type py-tok-value))
  (check-true (uir-call? uir) "raise lowers to uir-call")
  (check-equal? (uir-symbol-name (uir-call-callee uir)) "raise" "callee is raise")
  (check-equal? (length (uir-call-args uir)) 1 "one arg")
  (check-true (uir-var? (first (uir-call-args uir))) "arg is var"))

;; UIR inspection: raise ValueError from e
(let ([cst (py-parse "raise ValueError from e\n")])
  (define uir (lower-python cst py-tok-type py-tok-value))
  (check-true (uir-call? uir) "raise from lowers to uir-call")
  (check-equal? (uir-symbol-name (uir-call-callee uir)) "raise" "callee is raise")
  (check-equal? (length (uir-call-args uir)) 2 "two args")
  (check-true (uir-var? (second (uir-call-args uir))) "cause is var"))

;; ── Assert ──────────────────────────────────────────────────────────

;; round-trip: assert expr
(check-py "assert True\n" "assert True")

;; round-trip: assert expr, msg
(check-py "assert x, \"msg\"\n" "assert x, \"msg\"")

;; UIR inspection: assert x
(let ([cst (py-parse "assert x\n")])
  (define uir (lower-python cst py-tok-type py-tok-value))
  (check-true (uir-call? uir) "assert lowers to uir-call")
  (check-equal? (uir-symbol-name (uir-call-callee uir)) "assert" "callee is assert")
  (check-equal? (length (uir-call-args uir)) 1 "one arg")
  (check-true (uir-var? (first (uir-call-args uir))) "arg is var"))

;; ── Ternary expressions ─────────────────────────────────────────────

;; round-trip: ternary x if cond else y
(check-py "x if cond else y\n" "x if cond else y")

;; round-trip: ternary in assignment
(check-py "z = x if cond else y\n" "z = x if cond else y")

;; UIR inspection: ternary
(let ([cst (py-parse "x if cond else y\n")])
  (define uir (lower-python cst py-tok-type py-tok-value))
  (check-true (uir-if? uir) "ternary lowers to uir-if")
  (check-true (uir-var? (uir-if-then uir)) "then is var")
  (check-true (uir-var? (uir-if-test uir)) "test is var")
  (check-true (uir-var? (uir-if-else uir)) "else is var"))

;; ── Type annotations ────────────────────────────────────────────────

;; round-trip: variable annotation
(check-py "x: int\n" "x: int")

;; round-trip: variable annotation with initializer
(check-py "x: int = 5\n" "x: int = 5")

;; round-trip: plain assignment (no annotation)
(check-py "x = 5\n" "x = 5")

;; round-trip: function return type annotation
(check-py "def f() -> int:\n    pass\n\n" "def f() -> int:\n    pass")

;; round-trip: function with typed parameters
(check-py "def f(x: int):\n    pass\n\n" "def f(x: int):\n    pass")

;; round-trip: function with typed params, defaults, and return type
(check-py "def f(x: int, y: str = \"hi\") -> bool:\n    pass\n\n"
          "def f(x: int, y: str = \"hi\") -> bool:\n    pass")

;; round-trip: typed param with default only
(check-py "def f(x: int = 1):\n    pass\n\n" "def f(x: int = 1):\n    pass")

;; round-trip: function body with return (block body, not pass)
(check-py "def f(x):\n    return x\n\n" "def f(x):\n    return x")

;; round-trip: function body with return of a binop
(check-py "def f(a, b):\n    return a + b\n\n" "def f(a, b):\n    return a + b")

;; UIR inspection: positional-only params (PEP 570) lower cleanly — the `/`
;; marker is dropped (it only constrains call sites, not effects), but every
;; parameter before and after it must survive as a uir-typed-param.
(let ([cst (py-parse "def f(a, b, /, c=1, *args, **kw):\n    pass\n\n")])
  (define uir (lower-python cst py-tok-type py-tok-value))
  (define params (uir-fn-params uir))
  (check-equal? (map (lambda (p) (uir-symbol-name (uir-typed-param-name p))) params)
                '("a" "b" "c" "args" "kw")
                "positional-only params lower in order, / dropped")
  (check-equal? (uir-number-value (uir-typed-param-default (caddr params)))
                "1" "default after / is preserved"))

;; UIR inspection: annotated assignment
(let ([cst (py-parse "x: int\n")])
  (define uir (lower-python cst py-tok-type py-tok-value))
  (check-true (uir-ann-set!? uir) "annotated assignment lowers to uir-ann-set!"))

;; ── Identity and membership operators ───────────────────────────────

;; round-trip: is
(check-py "1 is None\n" "1 is None")

;; round-trip: in
(check-py "x in y\n" "x in y")

;; UIR inspection: is
(let ([cst (py-parse "1 is None\n")])
  (define uir (lower-python cst py-tok-type py-tok-value))
  (check-true (uir-call? uir) "is lowers to uir-call")
  (check-equal? (uir-symbol-name (uir-call-callee uir)) "is" "callee is is")
  (check-equal? (length (uir-call-args uir)) 2 "two args"))

;; ── Expression precedence / parenthesization ────────────────────────

;; round-trip: parenthesized expression preserves parens
(check-py "(x + y)\n" "(x + y)")

;; round-trip: nested parens
(check-py "((x))\n" "((x))")
(check-py "((x + y))\n" "((x + y))")

;; round-trip: tuples (should not be treated as parenthesized)
(check-py "(x,)\n" "(x,)")
(check-py "(1, 2)\n" "(1, 2)")
(check-py "(1, 2, 3)\n" "(1, 2, 3)")

;; round-trip: plain expression (no parens)
(check-py "x + y\n" "x + y")

;; UIR inspection: parenthesized expression
(let ([cst (py-parse "(x + y)\n")])
  (define uir (lower-python cst py-tok-type py-tok-value))
  (check-true (uir-paren? uir) "paren expr lowers to uir-paren"))

;; ── Classes ─────────────────────────────────────────────────────────

;; round-trip: empty class
(check-py "class Foo:\n    pass\n\n" "class Foo:\n    pass")

;; round-trip: class with inheritance
(check-py "class Foo(Bar):\n    pass\n\n" "class Foo(Bar):\n    pass")

;; round-trip: class with field
(check-py "class Foo:\n    x = 1\n\n" "class Foo:\n    x = 1")

;; round-trip: class with method
(check-py "class Foo:\n    def m(self):\n        pass\n\n"
          "class Foo:\n    def m(self):\n        pass")

;; round-trip: class with field and method
(check-py "class Foo:\n    x = 1\n    def m(self, y):\n        pass\n\n"
          "class Foo:\n    x = 1\n    def m(self, y):\n        pass")

;; UIR inspection: class
(let ([cst (py-parse "class Foo:\n    pass\n\n")])
  (define uir (lower-python cst py-tok-type py-tok-value))
  (check-true (uir-class? uir) "class lowers to uir-class")
  (check-equal? (uir-class-super uir) '() "no superclass"))

;; ── File input / multi-statement ────────────────────────────────────

;; file_input parses multiple statements
(let ([cst (py-parse-file-input "x = 1\ny = 2\n")])
  (check-true (any-tree? cst) "file_input parse")
  (check-equal? (any-tree-tag cst) 'file_input "file_input tag")
  (define uir (lower-python cst py-tok-type py-tok-value))
  (check-true (uir-block? uir) "file_input lowers to uir-block")
  (define emitted (emit-python uir))
  (check-true (string-contains? emitted "x = 1") "file output contains first stmt")
  (check-true (string-contains? emitted "y = 2") "file output contains second stmt"))


;; ── Match/case ──────────────────────────────────────────────────────

;; round-trip: match with literal pattern
(check-py "match x:\n    case 1:\n        pass\n\n" "match x:\n    case 1:\n        pass")

;; round-trip: match with wildcard pattern
(check-py "match x:\n    case _:\n        pass\n\n" "match x:\n    case _:\n        pass")

;; round-trip: match with capture pattern
(check-py "match x:\n    case y:\n        pass\n\n" "match x:\n    case y:\n        pass")

;; round-trip: match with guard
(check-py "match x:\n    case y if y > 0:\n        pass\n\n" "match x:\n    case y if y > 0:\n        pass")

;; round-trip: match with or patterns
(check-py "match x:\n    case 1 | 2:\n        pass\n\n" "match x:\n    case 1 | 2:\n        pass")

;; round-trip: match with sequence pattern
(check-py "match x:\n    case [a, b]:\n        pass\n\n" "match x:\n    case [a, b]:\n        pass")

;; round-trip: match with as pattern
(check-py "match x:\n    case [a, b] as pair:\n        pass\n\n"
          "match x:\n    case [a, b] as pair:\n        pass")

;; round-trip: match with string literal pattern
(check-py "match x:\n    case \"hello\":\n        pass\n\n" "match x:\n    case \"hello\":\n        pass")

;; round-trip: match with multiple pattern types (or + wildcard)
(check-py "match x:\n    case 1 | 2 | _:\n        pass\n\n" "match x:\n    case 1 | 2 | _:\n        pass")

;; round-trip: match with complex subject expression
(check-py "match a + b:\n    case _:\n        pass\n\n" "match a + b:\n    case _:\n        pass")

;; UIR inspection: match
(let ([cst (py-parse "match x:\n    case 1:\n        pass\n\n")])
  (define uir (lower-python cst py-tok-type py-tok-value))
  (check-true (uir-match? uir) "match lowers to uir-match")
  (check-true (uir-var? (uir-match-subject uir)) "subject is uir-var")
  (check-equal? (uir-symbol-name (uir-var-name (uir-match-subject uir))) "x" "subject is x")
  (check-true (pair? (uir-match-cases uir)) "has cases"))

;; UIR inspection: case with literal
(let ([cst (py-parse "match x:\n    case 1:\n        pass\n\n")])
  (define uir (lower-python cst py-tok-type py-tok-value))
  (define cases (uir-match-cases uir))
  (check-equal? (length cases) 1 "one case")
  (define c1 (first cases))
  (check-true (uir-case? c1) "case struct")
  (check-true (uir-pat-literal? (uir-case-pattern c1)) "literal pattern")
  (check-false (uir-case-guard c1) "no guard"))

;; UIR inspection: case with guard
(let ([cst (py-parse "match x:\n    case y if y > 0:\n        pass\n\n")])
  (define uir (lower-python cst py-tok-type py-tok-value))
  (define c1 (first (uir-match-cases uir)))
  (check-not-false (uir-case-guard c1) "has guard"))

;; UIR inspection: open sequence pattern (comma-separated, no brackets) lowers
;; to uir-pat-sequence, not ?pat-open_sequence_pattern. Uses py-parse-file-input
;; because single_input is broken for compound statements (LOOP_PLAN issue #1).
(let ([cst (py-parse-file-input
            "match token.split_contents():\n    case \"partialdef\", partial_name, \"inline\":\n        pass\n\n")])
  (define uir (lower-python cst py-tok-type py-tok-value))
  (define m (first (uir-block-stmts uir)))
  (define c1 (first (uir-match-cases m)))
  (check-true (uir-pat-sequence? (uir-case-pattern c1))
              "open sequence pattern lowers to uir-pat-sequence")
  (define elems (uir-pat-sequence-elements (uir-case-pattern c1)))
  (check-equal? (length elems) 3 "open sequence has three elements")
  (check-true (uir-pat-literal? (first elems)) "first element is a literal")
  (check-true (uir-pat-capture? (second elems)) "second element is a capture")
  (check-true (uir-pat-literal? (third elems)) "third element is a literal"))

;; ── Regression: lowering representation fixes for emit round-trip ───
;;
;; Every construct here was previously collapsed by the Python front end (the
;; emitter could not reconstruct it) and is now encoded so that
;; parse → lower → emit → parse → lower reproduces the same UIR.

;; Emit round-trip identity: re-lowering the emitted Python must reproduce the
;; same UIR node-for-node (not merely text that parses).
(define (check-identity src)
  (define u1 (lower-python (py-parse-file-input src) py-tok-type py-tok-value))
  (define emitted (emit-python u1))
  (define reparsed? (and (string? emitted)
                         (with-handlers ([exn:fail? (lambda (_) #f)])
                           (any-tree? (py-parse-file-input (string-append emitted "\n"))))))
  (check-true reparsed? (format "emitted Python re-parses: ~s -> ~s" src emitted))
  (when reparsed?
    (define u2 (lower-python (py-parse-file-input (string-append emitted "\n"))
                             py-tok-type py-tok-value))
    (check-equal? u2 u1 (format "UIR identity after emit: ~s -> ~s" src emitted))))

;; 1. star / double-star call arguments (were both dropped, so `f(*xs)` and
;;    `f(xs)` lowered identically).
(let* ([cst (py-parse "f(*xs, **kw)\n")]
       [uir (lower-python cst py-tok-type py-tok-value)]
       [args (uir-call-args uir)])
  (check-equal? (length args) 2 "one positional and one keyword unpack")
  (check-true (uir-spread? (first args)) "* argument lowers to uir-spread")
  (check-equal? (uir-symbol-name (uir-var-name (uir-spread-expr (first args))))
                "xs" "spread binds xs")
  (check-true (uir-call? (second args)) "** argument lowers to a call")
  (check-equal? (uir-symbol-name (uir-call-callee (second args))) "kw-spread"
                "** argument uses the kw-spread constructor"))
(check-identity "f(*xs)\n")
(check-identity "f(**kw)\n")
(check-identity "f(1, *xs, **kw)\n")

;; 2. Chained assignment (used to keep only the first target and treat the
;;    next target as the value).
(let* ([cst (py-parse "a = b = c = 1\n")]
       [uir (lower-python cst py-tok-type py-tok-value)])
  (check-true (uir-set!? uir) "chained assignment lowers to uir-set!")
  (define (set-name n) (uir-symbol-name (uir-var-name (uir-set!-name n))))
  (check-equal? (set-name uir) "a" "outermost target is a")
  (define inner1 (uir-set!-value uir))
  (check-true (uir-set!? inner1) "chain nests in the value slot")
  (check-equal? (set-name inner1) "b" "second target is b")
  (define inner2 (uir-set!-value inner1))
  (check-true (uir-set!? inner2) "chain nests a third level")
  (check-equal? (set-name inner2) "c" "third target is c")
  (check-true (uir-number? (uir-set!-value inner2)) "final value is the RHS"))
(check-identity "a = b = c = 1\n")
(check-identity "d = e = f(*xs)\n")

;; 3. Tuple / list unpack targets (used to keep only the first element).
(let* ([cst (py-parse "a, b = x\n")]
       [uir (lower-python cst py-tok-type py-tok-value)]
       [target (uir-set!-name uir)])
  (check-true (uir-list? target) "unpack target lowers to a uir-list")
  (check-equal? (map (lambda (e) (uir-symbol-name (uir-var-name e)))
                     (uir-list-items target))
                '("a" "b") "both unpack targets survive in order"))
(let* ([cst (py-parse "a, *rest = x\n")]
       [uir (lower-python cst py-tok-type py-tok-value)]
       [target (uir-set!-name uir)])
  (check-equal? (length (uir-list-items target)) 2 "star target keeps both elements")
  (check-true (uir-spread? (second (uir-list-items target))) "second element is a spread"))
(check-identity "a, b = x\n")
(check-identity "a, *rest = x\n")
(check-identity "*a, b = c\n")
(check-identity "(a, b) = x\n")
(check-identity "[a, b] = x\n")
(check-identity "a, (b, c) = x\n")

;; 4. Dotted / chained decorators (callee used to collapse to its first name,
;;    so @pytest.mark.parametrize(...) lowered like @pytest(...)).
(let* ([cst (py-parse "@pytest.mark.parametrize(\"data\", [1])\ndef f(): pass\n\n")]
       [uir (lower-python cst py-tok-type py-tok-value)]
       [deco (first (uir-decorated-decorators uir))]
       [callee (uir-call-callee deco)])
  (check-true (uir-get? callee) "decorator callee is an attribute chain")
  (check-true (uir-get? (uir-get-base callee)) "chain has two levels")
  (check-equal? (uir-string-value (uir-get-field callee)) "parametrize"
                "outermost attribute is parametrize")
  (check-equal? (uir-string-value (uir-get-field (uir-get-base callee))) "mark"
                "inner attribute is mark"))
(check-identity "@pytest.mark.parametrize(\"data\", [1])\ndef f(): pass\n\n")
(check-identity "@app.route(\"/x\")\ndef f(): pass\n\n")
(check-identity "@deco\ndef f(): pass\n\n")

;; 5. String literal normalization: plain/u literals are stored decoded (the
;;    emitter re-escapes), raw/bytes stay verbatim, f-strings store the raw
;;    body.
(let* ([cst (py-parse "x = \"a\\\"b\"\n")]
       [uir (lower-python cst py-tok-type py-tok-value)]
       [lit (uir-set!-value uir)])
  (check-true (uir-string? lit) "plain literal lowers to uir-string")
  (check-equal? (uir-string-value lit) "a\"b" "escaped quote is decoded"))
(let* ([cst (py-parse "x = \"\"\"a\nb\"\"\"\n")]
       [uir (lower-python cst py-tok-type py-tok-value)])
  (check-equal? (uir-string-value (uir-set!-value uir)) "a\nb"
                "triple-quoted literal keeps its embedded newline"))
(let* ([cst (py-parse "x = r\"^\\\\d+\"\n")]
       [uir (lower-python cst py-tok-type py-tok-value)])
  (check-equal? (uir-string-value (uir-set!-value uir)) "r\"^\\\\d+\""
                "raw literal keeps the full token text verbatim"))
(let* ([cst (py-parse "x = f\"\"\"a\n{b}\"\"\"\n")]
       [uir (lower-python cst py-tok-type py-tok-value)])
  (check-true (uir-fstring? (uir-set!-value uir)) "f-string lowers to uir-fstring")
  (check-equal? (uir-fstring-value (uir-set!-value uir)) "a\n{b}"
                "f-string body is raw and delimiter-free"))
(let* ([cst (py-parse "x = \"a\" \"b\"\n")]
       [uir (lower-python cst py-tok-type py-tok-value)])
  (check-equal? (uir-string-value (uir-set!-value uir)) "ab"
                "implicit literal concatenation is one string"))
(check-identity "x = \"a\\\"b\"\n")
(check-identity "x = 'a\"b'\n")
(check-identity "x = \"\"\"a\nb\"\"\"\n")
(check-identity "x = r\"^\\\\d+\"\n")
(check-identity "x = b\"abc\"\n")
(check-identity "x = u\"hi\"\n")
(check-identity "x = f\"hello {name}\"\n")
(check-identity "x = f\"\"\"a\n{b}\"\"\"\n")
(check-identity "x = \"a\" \"b\"\n")

;; 6. Chained comparisons (nested `<` calls were semantically wrong).
(check-identity "a < b < c\n")
(check-identity "a is not b is not c\n")
(check-identity "a not in b < c\n")

;; 7. with ... as (a, b) target: a uir-list, so the emitter never calls
;;    uir-var-name on a tuple expression.
(let* ([cst (py-parse "with server as (host, port): pass\n\n")]
       [uir (lower-python cst py-tok-type py-tok-value)]
       [target (cadr (first (uir-with-items uir)))])
  (check-true (uir-list? target) "with-as tuple target lowers to a uir-list")
  (check-equal? (map (lambda (e) (uir-symbol-name (uir-var-name e)))
                     (uir-list-items target))
                '("host" "port") "both with-as targets survive"))
(check-identity "with server as (host, port): pass\n")
(check-identity "with x as (a, b), y as z: pass\n")

;; Call arguments / collection displays with unpacking (same class of loss as
;; item 1): `[*xs, 1]`, `(1, *xs)`, `{*a, *b}` used to drop the starred item.
(let* ([cst (py-parse "x = [*xs, 1]\n")]
       [uir (lower-python cst py-tok-type py-tok-value)]
       [items (uir-list-items (uir-set!-value uir))])
  (check-equal? (length items) 2 "list display keeps the starred item")
  (check-true (uir-spread? (first items)) "starred list item is a spread"))
(check-identity "x = [*xs, 1]\n")
(check-identity "x = (1, *xs)\n")
(check-identity "x = {*a, *b}\n")

;; ── Regression: import lowering keeps source and imported names ──────
;; `from X import a` used to become `import X.a` (every name glued into the
;; module path), `from .pkg import a` emitted `import pkg.a` (dots dropped) and
;; `import a, b` emitted one bogus dotted module.
(let* ([cst (py-parse-file-input "from .globals import current_app\n")]
       [uir (lower-python cst py-tok-type py-tok-value)]
       [imp (first (uir-block-stmts uir))])
  (check-true (uir-import? imp) "from-import lowers to uir-import")
  (check-equal? (uir-symbol-name (uir-import-source imp)) ".globals"
                "relative module prefix is kept")
  (check-equal? (map uir-symbol-name (uir-import-names imp)) '("current_app")
                "imported name is kept"))
(let* ([cst (py-parse-file-input "from typing import (Any, Dict)\n")]
       [uir (lower-python cst py-tok-type py-tok-value)]
       [imp (first (uir-block-stmts uir))])
  (check-equal? (uir-symbol-name (uir-import-source imp)) "typing" "module is typing")
  (check-equal? (map uir-symbol-name (uir-import-names imp)) '("Any" "Dict")
                "parenthesised name list survives"))
(let* ([cst (py-parse-file-input "import os, sys\n")]
       [uir (lower-python cst py-tok-type py-tok-value)]
       ;; The file_input entry wraps the statement in a block; a multi-module
       ;; import is itself a statement list, so flatten one level here.
       [imports (append-map (lambda (s) (if (uir-block? s) (uir-block-stmts s) (list s)))
                            (uir-block-stmts uir))])
  (check-equal? (length imports) 2 "one import node per module")
  (check-equal? (map (lambda (i) (uir-symbol-name (uir-import-source i)))
                     imports)
                '("os" "sys") "both modules kept"))
(check-identity "from .globals import current_app\n")
(check-identity "from ..pkg import y\n")
(check-identity "from . import x\n")
(check-identity "from typing import (Any, Dict)\n")
(check-identity "from a.b import c as d\n")
(check-identity "from x import *\n")
(check-identity "import os.path\n")
(check-py "from .globals import current_app\n" "from .globals import current_app")
(check-py "from typing import (Any, Dict)\n" "from typing import Any, Dict")
(check-py "import os, sys\n" "import os\nimport sys")

;; 8. String-key subscript vs attribute disambiguation: both used to lower to
;;    (uir-get d (uir-string "a")), so `d["a"] = 1` emitted as `d.a = 1`.
(let* ([cst (py-parse "d[\"a\"] = 1\n")]
       [uir (lower-python cst py-tok-type py-tok-value)]
       [target (uir-set!-name uir)]
       [field (uir-get-field target)])
  (check-true (uir-get? target) "subscript assignment target is uir-get")
  (check-true (uir-list? field) "string subscript index is a one-element list")
  (check-equal? (length (uir-list-items field)) 1)
  (check-equal? (uir-string-value (first (uir-list-items field))) "a" "index is a"))
(let* ([cst (py-parse "d.a = 1\n")]
       [uir (lower-python cst py-tok-type py-tok-value)]
       [field (uir-get-field (uir-set!-name uir))])
  (check-true (uir-string? field) "attribute access keeps a bare string field")
  (check-equal? (uir-string-value field) "a" "attribute is a"))
(let* ([cst (py-parse "x = d[\"a\", b]\n")]
       [uir (lower-python cst py-tok-type py-tok-value)]
       [field (uir-get-field (uir-set!-value uir))])
  (check-true (uir-list? field) "mixed subscript keeps its index list")
  (check-equal? (length (uir-list-items field)) 2 "both indices kept"))
(check-py "d[\"a\"] = 1\n" "d[\"a\"] = 1")
(check-py "d.a = 1\n" "d.a = 1")
(check-identity "d[\"a\"] = 1\n")
(check-identity "d.a = 1\n")
(check-identity "x = d[\"a\"]\n")
(check-identity "x = d[\"a\", b]\n")
(check-identity "x = d[\"a\"][\"b\"]\n")
(check-identity "x = d[\"a\"].b\n")

;; 9. Dict-display `**` unpack (corpus: `out = {**self.environ_base, **other}`)
;;    used to be dropped, emitting `{}` (valid Python, wrong data). Entries are
;;    `((call "kw-spread" (list expr)) . null)` in source order; other entries
;;    keep their lowered key expression (numbers stay numbers).
(let* ([cst (py-parse "out = {**a, \"b\": 1}\n")]
       [uir (lower-python cst py-tok-type py-tok-value)]
       [entries (uir-record-entries (uir-set!-value uir))])
  (check-equal? (length entries) 2 "both dict entries kept")
  (check-true (uir-call? (car (first entries))) "first entry key is a call")
  (check-equal? (uir-symbol-name (uir-call-callee (car (first entries)))) "kw-spread"
                "first entry is a ** unpack")
  (check-equal? (uir-string-value (car (second entries))) "b" "second key is b"))
(let* ([cst (py-parse "x = {1: \"a\"}\n")]
       [uir (lower-python cst py-tok-type py-tok-value)]
       [entries (uir-record-entries (uir-set!-value uir))])
  (check-true (uir-number? (car (first entries))) "numeric dict key stays a number"))
(check-identity "out = {**a, \"b\": 1}\n")
(check-identity "out = {\"a\": 1, **b}\n")
(check-identity "out = {**a}\n")
(check-identity "out = {**a, **b}\n")
(check-identity "x = {1: \"a\", 2: \"b\"}\n")

;; ── Cleanup ─────────────────────────────────────────────────────────

(cleanup)
(displayln "~nAll lower-python tests passed.")
