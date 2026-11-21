#lang racket

(require rackunit
         racklr/tree
         racklr/gen-test)

;; ── Load the JavaScript parser ──────────────────────────────────────

(define-values (js-parse js-tokenize tok-type tok-value)
  (gen-and-load "../grammars/javascript/JavaScriptParser.g4"))

;; ── Tokenizer Tests ──────────────────────────────────────────────────

;; Keywords
(let ([tks (js-tokenize "var")])
  (check-equal? (length tks) 2)
  (check-equal? (tok-type (first tks)) 'Var))

(let ([tks (js-tokenize "function")])
  (check-equal? (length tks) 2)
  (check-equal? (tok-type (first tks)) 'Function_))

(let ([tks (js-tokenize "return")])
  (check-equal? (length tks) 2)
  (check-equal? (tok-type (first tks)) 'Return))

(let ([tks (js-tokenize "if")])
  (check-equal? (length tks) 2)
  (check-equal? (tok-type (first tks)) 'If))

(let ([tks (js-tokenize "else")])
  (check-equal? (length tks) 2)
  (check-equal? (tok-type (first tks)) 'Else))

(let ([tks (js-tokenize "while")])
  (check-equal? (length tks) 2)
  (check-equal? (tok-type (first tks)) 'While))

(let ([tks (js-tokenize "for")])
  (check-equal? (length tks) 2)
  (check-equal? (tok-type (first tks)) 'For))

(let ([tks (js-tokenize "true")])
  (check-equal? (length tks) 2)
  (check-equal? (tok-type (first tks)) 'BooleanLiteral))

(let ([tks (js-tokenize "false")])
  (check-equal? (length tks) 2)
  (check-equal? (tok-type (first tks)) 'BooleanLiteral))

(let ([tks (js-tokenize "null")])
  (check-equal? (length tks) 2)
  (check-equal? (tok-type (first tks)) 'NullLiteral))

(let ([tks (js-tokenize "new")])
  (check-equal? (length tks) 2)
  (check-equal? (tok-type (first tks)) 'New))

(let ([tks (js-tokenize "this")])
  (check-equal? (length tks) 2)
  (check-equal? (tok-type (first tks)) 'This))

(let ([tks (js-tokenize "class")])
  (check-equal? (length tks) 2)
  (check-equal? (tok-type (first tks)) 'Class))

;; Names/Identifiers
(let ([tks (js-tokenize "foo")])
  (check-equal? (length tks) 2)
  (check-equal? (tok-type (first tks)) 'Identifier)
  (check-equal? (tok-value (first tks)) "foo"))

;; Numbers
(let ([tks (js-tokenize "42")])
  (check-equal? (length tks) 2)
  (check-equal? (tok-type (first tks)) 'DecimalLiteral))

;; Strings
(let ([tks (js-tokenize "'hello'")])
  (check-equal? (length tks) 2)
  (check-equal? (tok-type (first tks)) 'StringLiteral))

;; Operators
(let ([tks (js-tokenize "=")])
  (check-equal? (length tks) 2)
  (check-equal? (tok-type (first tks)) 'Assign))

(let ([tks (js-tokenize "+")])
  (check-equal? (length tks) 2)
  (check-equal? (tok-type (first tks)) 'Plus))

(let ([tks (js-tokenize "==")])
  (check-equal? (length tks) 2)
  (check-equal? (tok-type (first tks)) 'Equals_))

(let ([tks (js-tokenize "===")])
  (check-equal? (length tks) 2)
  (check-equal? (tok-type (first tks)) 'IdentityEquals))

;; Multiple tokens
(let ([tks (js-tokenize "var x = 1;")])
  (check-equal? (length tks) 6)
  (check-equal? (tok-type (first tks)) 'Var)
  (check-equal? (tok-type (second tks)) 'Identifier)
  (check-equal? (tok-type (third tks)) 'Assign)
  (check-equal? (tok-type (fourth tks)) 'DecimalLiteral)
  (check-equal? (tok-type (fifth tks)) 'SemiColon)
  (check-equal? (tok-type (sixth tks)) 'EOF))

;; ── Parser Tests ─────────────────────────────────────────────────────

;; CST structure: program -> sourceElements (uses +/star, children may be lists)
(define (cst-contains-tag? cst tag)
  (let loop ([node cst])
    (cond [(cst-node? node)
           (or (eq? (cst-node-tag node) tag)
               (for/or ([c (cst-node-children node)])
                 (loop c)))]
          [(pair? node)
           (for/or ([c node]) (loop c))]
          [else #f])))

;; Variable declaration
(let ([cst (js-parse "var x = 1;")])
  (check-true (cst-node? cst))
  (check-equal? (cst-node-tag cst) 'program))

;; Function declaration
(let ([cst (js-parse "function foo() { return 42; }")])
  (check-true (cst-node? cst))
  (check-equal? (cst-node-tag cst) 'program))

;; If-else statement
(let ([cst (js-parse "if (true) { x = 1; } else { x = 2; }")])
  (check-true (cst-node? cst))
  (check-equal? (cst-node-tag cst) 'program))

;; Expression statement
(let ([cst (js-parse "1 + 2;")])
  (check-true (cst-node? cst))
  (check-equal? (cst-node-tag cst) 'program))

;; Function call
(let ([cst (js-parse "foo();")])
  (check-true (cst-node? cst))
  (check-equal? (cst-node-tag cst) 'program))

;; Assignment as expression statement
(let ([cst (js-parse "x = 1 + 2;")])
  (check-true (cst-node? cst))
  (check-equal? (cst-node-tag cst) 'program))

;; Variable with member expression
(let ([cst (js-parse "var z = a.b.c;")])
  (check-true (cst-node? cst))
  (check-equal? (cst-node-tag cst) 'program))

;; Variable with binary expression
(let ([cst (js-parse "var z = x + y * z;")])
  (check-true (cst-node? cst))
  (check-equal? (cst-node-tag cst) 'program))

;; Return statement
(let ([cst (js-parse "function foo() { return; }")])
  (check-true (cst-node? cst))
  (check-equal? (cst-node-tag cst) 'program))

;; While loop
(let ([cst (js-parse "while (true) { x = x - 1; }")])
  (check-true (cst-node? cst))
  (check-equal? (cst-node-tag cst) 'program))

;; ── Rejection Tests ──────────────────────────────────────────────────

;; parse returns #f for invalid input
(check-reject js-parse "var")
(check-reject js-parse "@#$%")
(check-reject js-parse "var x = ")

;; ── Issue #122: real-world constructs from express/axios ────────────
;; The cleaned grammar is generated parser-only (no semantic predicates), so
;; each construct is expressed with ordered alternatives instead. These are
;; the gaps the corpus found: class members, optional calls, logical
;; assignment, nested template literals and private/import-meta names.

(for ([src (in-list
            (list "class A { m() {} }"
                  "class A { x = 1; static y = 2; static { init(); } }"
                  "class A { constructor() { super(); } get x() { return 1; } }"
                  "a?.();"
                  "a.b?.().c;"
                  "a?.b?.c;"
                  "a ||= b; a &&= c;"
                  "const o = `x${`y${z}`}`;"
                  "const u = import.meta.url;"
                  "if (#x in obj) f();"
                  "for (const { a } of items) { g(a); }"
                  "for (const [k, v] of entries) { g(k, v); }"
                  "const { a, b: c, d = 1, ...rest } = o;"
                  "const [x, , y = 2, ...z] = xs;"))])
  (check-true (and (js-parse src) #t) (format "parses ~s" src)))

;; Clean up temp files
(cleanup)
(displayln "All JavaScript parser tests passed.")
