#lang racket

(require rackunit
         racklr/tree
         racklr/gen-test)

;; ── Load the Python3 parser ──────────────────────────────────────────

(define-values (py-parse py-tokenize tok-type tok-value)
  (gen-and-load-py "../grammars/python3/Python3Parser.g4"))

;; ── Tokenizer Tests ──────────────────────────────────────────────────

;; Keywords
(let ([tks (py-tokenize "def\n")])
  (check-equal? (length tks) 3 "def should produce 3 tokens")
  (check-equal? (tok-type (first tks)) 'DEF)
  (check-equal? (tok-value (first tks)) "def")
  (check-equal? (tok-type (second tks)) 'NEWLINE)
  (check-equal? (tok-type (third tks)) 'EOF))

(let ([tks (py-tokenize "class\n")])
  (check-equal? (length tks) 3)
  (check-equal? (tok-type (first tks)) 'CLASS))

(let ([tks (py-tokenize "if\n")])
  (check-equal? (length tks) 3)
  (check-equal? (tok-type (first tks)) 'IF))

(let ([tks (py-tokenize "while\n")])
  (check-equal? (length tks) 3)
  (check-equal? (tok-type (first tks)) 'WHILE))

(let ([tks (py-tokenize "for\n")])
  (check-equal? (length tks) 3)
  (check-equal? (tok-type (first tks)) 'FOR))

(let ([tks (py-tokenize "return\n")])
  (check-equal? (length tks) 3)
  (check-equal? (tok-type (first tks)) 'RETURN))

(let ([tks (py-tokenize "import\n")])
  (check-equal? (length tks) 3)
  (check-equal? (tok-type (first tks)) 'IMPORT))

(let ([tks (py-tokenize "True\n")])
  (check-equal? (length tks) 3)
  (check-equal? (tok-type (first tks)) 'TRUE))

(let ([tks (py-tokenize "None\n")])
  (check-equal? (length tks) 3)
  (check-equal? (tok-type (first tks)) 'NONE))

;; Numbers
(let ([tks (py-tokenize "42\n")])
  (check-equal? (length tks) 3)
  (check-equal? (tok-type (first tks)) 'NUMBER))

(let ([tks (py-tokenize "3.14\n")])
  (check-equal? (length tks) 3)
  (check-equal? (tok-type (first tks)) 'NUMBER)
  (check-equal? (tok-value (first tks)) "3.14"))

;; Float exponents must lex as a single NUMBER. EXPONENT_FLOAT combines
;; POINT_FLOAT with EXPONENT; a first-match malt stopped at the "1.5" prefix.
(let ([tks (py-tokenize "1.5e10\n")])
  (check-equal? (length tks) 3)
  (check-equal? (tok-type (first tks)) 'NUMBER)
  (check-equal? (tok-value (first tks)) "1.5e10"))

(let ([tks (py-tokenize "2.5e-3\n")])
  (check-equal? (length tks) 3)
  (check-equal? (tok-type (first tks)) 'NUMBER)
  (check-equal? (tok-value (first tks)) "2.5e-3"))

;; Names
(let ([tks (py-tokenize "foo\n")])
  (check-equal? (length tks) 3)
  (check-equal? (tok-type (first tks)) 'NAME)
  (check-equal? (tok-value (first tks)) "foo"))

;; Strings
(let ([tks (py-tokenize "'hello'\n")])
  (check-equal? (length tks) 3)
  (check-equal? (tok-type (first tks)) 'STRING))

;; String containing the letter f: the \f form-feed escape in the negated
;; string class must not leak a literal f into the match set.
(let ([tks (py-tokenize "\"the quick brown fox\"\n")])
  (check-equal? (length tks) 3 "f-containing string should be one STRING token")
  (check-equal? (tok-type (first tks)) 'STRING))

;; Two-char prefixes rf / br must lex as one STRING: the optional-prefix
;; alternatives [rR] and [rR][fF] both have to be explored.
(let ([tks (py-tokenize "rf\"raw f\"\n")])
  (check-equal? (length tks) 3 "rf prefix should be one STRING token")
  (check-equal? (tok-type (first tks)) 'STRING)
  (check-equal? (tok-value (first tks)) "rf\"raw f\""))

(let ([tks (py-tokenize "br'bytes'\n")])
  (check-equal? (length tks) 3 "br prefix should be one STRING token")
  (check-equal? (tok-type (first tks)) 'STRING)
  (check-equal? (tok-value (first tks)) "br'bytes'"))

;; Operators
(let ([tks (py-tokenize "=\n")])
  (check-equal? (length tks) 3)
  (check-equal? (tok-type (first tks)) 'ASSIGN))

(let ([tks (py-tokenize "<\n")])
  (check-equal? (length tks) 3)
  (check-equal? (tok-type (first tks)) 'LESS_THAN))

(let ([tks (py-tokenize ">\n")])
  (check-equal? (length tks) 3)
  (check-equal? (tok-type (first tks)) 'GREATER_THAN))

;; Multiple tokens on one line
(let ([tks (py-tokenize "x = 1\n")])
  (check-equal? (length tks) 5)
  (check-equal? (tok-type (first tks)) 'NAME)
  (check-equal? (tok-value (first tks)) "x")
  (check-equal? (tok-type (second tks)) 'ASSIGN)
  (check-equal? (tok-type (third tks)) 'NUMBER)
  (check-equal? (tok-value (third tks)) "1")
  (check-equal? (tok-type (fourth tks)) 'NEWLINE)
  (check-equal? (tok-type (fifth tks)) 'EOF))

;; ── Parser Tests ─────────────────────────────────────────────────────

;; CST structure: single_input -> simple_stmts -> simple_stmt -> group -> <statement>
;; Check that a specific tag exists somewhere in the parse tree
(define (cst-contains-tag? cst tag)
  (let loop ([node cst])
    (cond [(any-tree? node)
           (or (eq? (any-tree-tag node) tag)
               (for/or ([c (any-tree-children node)])
                 (loop c)))]
          [else #f])))

;; Simple keyword statements
(let ([cst (py-parse "pass\n")])
  (check-true (any-tree? cst))
  (check-equal? (any-tree-tag cst) 'single_input)
  (check-true (cst-contains-tag? cst 'pass_stmt)))

(let ([cst (py-parse "break\n")])
  (check-true (cst-contains-tag? cst 'break_stmt)))

(let ([cst (py-parse "continue\n")])
  (check-true (cst-contains-tag? cst 'continue_stmt)))

(let ([cst (py-parse "del x\n")])
  (check-true (cst-contains-tag? cst 'del_stmt)))

(let ([cst (py-parse "return\n")])
  (check-true (cst-contains-tag? cst 'return_stmt)))

(let ([cst (py-parse "return 1\n")])
  (check-true (cst-contains-tag? cst 'return_stmt)))

(let ([cst (py-parse "import os\n")])
  (check-true (cst-contains-tag? cst 'import_name)))

(let ([cst (py-parse "from os import path\n")])
  (check-true (cst-contains-tag? cst 'import_from)))

(let ([cst (py-parse "global x\n")])
  (check-true (cst-contains-tag? cst 'global_stmt)))

(let ([cst (py-parse "assert True\n")])
  (check-true (cst-contains-tag? cst 'assert_stmt)))

;; Simple assignment
(let ([cst (py-parse "x = 1\n")])
  (check-true (any-tree? cst))
  (check-equal? (any-tree-tag cst) 'single_input)
  (check-true (cst-contains-tag? cst 'expr_stmt)))

;; Multiple assignments
(let ([cst (py-parse "x = y = 1\n")])
  (check-true (any-tree? cst))
  (check-equal? (any-tree-tag cst) 'single_input)
  (check-true (cst-contains-tag? cst 'expr_stmt)))

;; ── Compound Statement Tests (require INDENT/DEDENT) ─────────────────

;; Function definition
(let ([cst (py-parse "def foo():\n    pass\n\n")])
  (check-true (any-tree? cst))
  (check-equal? (any-tree-tag cst) 'single_input)
  (check-true (cst-contains-tag? cst 'funcdef)))

;; If statement
(let ([cst (py-parse "if x:\n    pass\n\n")])
  (check-true (any-tree? cst))
  (check-equal? (any-tree-tag cst) 'single_input)
  (check-true (cst-contains-tag? cst 'if_stmt)))

;; While loop
(let ([cst (py-parse "while x:\n    pass\n\n")])
  (check-true (any-tree? cst))
  (check-equal? (any-tree-tag cst) 'single_input)
  (check-true (cst-contains-tag? cst 'while_stmt)))

;; For loop
(let ([cst (py-parse "for x in y:\n    pass\n\n")])
  (check-true (any-tree? cst))
  (check-equal? (any-tree-tag cst) 'single_input)
  (check-true (cst-contains-tag? cst 'for_stmt)))

;; Class definition
(let ([cst (py-parse "class Foo:\n    pass\n\n")])
  (check-true (any-tree? cst))
  (check-equal? (any-tree-tag cst) 'single_input)
  (check-true (cst-contains-tag? cst 'classdef)))

;; ── Rejection Tests ──────────────────────────────────────────────────

;; parse returns #f for invalid input (no NEWLINE)
(check-reject py-parse "x = 1")

;; parse returns #f for garbage
(check-reject py-parse "@#$%\n")

;; EOF without newline should fail single_input
(check-reject py-parse "pass")

;; Clean up temp files
(cleanup)
