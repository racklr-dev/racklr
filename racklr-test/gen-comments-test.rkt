#lang racket

;; Comment preservation in the generated lexer/parser (JSDoc blocker).
;;
;; With #:preserve-comments? the tokenizer keeps channel(HIDDEN) tokens
;; (comments) in the token stream, and the generated parse entry filters them
;; out before matching so the CST is unchanged. This unblocks JSDoc @effects
;; extraction on the TS/JS/JSX front ends.

(require rackunit
         racklr/tree
         racklr/gen-test)

;; ── Load the TypeScript parser, with and without comment preservation ──

(define-values (ts-parse ts-tokenize ts-tok-type ts-tok-value)
  (gen-and-load "../grammars/typescript/TypeScriptParser.g4"))

(define-values (tsc-parse tsc-tokenize tsc-tok-type tsc-tok-value)
  (gen-and-load-comments "../grammars/typescript/TypeScriptParser.g4"))

;; ── CST shape (tag-only, positions stripped) ──────────────────────────

(define (cst-shape t tk-type tk-value)
  (cond
    [(cst-node? t)
     (list 'node (cst-node-tag t)
           (map (λ (c) (cst-shape c tk-type tk-value)) (cst-node-children t)))]
    [(cst-leaf? t) (list 'leaf (cst-leaf-tag t) (cst-leaf-text t))]
    [(list? t) (map (λ (c) (cst-shape c tk-type tk-value)) t)]
    [(eq? t 'none) 'none]
    [else (list 'tok (tk-type t) (tk-value t))]))

;; ── Default path still skips comments ─────────────────────────────────

(let ([tks (ts-tokenize "// c\nlet x = 1;")])
  (check-equal? (map ts-tok-type tks)
                '(Let Identifier Assign DecimalLiteral SemiColon EOF)))

;; ── Preserve path keeps comment tokens ────────────────────────────────

(let ([tks (tsc-tokenize "// c\nlet x = 1;")])
  (check-equal? (map tsc-tok-type tks)
                '(SingleLineComment Let Identifier Assign DecimalLiteral SemiColon EOF))
  (check-equal? (tsc-tok-value (first tks)) "// c"))

(let ([tks (tsc-tokenize "/* hi */ let x = 1;")])
  (check-equal? (map tsc-tok-type tks)
                '(MultiLineComment Let Identifier Assign DecimalLiteral SemiColon EOF))
  (check-equal? (tsc-tok-value (first tks)) "/* hi */"))

;; ── Parsing is unchanged with comments present ────────────────────────

(let ([plain (tsc-parse "let x = 1;")]
      [with-comment (tsc-parse "// c\nlet x = 1;")]
      [with-block (tsc-parse "/* c */ let x = 1;")])
  (check-true (any-tree? plain))
  (check-equal? (cst-shape with-comment tsc-tok-type tsc-tok-value)
                (cst-shape plain tsc-tok-type tsc-tok-value))
  (check-equal? (cst-shape with-block tsc-tok-type tsc-tok-value)
                (cst-shape plain tsc-tok-type tsc-tok-value)))

(let ([plain (tsc-parse "function f() { return 1; }")]
      [with-comment (tsc-parse "/** @effects { llm: { max: 5 } } */\nfunction f() { return 1; }")])
  (check-true (any-tree? plain))
  (check-true (any-tree? with-comment))
  (check-equal? (cst-shape with-comment tsc-tok-type tsc-tok-value)
                (cst-shape plain tsc-tok-type tsc-tok-value)))

;; ── Cleanup ───────────────────────────────────────────────────────────

(cleanup)
(displayln "All gen-comments tests passed.")
