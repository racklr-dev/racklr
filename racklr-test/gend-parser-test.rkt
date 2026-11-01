#lang racket

(require rackunit
         rackunit/text-ui
         racket/list
         racklr/tree
         racklr/g4-parse
         racklr/gend-parser
         racklr/gend-parser/lexer
         racklr/gen-test)

;; ── Section 1: Generator produces valid Racket syntax ─────────────

;; Minimal grammar: single parser rule matching a string literal
;; Runs gen-and-load which verifies the generated module compiles and loads.
(let* ([grammar-cst (parse-g4 "grammar T; r : 'x' ;")]
       [module-src (generate-parser-module grammar-cst)])
  (check-true (string? module-src))
  (check-true (string-contains? module-src "parse-r"))
  (check-true (string-contains? module-src "tokenize"))
  (check-true (string-contains? module-src "#lang racket")))

;; Grammar with lexer rule
(let* ([grammar-cst (parse-g4 "grammar T; X : 'y' ; r : X ;")]
       [module-src (generate-parser-module grammar-cst)])
  (check-true (string-contains? module-src "X-match"))
  (check-true (string-contains? module-src "parse-r")))

;; ── Section 1b: Skipped multi-line tokens advance the position ─────
;; A skip/channel(HIDDEN) lexer rule can match text with embedded newlines — a
;; block comment, for example. The tokenizer must advance line/column across a
;; skipped token; otherwise the next token is reported on the comment's
;; opening line. Synthetic grammar, so this section needs no vendored tree.

(let* ([grammar-cst
        (parse-g4 (string-append "grammar T; "
                                 "COMMENT : '/*' .*? '*/' -> skip; "
                                 "WS : [ \\t\\r\\n]+ -> skip; "
                                 "ID : [a-z]+ ; "
                                 "r : ID+ ;"))]
       [module-src (generate-parser-module grammar-cst)]
       [tmp-path (build-path (current-directory) "gen-tmp-skip-pos.rkt")])
  (display-to-file module-src tmp-path #:exists 'replace)
  (define tk (dynamic-require tmp-path 'tokenize))
  (define tok-type (dynamic-require tmp-path 'token-type))
  (define tok-start (dynamic-require tmp-path 'token-start))
  (define ids (filter (λ (t) (eq? (tok-type t) 'ID))
                      (tk "aa /* multi\nline */ bb")))
  (check-equal? (map (λ (t) (source-pos-line (tok-start t))) ids) '(1 2)
                "the token after a skipped multi-line comment is on line 2")
  (check-equal? (list (source-pos-col (tok-start (second ids)))
                      (source-pos-offset (tok-start (second ids))))
                '(9 20))
  (delete-file tmp-path))

;; ── Section 2: Generated tokenizer works ──────────────────────────

(let-values ([(parse tok tok-type tok-val)
              (gen-and-load "../grammars-v4/json/JSON.g4")])
  ;; Tokenize simple tokens
  (let ([tks (tok "true")])
    (check-equal? (length tks) 2)
    (check-equal? (tok-type (first tks)) (string->symbol "true"))
    (check-equal? (tok-val (first tks)) "true")
    (check-equal? (tok-type (second tks)) 'EOF))

  (let ([tks (tok "42")])
    (check-equal? (length tks) 2)
    (check-equal? (tok-type (first tks)) 'NUMBER)
    (check-equal? (tok-val (first tks)) "42"))

  (let ([tks (tok "{}")])
    (check-equal? (length tks) 3)
    (check-equal? (tok-type (first tks)) (string->symbol "{"))
    (check-equal? (tok-type (second tks)) (string->symbol "}")))

  ;; Escaped codepoint classes in the grammar: a safe JSON string must accept
  ;; every codepoint except '"', '\\', and U+0000..U+001F. These were the
  ;; characters a stale generated parser rejected.
  (for ([s (in-list '("\"roundtrip\"" "\"1\"" "\"ABC\"" "\"u\"" "\"a\\nb\"" "\"\\u0041\""))])
    (define tks (tok s))
    (check-equal? (tok-type (first tks)) 'STRING (format "tokenizes JSON string ~s" s)))
  (check-exn exn:fail?
             (λ () (tok (string-append "\"a" (string (integer->char 0)) "b\"")))
             "raw control characters are not valid JSON string content"))

;; ── Section 3: Generated parser parses correctly ──────────────────

;; Test with a one-rule grammar matching a single literal
(let-values ([(parse tok tok-type tok-val)
              (gen-and-load "../grammars-v4/json/JSON.g4")])
  (check-true (any-tree? (parse "42")))
  (check-true (any-tree? (parse "\"hello\"")))
  (check-true (any-tree? (parse "true")))
  (check-true (any-tree? (parse "null")))
  (check-true (any-tree? (parse "{}")))
  (check-true (any-tree? (parse "[]")))
  (check-true (any-tree? (parse "{\"a\":1}"))))

;; ── Section 4: Lexer-only grammar (synthetic) ─────────────────────
;; Test that a grammar with only lexer rules and implicit parser literals works

(let-values ([(parse tok tok-type tok-val)
              (gen-and-load "../grammars-v4/lisp/lisp.g4")])
  ;; Just verify the module loads
  (check-true (procedure? parse))
  (check-true (procedure? tok)))

;; ── Section 5: CST structure verification ─────────────────────────
;; Note: generated parser embeds raw tokens as children, not leaf nodes.
;; Position tracking for tokens will be verified when racklr-v85 is fixed.

(let-values ([(parse tok tok-type tok-val)
              (gen-and-load "../grammars-v4/json/JSON.g4")])
  (let ([cst (parse "42")])
    (check-equal? (cst-node-tag cst) 'json)
    ;; Verify the CST has the right shape: json -> (value ...) (EOF)
    (check-true (>= (length (cst-node-children cst)) 2))))

;; ── Section 6: Alternation ────────────────────────────────────────
;; Grammar with alternatives: 'a' | 'b' | 'c'

(let* ([grammar-cst (parse-g4 "grammar T; r : 'a' | 'b' | 'c' ;")]
       [module-src (generate-parser-module grammar-cst)]
       [tmp-path (build-path (current-directory) "gen-tmp-alt.rkt")])
  (display-to-file module-src tmp-path #:exists 'replace)
  (define p (dynamic-require tmp-path 'parse))
  (define tk (dynamic-require tmp-path 'tokenize))
  (check-true (any-tree? (p "a")))
  (check-true (any-tree? (p "b")))
  (check-true (any-tree? (p "c")))
  (check-reject p "d")
  (delete-file tmp-path))

;; ── Section 7: Optional elements ──────────────────────────────────

(let* ([grammar-cst (parse-g4 "grammar T; r : 'x'? 'y' ;")]
       [module-src (generate-parser-module grammar-cst)]
       [tmp-path (build-path (current-directory) "gen-tmp-opt.rkt")])
  (display-to-file module-src tmp-path #:exists 'replace)
  (define p (dynamic-require tmp-path 'parse))
  (check-true (any-tree? (p "x y")))
  (check-true (any-tree? (p "y")))
  (check-reject p "x")
  (check-reject p "")
  (delete-file tmp-path))

;; ── Section 8: Star repetition ────────────────────────────────────

(let* ([grammar-cst (parse-g4 "grammar T; r : 'x'* ;")]
       [module-src (generate-parser-module grammar-cst)]
       [tmp-path (build-path (current-directory) "gen-tmp-star.rkt")])
  (display-to-file module-src tmp-path #:exists 'replace)
  (define p (dynamic-require tmp-path 'parse))
  (check-true (any-tree? (p "")))
  (check-true (any-tree? (p "x")))
  (check-true (any-tree? (p "x x x")))
  (check-reject p "y")
  (delete-file tmp-path))

;; ── Section 9: Plus repetition ────────────────────────────────────

(let* ([grammar-cst (parse-g4 "grammar T; r : 'x'+ ;")]
       [module-src (generate-parser-module grammar-cst)]
       [tmp-path (build-path (current-directory) "gen-tmp-plus.rkt")])
  (display-to-file module-src tmp-path #:exists 'replace)
  (define p (dynamic-require tmp-path 'parse))
  (check-true (any-tree? (p "x")))
  (check-true (any-tree? (p "x x x")))
  (check-reject p "")
  (check-reject p "y")
  (delete-file tmp-path))

;; ── Section 10: Grouped alternatives ──────────────────────────────

(let* ([grammar-cst (parse-g4 "grammar T; r : 'a' ('b' | 'c') 'd' ;")]
       [module-src (generate-parser-module grammar-cst)]
       [tmp-path (build-path (current-directory) "gen-tmp-group.rkt")])
  (display-to-file module-src tmp-path #:exists 'replace)
  (define p (dynamic-require tmp-path 'parse))
  (check-true (any-tree? (p "a b d")))
  (check-true (any-tree? (p "a c d")))
  (check-reject p "a d")
  (check-reject p "a x d")
  (delete-file tmp-path))

;; ── Section 11: Lexer rules with skip command ─────────────────────

(let* ([grammar-cst
        (parse-g4 "grammar T; WS : [ \\t\\n\\r]+ -> skip ; r : 'x' ;")]
       [module-src (generate-parser-module grammar-cst)]
       [tmp-path (build-path (current-directory) "gen-tmp-skip.rkt")])
  (display-to-file module-src tmp-path #:exists 'replace)
  (define p (dynamic-require tmp-path 'parse))
  (check-true (any-tree? (p "x")))
  (check-true (any-tree? (p "  x  ")))
  (check-true (any-tree? (p "\nx\n")))
  (delete-file tmp-path))

;; ── Section 12: Error handling ────────────────────────────────────

(let-values ([(parse tok tok-type tok-val)
              (gen-and-load "../grammars-v4/json/JSON.g4")])
  ;; Valid parse should return a tree
  (check-true (any-tree? (parse "42")))
  ;; Garbage should be rejected
  (check-reject parse "not-json"))

;; ── Section 13: Tokenizer whitespace handling ─────────────────────

(let-values ([(parse tok tok-type tok-val)
              (gen-and-load "../grammars-v4/json/JSON.g4")])
  ;; Tokenize with whitespace — should skip it
  (let ([tks (tok "  42  ")])
    (check-equal? (length tks) 2)            ;; NUMBER + EOF
    (check-equal? (tok-type (first tks)) 'NUMBER)
    (check-equal? (tok-val (first tks)) "42")))

;; ── Section 14: Multiple parser rules ─────────────────────────────

(let* ([grammar-cst
        (parse-g4 "grammar T; b : a 'y' ; a : 'x' ;")]
       [module-src (generate-parser-module grammar-cst)]
       [tmp-path (build-path (current-directory) "gen-tmp-multi.rkt")])
  (display-to-file module-src tmp-path #:exists 'replace)
  (define p (dynamic-require tmp-path 'parse))
  ;; Entry point 'b' requires 'x' then 'y' (via rule ref to 'a')
  (check-true (any-tree? (p "x y")))
  (check-reject p "x")
  (check-reject p "y")
  (delete-file tmp-path))

;; ── Section 15: Left-recursion detection ────────────────────────────

;; Left-recursive grammars produce a WARNING (not an error) and filter
;; out the left-recursive alternatives. The generated parser accepts
;; only the non-left-recursive subset of the language.
(let* ([lr-cst (parse-g4 "grammar T; expr : expr '+' expr | NUMBER ;")]
       [warning-port (open-output-string)]
       [module-src (parameterize ([current-error-port warning-port])
                     (generate-parser-module lr-cst))])
  (define warning-str (get-output-string warning-port))
  (check-true (string-contains? warning-str "Left-recursive")
              "left-recursive grammar prints a stderr warning")
  (check-true (string-contains? warning-str "expr")
              "warning names the left-recursive rule")
  (check-true (string-contains? warning-str "eliminating")
              "warning says left-recursion is being eliminated via iteration")
  (check-true (string? module-src)
              "module source still generated (with left-recursion elimination)"))

(let* ([ok-cst (parse-g4 "grammar T; expr : NUMBER '+' expr | NUMBER ;")]
       [module-src (generate-parser-module ok-cst)])
  (check-true (string? module-src))
  (check-true (string-contains? module-src "parse-expr")))

;; ── Section 16: Left-recursion elimination ────────────────────────
;; Grammars with left-recursive rules should parse correctly,
;; not just filter out the recursive alternatives.

;; Test: simple binary left-recursive rule with explicit lexer rule
(let* ([lr-cst (parse-g4 "grammar T; INT : [0-9]+ ; expr : expr '+' expr | INT ;")]
       [module-src (parameterize ([current-error-port (open-output-string)])
                     (generate-parser-module lr-cst))]
       [tmp-path (build-path (current-directory) "gen-tmp-lr.rkt")])
  (display-to-file module-src tmp-path #:exists 'replace)
  (define p (dynamic-require tmp-path 'parse))
  ;; Single INT should still work (base case)
  (check-true (any-tree? (p "1"))
              "single INT parses (base case)")
  ;; Binary expression should parse
  (check-true (any-tree? (p "1 + 2"))
              "1 + 2 parses (left-recursive binary)")
  ;; Multiple binary expressions should parse with left-associativity
  (check-true (any-tree? (p "1 + 2 + 3"))
              "1 + 2 + 3 parses (left-recursive chain)")
  ;; Verify CST shape: (1 + 2) should give left-assoc nesting
  (let ([cst (p "1 + 2")])
    (check-equal? (cst-node-tag cst) 'expr
                  "CST tag is expr")
    ;; expr -> [left, +, right] where left is also expr
    (define kids (cst-node-children cst))
    (check-equal? (length kids) 3
                  "binary expr has 3 children (left, op, right)")
    (check-true (cst-node? (first kids))
                "left child is a node (sub-expr)"))
  (delete-file tmp-path))

;; Test: multiple operators (two binary left-recursive alternatives)
(let* ([lr-cst (parse-g4 "grammar T; INT : [0-9]+ ; expr : expr '+' expr | expr '-' expr | INT ;")]
       [module-src (parameterize ([current-error-port (open-output-string)])
                     (generate-parser-module lr-cst))]
       [tmp-path (build-path (current-directory) "gen-tmp-lr3.rkt")])
  (display-to-file module-src tmp-path #:exists 'replace)
  (define p (dynamic-require tmp-path 'parse))
  (check-true (any-tree? (p "42")))
  (check-true (any-tree? (p "1 + 2")))
  (check-true (any-tree? (p "3 - 1")))
  (check-true (any-tree? (p "1 + 2 - 3")))
  (delete-file tmp-path))

;; ── Escaped codepoint handling in the generator ───────────────────
;; The checked-in JSON parser is a stale generated file (tracked follow-up);
;; these tests pin the *generator* behavior so a future regeneration is correct.

(test-case
 "unescape-g4-literal decodes \\uXXXX but not an escaped backslash before u"
 (check-equal? (unescape-g4-literal "A\\u0041B") "AAB")
 (check-equal? (unescape-g4-literal "A\\\\u0041B") "A\\u0041B")
 (check-equal? (unescape-g4-literal "x\\ny") "x\ny"))

(let* ([grammar "grammar T; r : S ; S : [\\u0041-\\u005A]+ ;"]
       [module-src (generate-parser-module (parse-g4 grammar))]
       [tmp-path (build-path (current-directory) "gen-tmp-escaped-class.rkt")])
  (display-to-file module-src tmp-path #:exists 'replace)
  (define p (dynamic-require tmp-path 'parse))
  (check-true (any-tree? (p "ABC")) "\\u0041-\\u005A range matches uppercase")
  (check-reject p "abc")
  (check-reject p "1")
  (delete-file tmp-path))

(let* ([grammar "grammar T; r : A ; A : '\\u0041' ;"]
       [module-src (generate-parser-module (parse-g4 grammar))]
       [tmp-path (build-path (current-directory) "gen-tmp-escaped-literal.rkt")])
  (display-to-file module-src tmp-path #:exists 'replace)
  (define p (dynamic-require tmp-path 'parse))
  (check-true (any-tree? (p "A")) "\\u0041 literal unescapes to A")
  (check-reject p "\\u0041")
  (delete-file tmp-path))

;; ── Cleanup ───────────────────────────────────────────────────────

(cleanup)

(displayln "All gend-parser tests passed.")
