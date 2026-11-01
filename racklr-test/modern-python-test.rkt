#lang racket

;; Modern Python 3 syntax: the tracked grammars/python3 front end plus lowering
;; and emit for the three constructs the Django front-end catalogue flagged as
;; PARSE-FAIL:
;;   - assignment expressions / walrus     (PEP 572, `x := value`)
;;   - positional-only parameters          (PEP 570, `def f(a, b, /, c)`)
;;   - type alias statements               (PEP 695, `type Alias[T] = value`)
;;
;; The single_input entry point is known to be broken for compound statements
;; (see LOOP_PLAN issue #1), so parse through file_input, the entry the actual
;; lowering pipeline uses.
;;
;; Run: cd racklr-test && racket modern-python-test.rkt

(require rackunit
         rackunit/text-ui
         racklr/tree
         racklr/uir
         racklr/gen-test
         racklr/lower-python
         racklr/emit-python)

;; ── Load the tracked modern Python3 parser ──────────────────────────

(define-values (_py-parse py-tokenize py-tok-type py-tok-value)
  (gen-and-load-py "../grammars/python3/Python3Parser.g4"))

(define py-parse-file
  (let ([mod-path (*last-module-path*)])
    (λ (src)
      (define tks ((dynamic-require mod-path 'tokenize) src))
      (define parsed ((dynamic-require mod-path 'parse-file_input) tks 0))
      (and (pair? parsed) (cadr parsed)))))

(define (parse-ok? src)
  (and (py-parse-file src) #t))

(define (cst-has? cst tag)
  (cond [(not cst) #f]
        [(any-tree? cst)
         (or (eq? (any-tree-tag cst) tag)
             (ormap (λ (k) (cst-has? k tag))
                    (if (cst-node? cst) (cst-node-children cst) '())))]
        [(pair? cst) (ormap (λ (k) (cst-has? k tag)) cst)]
        [else #f]))

;; Walk any UIR value for a struct matching `pred`.
(define (uir-contains? node pred)
  (cond [(pred node) #t]
        [(pair? node) (or (uir-contains? (car node) pred)
                          (uir-contains? (cdr node) pred))]
        [(struct? node)
         (ormap (λ (f) (uir-contains? f pred))
                (cdr (vector->list (struct->vector node))))]
        [else #f]))

(define (lower-source src)
  (define cst (py-parse-file src))
  (check-true (any-tree? cst) (format "parse ~s" src))
  (lower-python cst py-tok-type py-tok-value))

;; Lower and emit, then require the emitted Python to parse again.
(define (roundtrip src)
  (define u (lower-source src))
  (define out (emit-python u))
  (check-true (parse-ok? (string-append out "\n"))
              (format "emitted Python re-parses: ~s -> ~s" src out))
  out)

(define modern-python-tests
  (test-suite
   "modern Python 3 grammar"

   ;; ── Lexing: `type` stays a NAME (soft keyword) ──────────────────
   (test-case
    "type is a soft keyword, not a reserved word"
    (check-equal? (map py-tok-type (py-tokenize "type\n")) '(NAME NEWLINE EOF)
                  "`type` tokenizes as NAME")
    (check-equal? (map py-tok-type (py-tokenize "isinstance\n")) '(NAME NEWLINE EOF)
                  "ordinary identifiers still tokenize as NAME"))

   ;; ── Grammar acceptance ──────────────────────────────────────────
   (test-case
    "type alias statements parse"
    (check-true (cst-has? (py-parse-file "type T = int | str\n") 'type_alias)
                "simple type alias parses")
    (check-true (cst-has? (py-parse-file "type Pair[T, U] = tuple[T, U]\n") 'type_params)
                "generic type alias parses with type parameters")
    (check-true (cst-has? (py-parse-file "type = 1\n") 'expr_stmt)
                "assignment to a variable named type still parses as expr_stmt")
    (check-true (cst-has? (py-parse-file "type(x)\n") 'expr_stmt)
                "call to a function named type still parses as expr_stmt")
    (check-false (parse-ok? "foo bar = 1\n")
                 "the alias rule requires the literal `type` keyword"))

   (test-case
    "walrus expressions parse"
    (check-true (cst-has? (py-parse-file "if (n := len(x)) > 0:\n    pass\n\n")
                          'namedexpr_test)
                "walrus in an if condition")
    (check-true (cst-has? (py-parse-file "while (chunk := f.read(8)):\n    pass\n\n")
                          'namedexpr_test)
                "walrus in a while condition")
    (check-true (cst-has? (py-parse-file "xs = [(y := f(i)) for i in data]\n")
                          'namedexpr_test)
                "walrus in a comprehension element")
    (check-true (cst-has? (py-parse-file "g(x := 1)\n") 'namedexpr_test)
                "walrus as a call argument"))

   (test-case
    "positional-only parameters parse"
    (check-true (cst-has? (py-parse-file "def f(a, b, /, c): pass\n\n") 'typedargslist)
                "positional-only parameters in def")
    (check-true (cst-has? (py-parse-file "def f(a, /, b, *, c, **kw): pass\n\n")
                          'typedargslist)
                "positional-only composes with keyword-only and **kwargs")
    (check-true (cst-has? (py-parse-file "f = lambda a, /, b: a\n") 'varargslist)
                "positional-only parameters in lambda"))

   ;; ── Lowering: type alias ────────────────────────────────────────
   (test-case
    "type alias lowers to uir-type-alias"
    (let* ([u (lower-source "type T = int | str\n")]
           [alias (first (uir-block-stmts u))])
      (check-true (uir-type-alias? alias))
      (check-equal? (uir-symbol-name (uir-type-alias-name alias)) "T" "alias name")
      (check-equal? (uir-type-alias-type-params alias) '() "no type parameters")
      (check-true (uir-call? (uir-type-alias-value alias)) "alias value is an expression")
      (check-equal? (uir-symbol-name (uir-call-callee (uir-type-alias-value alias))) "|"
                    "alias value keeps the union operator")))

   (test-case
    "generic type alias keeps its type parameters"
    (let* ([u (lower-source "type ListOrSet[T] = list[T] | set[T]\n")]
           [alias (first (uir-block-stmts u))])
      (check-equal? (map uir-symbol-name (uir-type-alias-type-params alias)) '("T"))))

   ;; ── Lowering: walrus ────────────────────────────────────────────
   (test-case
    "walrus lowers to uir-walrus"
    (let ([u (lower-source "if (n := len(x)) > 0:\n    pass\n\n")])
      (check-true (uir-contains? u uir-walrus?) "walrus lowers to uir-walrus")
      (let ([w (let find ([n u])
                 (cond [(uir-walrus? n) n]
                       [(struct? n)
                        (ormap find (cdr (vector->list (struct->vector n))))]
                       [(pair? n) (or (find (car n)) (find (cdr n)))]
                       [else #f]))])
        (check-true (uir-var? (uir-walrus-name w)) "walrus target is a variable")
        (check-equal? (uir-symbol-name (uir-var-name (uir-walrus-name w))) "n"
                      "walrus target name")
        (check-true (uir-call? (uir-walrus-value w)) "walrus value is an expression"))))

   ;; ── Lowering: positional-only marker is dropped (declared loss) ──
   (test-case
    "the positional-only marker is not lowered as a parameter"
    (let* ([u (lower-source "def f(a, b, /, c):\n    return a\n\n")]
           [fn (first (uir-block-stmts u))])
      (check-true (uir-fn? fn) "def lowers to uir-fn")
      (check-equal? (map (λ (p) (uir-symbol-name (uir-typed-param-name p)))
                         (uir-fn-params fn))
                    '("a" "b" "c"))
      (check-true (not (ormap (λ (p) (and (uir-symbol? p)
                                          (equal? (uir-symbol-name p) "/")))
                              (uir-fn-params fn)))
                  "no `/` sentinel leaks into the parameter list")))

   ;; ── Emit round-trips ───────────────────────────────────────────
   (test-case
    "type aliases emit and re-parse"
    (check-equal? (roundtrip "type T = int | str\n") "type T = int | str")
    (check-equal? (roundtrip "type ListOrSet[T] = list[T] | set[T]\n")
                  "type ListOrSet[T] = list[T] | set[T]"))

   (test-case
    "walrus emits and re-parses"
    (check-equal? (roundtrip "if (n := len(x)) > 0:\n    pass\n\n")
                  "if (n := len(x)) > 0:\n    pass")
    (check-equal? (roundtrip "x = (y := 1)\n") "x = (y := 1)"
                  "parenthesised walrus keeps one pair of parentheses"))

   (test-case
    "positional-only marker drops at emit (declared loss)"
    ;; The UIR has no positional-only flag, so emitted params are ordinary
    ;; positional-or-keyword params; the emitted code is valid Python.
    (check-equal? (roundtrip "def f(a, b, /, c):\n    return a\n\n")
                  "def f(a, b, c):\n    return a")
    (check-equal? (roundtrip "f = lambda a, /, b: a\n") "f = lambda a, b: a"))

   ;; ── Serialization round-trip for the new UIR nodes ──────────────
   (test-case
    "new UIR nodes serialize"
    (let ([w (uir-walrus (uir-var (uir-symbol "n")) (uir-number "1"))])
      (check-true (uir? w))
      (check-equal? (uir-tag w) 'walrus)
      (check-equal? (sexp->uir (uir->sexp w)) w "walrus sexp round-trip")
      (check-equal? (json->uir (uir->json w)) w "walrus json round-trip"))
    (let ([ta (uir-type-alias (uir-symbol "T")
                              (list (uir-symbol "K"))
                              (uir-call (uir-symbol "|")
                                        (list (uir-symbol "int") (uir-symbol "str"))))])
      (check-true (uir? ta))
      (check-equal? (uir-tag ta) 'type-alias)
      (check-equal? (sexp->uir (uir->sexp ta)) ta "type alias sexp round-trip")
      (check-equal? (json->uir (uir->json ta)) ta "type alias json round-trip")))))

;; ── Run and clean up ────────────────────────────────────────────────

(define failure-count (run-tests modern-python-tests))
(cleanup)
(exit (if (zero? failure-count) 0 1))
