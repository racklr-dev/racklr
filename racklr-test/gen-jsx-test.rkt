#lang racket

(require rackunit
         racklr/tree
         racklr/uir
         racklr/gen-test
         racklr/lower-jsx
         racklr/emit-jsx)

;; ── Load the JSX parser ─────────────────────────────────────────────

(define-values (jsx-parse jsx-tokenize tok-type tok-value)
  (gen-and-load "../grammars/jsx/JSXParser.g4"))

;; ── Tokenizer smoke tests ───────────────────────────────────────────

(for ([src (list "<div/>"
                 "<div>hello</div>"
                 "<div className=\"foo\">hi</div>"
                 "<div>{name}</div>"
                 "<div className={cls}>Hello {name}!</div>")])
  (printf "--- TOKENIZE: ~a ---\n" src)
  (define tks (jsx-tokenize src))
  (check-true (pair? tks)))

;; ── Parser smoke tests ──────────────────────────────────────────────

(printf "\n=== PARSER TESTS ===\n\n")

(for ([src (list "<div/>"
                 "<div>hello</div>"
                 "<div className=\"foo\">hi</div>"
                 "<div>{name}</div>"
                 "<div className={cls}>Hello!</div>"
                 "<div><span/></div>"
                 "<div><span>hello</span></div>")])
  (let ([cst (jsx-parse src)])
    (check-equal? (any-tree-tag cst) 'jsxElement)))

;; ── Lowering tests ──────────────────────────────────────────────────

(printf "\n=== LOWERING TESTS ===\n\n")

;; Self-closing: <div/>
(let* ([cst (jsx-parse "<div/>")]
       [uir (lower-jsx cst #:tk-type tok-type #:tk-value tok-value)])
  (check-pred uir-element? uir)
  (check-equal? (uir-element-tag uir) (uir-string "div"))
  (check-equal? (uir-element-attrs uir) '())
  (check-equal? (uir-element-children uir) '())
  (printf "PASS: lower <div/> -> ~a\n" (uir->sexp uir)))

;; With text: <div>hello</div>
(let* ([cst (jsx-parse "<div>hello</div>")]
       [uir (lower-jsx cst #:tk-type tok-type #:tk-value tok-value)])
  (check-pred uir-element? uir)
  (check-equal? (uir-element-tag uir) (uir-string "div"))
  (define children (uir-element-children uir))
  (check-true (pair? children))
  (check-pred uir-text-node? (first children))
  (printf "PASS: lower <div>hello</div> -> ~a\n" (uir->sexp uir)))

;; With attribute: <div className="foo">hi</div>
(let* ([cst (jsx-parse "<div className=\"foo\">hi</div>")]
       [uir (lower-jsx cst #:tk-type tok-type #:tk-value tok-value)])
  (check-pred uir-element? uir)
  (define attrs (uir-element-attrs uir))
  (check-true (pair? attrs))
  (define attr (first attrs))
  (check-equal? (uir-symbol-name (uir-attribute-name attr)) 'className)
  (check-equal? (uir-attribute-value attr) (uir-string "foo"))
  (printf "PASS: lower <div className=\"foo\">hi</div> -> ~a\n" (uir->sexp uir)))

;; With expression: <div>{name}</div>
(let* ([cst (jsx-parse "<div>{name}</div>")]
       [uir (lower-jsx cst #:tk-type tok-type #:tk-value tok-value)])
  (check-pred uir-element? uir)
  (define children (uir-element-children uir))
  (check-true (pair? children))
  (check-pred uir-jsx-expr? (first children))
  (check-equal? (uir-jsx-expr-content (first children)) "name")
  (printf "PASS: lower <div>{name}</div> -> ~a\n" (uir->sexp uir)))

;; With expression attribute: <div className={cls}>Hello!</div>
(let* ([cst (jsx-parse "<div className={cls}>Hello!</div>")]
       [uir (lower-jsx cst #:tk-type tok-type #:tk-value tok-value)])
  (check-pred uir-element? uir)
  (define attrs (uir-element-attrs uir))
  (define attr (first attrs))
  (check-pred uir-jsx-expr? (uir-attribute-value attr))
  (check-equal? (uir-jsx-expr-content (uir-attribute-value attr)) "cls")
  (printf "PASS: lower <div className={cls}>Hello!</div> -> ~a\n" (uir->sexp uir)))

;; Nested: <div><span/></div>
(let* ([cst (jsx-parse "<div><span/></div>")]
       [uir (lower-jsx cst #:tk-type tok-type #:tk-value tok-value)])
  (check-pred uir-element? uir)
  (define children (uir-element-children uir))
  (check-true (pair? children))
  (check-pred uir-element? (first children))
  (printf "PASS: lower <div><span/></div> -> ~a\n" (uir->sexp uir)))

;; ── Emission tests ────────────────────────────────────────────────────

(printf "\n=== EMIT TESTS ===\n\n")

(define (lower-and-emit src)
  (define cst (jsx-parse src))
  (define uir (lower-jsx cst #:tk-type tok-type #:tk-value tok-value))
  (emit-jsx uir))

;; Self-closing: <div/> → React.createElement("div", null)
(let ([out (lower-and-emit "<div/>")])
  (check-equal? out "React.createElement(\"div\", null)")
  (printf "PASS: emit <div/> -> ~a\n" out))

;; With text: <div>hello</div> → React.createElement("div", null, "hello")
(let ([out (lower-and-emit "<div>hello</div>")])
  (check-equal? out "React.createElement(\"div\", null, \"hello\")")
  (printf "PASS: emit <div>hello</div> -> ~a\n" out))

;; With attribute: <div className="foo">hi</div>
(let ([out (lower-and-emit "<div className=\"foo\">hi</div>")])
  (check-equal? out "React.createElement(\"div\", { className: \"foo\" }, \"hi\")")
  (printf "PASS: emit <div className=\"foo\">hi</div> -> ~a\n" out))

;; With expression child: <div>{name}</div>
(let ([out (lower-and-emit "<div>{name}</div>")])
  (check-equal? out "React.createElement(\"div\", null, name)")
  (printf "PASS: emit <div>{name}</div> -> ~a\n" out))

;; With expression attribute: <div className={cls}>Hello!</div>
(let ([out (lower-and-emit "<div className={cls}>Hello!</div>")])
  (check-equal? out "React.createElement(\"div\", { className: cls }, \"Hello!\")")
  (printf "PASS: emit <div className={cls}>Hello!</div> -> ~a\n" out))

;; Nested: <div><span/></div>
(let ([out (lower-and-emit "<div><span/></div>")])
  (check-equal? out "React.createElement(\"div\", null, React.createElement(\"span\", null))")
  (printf "PASS: emit <div><span/></div> -> ~a\n" out))

;; Nested with text: <div><span>hello</span></div>
(let ([out (lower-and-emit "<div><span>hello</span></div>")])
  (check-equal? out "React.createElement(\"div\", null, React.createElement(\"span\", null, \"hello\"))")
  (printf "PASS: emit <div><span>hello</span></div> -> ~a\n" out))

;; Complex: <div className={cls}>Hello {name}!</div>
(let ([out (lower-and-emit "<div className={cls}>Hello {name}!</div>")])
  (check-equal? out "React.createElement(\"div\", { className: cls }, \"Hello \", name, \"!\")")
  (printf "PASS: emit <div className={cls}>Hello {name}!</div> -> ~a\n" out))
