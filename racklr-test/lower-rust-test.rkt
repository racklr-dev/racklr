#lang racket

;; racklr-test/lower-rust-test.rkt — Rust front end (P2.3).
;;
;; Acceptance proof for P2.1/P2.2: a new front end is a grammar plus a lowering
;; hook plus a token lexicon, discovered as data — no core `.rkt` file was
;; edited. This suite covers:
;;   * registry discovery of the Rust spec (grammar + hook + lexicon)
;;   * CST → UIR lowering against hand-built expected UIR for representative
;;     Rust source
;;   * round-trip: canonical UIR serialization identity (versioned sexp, and
;;     JSON where the pre-existing serializer supports it)
;;   * a realistic multi-item program, and UIR consumability by an existing
;;     emitter
;;
;; Round-trip scope, stated honestly: `uir-while` has a writer but no reader in
;; uir/serialize.rkt, and uir-match/uir-pat-* have no serializer at all (both
;; pre-existing gaps in a core file this ticket must not edit). Those samples
;; are covered by shape tests, and the serialization round-trip is asserted on
;; the serializable subset.

(require rackunit
         rackunit/text-ui
         racklr/tree
         racklr/uir
         racklr/gen-test
         racklr/frontends
         racklr/lower-rust
         racklr/emit-python)

;; ── The front end under test, loaded through the generic registry ───

(define specs (discover-front-ends))
(define rust-spec
  (for/first ([s (in-list specs)] #:when (eq? (front-end-name s) 'rust)) s))

(define-values (rust-parse rust-tokenize rust-tok-type rust-tok-value)
  (gen-and-load (front-end-grammar-file rust-spec)))

(define (lower src)
  ((front-end-lower rust-spec) (rust-parse src) rust-tok-type rust-tok-value))

;; ── Shape helpers ───────────────────────────────────────────────────

(define (block-of uir)
  (check-true (uir-block? uir) "crate lowers to uir-block")
  (uir-block-stmts uir))

(define (only-item src)
  (let* ([stmts (block-of (lower src))])
    (check-equal? (length stmts) 1 (format "one top-level item in ~s" src))
    (first stmts)))

(define (check-shape src expected [label ""])
  (check-equal? (lower src) expected
                (format "~a lowering of ~s" label src)))

(define (serializable-roundtrip src)
  (define u (lower src))
  (check-equal? (versioned-sexp->uir (uir->versioned-sexp u)) u
                (format "versioned-sexp round-trip of ~s" src)))

(define (json-roundtrip src)
  (define u (lower src))
  (check-equal? (json->uir (uir->json u)) u
                (format "JSON round-trip of ~s" src)))

;; ── Registry (the zero-core-edit seam) ──────────────────────────────

(define suite
  (test-suite
   "Rust front end (grammar + lowering hook + token lexicon)"

   (test-case "registry discovers the Rust spec as data"
     (check-not-false (memq 'rust (map front-end-name specs))
                 "the registry discovers the Rust spec")
     (check-equal? (front-end-grammar-path rust-spec) "grammars/rust/RustParser.g4")
     (check-true (file-exists? (front-end-grammar-file rust-spec))
                 "the spec's grammar file exists in the checkout")
     (check-true (procedure? (front-end-lower rust-spec))
                 "the spec carries a lowering hook")
     (check-not-false (front-end-lexicon rust-spec)
                 "the spec carries the Rust token lexicon"))

   (test-case "crate and function lowering"
     (check-shape "" (uir-block '()))
     (check-shape "fn main() {}"
                  (uir-block (list (uir-fn (uir-symbol "main") '()
                                           (uir-block '()) #f #f))))
     (check-shape "fn add(x: i32, y: i32) -> i32 { x + y }"
                  (uir-block
                   (list (uir-fn
                          (uir-symbol "add")
                          (list (uir-typed-param (uir-symbol "x") (uir-symbol "i32") #f #f)
                                (uir-typed-param (uir-symbol "y") (uir-symbol "i32") #f #f))
                          (uir-block
                           (list (uir-return
                                  (uir-call (uir-symbol "+")
                                            (list (uir-var (uir-symbol "x"))
                                                  (uir-var (uir-symbol "y")))))))
                          (uir-symbol "i32")
                          #f))))
     ;; effects are #f so the shared inference pass can populate them
     (check-false (uir-fn-effects (only-item "fn f() {}"))))

   (test-case "let bindings: plain and typed"
     (check-shape "fn f() { let x = 1; let y: i32 = 2; }"
                  (uir-block
                   (list (uir-fn
                          (uir-symbol "f") '()
                          (uir-block
                           (list (uir-set! (uir-var (uir-symbol "x")) (uir-number "1"))
                                 (uir-ann-set! (uir-var (uir-symbol "y"))
                                               (uir-symbol "i32")
                                               (uir-number "2"))))
                          #f #f)))))

   (test-case "control flow: if / while / for / loop / return"
     (check-shape "fn f() { if x > 0 { 1 } else { 0 } }"
                  (uir-block
                   (list (uir-fn
                          (uir-symbol "f") '()
                          (uir-block
                           (list (uir-if
                                  (uir-call (uir-symbol ">")
                                            (list (uir-var (uir-symbol "x"))
                                                  (uir-number "0")))
                                  (uir-block (list (uir-return (uir-number "1"))))
                                  (uir-block (list (uir-return (uir-number "0")))))))
                          #f #f))))
     (check-shape "fn f() { while x < 3 { x = x + 1; } }"
                  (uir-block
                   (list (uir-fn
                          (uir-symbol "f") '()
                          (uir-block
                           (list (uir-while
                                  (uir-call (uir-symbol "<")
                                            (list (uir-var (uir-symbol "x"))
                                                  (uir-number "3")))
                                  (uir-block
                                   (list (uir-set!
                                          (uir-var (uir-symbol "x"))
                                          (uir-call (uir-symbol "+")
                                                    (list (uir-var (uir-symbol "x"))
                                                          (uir-number "1"))))))
                                  (uir-null))))
                          #f #f))))
     (check-shape "fn f() { for x in ys { g(x); } }"
                  (uir-block
                   (list (uir-fn
                          (uir-symbol "f") '()
                          (uir-block
                           (list (uir-for-each
                                  (uir-var (uir-symbol "x"))
                                  (uir-var (uir-symbol "ys"))
                                  (uir-block
                                   (list (uir-call (uir-var (uir-symbol "g"))
                                                   (list (uir-var (uir-symbol "x"))))))
                                  (uir-null))))
                          #f #f))))
     (check-shape "fn f() { loop { break; } }"
                  (uir-block
                   (list (uir-fn
                          (uir-symbol "f") '()
                          (uir-block
                           (list (uir-while (uir-bool #t)
                                            (uir-block (list (uir-symbol "break")))
                                            (uir-null))))
                          #f #f)))))

   (test-case "items: struct, tuple struct, enum"
     (check-shape "struct P { x: i32, y: i32 }"
                  (uir-block
                   (list (uir-class (uir-symbol "P") '()
                                    (list (uir-field (uir-symbol "x") (uir-symbol "i32") (uir-null))
                                          (uir-field (uir-symbol "y") (uir-symbol "i32") (uir-null)))
                                    '()))))
     (check-shape "struct Tup(i32, i32);"
                  (uir-block
                   (list (uir-class (uir-symbol "Tup") '()
                                    (list (uir-field (uir-symbol "_0") (uir-symbol "i32") (uir-null))
                                          (uir-field (uir-symbol "_1") (uir-symbol "i32") (uir-null)))
                                    '()))))
     (check-shape "enum Color { Red, Custom(u8), Named { n: u8 } }"
                  (uir-block
                   (list (uir-enum
                          (uir-symbol "Color")
                          (list (uir-enum-variant (uir-symbol "Red") '() #f)
                                (uir-enum-variant
                                 (uir-symbol "Custom")
                                 (list (uir-typed-param (uir-symbol "_0") (uir-symbol "u8") #f #f))
                                 #f)
                                (uir-enum-variant
                                 (uir-symbol "Named")
                                 (list (uir-typed-param (uir-symbol "n") (uir-symbol "u8") #f #f))
                                 #f)))))))

   (test-case "impl methods fold into the struct's class; trait → interface"
     (check-shape "struct P { x: i32 } impl P { fn get(&self) -> i32 { self.x } }"
                  (uir-block
                   (list (uir-class
                          (uir-symbol "P") '()
                          (list (uir-field (uir-symbol "x") (uir-symbol "i32") (uir-null)))
                          (list (uir-method
                                 (uir-symbol "get")
                                 (list (uir-typed-param (uir-symbol "self") #f #f #f))
                                 (uir-block
                                  (list (uir-return (uir-get (uir-var (uir-symbol "self"))
                                                             (uir-string "x")))))
                                 'public))))))
     (check-shape "trait Area { fn area(&self) -> f64; }"
                  (uir-block
                   (list (uir-interface
                          (uir-symbol "Area")
                          (list (uir-method
                                 (uir-symbol "area")
                                 (list (uir-typed-param (uir-symbol "self") #f #f #f))
                                 (uir-null) 'public)))))))

   (test-case "use and mod lower to import/module"
     (check-shape "use std::collections::HashMap;"
                  (uir-block
                   (list (uir-import (uir-symbol "std::collections::HashMap") '()))))
     (check-shape "mod m { fn f() {} }"
                  (uir-block
                   (list (uir-module (uir-symbol "m") '() '()
                                     (uir-block
                                      (list (uir-fn (uir-symbol "f") '()
                                                    (uir-block '()) #f #f))))))))

   (test-case "expressions: operators are re-associated with Rust precedence"
     (check-shape "fn f() -> i32 { 1 + 2 * 3 }"
                  (uir-block
                   (list (uir-fn
                          (uir-symbol "f") '()
                          (uir-block
                           (list (uir-return
                                  (uir-call (uir-symbol "+")
                                            (list (uir-number "1")
                                                  (uir-call (uir-symbol "*")
                                                            (list (uir-number "2")
                                                                  (uir-number "3"))))))))
                          (uir-symbol "i32") #f))))
     (check-shape "fn f() { x = 1 + 2; }"
                  (uir-block
                   (list (uir-fn
                          (uir-symbol "f") '()
                          (uir-block
                           (list (uir-set!
                                  (uir-var (uir-symbol "x"))
                                  (uir-call (uir-symbol "+")
                                            (list (uir-number "1") (uir-number "2"))))))
                          #f #f)))))

   (test-case "expressions: calls, methods, fields, index, literals"
     (check-shape
      "fn f() { let a = g(1, 2); let b = o.m(3); let c = o.f; let d = xs[0]; let e = P { x: 1 }; let t = (1, 2); let l = [1, 2]; }"
      (uir-block
       (list (uir-fn
              (uir-symbol "f") '()
              (uir-block
               (list
                (uir-set! (uir-var (uir-symbol "a"))
                          (uir-call (uir-var (uir-symbol "g"))
                                    (list (uir-number "1") (uir-number "2"))))
                (uir-set! (uir-var (uir-symbol "b"))
                          (uir-call (uir-get (uir-var (uir-symbol "o")) (uir-string "m"))
                                    (list (uir-number "3"))))
                (uir-set! (uir-var (uir-symbol "c"))
                          (uir-get (uir-var (uir-symbol "o")) (uir-string "f")))
                (uir-set! (uir-var (uir-symbol "d"))
                          (uir-get (uir-var (uir-symbol "xs")) (uir-number "0")))
                (uir-set! (uir-var (uir-symbol "e"))
                          (uir-new (uir-symbol "P") (list (uir-number "1"))))
                (uir-set! (uir-var (uir-symbol "t"))
                          (uir-call (uir-symbol "tuple")
                                    (list (uir-number "1") (uir-number "2"))))
                (uir-set! (uir-var (uir-symbol "l"))
                          (uir-list (list (uir-number "1") (uir-number "2"))))))
              #f #f)))))

   (test-case "macro invocation and impl self receiver"
     (check-shape "fn f() { println!(\"hi\"); }"
                  (uir-block
                   (list (uir-fn
                          (uir-symbol "f") '()
                          (uir-block
                           (list (uir-call (uir-var (uir-symbol "println!"))
                                           (list (uir-string "hi")))))
                          #f #f))))
     (check-shape "impl P { fn sum(&self) -> i32 { self.x + self.y } }"
                  (uir-block
                   (list (uir-class
                          (uir-symbol "P") '() '()
                          (list (uir-method
                                 (uir-symbol "sum")
                                 (list (uir-typed-param (uir-symbol "self") #f #f #f))
                                 (uir-block
                                  (list (uir-return
                                         (uir-call (uir-symbol "+")
                                                   (list (uir-get (uir-var (uir-symbol "self"))
                                                                  (uir-string "x"))
                                                         (uir-get (uir-var (uir-symbol "self"))
                                                                  (uir-string "y")))))))
                                 'public)))))))

   (test-case "match lowers to uir-match with patterns"
     (let ([uir (lower "fn f() { match x { 1 => 2, _ => 3 } }")])
       (define fn (first (block-of uir)))
       (check-true (uir-fn? fn))
       (define body (uir-block-stmts (uir-fn-body fn)))
       (check-equal? (length body) 1)
       (define m (first body))
       (check-true (uir-match? m) "match statement becomes uir-match")
       (check-equal? (uir-var-name (uir-match-subject m)) (uir-symbol "x"))
       (check-equal? (length (uir-match-cases m)) 2)
       (check-true (uir-pat-literal? (uir-case-pattern (first (uir-match-cases m)))))
       (check-true (uir-pat-wildcard? (uir-case-pattern (second (uir-match-cases m)))))))

   (test-case "closures lower to anonymous uir-fn"
     (let* ([uir (lower "fn f() { let c = |x: i32| x + 1; }")]
            [fn (first (block-of uir))]
            [set-expr (first (uir-block-stmts (uir-fn-body fn)))])
       (check-true (uir-set!? set-expr))
       (define clo (uir-set!-value set-expr))
       (check-true (uir-fn? clo) "closure becomes uir-fn")
       (check-false (uir-fn-name clo) "closure is anonymous")
       (check-equal? (length (uir-fn-params clo)) 1)))

   (test-case "round-trip: canonical UIR serialization identity"
     ;; Everything without uir-while / uir-match (the two pre-existing
     ;; serializer gaps) survives the versioned sexp round-trip exactly.
     (for ([src (in-list '(""
                           "fn main() {}"
                           "fn add(x: i32, y: i32) -> i32 { x + y }"
                           "fn f() { let x = 1; let y: i32 = 2; }"
                           "fn f() { if x > 0 { 1 } else { 0 } }"
                           "fn f() { for x in ys { g(x); } }"
                           "struct P { x: i32, y: i32 }"
                           "struct Tup(i32, i32);"
                           "enum Color { Red, Custom(u8) }"
                           "struct P { x: i32 } impl P { fn get(&self) -> i32 { self.x } }"
                           "trait Area { fn area(&self) -> f64; }"
                           "use std::collections::HashMap;"
                           "mod m { fn f() {} }"
                           "fn f() { let a = g(1, 2); let b = o.m(3); }"))])
       (serializable-roundtrip src))
     ;; JSON identity on the subset the pre-existing JSON serializer handles
     ;; (#f-valued typed-param defaults and raw symbol fields do not survive).
     (for ([src (in-list '("" "fn f() { let x = 1; }"
                            "struct P { x: i32 }"
                            "enum Color { Red }"))])
       (json-roundtrip src)))

   (test-case "realistic multi-item program lowers end to end"
     (define src
       (string-append
        "struct Point { x: i32, y: i32 }\n"
        "impl Point {\n"
        "  fn new(x: i32, y: i32) -> Point { Point { x: x, y: y } }\n"
        "  fn sum(&self) -> i32 { self.x + self.y }\n"
        "}\n"
        "enum Shape { Circle(f64), Rect { w: f64, h: f64 } }\n"
        "trait Area { fn area(&self) -> f64; }\n"
        "fn main() { let p = Point { x: 1, y: 2 }; let s = p.sum(); println!(\"hi\"); }\n"))
     (define items (block-of (lower src)))
     (check-equal? (length items) 4)  ; struct+impl merge into one class
     (define point (first items))
     (check-true (uir-class? point))
     (check-equal? (uir-symbol-name (uir-class-name point)) "Point")
     (check-equal? (length (uir-class-fields point)) 2)
     (check-equal? (length (uir-class-methods point)) 2)
     (check-true (for/or ([i items]) (and (uir-enum? i)
                                          (equal? (uir-symbol-name (uir-enum-name i))
                                                  "Shape"))))
     (check-true (for/or ([i items]) (uir-interface? i)))
     (check-true (uir? (lower src))))

   (test-case "lowered UIR is consumable by an existing language emitter"
     ;; The shared UIR is the contract between front ends and back ends: the
     ;; untouched Python emitter consumes the Rust-lowered UIR.
     (define py (emit-python (lower "fn add(x: i32, y: i32) -> i32 { x + y }")))
     (check-true (string-contains? py "def add("))
     (check-true (string-contains? py "x + y")))))

(define results (run-tests suite))
(cleanup)
(exit results)
