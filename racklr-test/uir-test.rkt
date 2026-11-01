#lang racket

(require rackunit
         racklr/uir)

;; ── Constructors ───────────────────────────────────────────────────

(define (test-null-construction)
  (define n (uir-null))
  (check-true (uir? n) "uir-null satisfies uir?")
  (check-equal? (uir-tag n) 'null "uir-null tag is 'null"))

(define (test-bool-construction)
  (define b (uir-bool #t))
  (check-true (uir? b) "uir-bool satisfies uir?")
  (check-equal? (uir-tag b) 'bool "uir-bool tag is 'bool")
  (check-true (uir-bool-value b) "uir-bool-value for #t"))

(define (test-number-construction)
  (define n (uir-number "3.14"))
  (check-true (uir? n) "uir-number satisfies uir?")
  (check-equal? (uir-tag n) 'number "uir-number tag is 'number")
  (check-equal? (uir-number-value n) "3.14" "uir-number-value"))

(define (test-string-construction)
  (define s (uir-string "hello"))
  (check-true (uir? s) "uir-string satisfies uir?")
  (check-equal? (uir-tag s) 'string "uir-string tag is 'string")
  (check-equal? (uir-string-value s) "hello" "uir-string-value"))

(define (test-list-construction)
  (define l (uir-list (list (uir-null) (uir-bool #f) (uir-number "42"))))
  (check-true (uir? l) "uir-list satisfies uir?")
  (check-equal? (uir-tag l) 'list "uir-list tag is 'list")
  (check-equal? (length (uir-list-items l)) 3 "uir-list has 3 items"))

(define (test-record-construction)
  (define r (uir-record (list (cons (uir-string "x") (uir-number "1"))
                              (cons (uir-string "y") (uir-bool #t)))))
  (check-true (uir? r) "uir-record satisfies uir?")
  (check-equal? (uir-tag r) 'record "uir-record tag is 'record")
  (check-equal? (length (uir-record-entries r)) 2 "uir-record has 2 entries"))

(define (test-symbol-construction)
  (define s (uir-symbol "foo"))
  (check-true (uir? s) "uir-symbol satisfies uir?")
  (check-equal? (uir-tag s) 'symbol "uir-symbol tag is 'symbol")
  (check-equal? (uir-symbol-name s) "foo" "uir-symbol-name"))

(define (test-fn-construction)
  (define f (uir-fn #f (list (uir-symbol "x") (uir-symbol "y"))
                     (uir-call (uir-var (uir-symbol "+"))
                               (list (uir-var (uir-symbol "x"))
                                     (uir-var (uir-symbol "y"))))
                     #f #f))
  (check-true (uir? f) "uir-fn satisfies uir?")
  (check-equal? (uir-tag f) 'fn "uir-fn tag is 'fn")
  (check-equal? (length (uir-fn-params f)) 2 "uir-fn has 2 params")
  (check-true (uir-call? (uir-fn-body f)) "uir-fn body is uir-call"))

(define (test-call-construction)
  (define c (uir-call (uir-symbol "f") (list (uir-number "1") (uir-bool #t))))
  (check-true (uir? c) "uir-call satisfies uir?")
  (check-equal? (uir-tag c) 'call "uir-call tag is 'call")
  (check-equal? (uir-symbol-name (uir-call-callee c)) "f" "uir-call callee")
  (check-equal? (length (uir-call-args c)) 2 "uir-call has 2 args"))

(define (test-let-construction)
  (define l (uir-let (uir-symbol "x") (uir-number "42") (uir-var (uir-symbol "x"))))
  (check-true (uir? l) "uir-let satisfies uir?")
  (check-equal? (uir-tag l) 'let "uir-let tag is 'let")
  (check-equal? (uir-symbol-name (uir-let-name l)) "x" "uir-let name")
  (check-equal? (uir-number-value (uir-let-value l)) "42" "uir-let value")
  (check-true (uir-var? (uir-let-body l)) "uir-let body is uir-var"))

(define (test-var-construction)
  (define v (uir-var (uir-symbol "x")))
  (check-true (uir? v) "uir-var satisfies uir?")
  (check-equal? (uir-tag v) 'var "uir-var tag is 'var")
  (check-equal? (uir-symbol-name (uir-var-name v)) "x" "uir-var name"))

(define (test-set-construction)
  (define s (uir-set! (uir-symbol "x") (uir-number "99")))
  (check-true (uir? s) "uir-set! satisfies uir?")
  (check-equal? (uir-tag s) 'set! "uir-set! tag is 'set!")
  (check-equal? (uir-symbol-name (uir-set!-name s)) "x" "uir-set! name")
  (check-equal? (uir-number-value (uir-set!-value s)) "99" "uir-set! value"))

(define (test-if-construction)
  (define i (uir-if (uir-bool #t) (uir-number "1") (uir-number "0")))
  (check-true (uir? i) "uir-if satisfies uir?")
  (check-equal? (uir-tag i) 'if "uir-if tag is 'if")
  (check-true (uir-bool-value (uir-if-test i)) "uir-if test is #t")
  (check-equal? (uir-number-value (uir-if-then i)) "1" "uir-if then")
  (check-equal? (uir-number-value (uir-if-else i)) "0" "uir-if else"))

(define (test-block-construction)
  (define b (uir-block (list (uir-number "1") (uir-number "2") (uir-number "3"))))
  (check-true (uir? b) "uir-block satisfies uir?")
  (check-equal? (uir-tag b) 'block "uir-block tag is 'block")
  (check-equal? (length (uir-block-stmts b)) 3 "uir-block has 3 stmts"))

(define (test-return-construction)
  (define r (uir-return (uir-number "42")))
  (check-true (uir? r) "uir-return satisfies uir?")
  (check-equal? (uir-tag r) 'return "uir-return tag is 'return")
  (check-equal? (uir-number-value (uir-return-value r)) "42" "uir-return value"))

;; ── Tier 1 Construction ─────────────────────────────────────────────

(define (test-class-construction)
  (define c (uir-class (uir-symbol "Foo") '()
                       (list (uir-field (uir-symbol "x") (uir-symbol "int") (uir-null)))
                       (list (uir-method (uir-symbol "m") '() (uir-null) 'public))))
  (check-true (uir? c) "uir-class satisfies uir?")
  (check-equal? (uir-tag c) 'class "uir-class tag is 'class")
  (check-equal? (uir-symbol-name (uir-class-name c)) "Foo" "uir-class name")
  (check-equal? (uir-class-super c) '() "uir-class no super")
  (check-equal? (length (uir-class-fields c)) 1 "uir-class has 1 field")
  (check-equal? (length (uir-class-methods c)) 1 "uir-class has 1 method"))

(define (test-method-construction)
  (define m (uir-method (uir-symbol "bar")
                        (list (uir-symbol "x"))
                        (uir-return (uir-var (uir-symbol "x")))
                        'private))
  (check-true (uir? m) "uir-method satisfies uir?")
  (check-equal? (uir-tag m) 'method "uir-method tag is 'method")
  (check-equal? (uir-symbol-name (uir-method-name m)) "bar" "uir-method name")
  (check-equal? (length (uir-method-params m)) 1 "uir-method has 1 param")
  (check-equal? (uir-method-visibility m) 'private "uir-method visibility"))

(define (test-field-construction)
  (define f (uir-field (uir-symbol "count") (uir-symbol "number") (uir-number "0")))
  (check-true (uir? f) "uir-field satisfies uir?")
  (check-equal? (uir-tag f) 'field "uir-field tag is 'field")
  (check-equal? (uir-symbol-name (uir-field-name f)) "count" "uir-field name")
  (check-true (uir-symbol? (uir-field-type f)) "uir-field has type annotation")
  (check-equal? (uir-number-value (uir-field-init f)) "0" "uir-field init value"))

(define (test-new-construction)
  (define n (uir-new (uir-symbol "Point") (list (uir-number "1") (uir-number "2"))))
  (check-true (uir? n) "uir-new satisfies uir?")
  (check-equal? (uir-tag n) 'new "uir-new tag is 'new")
  (check-equal? (uir-symbol-name (uir-new-class n)) "Point" "uir-new class")
  (check-equal? (length (uir-new-args n)) 2 "uir-new has 2 args"))

(define (test-interface-construction)
  (define i (uir-interface (uir-symbol "Serializable")
                           (list (uir-method (uir-symbol "serialize") '() (uir-null) 'public))))
  (check-true (uir? i) "uir-interface satisfies uir?")
  (check-equal? (uir-tag i) 'interface "uir-interface tag is 'interface")
  (check-equal? (uir-symbol-name (uir-interface-name i)) "Serializable" "uir-interface name"))

(define (test-module-construction)
  (define m (uir-module (uir-symbol "app")
                        (list (uir-import (uir-symbol "core") (list (uir-symbol "foo"))))
                        (list (uir-export (list (uir-symbol "main"))))
                        (uir-block (list (uir-number "42")))))
  (check-true (uir? m) "uir-module satisfies uir?")
  (check-equal? (uir-tag m) 'module "uir-module tag is 'module")
  (check-equal? (uir-symbol-name (uir-module-name m)) "app" "uir-module name")
  (check-equal? (length (uir-module-imports m)) 1 "uir-module has 1 import")
  (check-equal? (length (uir-module-exports m)) 1 "uir-module has 1 export"))

(define (test-import-construction)
  (define i (uir-import (uir-symbol "stdlib") (list (uir-symbol "map") (uir-symbol "filter"))))
  (check-true (uir? i) "uir-import satisfies uir?")
  (check-equal? (uir-tag i) 'import "uir-import tag is 'import")
  (check-equal? (uir-symbol-name (uir-import-source i)) "stdlib" "uir-import source")
  (check-equal? (length (uir-import-names i)) 2 "uir-import has 2 names"))

(define (test-export-construction)
  (define e (uir-export (list (uir-symbol "main") (uir-symbol "init"))))
  (check-true (uir? e) "uir-export satisfies uir?")
  (check-equal? (uir-tag e) 'export "uir-export tag is 'export")
  (check-equal? (length (uir-export-names e)) 2 "uir-export has 2 names"))

;; ── Tier 2 Construction ─────────────────────────────────────────────

(define (test-component-construction)
  (define c (uir-component (uir-symbol "Counter")
                           (list (uir-field (uir-symbol "init") (uir-symbol "number") (uir-number "0")))
                           (list (uir-state (uir-symbol "count") (uir-number "0")))
                           (list (uir-effect (list (uir-symbol "count"))
                                             (uir-call (uir-symbol "console.log") (list (uir-symbol "count")))))
                           (uir-element (uir-symbol "div")
                                        (list (uir-attribute (uir-symbol "class") (uir-string "counter")))
                                        (list (uir-text-node (uir-string "Hello")))
                                        (list (uir-event (uir-string "click") (uir-symbol "onClick"))))))
  (check-true (uir? c) "uir-component satisfies uir?")
  (check-equal? (uir-tag c) 'component "uir-component tag is 'component")
  (check-equal? (uir-symbol-name (uir-component-name c)) "Counter" "uir-component name")
  (check-equal? (length (uir-component-props c)) 1 "uir-component has 1 prop")
  (check-equal? (length (uir-component-states c)) 1 "uir-component has 1 state")
  (check-equal? (length (uir-component-effects c)) 1 "uir-component has 1 effect"))

(define (test-element-construction)
  (define e (uir-element (uir-symbol "div")
                         (list (uir-attribute (uir-symbol "id") (uir-string "root")))
                         (list (uir-text-node (uir-string "text")))
                         '()))
  (check-true (uir? e) "uir-element satisfies uir?")
  (check-equal? (uir-tag e) 'element "uir-element tag is 'element")
  (check-equal? (uir-symbol-name (uir-element-tag e)) "div" "uir-element tag name")
  (check-equal? (length (uir-element-attrs e)) 1 "uir-element has 1 attr")
  (check-equal? (length (uir-element-children e)) 1 "uir-element has 1 child"))

(define (test-attribute-construction)
  (define a (uir-attribute (uir-symbol "href") (uir-string "/about")))
  (check-true (uir? a) "uir-attribute satisfies uir?")
  (check-equal? (uir-tag a) 'attribute "uir-attribute tag is 'attribute")
  (check-equal? (uir-symbol-name (uir-attribute-name a)) "href" "uir-attribute name"))

(define (test-event-construction)
  (define e (uir-event (uir-string "submit")
                       (uir-fn #f (list (uir-symbol "e")) (uir-null) #f #f)))
  (check-true (uir? e) "uir-event satisfies uir?")
  (check-equal? (uir-tag e) 'event "uir-event tag is 'event")
  (check-equal? (uir-string-value (uir-event-name e)) "submit" "uir-event name"))

(define (test-slot-construction)
  (define s (uir-slot (uir-symbol "header") (uir-text-node (uir-string "Default"))))
  (check-true (uir? s) "uir-slot satisfies uir?")
  (check-equal? (uir-tag s) 'slot "uir-slot tag is 'slot")
  (check-equal? (uir-symbol-name (uir-slot-name s)) "header" "uir-slot name"))

(define (test-text-node-construction)
  (define t (uir-text-node (uir-string "Hello world")))
  (check-true (uir? t) "uir-text-node satisfies uir?")
  (check-equal? (uir-tag t) 'text-node "uir-text-node tag is 'text-node")
  (check-equal? (uir-string-value (uir-text-node-content t)) "Hello world" "uir-text-node content"))

(define (test-style-construction)
  (define s (uir-style (list (cons (uir-string "color") (uir-string "red"))
                             (cons (uir-string "font-size") (uir-string "16px")))))
  (check-true (uir? s) "uir-style satisfies uir?")
  (check-equal? (uir-tag s) 'style "uir-style tag is 'style")
  (check-equal? (length (uir-style-styles s)) 2 "uir-style has 2 entries"))

(define (test-effect-construction)
  (define ef (uir-effect (list (uir-symbol "count"))
                         (uir-block (list (uir-call (uir-symbol "log") (list (uir-symbol "count")))))))
  (check-true (uir? ef) "uir-effect satisfies uir?")
  (check-equal? (uir-tag ef) 'effect "uir-effect tag is 'effect"))

(define (test-state-construction)
  (define s (uir-state (uir-symbol "items") (uir-list '())))
  (check-true (uir? s) "uir-state satisfies uir?")
  (check-equal? (uir-tag s) 'state "uir-state tag is 'state")
  (check-equal? (uir-symbol-name (uir-state-name s)) "items" "uir-state name"))

(define (test-effect-entry-construction)
  (define e (uir-effect-entry (uir-symbol "llm") 'may
                              (list (uir-symbol "query")) 5
                              (uir-symbol "reads=search_index")))
  (check-true (uir? e) "uir-effect-entry satisfies uir?")
  (check-equal? (uir-tag e) 'effect-entry "uir-effect-entry tag is 'effect-entry")
  (check-equal? (uir-symbol-name (uir-effect-entry-name e)) "llm" "uir-effect-entry name")
  (check-equal? (uir-effect-entry-modality e) 'may "uir-effect-entry modality")
  (check-equal? (length (uir-effect-entry-ops e)) 1 "uir-effect-entry ops")
  (check-equal? (uir-effect-entry-budget e) 5 "uir-effect-entry budget")
  (check-equal? (uir-symbol-name (uir-effect-entry-scope e)) "reads=search_index" "uir-effect-entry scope"))

(define (test-effect-row-construction)
  (define r (uir-effect-row
             (list (uir-effect-entry (uir-symbol "llm") 'may '() 5 #f)
                   (uir-effect-entry (uir-symbol "db") 'may '() #f (uir-symbol "reads=search_index")))))
  (check-true (uir? r) "uir-effect-row satisfies uir?")
  (check-equal? (uir-tag r) 'effect-row "uir-effect-row tag is 'effect-row")
  (check-equal? (length (uir-effect-row-entries r)) 2 "uir-effect-row has 2 entries"))

(define (test-subtype-construction)
  (define s (uir-subtype (uir-effect-row '())
                         (uir-effect-row (list (uir-effect-entry (uir-symbol "net") 'may '() #f #f)))))
  (check-true (uir? s) "uir-subtype satisfies uir?")
  (check-equal? (uir-tag s) 'subtype "uir-subtype tag is 'subtype")
  (check-true (uir-effect-row? (uir-subtype-left s)) "uir-subtype left")
  (check-equal? (length (uir-effect-row-entries (uir-subtype-right s))) 1 "uir-subtype right"))

;; ── Serialization Round-Trip ───────────────────────────────────────

(define (test-sexp-roundtrip)
  ;; Null
  (let* ([u1 (uir-null)]
         [s1 (uir->sexp u1)]
         [u2 (sexp->uir s1)])
    (check-equal? s1 '(null) "uir-null sexp")
    (check-equal? (uir-tag u2) 'null "uir-null round-trip"))

  ;; Bool
  (let* ([u1 (uir-bool #f)]
         [s1 (uir->sexp u1)]
         [u2 (sexp->uir s1)])
    (check-equal? s1 '(bool #f) "uir-bool sexp")
    (check-false (uir-bool-value u2) "uir-bool round-trip value"))

  ;; Number
  (let* ([u1 (uir-number "1.5e10")]
         [s1 (uir->sexp u1)]
         [u2 (sexp->uir s1)])
    (check-equal? s1 '(number "1.5e10") "uir-number sexp")
    (check-equal? (uir-number-value u2) "1.5e10" "uir-number round-trip"))

  ;; String
  (let* ([u1 (uir-string "hello")]
         [s1 (uir->sexp u1)]
         [u2 (sexp->uir s1)])
    (check-equal? s1 '(string "hello") "uir-string sexp")
    (check-equal? (uir-string-value u2) "hello" "uir-string round-trip"))

  ;; Symbol
  (let* ([u1 (uir-symbol "my-var")]
         [s1 (uir->sexp u1)]
         [u2 (sexp->uir s1)])
    (check-equal? s1 '(symbol "my-var") "uir-symbol sexp")
    (check-equal? (uir-symbol-name u2) "my-var" "uir-symbol round-trip"))

  ;; List
  (let* ([u1 (uir-list (list (uir-null) (uir-bool #t)))]
         [s1 (uir->sexp u1)]
         [u2 (sexp->uir s1)])
    (check-equal? s1 '(list (null) (bool #t)) "uir-list sexp")
    (check-equal? (length (uir-list-items u2)) 2 "uir-list round-trip items"))

  ;; Record
  (let* ([u1 (uir-record (list (cons (uir-string "a") (uir-number "1"))))]
         [s1 (uir->sexp u1)]
         [u2 (sexp->uir s1)])
    (check-equal? s1 '(record ((string "a") (number "1"))) "uir-record sexp")
    (check-equal? (length (uir-record-entries u2)) 1 "uir-record round-trip entries"))

  ;; Fn
  (let* ([u1 (uir-fn #f (list (uir-symbol "x")) (uir-var (uir-symbol "x")) #f #f)]
         [s1 (uir->sexp u1)]
         [u2 (sexp->uir s1)])
    (check-equal? s1 '(fn #f ((symbol "x")) (var (symbol "x")) #f #f) "uir-fn sexp")
    (check-true (uir-fn? u2) "uir-fn round-trip"))

  ;; Call
  (let* ([u1 (uir-call (uir-symbol "f") (list (uir-number "1")))]
         [s1 (uir->sexp u1)]
         [u2 (sexp->uir s1)])
    (check-equal? s1 '(call (symbol "f") ((number "1"))) "uir-call sexp")
    (check-equal? (uir-symbol-name (uir-call-callee u2)) "f" "uir-call round-trip"))

  ;; Let
  (let* ([u1 (uir-let (uir-symbol "x") (uir-number "1") (uir-var (uir-symbol "x")))]
         [s1 (uir->sexp u1)]
         [u2 (sexp->uir s1)])
    (check-equal? s1 '(let (symbol "x") (number "1") (var (symbol "x"))) "uir-let sexp")
    (check-true (uir-let? u2) "uir-let round-trip"))

  ;; Var
  (let* ([u1 (uir-var (uir-symbol "x"))]
         [s1 (uir->sexp u1)]
         [u2 (sexp->uir s1)])
    (check-equal? s1 '(var (symbol "x")) "uir-var sexp")
    (check-equal? (uir-symbol-name (uir-var-name u2)) "x" "uir-var round-trip"))

  ;; Set!
  (let* ([u1 (uir-set! (uir-symbol "x") (uir-number "99"))]
         [s1 (uir->sexp u1)]
         [u2 (sexp->uir s1)])
    (check-equal? s1 '(set! (symbol "x") (number "99")) "uir-set! sexp")
    (check-true (uir-set!? u2) "uir-set! round-trip"))

  ;; If
  (let* ([u1 (uir-if (uir-bool #t) (uir-number "1") (uir-number "0"))]
         [s1 (uir->sexp u1)]
         [u2 (sexp->uir s1)])
    (check-equal? s1 '(if (bool #t) (number "1") (number "0")) "uir-if sexp")
    (check-true (uir-bool-value (uir-if-test u2)) "uir-if round-trip"))

  ;; Block
  (let* ([u1 (uir-block (list (uir-number "1") (uir-number "2")))]
         [s1 (uir->sexp u1)]
         [u2 (sexp->uir s1)])
    (check-equal? s1 '(block ((number "1") (number "2"))) "uir-block sexp")
    (check-equal? (length (uir-block-stmts u2)) 2 "uir-block round-trip"))

  ;; Return
  (let* ([u1 (uir-return (uir-number "42"))]
         [s1 (uir->sexp u1)]
         [u2 (sexp->uir s1)])
    (check-equal? s1 '(return (number "42")) "uir-return sexp")
    (check-equal? (uir-number-value (uir-return-value u2)) "42" "uir-return round-trip"))

  ;; Class
  (let* ([u1 (uir-class (uir-symbol "Foo") '()
                        (list (uir-field (uir-symbol "x") (uir-symbol "int") (uir-number "1")))
                        (list (uir-method (uir-symbol "m") '() (uir-null) 'public)))]
         [s1 (uir->sexp u1)]
         [u2 (sexp->uir s1)])
    (check-true (uir-class? u2) "uir-class round-trip type")
    (check-equal? (uir-symbol-name (uir-class-name u2)) "Foo" "uir-class round-trip name"))

  ;; Method
  (let* ([u1 (uir-method (uir-symbol "f") (list (uir-symbol "x")) (uir-var (uir-symbol "x")) 'public)]
         [s1 (uir->sexp u1)]
         [u2 (sexp->uir s1)])
    (check-true (uir-method? u2) "uir-method round-trip type")
    (check-equal? (uir-method-visibility u2) 'public "uir-method round-trip visibility"))

  ;; Field
  (let* ([u1 (uir-field (uir-symbol "y") (uir-null) (uir-null))]
         [s1 (uir->sexp u1)]
         [u2 (sexp->uir s1)])
    (check-true (uir-field? u2) "uir-field round-trip type")
    (check-true (uir-null? (uir-field-init u2)) "uir-field round-trip init"))

  ;; New
  (let* ([u1 (uir-new (uir-symbol "Pair") (list (uir-number "1") (uir-number "2")))]
         [s1 (uir->sexp u1)]
         [u2 (sexp->uir s1)])
    (check-true (uir-new? u2) "uir-new round-trip type")
    (check-equal? (uir-symbol-name (uir-new-class u2)) "Pair" "uir-new round-trip class"))

  ;; Interface
  (let* ([u1 (uir-interface (uir-symbol "Ser") '())]
         [s1 (uir->sexp u1)]
         [u2 (sexp->uir s1)])
    (check-true (uir-interface? u2) "uir-interface round-trip type"))

  ;; Module
  (let* ([u1 (uir-module (uir-symbol "m") '() '() (uir-null))]
         [s1 (uir->sexp u1)]
         [u2 (sexp->uir s1)])
    (check-true (uir-module? u2) "uir-module round-trip type"))

  ;; Import
  (let* ([u1 (uir-import (uir-symbol "lib") (list (uir-symbol "x")))]
         [s1 (uir->sexp u1)]
         [u2 (sexp->uir s1)])
    (check-true (uir-import? u2) "uir-import round-trip type")
    (check-equal? (uir-symbol-name (uir-import-source u2)) "lib" "uir-import round-trip source"))

  ;; Export
  (let* ([u1 (uir-export (list (uir-symbol "run")))]
         [s1 (uir->sexp u1)]
         [u2 (sexp->uir s1)])
    (check-true (uir-export? u2) "uir-export round-trip type")
    (check-equal? (length (uir-export-names u2)) 1 "uir-export round-trip names"))

  ;; Component
  (let* ([u1 (uir-component (uir-symbol "Btn") '() '() '()
                            (uir-element (uir-symbol "button") '() '() '()))]
         [s1 (uir->sexp u1)]
         [u2 (sexp->uir s1)])
    (check-true (uir-component? u2) "uir-component round-trip type"))

  ;; Element
  (let* ([u1 (uir-element (uir-symbol "span") '()
                          (list (uir-text-node (uir-string "hi"))) '())]
         [s1 (uir->sexp u1)]
         [u2 (sexp->uir s1)])
    (check-true (uir-element? u2) "uir-element round-trip type")
    (check-equal? (length (uir-element-children u2)) 1 "uir-element round-trip children"))

  ;; Attribute
  (let* ([u1 (uir-attribute (uir-symbol "class") (uir-string "box"))]
         [s1 (uir->sexp u1)]
         [u2 (sexp->uir s1)])
    (check-true (uir-attribute? u2) "uir-attribute round-trip type")
    (check-equal? (uir-string-value (uir-attribute-value u2)) "box" "uir-attribute round-trip value"))

  ;; Event
  (let* ([u1 (uir-event (uir-string "click") (uir-fn #f '() (uir-null) #f #f))]
         [s1 (uir->sexp u1)]
         [u2 (sexp->uir s1)])
    (check-true (uir-event? u2) "uir-event round-trip type"))

  ;; Slot
  (let* ([u1 (uir-slot (uir-symbol "children") (uir-null))]
         [s1 (uir->sexp u1)]
         [u2 (sexp->uir s1)])
    (check-true (uir-slot? u2) "uir-slot round-trip type"))

  ;; Text node
  (let* ([u1 (uir-text-node (uir-string "Hello"))]
         [s1 (uir->sexp u1)]
         [u2 (sexp->uir s1)])
    (check-true (uir-text-node? u2) "uir-text-node round-trip type")
    (check-equal? (uir-string-value (uir-text-node-content u2)) "Hello" "uir-text-node round-trip content"))

  ;; Style
  (let* ([u1 (uir-style (list (cons (uir-string "color") (uir-string "blue"))))]
         [s1 (uir->sexp u1)]
         [u2 (sexp->uir s1)])
    (check-true (uir-style? u2) "uir-style round-trip type")
    (check-equal? (length (uir-style-styles u2)) 1 "uir-style round-trip entries"))

  ;; Effect
  (let* ([u1 (uir-effect (list (uir-symbol "x")) (uir-block (list (uir-null))))]
         [s1 (uir->sexp u1)]
         [u2 (sexp->uir s1)])
    (check-true (uir-effect? u2) "uir-effect round-trip type"))

  ;; State
  (let* ([u1 (uir-state (uir-symbol "loading") (uir-bool #f))]
         [s1 (uir->sexp u1)]
         [u2 (sexp->uir s1)])
    (check-true (uir-state? u2) "uir-state round-trip type")
    (check-equal? (uir-symbol-name (uir-state-name u2)) "loading" "uir-state round-trip name"))

  ;; Effect entry
  (let* ([u1 (uir-effect-entry (uir-symbol "llm") 'may
                               (list (uir-symbol "query")) 5
                               (uir-symbol "reads=search_index"))]
         [s1 (uir->sexp u1)]
         [u2 (sexp->uir s1)])
    (check-equal? s1 '(effect-entry (symbol "llm") may ((symbol "query")) 5 (symbol "reads=search_index"))
                  "uir-effect-entry sexp")
    (check-equal? (uir-effect-entry-modality u2) 'may "uir-effect-entry round-trip modality")
    (check-equal? (uir-effect-entry-budget u2) 5 "uir-effect-entry round-trip budget"))

  ;; Effect row
  (let* ([u1 (uir-effect-row (list (uir-effect-entry (uir-symbol "db") 'must-not '() #f #f)))]
         [s1 (uir->sexp u1)]
         [u2 (sexp->uir s1)])
    (check-equal? s1 '(effect-row (effect-entry (symbol "db") must-not () #f #f))
                  "uir-effect-row sexp")
    (check-equal? (length (uir-effect-row-entries u2)) 1 "uir-effect-row round-trip entries"))

  ;; Subtype
  (let* ([u1 (uir-subtype (uir-effect-row '())
                          (uir-effect-row (list (uir-effect-entry (uir-symbol "net") 'may '() #f #f))))]
         [s1 (uir->sexp u1)]
         [u2 (sexp->uir s1)])
    (check-true (uir-subtype? u2) "uir-subtype round-trip type")
    (check-equal? (length (uir-effect-row-entries (uir-subtype-right u2))) 1 "uir-subtype round-trip right")))

;; A while loop is the loop-as-data control-flow node; it must round-trip in
;; both directions (the codec is schema-driven, so this also guards the
;; `while` node kind's schema entry).
(define (test-while-roundtrip)
  (define u1 (uir-while (uir-bool #t)
                        (uir-set! (uir-symbol "x") (uir-number "1"))
                        (uir-null)))
  (check-equal? (sexp->uir (uir->sexp u1)) u1 "uir-while sexp round-trip")
  (check-equal? (json->uir (uir->json u1)) u1 "uir-while json round-trip"))

;; ── JSON Serialization Round-Trip ───────────────────────────────────

(define (test-json-roundtrip)
  (define json-str "{\"a\": 1, \"b\": true}")

  ;; Represent as UIR: record with string keys and number/bool values
  (define u1
    (uir-record
     (list (cons (uir-string "a") (uir-number "1"))
           (cons (uir-string "b") (uir-bool #t)))))
  (define j (uir->json u1))
  (define u2 (json->uir j))
  (check-true (uir-record? u2) "json round-trip preserves record type")
  (check-equal? (length (uir-record-entries u2)) 2 "json round-trip preserves entries"))

;; ── Schema versioning and migration ────────────────────────────────

(define (test-versioned-sexp)
  (define u (uir-number "42"))
  (define vsexp (uir->versioned-sexp u))
  (check-equal? (car vsexp) 'uir "version envelope is tagged 'uir")
  (check-equal? (cadr vsexp) UIR-SCHEMA-VERSION "envelope carries the current schema version")
  (check-equal? (versioned-sexp->uir vsexp) u "versioned round-trip is identity")
  (check-equal? (json->uir (uir->json u)) u "json round-trip goes through the version envelope"))

(define (test-schema-migration)
  ;; The migration chain is what turns a struct field-count change into a
  ;; controlled event. Exercise it with a toy v0->v1 migration over a local
  ;; table so the global registry is left untouched.
  (define (v0->v1 p)
    (cond [(and (pair? p) (eq? (car p) 'number) (integer? (cadr p)))
           `(number ,(number->string (cadr p)))]
          [(pair? p)
           (cons (v0->v1 (car p)) (v0->v1 (cdr p)))]
          [else p]))
  (define migrations (make-hasheqv))
  (hash-set! migrations 0 v0->v1)
  ;; v1 -> v2 is the identity (v2 only adds a trailing defaulted payload slot).
  (hash-set! migrations 1 (lambda (p) p))
  (check-equal? (migrate-uir-payload '(number "42") UIR-SCHEMA-VERSION migrations)
                '(number "42")
                "migration at current version is a no-op")
  (check-equal? (migrate-uir-payload '(number 42) 0 migrations)
                '(number "42")
                "v0->v1 migration rewrites bare integers to strings")
  (define u (versioned-sexp->uir `(uir 0 (number 42)) migrations))
  (check-equal? (uir-number-value u) "42" "versioned reader migrates before parsing")
  (check-exn exn:fail?
             (lambda () (versioned-sexp->uir '(uir 0 (number 42))))
             "missing migration raises")
  (check-exn exn:fail?
             (lambda () (versioned-sexp->uir `(uir ,(add1 UIR-SCHEMA-VERSION) (number "42"))))
             "future schema version raises")
  (check-exn exn:fail?
             (lambda () (versioned-sexp->uir '(number "42")))
             "unversioned sexp raises"))

;; ── uir-tag ────────────────────────────────────────────────────────

(define (test-uir-tag)
  (check-equal? (uir-tag (uir-null)) 'null)
  (check-equal? (uir-tag (uir-bool #t)) 'bool)
  (check-equal? (uir-tag (uir-number "1")) 'number)
  (check-equal? (uir-tag (uir-string "x")) 'string)
  (check-equal? (uir-tag (uir-list '())) 'list)
  (check-equal? (uir-tag (uir-record '())) 'record)
  (check-equal? (uir-tag (uir-symbol "x")) 'symbol)
  (check-equal? (uir-tag (uir-fn #f '() (uir-null) #f #f)) 'fn)
  (check-equal? (uir-tag (uir-call (uir-null) '())) 'call)
  (check-equal? (uir-tag (uir-let (uir-symbol "x") (uir-null) (uir-null))) 'let)
  (check-equal? (uir-tag (uir-var (uir-symbol "x"))) 'var)
  (check-equal? (uir-tag (uir-set! (uir-symbol "x") (uir-null))) 'set!)
  (check-equal? (uir-tag (uir-if (uir-null) (uir-null) (uir-null))) 'if)
  (check-equal? (uir-tag (uir-block '())) 'block)
  (check-equal? (uir-tag (uir-return (uir-null))) 'return)
  (check-equal? (uir-tag (uir-class (uir-symbol "A") '() '() '())) 'class)
  (check-equal? (uir-tag (uir-method (uir-symbol "m") '() (uir-null) 'public)) 'method)
  (check-equal? (uir-tag (uir-field (uir-symbol "f") (uir-null) (uir-null))) 'field)
  (check-equal? (uir-tag (uir-new (uir-symbol "X") '())) 'new)
  (check-equal? (uir-tag (uir-interface (uir-symbol "I") '())) 'interface)
  (check-equal? (uir-tag (uir-module (uir-symbol "M") '() '() (uir-null))) 'module)
  (check-equal? (uir-tag (uir-import (uir-symbol "L") '())) 'import)
  (check-equal? (uir-tag (uir-export '())) 'export)
  (check-equal? (uir-tag (uir-component (uir-symbol "X") '() '() '() (uir-null))) 'component)
  (check-equal? (uir-tag (uir-element (uir-symbol "div") '() '() '())) 'element)
  (check-equal? (uir-tag (uir-attribute (uir-symbol "x") (uir-null))) 'attribute)
  (check-equal? (uir-tag (uir-event (uir-string "e") (uir-null))) 'event)
  (check-equal? (uir-tag (uir-slot (uir-symbol "s") (uir-null))) 'slot)
  (check-equal? (uir-tag (uir-text-node (uir-string ""))) 'text-node)
  (check-equal? (uir-tag (uir-style '())) 'style)
  (check-equal? (uir-tag (uir-effect '() (uir-null))) 'effect)
  (check-equal? (uir-tag (uir-state (uir-symbol "x") (uir-null))) 'state)
  (check-equal? (uir-tag (uir-effect-row '())) 'effect-row)
  (check-equal? (uir-tag (uir-effect-entry (uir-symbol "e") 'may '() #f #f)) 'effect-entry)
  (check-equal? (uir-tag (uir-subtype (uir-effect-row '()) (uir-effect-row '()))) 'subtype))

;; ── Optional parameter kinds (trailing defaulted payload field) ────

(define (test-typed-param-construction)
  (define p (uir-typed-param (uir-symbol "x") (uir-symbol "int") #f 'kw-only))
  (check-true (uir? p) "uir-typed-param satisfies uir?")
  (check-equal? (uir-tag p) 'typed-param "uir-typed-param tag is 'typed-param")
  (check-equal? (uir-symbol-name (uir-typed-param-name p)) "x" "uir-typed-param name")
  (check-equal? (uir-symbol-name (uir-typed-param-type p)) "int" "uir-typed-param type")
  (check-false (uir-typed-param-default p) "uir-typed-param default is #f")
  (check-equal? (uir-typed-param-kind p) 'kw-only "uir-typed-param kind")
  (check-equal? (uir-typed-param-kind
                 (uir-typed-param (uir-symbol "y") #f #f #f))
                #f
                "a parameter without a kind reads back as #f"))

(define (test-typed-param-serialization)
  ;; The kind field is trailing and defaulted: the payload omits it at #f and
  ;; writes it as a string otherwise, so JSON's symbol/string mapping keeps it.
  (define with-kind (uir-typed-param (uir-symbol "x") #f #f 'var-positional))
  (define without-kind (uir-typed-param (uir-symbol "x") #f #f #f))
  (check-equal? (uir->sexp without-kind)
                '(typed-param (symbol "x") #f #f)
                "defaulted kind field is omitted from the sexp payload")
  (check-equal? (uir->sexp with-kind)
                '(typed-param (symbol "x") #f #f "var-positional")
                "a set kind serializes as a string")
  (check-equal? (sexp->uir (uir->sexp with-kind)) with-kind
                "typed-param sexp round trip")
  (check-equal? (json->uir (uir->json with-kind)) with-kind
                "typed-param JSON round trip")
  (check-equal? (uir->json with-kind)
                "[\"uir\",2,[\"typed-param\",[\"symbol\",\"x\"],false,false,\"var-positional\"]]"
                "typed-param JSON payload")
  ;; Legacy payloads stay readable: the v2 payload extension is the optional
  ;; trailing kind, and the v1 -> v2 migration is the identity.
  (check-equal? (sexp->uir '(typed-param (symbol "x") #f #f)) without-kind
                "three-element typed-param payload fills kind = #f")
  (check-equal? (versioned-sexp->uir '(uir 1 (typed-param (symbol "x") #f #f)))
                without-kind
                "v1 payload is read through the identity migration")
  (check-equal? (versioned-sexp->uir (uir->versioned-sexp with-kind)) with-kind
                "current versioned payload round trip")
  ;; The kind domain is closed.
  (check-exn exn:fail?
             (lambda () (sexp->uir '(typed-param (symbol "x") #f #f "bogus")))
             "an unknown parameter kind is refused when reading sexp")
  (check-exn exn:fail?
             (lambda ()
               (json->uir
                "[\"uir\",2,[\"typed-param\",[\"symbol\",\"x\"],false,false,\"bogus\"]]"))
             "an unknown parameter kind is refused when reading JSON"))

(define (test-schema-field-defaults)
  ;; The typed-param kind slot is described as data: a trailing `param-kind`
  ;; field with a #f default.
  (define d (schema-kind-decl-by-tag UIR-SCHEMA 'typed-param))
  (check-equal? (schema-kind-field-names d) '(name type default kind)
                "typed-param schema fields")
  (check-equal? (schema-kind-field-codecs d)
                '(uir optional-uir optional-uir param-kind)
                "typed-param schema codecs")
  (check-equal? (schema-kind-field-defaults d)
                '(required required required false)
                "typed-param schema default markers")
  (define l (layout-by-tag 'typed-param UIR-LAYOUTS))
  (check-equal? (uir-layout-defaults l) '(required required required false)
                "the generated layout carries the default markers")
  ;; Defaulted fields must be a trailing suffix, must be #f-defaultable codecs,
  ;; and must use a known marker.
  (check-exn exn:fail?
             (lambda ()
               (check-schema!
                (schema-append-kind
                 UIR-SCHEMA 'bad-defaulted 'uir-bad-defaulted #f
                 '((a uir required) (b optional-uir false) (c uir required)))))
             "a required field after a defaulted field is refused")
  (check-exn exn:fail?
             (lambda ()
               (check-schema!
                (schema-append-kind
                 UIR-SCHEMA 'bad-defaulted 'uir-bad-defaulted #f
                 '((a uir nonsense)))))
             "an unknown default marker is refused")
  (check-exn exn:fail?
             (lambda ()
               (check-schema!
                (schema-append-kind
                 UIR-SCHEMA 'bad-defaulted 'uir-bad-defaulted #f
                 '((a uir false)))))
             "a non-#f-defaultable codec cannot default to #f"))

;; ── The schema as self-describing UIR data ─────────────────────────

(define (test-schema-is-uir-data)
  ;; The schema is a UIR value: node kinds, fields, and types are data.
  (check-true (uir? UIR-SCHEMA) "the schema value is a UIR node")
  (check-equal? (uir-tag UIR-SCHEMA) 'schema "the schema value's UIR tag")
  (check-true (uir-schema? UIR-SCHEMA) "the schema value is a uir-schema")
  (check-true (> (length (schema-kind-decls UIR-SCHEMA)) 80)
              "the schema declares every node kind")
  (check-true (uir? (first (schema-kind-decls UIR-SCHEMA)))
              "node kind declarations are themselves UIR data")
  ;; The fixed point: the hand-written kernel alone writes the schema to UIR
  ;; data and reads it back to the identical value. KERNEL-LAYOUTS knows only
  ;; the bootstrap vocabulary, so this needs no generated layout.
  (check-equal? (sexp->uir (uir->sexp UIR-SCHEMA KERNEL-LAYOUTS) KERNEL-LAYOUTS)
                UIR-SCHEMA
                "kernel-only UIR round trip of the schema value")
  ;; With the full registry the schema round-trips as ordinary UIR data.
  (check-equal? (sexp->uir (uir->sexp UIR-SCHEMA)) UIR-SCHEMA
                "full UIR round trip of the schema value")
  (check-equal? (json->uir (uir->json UIR-SCHEMA)) UIR-SCHEMA
                "JSON round trip of the schema value"))

(define (test-schema-self-describes)
  ;; The schema validates against the declarations it itself contains.
  (check-true (check-schema! UIR-SCHEMA)
              "the schema validates under its own declarations")
  (check-equal?
   (for/list ([d (in-list (schema-kind-decls UIR-SCHEMA))]
              #:when (uir-node-kind-kernel? d))
     (uir-node-kind-kind d))
   '(schema node-kind schema-field)
   "the schema declares its own bootstrap node kinds")
  ;; The kernel-owned declarations match the hand-written kernel exactly.
  (for ([l (in-list KERNEL-LAYOUTS)])
    (define d (schema-kind-decl-by-tag UIR-SCHEMA (uir-layout-tag l)))
    (check-true (uir-node-kind? d)
                (format "kernel kind ~a is declared by the schema" (uir-layout-tag l)))
    (check-equal? (schema-kind-field-names d) (uir-layout-field-names l)
                  (format "kernel kind ~a fields match the kernel" (uir-layout-tag l)))
    (check-equal? (schema-kind-field-codecs d) (uir-layout-codecs l)
                  (format "kernel kind ~a codecs match the kernel" (uir-layout-tag l))))
  ;; Every declared non-kernel kind was generated into the layout registry.
  (for ([d (in-list (schema-kind-decls UIR-SCHEMA))]
        #:unless (uir-node-kind-kernel? d))
    (define tag (uir-node-kind-kind d))
    (define l (layout-by-tag tag UIR-LAYOUTS))
    (check-true (and l #t) (format "node kind ~a has a generated layout" tag))
    (check-equal? (schema-kind-field-codecs d) (uir-layout-codecs l)
                  (format "node kind ~a codecs match the schema" tag))))

(define (test-schema-versioning)
  (check-equal? UIR-SCHEMA-VERSION (uir-schema-version UIR-SCHEMA)
                "the payload version is declared by the schema value")
  (check-equal? UIR-SCHEMA-VERSION 2
                "v2 adds the optional trailing typed-param kind slot")
  (check-equal? UIR-KERNEL-VERSION (uir-schema-kernel-version UIR-SCHEMA)
                "the schema declares the hand-written kernel version it requires")
  (check-equal? UIR-KERNEL-VERSION 2
                "kernel v2 knows field defaults and the param-kind codec")
  (check-exn exn:fail?
             (lambda ()
               (check-schema! (struct-copy uir-schema UIR-SCHEMA [kernel-version 99])))
             "a schema from another kernel version is refused")
  (check-exn exn:fail?
             (lambda ()
               (check-schema!
                (schema-append-kind UIR-SCHEMA 'bad-kind 'uir-bad-kind #f
                                    '((x nonsense-codec)))))
             "an unknown field codec is refused")
  (check-exn exn:fail?
             (lambda ()
               (check-schema! (schema-append-kind UIR-SCHEMA 'fn 'uir-fn2 #f '())))
             "a duplicate node kind tag is refused")
  (check-exn exn:fail?
             (lambda () (validate-uir-against-schema 42 UIR-SCHEMA))
             "the schema validator refuses a non-UIR value"))

;; An extended schema: the same canonical schema plus one new node kind, added
;; as data only. The generator synthesises its struct, constructor, predicate,
;; accessors, layout, and serialization clause; no core file changes.
(module ext racket
  (require racklr/uir/kernel
           racklr/uir/schema-data
           racklr/uir/schema-check
           racklr/uir/schema-macro
           (for-syntax racklr/uir/kernel
                       racklr/uir/schema-data
                       racklr/uir/schema-check))
  (begin-for-syntax
    (define EXT-SCHEMA
      (schema-append-kind UIR-SCHEMA 'demo 'uir-demo #f
                          '((name uir) (flag bool)))))
  (define EXT-SCHEMA
    (schema-append-kind UIR-SCHEMA 'demo 'uir-demo #f
                        '((name uir) (flag bool))))
  (check-schema! EXT-SCHEMA)
  (define-uir-kinds EXT-SCHEMA)
  (provide (all-defined-out)))

(module+ main
  (require (prefix-in ext: (submod ".." ext)))
  ;; Layout registry plus the one generated from the extended schema value.
  (define ext-layouts (append UIR-LAYOUTS ext:UIR-LAYOUTS))

  (define (test-schema-extension)
    (check-false (schema-kind-decl-by-tag UIR-SCHEMA 'demo)
                 "the canonical schema has no demo kind")
    (define node (ext:uir-demo (uir-symbol "extended") #t))
    (check-true (ext:uir-demo? node) "the generated predicate recognises the new kind")
    (check-equal? (ext:uir-demo-name node) (uir-symbol "extended")
                  "the generated accessor reads the new kind")
    (check-equal? (ext:uir-demo-flag node) #t
                  "the generated accessor reads a scalar field")
    (check-true (uir? node ext-layouts)
                "generic uir? accepts the generated kind through a layout registry")
    (check-equal? (uir-tag node ext-layouts) 'demo
                  "generic uir-tag sees the generated kind")
    (check-false (uir? node)
                 "the canonical registry does not know the generated kind")
    (check-equal? (uir->sexp node ext-layouts) '(demo (symbol "extended") #t)
                  "the generated serializer writes the new kind")
    (check-equal? (sexp->uir (uir->sexp node ext-layouts) ext-layouts) node
                  "the generated reader reads the new kind")
    (check-equal? (json->uir (uir->json node ext-layouts) ext-layouts) node
                  "the generated JSON codec reads and writes the new kind"))

  (test-null-construction)
  (test-bool-construction)
  (test-number-construction)
  (test-string-construction)
  (test-list-construction)
  (test-record-construction)
  (test-symbol-construction)
  (test-fn-construction)
  (test-call-construction)
  (test-let-construction)
  (test-var-construction)
  (test-set-construction)
  (test-if-construction)
  (test-block-construction)
  (test-return-construction)
  (test-class-construction)
  (test-method-construction)
  (test-field-construction)
  (test-new-construction)
  (test-interface-construction)
  (test-module-construction)
  (test-import-construction)
  (test-export-construction)
  (test-component-construction)
  (test-element-construction)
  (test-attribute-construction)
  (test-event-construction)
  (test-slot-construction)
  (test-text-node-construction)
  (test-style-construction)
  (test-effect-construction)
  (test-state-construction)
  (test-effect-entry-construction)
  (test-effect-row-construction)
  (test-subtype-construction)
  (test-sexp-roundtrip)
  (test-while-roundtrip)
  (test-json-roundtrip)
  (test-versioned-sexp)
  (test-schema-migration)
  (test-uir-tag)
  (test-typed-param-construction)
  (test-typed-param-serialization)
  (test-schema-field-defaults)
  (test-schema-is-uir-data)
  (test-schema-self-describes)
  (test-schema-versioning)
  (test-schema-extension)
  (displayln "All uir tests passed."))
