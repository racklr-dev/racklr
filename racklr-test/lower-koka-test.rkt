#lang racket

;; Koka 3.x front-end tests: parse → lower → emit → re-parse, plus UIR-shape
;; assertions for every construct family the lowerer covers.
;;
;; The round-trip contract is the capability battery's: the emitted text must
;; re-parse, and no `?`-prefixed fallback symbol may remain in the UIR (a
;; fallback would mean lowering silently gave up on a construct). Shape
;; assertions pin the semantic content the battery cannot see (typed rows,
;; declarator kinds, handler parameter names, extern targets, …).
;;
;; Run from racklr-test/:
;;   cd racklr-test && PLTCOLLECTS="<checkout>:" racket lower-koka-test.rkt

(require rackunit
         rackunit/text-ui
         racklr/tree
         racklr/uir
         racklr/gen-test
         racklr/lower-koka
         racklr/emit-koka)

(define-values (koka-parse koka-tokenize koka-tok-type koka-tok-value)
  (gen-and-load-layout "../grammars/koka/Koka.g4"))

;; ── helpers ─────────────────────────────────────────────────────────

(define (fallback-symbol? uir)
  (cond
    [(uir-symbol? uir)
     (define s (uir-symbol-name uir))
     (and (> (string-length s) 0) (char=? (string-ref s 0) #\?))]
    [(struct? uir)
     (for/or ([f (in-list (cdr (vector->list (struct->vector uir))))])
       (fallback-symbol? f))]
    [(pair? uir) (or (fallback-symbol? (car uir)) (fallback-symbol? (cdr uir)))]
    [else #f]))

(define (parse! src)
  (define cst (koka-parse src))
  (check-true (any-tree? cst) (format "parse ~s" src))
  cst)

(define (lower! src)
  (lower-koka (parse! src) koka-tok-type koka-tok-value))

;; Round-trip one source text; returns the UIR for shape assertions.
(define (check-roundtrip src)
  (define uir (lower! src))
  (check-true (uir? uir) (format "lower ~s" src))
  (check-false (fallback-symbol? uir)
               (format "no unhandled-node fallback in ~s" src))
  (define emitted (emit-koka uir))
  (check-true (and (string? emitted) (positive? (string-length emitted)))
              (format "emit produced text for ~s" src))
  (check-true (any-tree? (koka-parse emitted))
              (format "re-parse emitted text for ~s: ~a" src emitted))
  uir)

(define (stmt-tags uir)
  (map uir-tag (uir-block-stmts uir)))

;; ── declarations: module / imports ──────────────────────────────────

(define (test-module-and-imports)
  (define uir (check-roundtrip
               "module fixtures/app/main\nimport std/core/types\npub import std/text/parse\nimport elem = sys/dom/html/htmlElement\npub fun answer() : total int\n  42"))
  (check-true (and (member 'module (stmt-tags uir)) #t)
              "module header lowers to uir-module")
  (check-true (for/or ([s (in-list (uir-block-stmts uir))])
                (and (uir-import? s)
                     (equal? (uir-symbol-name (uir-import-source s)) "std/core/types")))
              "import lowers to uir-import with the qualified path")
  (check-true (for/or ([s (in-list (uir-block-stmts uir))])
                (and (uir-import-alias? (flatten-decorated s))))
              "aliased import lowers to uir-import-alias"))

(define (flatten-decorated node)
  (if (uir-decorated? node) (flatten-decorated (uir-decorated-inner node)) node))

;; ── functions: type params, parameter types, result type, effect row ─

(define (test-function-signatures)
  (define uir (check-roundtrip
               "pub fun swap<a, b>(pair : (a, b)) : total (b, a)\n  (pair.snd, pair.fst)\nfun risky(n : int) : <exn, div> int\n  1\npub fun mapped<a, b, e>(xs : list<a>, f : (a) -> e b) : e list<b>\n  xs.map(f)"))
  (define fns (filter uir-fn? (map flatten-decorated (uir-block-stmts uir))))
  (check-equal? (length fns) 3 "three function declarations lower")
  (define swap (first fns))
  (check-equal? (uir-symbol-name (uir-fn-name swap)) "swap" "function name")
  (check-equal? (length (uir-fn-params swap)) 1 "typed parameter lowers")
  (check-equal? (uir-symbol-name (uir-typed-param-name (first (uir-fn-params swap)))) "pair")
  (check-true (uir-effect-row? (uir-fn-effects swap)) "declared `total` lowers to an empty row")
  (check-equal? (length (uir-effect-row-entries (uir-fn-effects swap))) 0
                "empty row for total")
  (define risky (second fns))
  (check-equal? (map (λ (e) (uir-symbol-name (uir-effect-entry-name e)))
                     (uir-effect-row-entries (uir-fn-effects risky)))
                '("exn" "div")
                "declared effect row keeps both entries")
  (define mapped (third fns))
  (check-equal? (map (λ (e) (uir-symbol-name (uir-effect-entry-name e)))
                     (uir-effect-row-entries (uir-fn-effects mapped)))
                '("e")
                "effect-variable row lowers to one entry"))

(define (test-type-parameters-are-decorators)
  (define fn (flatten-decorated (first (uir-block-stmts
                                        (lower! "pub fun id<a>(x : a) : a\n  x")))))
  (define-values (mods tps inner)
    (let loop ([n (first (uir-block-stmts (lower! "pub fun id<a>(x : a) : a\n  x")))]
                [acc '()])
      (if (uir-decorated? n)
          (loop (uir-decorated-inner n)
                (append acc (uir-decorated-decorators n)))
          (values acc '() n))))
  (check-true (pair? mods) "pub is carried as a declaration decorator")
  (check-true (for/or ([d (in-list mods)] #:when (uir-type-params? d))
                (pair? (uir-type-params-params d)))
              "explicit type parameters are carried as a uir-type-params decorator")
  (check-true (uir-fn? inner) "the decorated inner node is the function"))

;; ── bindings: val / var / := ────────────────────────────────────────

(define (test-bindings)
  (define uir (check-roundtrip
               "pub val greeting : string = \"hello\"\nval numbers = [1, 2, 3]\nfun counter(start : int) : total int\n  var n := start\n  n := n + 1\n  val step = 2\n  n + step"))
  (check-true (for/or ([s (in-list (uir-block-stmts uir))]) (uir-ann-set!? (flatten-decorated s)))
              "typed top-level val lowers to uir-ann-set!")
  (check-true (for/or ([s (in-list (uir-block-stmts uir))])
                (and (uir-let? (flatten-decorated s))
                     (equal? (uir-symbol-name (uir-let-name (flatten-decorated s)))
                             "numbers")))
              "plain val lowers to uir-let")
  (define fn (for/first ([s (in-list (uir-block-stmts uir))]
                         #:when (uir-fn? (flatten-decorated s)))
               (flatten-decorated s)))
  (define body-stmts (uir-block-stmts (uir-fn-body fn)))
  (check-true (for/or ([s (in-list body-stmts)])
                (and (uir-decorated? s)
                     (equal? (uir-symbol-name (first (uir-decorated-decorators s))) "var")))
              "local var declaration is a var-decorated binding")
  (check-true (for/or ([s (in-list body-stmts)]) (uir-set!? s))
              "`:=` assignment lowers to uir-set!"))

;; ── type declarations: enum / struct / alias ────────────────────────

(define (test-type-declarations)
  (define uir (check-roundtrip
               "pub type shape\n  Circle(radius : int)\n  Rect(width : int, height : int)\npub struct point(x : int, y : int = 0)\npub alias pair<a> = (a, a)\ntype global-scope :: S"))
  (define stmts (map flatten-decorated (uir-block-stmts uir)))
  (check-true (for/or ([s (in-list stmts)])
                (and (uir-enum? s) (equal? (uir-symbol-name (uir-enum-name s)) "shape")))
              "algebraic type lowers to uir-enum")
  (define enum (for/first ([s (in-list stmts)] #:when (uir-enum? s)) s))
  (check-equal? (map (λ (v) (uir-symbol-name (uir-enum-variant-name v)))
                     (uir-enum-variants enum))
                '("Circle" "Rect") "constructors lower to variants")
  (define struct (for/first ([s (in-list stmts)] #:when (uir-struct? s)) s))
  (check-equal? (uir-symbol-name (uir-struct-name struct)) "point" "struct lowers to uir-struct")
  (check-equal? (length (uir-struct-fields struct)) 2 "struct fields lower")
  (check-true (not (not (uir-typed-param-default (second (uir-struct-fields struct)))))
              "struct field default survives")
  (define alias (for/first ([s (in-list stmts)] #:when (uir-type-alias? s)) s))
  (check-equal? (length (uir-type-alias-type-params alias)) 1
                "alias type parameters lower into the alias node")
  (check-roundtrip "pub type nmd :: X"))

;; ── effect declarations ─────────────────────────────────────────────

(define (test-effect-declarations)
  (define uir (check-roundtrip
               "pub effect state<s>\n  fun get() : s\n  fun put(x : s) : ()\npub effect exn\n  final ctl throw-exn(exn : exception) : a\nnamed effect logger\n  val current-name : string\neffect fun total-count() : int"))
  (define decls (filter uir-effect-decl? (map flatten-decorated (uir-block-stmts uir))))
  (check-equal? (length decls) 4 "four effect declarations lower")
  (check-equal? (map (λ (op) (uir-symbol-name (uir-effect-op-name op)))
                     (uir-effect-decl-ops (first decls)))
                '("get" "put") "fun operations lower")
  (check-equal? (length (uir-effect-decl-ops (second decls))) 1 "ctl operation lowers")
  (check-true (for/or ([op (in-list (uir-effect-decl-ops (second decls)))])
                (and (uir-decorated? op)
                     (for/or ([d (in-list (uir-decorated-decorators op))])
                       (and (uir-symbol? d) (equal? (uir-symbol-name d) "final")))))
              "a `final ctl` operation keeps its control kind decorator")
  (check-true (for/or ([d (in-list decls)])
                (equal? (uir-symbol-name (uir-effect-decl-name d)) "logger"))
              "named effect lowers")
  (check-roundtrip "pub effect foo\n  val peek : int"))

;; ── extern declarations and extern imports ───────────────────────────

(define (test-externs)
  (define uir (check-roundtrip
               "extern import\n  c  file \"ext.c\"\n  js file \"ext.js\"\nextern main-console : forall<a, e> (main : () -> e a) -> e a\nextern ext-add(x : int, y : int) : int\n  c  inline \"((#1) + (#2))\"\npub extern @ext-null(x : int) : maybe<int>\n  js inline \"(#1)\""))
  (define stmts (map flatten-decorated (uir-block-stmts uir)))
  (check-equal? (length (filter uir-extern-import? stmts)) 2
                "each extern-import target group lowers to uir-extern-import")
  (define target (uir-extern-import-target (first (filter uir-extern-import? stmts))))
  (check-equal? (uir-symbol-name target) "c" "extern-import target survives")
  (define ev (for/first ([s (in-list stmts)] #:when (uir-extern-fun? s)) s))
  (check-true (uir-extern-fun? ev) "extern with parameters lowers to uir-extern-fun")
  (check-equal? (length (uir-extern-fun-entries ev)) 1 "extern body entry survives")
  (check-true (uir-extern-value? (for/first ([s (in-list stmts)] #:when (uir-extern-value? s)) s))
              "extern with a type scheme lowers to uir-extern-value"))

;; ── control flow: if/elif/return, match patterns and guards ──────────

(define (test-control-flow)
  (define uir (check-roundtrip
               "fun sign(x : int) : total int\n  if x < 0 then\n    -1\n  elif x == 0 then\n    0\n  else\n    1\nfun early(xs : list<int>) : total int\n  if xs.is-empty then return 0\n  xs.head\nfun classify(x : maybe<int>) : total int\n  match x\n    Just(0) -> 0\n    Just(v) | v > 0 -> v\n    whole as Just(v) -> 0 - v\n    Nothing -> 0"))
  (define fns (filter uir-fn? (map flatten-decorated (uir-block-stmts uir))))
  (check-true (for/or ([f (in-list fns)])
                (uir-if? (first (uir-block-stmts (uir-fn-body f)))))
              "if/elif lowers to nested uir-if")
  (check-true (for/or ([f (in-list fns)])
                (uir-match? (first (uir-block-stmts (uir-fn-body f)))))
              "match lowers to uir-match")
  (define match (for/first ([f (in-list fns)]
                            #:when (uir-match? (first (uir-block-stmts (uir-fn-body f)))))
                  (first (uir-block-stmts (uir-fn-body f)))))
  (check-equal? (length (uir-match-cases match)) 4 "four match cases")
  (check-true (for/or ([c (in-list (uir-match-cases match))])
                (not (uir-null? (uir-case-guard c))))
              "a match guard is carried in uir-case-guard")
  (check-true (for/or ([c (in-list (uir-match-cases match))])
                (uir-pat-as? (uir-case-pattern c)))
              "as-pattern lowers to uir-pat-as"))

;; ── lambdas, trailing lambdas, dot chains, literals ─────────────────

(define (test-lambdas-and-expressions)
  (define uir (check-roundtrip
               "fun pipeline(steps : list<string>) : total list<string>\n  steps.map(fn(s) s)\nfun apply2(f : (int) -> int, x : int) : total int\n  f(x)\nfun literals() : total (char, float, string)\n  ('a', 1.5e3, r#\"raw \"quoted\" text\"#)"))
  (define fns (filter uir-fn? (map flatten-decorated (uir-block-stmts uir))))
  (check-true (for/or ([f (in-list fns)])
                (let walk ([n (uir-fn-body f)])
                  (cond [(uir-fn? n) #t]
                        [(struct? n) (for/or ([x (in-list (cdr (vector->list (struct->vector n))))]) (walk x))]
                        [(pair? n) (or (walk (car n)) (walk (cdr n)))]
                        [else #f])))
              "lambda/trailing lambda lowers to a nested uir-fn")
  (define lits (first (filter (λ (f) (equal? (uir-symbol-name (uir-fn-name f)) "literals")) fns)))
  (define body (first (uir-block-stmts (uir-fn-body lits))))
  (check-true (for/or ([a (in-list (uir-call-args body))]) (uir-char? a))
              "char literal lowers to uir-char")
  (check-true (for/or ([a (in-list (uir-call-args body))])
                (and (uir-string? a) (string-contains? (uir-string-value a) "\"quoted\"")))
              "raw string value is preserved as a string literal value"))

;; ── fixity, handlers, masks, ctx ────────────────────────────────────

(define (test-fixity)
  (define uir (check-roundtrip "pub infixl 70 (*), (/)\npub infixr 60 (++)"))
  (define fixities (filter uir-fixity? (map flatten-decorated (uir-block-stmts uir))))
  (check-equal? (length fixities) 2 "fixity declarations lower")
  (check-equal? (uir-fixity-precedence (first fixities)) 70 "fixity precedence")
  (check-equal? (length (uir-fixity-names (first fixities))) 2 "fixity names"))

(define (test-handlers)
  (define uir (check-roundtrip
               "effect counter\n  ctl bump(n : int) : ()\nfun run-counter(action : () -> <counter|e> a) : e (a, int)\n  with handler\n    return(x)  (x, 0)\n    ctl bump(n)  n\n  action()\nfun run-handle(action : () -> e a) : e a\n  handle(action)\n    return(x)  x\nfun run-mask(action : () -> e a) : e a\n  with mask<ndet> in action()"))
  (define fns (filter uir-fn? (map flatten-decorated (uir-block-stmts uir))))
  (define handlers
    (append* (for/list ([f (in-list fns)])
               (filter uir-handler?
                       (append* (for/list ([s (in-list (uir-block-stmts (uir-fn-body f)))])
                                  (list (flatten-decorated s))))))))
  (check-true (pair? handlers) "with/handle handler statements lower to uir-handler")
  (check-true (for/or ([h (in-list handlers)])
                (for/or ([op (in-list (uir-handler-ops h))])
                  (pair? (uir-handler-op-params op))))
              "handler operation parameter names survive")
  (check-true (for/or ([f (in-list fns)])
                (let walk ([n (uir-fn-body f)])
                  (cond [(uir-mask? n) (pair? (uir-mask-effects n))]
                        [(struct? n) (for/or ([x (in-list (cdr (vector->list (struct->vector n))))]) (walk x))]
                        [(pair? n) (or (walk (car n)) (walk (cdr n)))]
                        [else #f])))
              "mask statement lowers to uir-mask with effects"))

(define (test-ctx-and-hole)
  (define uir (check-roundtrip
               "fun ctx-use(xs : list<int>) : total list<int>\n  ctx Cons(0, hole)\nfun etahole(xs : list<int>) : total list<int>\n  xs.map(_)\n  xs"))
  (define fns (filter uir-fn? (map flatten-decorated (uir-block-stmts uir))))
  (check-true (for/or ([f (in-list fns)])
                (let walk ([n (uir-fn-body f)])
                  (cond [(and (uir-call? n) (uir-symbol? (uir-call-callee n))
                              (equal? (uir-symbol-name (uir-call-callee n)) "@cctx-create")) #t]
                        [(struct? n) (for/or ([x (in-list (cdr (vector->list (struct->vector n))))]) (walk x))]
                        [(pair? n) (or (walk (car n)) (walk (cdr n)))]
                        [else #f])))
              "ctx expression lowers to the context-create application"))

(define (test-implicit-references)
  (define uir (check-roundtrip
               "pub fun show(xs : list<a>, ?show : a -> e string) : e string\n  show(xs, ?show = show)"))
  (check-true (for/or ([s (in-list (uir-block-stmts uir))])
                (let walk ([n (flatten-decorated s)])
                  (cond [(uir-implicit-ref? n) #t]
                        [(struct? n) (for/or ([x (in-list (cdr (vector->list (struct->vector n))))]) (walk x))]
                        [(pair? n) (or (walk (car n)) (walk (cdr n)))]
                        [else #f])))
              "implicit parameter reference lowers to uir-implicit-ref, not a ?-symbol"))

;; ── suite ───────────────────────────────────────────────────────────

(define suite
  (test-suite
   "Koka 3.x lower/emit"
   (test-case "module and imports" (test-module-and-imports))
   (test-case "function signatures" (test-function-signatures))
   (test-case "type parameters" (test-type-parameters-are-decorators))
   (test-case "bindings" (test-bindings))
   (test-case "type declarations" (test-type-declarations))
   (test-case "effect declarations" (test-effect-declarations))
   (test-case "externs" (test-externs))
   (test-case "control flow" (test-control-flow))
   (test-case "lambdas and expressions" (test-lambdas-and-expressions))
   (test-case "fixity" (test-fixity))
   (test-case "handlers" (test-handlers))
   (test-case "ctx and hole" (test-ctx-and-hole))
   (test-case "implicit references" (test-implicit-references))))

(exit (run-tests suite))
