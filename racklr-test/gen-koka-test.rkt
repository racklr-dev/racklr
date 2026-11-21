#lang racket

;; Grammar-generation + tokenizer/layout + parse tests for the Koka front end.
;;
;; The tracked grammar is a Koka 3.x port: layout is Koka's own indentation
;; pass (braces/semicolons inserted from column positions), selected by the
;; grammar's `options { layout = koka; }` and implemented as data in
;; racklr/gend-parser/language-policy.rkt.

(require rackunit
         rackunit/text-ui
         racklr/tree
         racklr/gen-test
         racklr/g4-parse
         racklr/gend-parser)

(define-values (koka-parse koka-tokenize koka-tok-type koka-tok-value)
  (gen-and-load-layout "../grammars/koka/Koka.g4"))

(define (tok-types in)
  (map koka-tok-type (koka-tokenize in)))

(define (parses? in)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (and (koka-parse in) #t)))

;; ── Layout ──────────────────────────────────────────────────────────
;;
;; Koka layout inserts `{`, `;`, `}` from indentation; the parser must never
;; see the NEWLINE tokens that carry the line breaks.

(define parses-samples
  (list
   (cons "module + imports"
         "module fixtures/app\nimport std/core/list\nimport std/core/types")
   (cons "value binding with type"
         "val answer : int = 42")
   (cons "function with signature and effect row"
         "pub fun add( x : int, y : int ) : total int\n  x + y")
   (cons "function with multi-effect row"
         "fun risky( x : int ) : <exn, div> int\n  throw(\"no\")")
   (cons "function with type parameters"
         "pub fun identity<a>( x : a ) : total a\n  x")
   (cons "lambda"
         "val inc = fn(x : int) -> x + 1")
   (cons "if / elif / else"
         "fun sign( n : int ) : total int\n  if n < 0 then 0 - 1\n  elif n == 0 then 0\n  else 1")
   (cons "match with patterns and guards"
         "fun f( xs : list<int> ) : total int\n  match xs\n    Nil -> 0\n    Cons(x, xx) | x > 0 -> x\n    Cons(_, xx) -> f(xx)")
   (cons "braced match"
         "fun g( n : int ) : total int\n  match n { 0 -> 1; _ -> 2 }")
   (cons "type declaration"
         "pub type color\n  Red\n  Green\n  Blue")
   (cons "polymorphic type declaration"
         "pub type maybe<a>\n  Nothing\n  Just(value : a)")
   (cons "struct declaration"
         "pub struct point(\n  x : int,\n  y : int\n)")
   (cons "type alias"
         "pub alias parser<e,a> = () -> <parse|e> a")
   (cons "effect declaration"
         "pub effect state\n  fun get() : int\n  fun set( x : int ) : ()")
   (cons "effect with a ctl operation"
         "effect abort\n  ctl abort( msg : string ) : a")
   (cons "named effect"
         "named effect scope\n  fun scoped() : ()")
   (cons "handler expression"
         "fun handled() : total int\n  with handler<state>\n    return(x) -> x\n    fun get() -> 1\n  42")
   (cons "extern declaration with body"
         "extern ticks() : ndet int\n  c  \"kk_timer_ticks\"\n  js \"_ticks\"")
   (cons "extern import"
         "extern import\n  c  file \"inline-ext\"\n  js file \"inline-ext.js\"")
   (cons "extern with @-name"
         "pub extern @null-any( x : a ) : total null<a>")
   (cons "operator declaration and fixity"
         "pub infixr 60 (+++)\npub fun (+++)( a : int, b : int ) : total int\n  a + b")
   (cons "named and implicit arguments"
         "fun f( ?show : a -> string ) : string\n  ?show(1)\nval x = f(show = fn(i : int) -> \"n\")")
   (cons "bracket and dot continuations"
         "val matrix = [ [1, 2],\n               [3, 4] ]\nfun chained( x : int ) : total int\n  x\n    .inc\n    .inc")
   (cons ":: kind annotation"
         "pub type total :: E\npub type effect-extend :: (X,E) -> E")
   (cons "mask with several effects"
         "fun masked( f : () -> <ndet,div> a ) : <ndet,div> a\n  mask<ndet,div> f()")
   (cons "scoped effect"
         "scoped effect heap<s>\n  fun read() : s")
   (cons "named scoped effect"
         "named scoped effect file<s>\n  fun write() : ()")
   (cons "@open import"
         "@open import std/core/types")
   (cons "conditional import with a backend guard"
         "import std/core/types\nimport backend-mod\n  js backend-mod-js\n  c[host=libc] backend-mod-c")))

(define suite
  (test-suite
   "Koka 3.x grammar/tokenizer"
(test-case "layout inserts braces and semicolons, no NEWLINE reaches parse"
  (check-false (member 'NEWLINE (tok-types "fun f()\n  x")) "no NEWLINE token")
  (check-true (and (member 'OPEN_BRACE (tok-types "fun f()\n  x")) #t) "INDENT is OPEN_BRACE")
  (check-true (and (member 'CLOSE_BRACE (tok-types "fun f()\n  x")) #t) "DEDENT is CLOSE_BRACE")
  (check-true (and (member 'SEMI (tok-types "val x = 1\nval y = 2")) #t) "same column is SEMI"))

(test-case "expression continuation lines do not break the statement"
  ;; A line ending in an operator, or starting with one, continues the
  ;; expression (upstream Layout.hs `isExprContinuation`).
  (check-false (member 'OPEN_BRACE (tok-types "val x = 1 +\n  2")) "no block for a trailing operator")
  (check-false (member 'OPEN_BRACE (tok-types "val x = 1\n  + 2")) "no block for a leading operator")
  (check-false (member 'OPEN_BRACE (tok-types "val x = f(\n  1,\n  2)")) "no block inside brackets")
  (let* ([ts (tok-types "fun f() : total int\n  x\n    .inc")]
         [dot (index-of ts 'DOT)])
    (check-true (and dot #t) "leading dot token present")
    (check-false (and dot (member 'SEMI (take ts dot))) "no statement break before a leading dot")))

(test-case "layout closes blocks at a dedent and at EOF"
  (check-true (parses? "fun f() : total int\n  if True then 1 else 2")
              "if/else inside an indented body")
  (check-true (parses? "fun f() : total int\n  val x = 1\n  x") "two statements at the same column"))

;; ── Lexer ───────────────────────────────────────────────────────────

(test-case "comments are skipped; block comments nest"
  (check-true (parses? "// line comment\nval x = 1") "line comment")
  (check-true (parses? "/* block /* nested */ still a comment */\nval x = 1")
              "nested block comment")
  (check-true (parses? "val x = 1 // trailing comment\nval y = 2") "trailing line comment"))

(test-case "literals"
  (check-true (parses? "val a = 0x2A\nval b = 1_000_000\nval c = 3.14\nval d = 1.0e10\nval e = -7")
              "integer, hex, separators, float, signs")
  (check-true (parses? "val c = 'a'\nval hex = '\\x7F'\nval nl = '\\n'") "char literals and escapes")
  (check-true (parses? "val s = \"a \\\"quoted\\\" word\\t\"") "string escapes")
  (check-true (parses? "val raw-text = r\"C:\\temp\\x\"") "raw string")
  (check-true (parses? "val raw-hash = r#\"a \"quoted\" word\"#") "hash-delimited raw string")
  (check-true (parses? "val multi = r\"\nline one\nline two\"") "multi-line raw string"))

(test-case "angle-bar runs split like upstream"
  (check-true (parses? "val xs : list<list<int>> = []") "nested type arguments close with >>")
  (check-true (parses? "val f = fn(x : int) -> x != 0") "operators containing angle-bar characters"))

(test-case "prefix operators start a new statement, not a continuation"
  (check-true (parses? "fun f() : total bool\n  !True") "body starting with !")
  (check-true (parses? "fun g() : total int\n  ~7") "body starting with ~"))

(test-case "with-bindings accept patterns and wildcards"
  (check-true (parses? "fun w() : total int\n  with _x = 1\n  0") "wildcard-prefixed with binding")
  (check-true (parses? "fun w2() : total int\n  with _ = 1\n  0") "wildcard with binding")
  (check-true (parses? "fun w3() : total int\n  with (a, b) = (1, 2)\n  a") "tuple-pattern with binding"))

(test-case "an empty block comment does not swallow the file"
  (check-true (parses? "/**/ val x = 1") "/* */ followed by a declaration"))

(test-case "identifiers"
  (check-true (parses? "val kebab-case-name = 1") "kebab-case identifier")
  (check-true (parses? "val @primitive = 1") "@-prefixed identifier")
  (check-true (parses? "val odd? = True") "legacy predicate-style identifier (Koka 1.x sources)"))

;; ── Parser ──────────────────────────────────────────────────────────

(test-case "parses Koka 3.x constructs"
  (for ([s (in-list parses-samples)])
    (check-true (parses? (cdr s)) (car s))))

(test-case "rejects malformed programs"
  (check-false (parses? "fun") "keyword without a function")
  (check-false (parses? "val x =") "binding without a value")
  (check-false (parses? "fun f( x : int ) : int\n  x +\n  ") "trailing operator at EOF"))
   ))

(exit (run-tests suite))
