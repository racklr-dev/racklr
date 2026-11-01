#lang racket

;; Tests for the tracked Pass-1 OCaml grammar: Phase 1 ("grammar
;; acquisition") of docs/plans/ocaml-pipeline.md.
;;
;; The lexer/parser pair under grammars/ocaml/ is authored, not vendored:
;; upstream antlr/grammars-v4 has no ocaml/ directory. See
;; grammars/ocaml/README.md for why, the reference used, and the known limits.
;; OCaml is not layout-sensitive, so this uses plain gen-and-load (no
;; NEWLINE/INDENT/DEDENT injection); gen-and-load also resolves the parser's
;; `tokenVocab = OCamlLexer` from the same tracked directory.

(require rackunit
         rackunit/text-ui
         racklr/tree
         racklr/gen-test)

(define-values (ocaml-parse ocaml-tokenize tok-type tok-value)
  (gen-and-load "../grammars/ocaml/OCamlParser.g4"))

;; ── Helpers ─────────────────────────────────────────────────────────

(define (tok-types in) (map tok-type (ocaml-tokenize in)))

(define (check-token-types in expected)
  (check-equal? (tok-types in) (append expected '(EOF))
                (format "tokenize ~s" in)))

;; Walk every CST node and every token in the generated tree (children of a
;; node are nodes, tokens, or lists produced by star/plus; `none` marks an
;; absent optional).
(define (walk-all n f)
  (cond [(any-tree? n) (f n) (for-each (lambda (c) (walk-all c f)) (any-tree-children n))]
        [(pair? n) (walk-all (car n) f) (walk-all (cdr n) f)]
        [else (f n)]))

(define (node? x) (any-tree? x))

(define (find-node cst tag)
  (let ([found #f])
    (walk-all cst (lambda (x)
                    (if (and (not found) (node? x) (eq? (any-tree-tag x) tag))
                        (set! found x)
                        (void))))
    found))

(define (count-nodes cst tag)
  (let ([n 0])
    (walk-all cst (lambda (x) (if (and (node? x) (eq? (any-tree-tag x) tag))
                                  (set! n (+ n 1))
                                  (void))))
    n))

(define (has-token-type? cst tt)
  (let ([hit #f])
    (walk-all cst
              (lambda (x)
                (if (and (not (node? x)) (not (pair? x)) (not (symbol? x))
                         (not (null? x)) (eq? (tok-type x) tt))
                    (set! hit #t)
                    (void))))
    hit))

;; Parse a fixture that must be valid, assert the CST root and that the
;; expected grammar rule is present somewhere in the tree. A fixture that
;; raises is a real failure, not a skipped check.
(define (check-parses in tag)
  (define cst (ocaml-parse in))
  (check-true (any-tree? cst) (format "parse ~s: expected a tree" in))
  (check-true (and (find-node cst tag) #t)
              (format "parse ~s: expected rule ~a in tree" in tag))
  cst)

;; Child rule/token kinds of a node, e.g. '(mul_expr PLUS mul_expr).
(define (child-kinds n)
  (for/list ([c (any-tree-children n)])
    (cond [(node? c) (any-tree-tag c)]
          [(pair? c) 'list]
          [else (tok-type c)])))

;; First node child of each kind, e.g. for a left-recursive rule the binary
;; alternative's operands; inline groups (`(PLUS|MINUS)`) are `group` nodes.
(define (node-kids n) (filter node? (any-tree-children n)))

(define (node-kid n i) (list-ref (node-kids n) i))

;; ── Tokenizer ───────────────────────────────────────────────────────

(define keyword-cases
  '(["let" LET] ["rec" REC] ["and" AND] ["in" IN] ["fun" FUN]
    ["function" FUNCTION] ["if" IF] ["then" THEN] ["else" ELSE]
    ["match" MATCH] ["with" WITH] ["when" WHEN] ["type" TYPE] ["of" OF]
    ["module" MODULE] ["struct" STRUCT] ["end" END] ["open" OPEN]
    ["begin" BEGIN] ["mutable" MUTABLE] ["as" AS] ["mod" MOD]))

(define operator-cases
  '(["+" PLUS] ["-" MINUS] ["*" STAR] ["/" SLASH] ["**" POW]
    ["=" ASSIGN] ["==" EQEQ] ["<>" NEQ] ["<" LT] [">" GT] ["<=" LE] [">=" GE]
    ["&&" ANDAND] ["||" OROR] ["::" COLONCOLON] [":=" COLONEQ] ["@" AT]
    ["^" CARET] ["->" ARROW] ["+=" PLUSEQ] ["+." PLUSDOT] ["-." MINUSDOT]
    ["*." STARDOT] ["/." SLASHDOT] ["!" BANG] ["!=" BANGEQ] ["|" PIPE]
    ["%" PERCENT] ["~" TILDE] ["?" QUESTION]))

(define punctuation-cases
  '(["(" LPAREN] [")" RPAREN] ["[" LBRACK] ["]" RBRACK] ["{" LBRACE]
    ["}" RBRACE] ["," COMMA] [";" SEMI] [";;" SEMISEMI] [":" COLON] ["." DOT]))

(define suite
  (test-suite
   "OCaml grammar/tokenizer"

   (test-case "tokenizer: keywords"
     (for ([c keyword-cases])
       (check-token-types (first c) (list (second c)))))

   (test-case "tokenizer: identifiers, constructors, type variables"
     ;; Keyword-prefixed words must stay identifiers (longest match wins).
     (for ([w '("letter" "lets" "inlet" "matchbox" "foo" "x'" "_x")])
       (check-token-types w '(IDENT)))
     (check-equal? (tok-value (first (ocaml-tokenize "foo"))) "foo")
     (check-token-types "Some" '(CONSTRUCTOR))
     (check-token-types "SomeThing" '(CONSTRUCTOR))
     (check-token-types "'a" '(QUOTE_IDENT))
     (check-token-types "_" '(UNDERSCORE)))

   (test-case "tokenizer: int literals"
     (for ([i '("0" "42" "0xff" "0o17" "0b1010" "1_000")])
       (check-token-types i '(INT)))
     ;; INT/FLOAT disambiguation: the decimal point makes the longer match.
     (check-token-types "42" '(INT))
     (check-equal? (tok-value (first (ocaml-tokenize "0xff"))) "0xff"))

   (test-case "tokenizer: float literals"
     (for ([f '("3.14" "1." "1e10" "2.5e-3" "1_000.5")])
       (check-token-types f '(FLOAT)))
     (check-equal? (tok-value (first (ocaml-tokenize "2.5e-3"))) "2.5e-3"))

   (test-case "tokenizer: strings and chars"
     (check-token-types "\"hello\"" '(STRING))
     (check-token-types "\"\"" '(STRING))
     ;; A backslash consumes the next char, so escaped quotes do not end the
     ;; token: one STRING whose value is the whole literal.
     (let* ([lit "\"a \\\"b\\\" c\""]
            [t (first (ocaml-tokenize lit))])
       (check-equal? (tok-type t) 'STRING)
       (check-equal? (tok-value t) lit))
     (check-token-types "'a'" '(CHAR_LITERAL))
     (check-token-types "'\\n'" '(CHAR_LITERAL))
     ;; Escaped apostrophe char literal vs type variable: maximal munch.
     (check-token-types "'\\''" '(CHAR_LITERAL))
     (check-token-types "'a" '(QUOTE_IDENT)))

   (test-case "tokenizer: operators"
     (for ([c operator-cases])
       (check-token-types (first c) (list (second c)))))

   (test-case "tokenizer: punctuation"
     (for ([c punctuation-cases])
       (check-token-types (first c) (list (second c)))))

   (test-case "tokenizer: nested comments are skipped"
     (check-token-types "(* single *) 42" '(INT))
     (check-token-types "(* outer (* inner *) still outer *) 42" '(INT))
     (check-token-types "(* a (* b (* c *) d *) e *) 42" '(INT))
     (check-token-types "(* comment only *)" '())
     (check-token-types "let (* between *) x = 1" '(LET IDENT ASSIGN INT))
     (check-token-types "1 (* between *) + (* tokens *) 2" '(INT PLUS INT))
     (check-token-types "(* line one\n   line two *) 42" '(INT)))
     ;; Note: an unterminated comment is a known limitation (README): it
     ;; consumes to EOF silently instead of raising, so it is not asserted
     ;; here as either success or failure.

   (test-case "tokenizer: token sequences"
     (check-token-types "let x = 1" '(LET IDENT ASSIGN INT))
     (check-token-types "f ~x:1 ?y:2 z"
                        '(IDENT TILDE IDENT COLON INT QUESTION IDENT COLON INT IDENT))
     (check-token-types "x :: xs" '(IDENT COLONCOLON IDENT))
     (check-equal? (tok-types "") '(EOF)))

   (test-case "tokenizer: unsupported input raises"
     (check-exn exn:fail? (lambda () (ocaml-tokenize "#")))
     (check-exn exn:fail? (lambda () (ocaml-tokenize "\"unterminated")))
     (check-exn exn:fail? (lambda () (ocaml-tokenize "''"))))

   ;; ── Parser: let bindings ─────────────────────────────────────────

   (test-case "parser: empty program"
     (let ([cst (ocaml-parse "")])
       (check-true (any-tree? cst))
       (check-eq? (any-tree-tag cst) 'program)))

   (test-case "parser: let ... in"
     (let ([cst (check-parses "let x = 1 in x + 1" 'let_in_expr)])
       (check-equal? (count-nodes cst 'let_binding) 1))
     (check-parses "let f (x : int) (y : int) : int = x + y in f 1 2"
                   'let_in_expr))

   (test-case "parser: let rec ... and"
     (let ([cst (check-parses
                 "let rec even n = if n = 0 then true else odd (n - 1) and odd n = if n = 0 then false else even (n - 1) in even 4"
                 'let_in_expr)])
       (check-equal? (count-nodes cst 'let_binding) 2))
     (let ([cst (check-parses "let rec f x = g x and g y = y" 'let_definition)])
       (check-equal? (count-nodes cst 'let_binding) 2)))

   (test-case "parser: fun, function and lambdas"
     (check-parses "fun x -> x + 1" 'fun_expr)
     (check-parses "fun ~x ?y -> x" 'fun_expr)
     (check-parses "function | Some x -> x | None -> 0" 'function_expr)
     (check-parses "let f = fun x y -> x + y" 'let_definition))

   (test-case "parser: calls including labeled arguments"
     (let ([cst (check-parses "f ~x:1 ?y:2 z" 'labeled_argument)])
       (check-true (> (count-nodes cst 'app_expr) 0)))
     (check-parses "f p.x" 'postfix_expr)
     (check-parses "Effect.perform (Xchg 1)" 'app_expr)
     (check-parses "f ~x:(g y) { a = 2 }" 'record_expr))

   (test-case "parser: operators and precedence"
     ;; Generated left-recursive rules wrap their base case in a node of the
     ;; same rule tag, and inline groups become `group` nodes, so the child
     ;; shape of `1 + 2 * 3` is add_expr(add_expr(1), group(+), mul_expr).
     ;; The precedence assertion is which operand owns the tighter operator.
     (let* ([cst (check-parses "1 + 2 * 3" 'add_expr)]
            [add (find-node cst 'add_expr)])
       (check-equal? (child-kinds add) '(add_expr group mul_expr))
       (check-eq? (any-tree-tag (node-kid add 2)) 'mul_expr)
       (check-true (has-token-type? (node-kid add 2) 'STAR))
       (check-false (has-token-type? (node-kid add 0) 'STAR)))
     ;; 1 * 2 + 3 groups the multiplication under the left operand instead.
     (let* ([cst (check-parses "1 * 2 + 3" 'add_expr)]
            [add (find-node cst 'add_expr)])
       (check-true (has-token-type? (node-kid add 0) 'STAR))
       (check-false (has-token-type? (node-kid add 2) 'STAR)))
     ;; || is below &&: a && b || c = (a && b) || c.
     (let* ([cst (check-parses "a && b || c" 'or_expr)]
            [or-n (find-node cst 'or_expr)])
       (check-equal? (child-kinds or-n) '(or_expr OROR and_expr))
       (check-true (has-token-type? (node-kid or-n 0) 'ANDAND))
       (check-false (has-token-type? (node-kid or-n 1) 'ANDAND)))
     ;; ** is right-associative: 2 ** 3 ** 2 = 2 ** (3 ** 2).
     (let* ([cst (check-parses "2 ** 3 ** 2" 'pow_expr)]
            [pow (find-node cst 'pow_expr)])
       (check-equal? (child-kinds pow) '(unary_expr group))
       (check-eq? (any-tree-tag (node-kid (node-kid pow 1) 0)) 'pow_expr))
     ;; :: is right-associative and its right operand is another cons_expr.
     (let* ([cst (check-parses "x :: y :: []" 'cons_expr)]
            [cons (find-node cst 'cons_expr)])
       (check-equal? (child-kinds cons) '(at_expr group))
       (check-eq? (any-tree-tag (node-kid (node-kid cons 1) 0)) 'cons_expr))
     ;; := is right-associative; unary ! binds tighter than +.
     (let* ([cst (check-parses "x := !x + 1" 'assign_expr)]
            [assign (find-node cst 'assign_expr)])
       (check-equal? (child-kinds assign) '(or_expr group))
       (check-eq? (any-tree-tag (node-kid (node-kid assign 1) 0)) 'assign_expr))
     ;; ^ binds tighter than @ (the @ layer's left operand is a caret_expr).
     (let* ([cst (check-parses "a ^ b @ c" 'at_expr)]
            [at (find-node cst 'at_expr)])
       (check-equal? (child-kinds at) '(at_expr AT caret_expr))
       (check-true (has-token-type? (node-kid at 0) 'CARET)))
     ;; Comparisons chain left-associatively at the level above cons.
     (let* ([cst (check-parses "1 = 2 <> 3" 'cmp_expr)]
            [cmp (find-node cst 'cmp_expr)])
       (check-equal? (child-kinds cmp) '(cmp_expr group cons_expr)))
     (check-parses "1. +. 2.5e-3 *. 2." 'add_expr))

   (test-case "parser: if/then/else"
     (check-parses "if x > 0 then 1 else 2" 'if_expr)
     (check-parses "if x then 1" 'if_expr)
     (check-parses "let x = if a then (fun y -> y) else (fun y -> y + 1)" 'if_expr))

   (test-case "parser: match, patterns, when guards"
     (let ([cst (check-parses
                 "match x with | Some n when n > 0 -> n | Some _ -> 0 | None -> 0"
                 'match_expr)])
       (check-equal? (count-nodes cst 'match_case) 3)
       (check-true (has-token-type? cst 'WHEN) "when guard token consumed")
       (check-true (> (count-nodes cst 'app_pattern) 0) "constructor pattern"))
     ;; Constructor sub-patterns, tuple sub-patterns, as-patterns.
     (check-parses "match x with A (B y, z) -> y | _ -> 0" 'match_expr)
     (check-parses "match x with (A y as z) -> z" 'match_expr)
     ;; Or-patterns, cons patterns, list and record patterns.
     (check-parses "match x with | A | B -> 1 | C when true -> 2" 'match_expr)
     (check-parses "match xs with [] -> 0 | x :: rest -> x" 'match_expr)
     (check-parses "match p with { x = a; y = b } -> a" 'record_pattern))

   (test-case "parser: algebraic data types"
     (let ([cst (check-parses "type shape = Circle of float | Square of float * float"
                              'variant_type)])
       (check-equal? (count-nodes cst 'constructor_decl) 2))
     (check-parses "type t = A | B of int | C of int * string" 'type_definition)
     (check-parses "type 'a tree = Leaf | Node of 'a tree * 'a tree" 'type_params)
     (check-parses "type t = int list" 'type_binding)
     (check-parses "type t = (int, string) result" 'type_expr)
     (check-parses "type t = int -> int -> int" 'type_expr))

   (test-case "parser: record types and record expressions"
     (check-parses "type point = { x : int; y : int; mutable z : int }" 'record_type)
     (let ([cst (check-parses "{ x = 1; y = 2 }" 'record_expr)])
       (check-equal? (count-nodes cst 'record_field) 2))
     (check-parses "{ p with x = 2 }" 'record_expr)
     (check-parses "let m = { x = 1; y = 2 } in m.x" 'postfix_expr)
     (check-parses "type t = A of int * string" 'variant_type))

   (test-case "parser: modules and open"
     (let ([cst (check-parses
                 "module M = struct open N type t = A let f x = x + 1 end"
                 'module_definition)])
       (check-equal? (count-nodes cst 'module_definition) 1)
       (check-true (> (count-nodes cst 'open_definition) 0)))
     (check-parses "module M = N" 'module_expr)
     (check-parses "open M" 'open_definition)
     (check-parses "open Effect.Deep" 'open_definition))

   (test-case "parser: effect declaration, perform and handler shape"
     ;; OCaml 5 effects are a GADT extension plus ordinary call/match/record
     ;; syntax; per the plan they need no new grammar forms beyond these.
     (let ([cst (check-parses "type _ Effect.t += Xchg : int -> int Effect.t"
                              'extension_definition)])
       (check-equal? (count-nodes cst 'constructor_decl) 1))
     (check-parses "type _ Effect.t += A : int Effect.t | B : unit Effect.t"
                   'extension_definition)
     (check-parses "let () = Effect.perform (Xchg 1)" 'postfix_expr)
     (let ([handler (string-append
                     "let handler () = "
                     "Effect.Deep.match_with "
                     "(fun () -> Effect.perform (Xchg 1)) () "
                     "{ retc = (fun v -> v); exnc = (fun e -> raise e); "
                     "effc = (fun (type a) (eff : a Effect.t) -> "
                     "match eff with "
                     "| Xchg n -> Some (fun (k : (a, _) continuation) -> continue k n) "
                     "| _ -> None) }")])
       (let ([cst (check-parses handler 'record_expr)])
         (check-equal? (count-nodes cst 'match_case) 2)
         (check-true (> (count-nodes cst 'fun_expr) 0)))))

   (test-case "parser: combined Phase-1 fixture program"
     (define fixture
       (string-append
        "type shape = Circle of float | Square of float * float\n"
        "type point = { x : float; y : float; mutable tag : string }\n"
        "type _ Effect.t += Xchg : int -> int Effect.t\n"
        "module Geometry = struct\n"
        "  open Shape\n"
        "  let area s = match s with\n"
        "    | Circle r -> 3.14159 *. r *. r\n"
        "    | Square (w, h) -> w *. h\n"
        "  let describe ~prefix p = prefix ^ string_of_float p.x\n"
        "end\n"
        "let run () =\n"
        "  let rec loop n acc = if n = 0 then acc else loop (n - 1) (acc + n) in\n"
        "  loop 10 0\n"
        "let handler () =\n"
        "  Effect.Deep.match_with (fun () -> Effect.perform (Xchg 1)) ()\n"
        "    { retc = (fun v -> v); exnc = (fun e -> raise e);\n"
        "      effc = (fun (type a) (eff : a Effect.t) ->\n"
        "        match eff with\n"
        "        | Xchg n -> Some (fun (k : (a, _) continuation) -> continue k n)\n"
        "        | _ -> None) }\n"))
     (let ([cst (ocaml-parse fixture)])
       (check-true (any-tree? cst) "combined fixture parses")
       (check-eq? (any-tree-tag cst) 'program)
       (check-equal? (count-nodes cst 'type_definition) 3)
       (check-equal? (count-nodes cst 'module_definition) 1)
       (check-equal? (count-nodes cst 'extension_definition) 1)
       (check-equal? (count-nodes cst 'record_expr) 1)
       (check-equal? (count-nodes cst 'let_binding) 5)
       (check-equal? (count-nodes cst 'match_case) 4)))

   (test-case "parser: rejection (must not return a tree)"
     (for ([bad '("let x ="
                  "let x = 1 in"
                  "match x with"
                  "type t ="
                  "type t = { x : }"
                  "module M = struct"
                  "if x then"
                  "open"
                  "let x = 1 end"
                  "f (1 + "
                  "@#$%"
                  "\"unterminated")])
       (check-reject ocaml-parse bad)))))

(define failures (run-tests suite))
(cleanup)
(exit failures)
