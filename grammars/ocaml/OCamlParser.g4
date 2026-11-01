// OCaml parser for racklr (authored, pass 1).
//
// No upstream antlr/grammars-v4 OCaml grammar exists, so this pair is
// authored rather than vendored; see OCamlLexer.g4 and README.md for the
// acquisition record and the reference used. It covers the Phase-1 fixture
// set in docs/plans/ocaml-pipeline.md and nothing beyond it.
//
// Generator constraints this grammar is written against (racklr's generator
// is a PEG-style recursive-descent generator, not ANTLR4):
//   - Alternatives are tried first-success inside a sequence; a rule with
//     several alternatives picks the longest match, ties to grammar order.
//     Optional/star/plus are greedy and do not backtrack.
//   - Direct left recursion is rewritten to an accumulator loop (used for the
//     binary-operator layers below); indirect left recursion is not.
//   - Semantic predicates/actions and `~(...)` sets in the parser are
//     ignored, so none are used here.
// This is why the expression grammar is fully layered (one rule per OCaml
// precedence level) instead of a flat `expr (OP expr)*` with precedence
// fixes, and why `;` sequences only appear in the dedicated `seq` rule:
// a general sequence expression would be greedily swallowed inside list and
// record literals, whose separators are also `;`.
//
// Pass-1 constructs and the rules that own them:
//   let ... in / let rec ... and   -> let_in_expr, let_binding
//   fun / function                 -> fun_expr, function_expr
//   calls incl. labeled args       -> app_expr, argument, labeled_argument
//   operators and precedence       -> tuple_expr ... pow_expr, unary_expr
//   if/then/else                   -> if_expr
//   match, patterns, when guards   -> match_expr, match_cases, pattern layer
//   type ... = A | B of int        -> type_definition, variant_type
//   record types / record exprs    -> record_type, record_expr
//   module ... = struct ... end    -> module_definition, module_expr
//   open                           -> open_definition
//   effect declaration/perform     -> extension_definition, then ordinary
//                                     call/record/match rules for handlers
//
// Named Pass-1 gaps (they fail to parse, they are not silently accepted):
// functors, first-class modules, sig/val/include, objects/classes,
// polymorphic variants, GADTs beyond the record/variant subset, `@@`/`|>`,
// `<-` mutation, loops (while/for), exceptions/try, and expression sequences
// (seq is only parsed where `;` cannot also be a list/record separator, i.e.
// begin/end and parentheses).

parser grammar OCamlParser;

options { tokenVocab = OCamlLexer; }

// ── Program ─────────────────────────────────────────────────────────

program : (item | SEMISEMI)* EOF ;

item
  : let_definition
  | type_definition
  | module_definition
  | open_definition
  | expr
  ;

// ── Let bindings ────────────────────────────────────────────────────
// `let f x y = e` parses its LHS as IDENT pattern+; `let (a, b) = e` and
// `let () = e` go through pattern. The two alternatives are resolved by the
// rule's longest match, so `let f x` prefers the two-argument form.

let_definition : LET REC? let_binding (AND let_binding)* ;
let_in_expr    : LET REC? let_binding (AND let_binding)* IN expr ;
let_binding    : let_lhs (COLON type_expr)? ASSIGN expr ;
// Function bindings: `let f x y =` is one IDENT plus argument patterns; a
// binding that is a single pattern (`let (a, b) =`, `let () =`, `let x =`)
// goes through `pattern`. Labeled parameters (`let f ~x =`) are accepted at
// the binding site too, so handler/helper definitions in the fixture set can
// declare the same labels they are called with.
let_lhs        : IDENT let_arg+ | pattern ;
let_arg        : (TILDE | QUESTION) IDENT (COLON type_expr)? | pattern ;

// ── Type declarations ───────────────────────────────────────────────

type_definition
  : TYPE extension_definition
  | TYPE type_params? type_binding (AND type_binding)*
  ;

// `type _ Effect.t += Xchg : int -> int Effect.t` (OCaml 5 effect syntax).
extension_definition
  : (type_params | UNDERSCORE)? CONSTRUCTOR (DOT (CONSTRUCTOR | IDENT))* PLUSEQ constructor_decl (PIPE constructor_decl)*
  ;

type_binding : type_params? IDENT ASSIGN type_rhs ;
type_params  : QUOTE_IDENT+ | LPAREN QUOTE_IDENT (COMMA QUOTE_IDENT)* RPAREN ;

type_rhs
  : record_type
  | variant_type
  | type_expr
  ;

// `of` for ordinary variants, `:` for the GADT-style effect extension.
constructor_decl : CONSTRUCTOR ((OF | COLON) type_expr)? ;
variant_type     : constructor_decl (PIPE constructor_decl)* ;

record_type       : LBRACE (record_type_field (SEMI record_type_field)* SEMI?)? RBRACE ;
record_type_field : MUTABLE? IDENT COLON type_expr ;

// Type expressions, low to high precedence: `->` (right), `*` tuple (left),
// then a left-associative application chain (`int list`, `(int, string)
// result`, `int Effect.t`).
type_expr  : tuple_type (ARROW type_expr)? ;
tuple_type : type_app (STAR type_app)* ;
type_app   : atomic_type+ ;

atomic_type
  : QUOTE_IDENT
  | LPAREN type_expr (COMMA type_expr)* RPAREN
  | LPAREN RPAREN
  | UNDERSCORE
  | type_path
  ;

// Module-qualified type: `Effect.t`, `Foo.Bar.t`.
type_path : (IDENT | CONSTRUCTOR) (DOT (IDENT | CONSTRUCTOR))* ;

// ── Modules and opens ───────────────────────────────────────────────

module_definition : MODULE CONSTRUCTOR ASSIGN module_expr ;
module_expr       : STRUCT (item | SEMISEMI)* END | module_path ;
module_path       : CONSTRUCTOR (DOT CONSTRUCTOR)* ;
open_definition   : OPEN module_path ;

// ── Expressions ─────────────────────────────────────────────────────
// One rule per OCaml precedence level, lowest first. Constructs (let/fun/
// function/if/match) are alternatives at `expr`; they are operands only when
// parenthesized, which is how the manual's "extend as far right as possible"
// forms are usually written in the Phase-1 fixtures.

expr
  : let_in_expr
  | fun_expr
  | function_expr
  | if_expr
  | match_expr
  | tuple_expr
  ;

if_expr       : IF expr THEN expr (ELSE expr)? ;
fun_expr      : FUN fun_params ARROW expr ;
fun_params    : fun_param+ ;
// `(type a)` is the locally abstract type parameter used by effect-handler
// code (`fun (type a) (eff : a Effect.t) -> ...`); it is a Pass-1 concession
// so the plan's handler shape parses.
fun_param     : LPAREN TYPE IDENT RPAREN
              | (TILDE | QUESTION) IDENT (COLON type_expr)?
              | pattern (COLON type_expr)?
              ;
function_expr : FUNCTION match_cases ;

match_expr  : MATCH expr WITH match_cases ;
match_cases : PIPE? match_case (PIPE match_case)* ;
match_case  : pattern (WHEN expr)? ARROW expr ;

// Tuple (`,`), then `:=`, `||`, `&&`, comparisons, `::`, `@`, `^`, `+ -`,
// `* / mod`, `**` (right), prefix `-`/`+`/`!`, application, postfix.
// Note the expression `::` is right-associative and lower precedence than
// `@`/`^`, matching the manual's table; it is a separate layer from the
// pattern `::` because both sides have different grammars.
tuple_expr  : assign_expr (COMMA assign_expr)* ;
assign_expr : or_expr (COLONEQ assign_expr)? ;
or_expr     : or_expr OROR and_expr | and_expr ;
and_expr    : and_expr ANDAND cmp_expr | cmp_expr ;
cmp_expr    : cmp_expr (ASSIGN | NEQ | LT | GT | LE | GE | EQEQ | BANGEQ) cons_expr | cons_expr ;
cons_expr   : at_expr (COLONCOLON cons_expr)? ;
at_expr     : at_expr AT caret_expr | caret_expr ;
caret_expr  : caret_expr CARET add_expr | add_expr ;
add_expr    : add_expr (PLUS | MINUS | PLUSDOT | MINUSDOT) mul_expr | mul_expr ;
mul_expr    : mul_expr (STAR | SLASH | PERCENT | MOD | STARDOT | SLASHDOT) pow_expr | pow_expr ;
pow_expr    : unary_expr (POW pow_expr)? ;
unary_expr  : (MINUS | PLUS | BANG) unary_expr | app_expr ;

app_expr     : app_expr argument | postfix_expr ;
// The argument is a postfix expression, not just an atom: postfix `r.x` binds
// tighter than application, so `f p.x` is `f (p.x)`, and a labeled argument's
// value stays atomic (`f ~x:1`, `f ~x:(g y)`).
argument     : labeled_argument | postfix_expr ;
labeled_argument : (TILDE | QUESTION) IDENT (COLON atomic_expr)? ;

// Field access `r.x` / `module.value`, array and string index `a.(i)`/`s.[i]`.
postfix_expr : atomic_expr (DOT (IDENT | CONSTRUCTOR) | DOT LPAREN expr RPAREN | DOT LBRACK expr RBRACK)* ;

atomic_expr
  : INT
  | FLOAT
  | STRING
  | CHAR_LITERAL
  | IDENT
  | CONSTRUCTOR
  | LPAREN RPAREN
  | LPAREN seq RPAREN
  | LPAREN expr RPAREN
  | BEGIN seq END
  | BEGIN expr END
  | LBRACE record_expr RBRACE
  | LBRACK list_expr RBRACK
  | LBRACK RBRACK
  ;

// `;` sequences: only where the grammar is unambiguous about them.
seq : expr (SEMI expr)+ ;

record_expr   : IDENT WITH record_fields? | record_fields? ;
record_fields : record_field (SEMI record_field)* SEMI? ;
record_field  : IDENT ASSIGN expr | IDENT ;
list_expr     : expr (SEMI expr)* SEMI? ;

// ── Patterns ────────────────────────────────────────────────────────
// Left recursion only for `as` (accumulator loop); `|` or-patterns,
// `::` cons, constructor application and the atomic patterns cover the
// Phase-1 match fixtures: literals, `_`, variables, constructors with
// sub-patterns, tuples, lists, records and `(p : t)`.

pattern     : pattern AS IDENT | or_pattern ;
or_pattern  : cons_pattern (PIPE cons_pattern)* ;
cons_pattern : app_pattern (COLONCOLON cons_pattern)? ;
app_pattern : CONSTRUCTOR atomic_pattern+ | atomic_pattern ;

atomic_pattern
  : UNDERSCORE
  | IDENT
  | CONSTRUCTOR
  | INT
  | FLOAT
  | STRING
  | CHAR_LITERAL
  | LPAREN RPAREN
  | LPAREN TYPE IDENT RPAREN
  | LPAREN pattern COLON type_expr RPAREN
  | LPAREN pattern COMMA pattern (COMMA pattern)* RPAREN
  | LPAREN pattern RPAREN
  | LBRACK RBRACK
  | LBRACK pattern (SEMI pattern)* SEMI? RBRACK
  | LBRACE record_pattern RBRACE
  ;

record_pattern    : record_pat_field (SEMI record_pat_field)* ;
record_pat_field  : IDENT ASSIGN pattern | IDENT ;
