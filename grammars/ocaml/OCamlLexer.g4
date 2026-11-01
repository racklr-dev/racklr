// OCaml lexer for racklr (authored, pass 1).
//
// There is no upstream antlr/grammars-v4 OCaml grammar (the upstream tree has
// no ocaml/ directory; verified 2026-11), and the plans' alternatives are to
// author OCamlLexer/OCamlParser or port the third-party tree-sitter grammar.
// This file is authored from the OCaml 5.2 manual's lexical conventions. The
// plan's other option, porting tree-sitter-ocaml, was considered and rejected
// (its license and node model are not the repository's); no third-party
// grammar text was copied, so none is reproduced here.
//
// Pass-1 scope (see docs/plans/ocaml-pipeline.md and README.md here):
//   - keywords, identifiers, type variables, int/float/string/char literals
//   - operators and punctuation used by the Phase-1 fixture set
//   - nested `(* ... *)` comments, skipped by a mode stack (see below)
//
// Deliberate pass-1 omissions (named gaps, not bugs): backtick polymorphic
// variants, quoted string extensions ({|...|}), raw identifiers (\#foo),
// labelled-argument type syntax, byte literals, and every keyword for the
// constructs the Pass-1 plan skips (sig/val/include/object/class/functor).
// Unsupported syntax lexes as IDENT/CONSTRUCTOR and then fails to parse, or
// raises the tokenizer's "unexpected char" error where no rule matches.
//
// This is a separate lexer grammar: the parser names OCamlLexer via
// tokenVocab, which racklr/gen-test's gen-and-load resolves from this
// directory.

lexer grammar OCamlLexer;

// ── Keywords ─────────────────────────────────────────────────────────
// Defined before IDENT: the tokenizer is maximal-munch with ties going to
// the first defined rule, so `let` must win over an IDENT of the same length
// while `letter` still lexes as one IDENT (longer match).

LET      : 'let' ;
REC      : 'rec' ;
AND      : 'and' ;
IN       : 'in' ;
FUN      : 'fun' ;
FUNCTION : 'function' ;
IF       : 'if' ;
THEN     : 'then' ;
ELSE     : 'else' ;
MATCH    : 'match' ;
WITH     : 'with' ;
WHEN     : 'when' ;
TYPE     : 'type' ;
OF       : 'of' ;
MODULE   : 'module' ;
STRUCT   : 'struct' ;
END      : 'end' ;
OPEN     : 'open' ;
BEGIN    : 'begin' ;
MUTABLE  : 'mutable' ;
AS       : 'as' ;
MOD      : 'mod' ;

// ── Strings and chars ───────────────────────────────────────────────
// The negated char classes are written with `~[...]` (negation of a class)
// rather than `[^...]`: racklr's generated cc-match does not implement `^`
// negation inside a class, but it does implement `~` around one.
// A backslash always consumes the following character, so `\"`, `\\` and the
// OCaml numeric escapes `\123` / `\x41` / `\o101` inside literals are covered
// for tokenization purposes; the literal is not unescaped in Pass 1.

STRING : '"' ( '\\' . | ~["\\\r\n] )* '"' ;
CHAR_LITERAL : '\'' ( '\\' . | ~['\\\r\n] ) '\'' ;

// ── Identifiers ──────────────────────────────────────────────────────
// OCaml identifiers may contain apostrophes (`x'`). Type variables and char
// literals both start with an apostrophe: CHAR_LITERAL is defined above so
// that on the three-character tie for `'a'` the char literal wins (the
// first-defined rule wins a maximal-munch tie); `'a` with no closing quote is
// a two-character QUOTE_IDENT.
// `_` is the wildcard pattern / wildcard type parameter; UNDEFINED later it
// would be swallowed by IDENT on the one-character tie, so it is defined
// first here.

UNDERSCORE  : '_' ;
IDENT       : [a-z_] [a-zA-Z0-9_']* ;
CONSTRUCTOR : [A-Z] [a-zA-Z0-9_']* ;
QUOTE_IDENT : '\'' [a-z_] [a-zA-Z0-9_']* ;

// ── Comments: nested `(* ... *)` ────────────────────────────────────
// A regular lexer rule cannot match balanced nesting. The mode stack can:
// every `(*` pushes COMMENT (from default or from COMMENT itself) and every
// `*)` pops one level, so nesting depth is the mode-stack depth.
// Limitation: an unterminated comment is not reported; the tokenizer reaches
// EOF in COMMENT mode and returns EOF.

COMMENT_OPEN : '(*' -> skip, pushMode(COMMENT) ;

// ── Numeric literals ────────────────────────────────────────────────
// Decimal, hex, octal and binary ints, with OCaml's `_` digit separators.
// FLOAT must be tried by maximal munch against INT: `3.14` is 4 chars, `3` is
// 1, so the longer FLOAT wins; a bare `1.` is a valid OCaml float.

fragment DIGIT : [0-9] ;
fragment BINDIGIT : [01] ;
fragment OCTDIGIT : [0-7] ;
fragment HEXDIGIT : [0-9a-fA-F] ;
fragment EXP : [eE] [+-]? DIGIT DIGIT* ;

INT
  : DIGIT (DIGIT | '_')*
  | '0' [xX] HEXDIGIT (HEXDIGIT | '_')*
  | '0' [oO] OCTDIGIT (OCTDIGIT | '_')*
  | '0' [bB] BINDIGIT (BINDIGIT | '_')*
  ;

FLOAT
  : DIGIT (DIGIT | '_')* '.' (DIGIT | '_')* EXP?
  | DIGIT (DIGIT | '_')* EXP
  ;


// ── Punctuation and operators ───────────────────────────────────────
// Maximal munch makes `;;` beat `;`, `::`/`:=` beat `:`, `->` beat `-`,
// `**` beat `*`, and the two-char comparisons beat their one-char prefixes.

LPAREN    : '(' ;
RPAREN    : ')' ;
LBRACK    : '[' ;
RBRACK    : ']' ;
LBRACE    : '{' ;
RBRACE    : '}' ;
SEMISEMI  : ';;' ;
SEMI      : ';' ;
COMMA     : ',' ;
DOT       : '.' ;
COLONCOLON: '::' ;
COLONEQ   : ':=' ;
COLON     : ':' ;
PIPE      : '|' ;
ARROW     : '->' ;
PLUSEQ    : '+=' ;
ASSIGN    : '=' ;
PLUSDOT   : '+.' ;
MINUSDOT  : '-.' ;
STARDOT   : '*.' ;
SLASHDOT  : '/.' ;
EQEQ      : '==' ;
NEQ       : '<>' ;
LE        : '<=' ;
GE        : '>=' ;
LT        : '<' ;
GT        : '>' ;
BANGEQ    : '!=' ;
BANG      : '!' ;
PLUS      : '+' ;
MINUS     : '-' ;
POW       : '**' ;
STAR      : '*' ;
SLASH     : '/' ;
PERCENT   : '%' ;
ANDAND    : '&&' ;
OROR      : '||' ;
AT        : '@' ;
CARET     : '^' ;
TILDE     : '~' ;
QUESTION  : '?' ;

// ── Comment mode ────────────────────────────────────────────────────
// Only reachable through COMMENT_OPEN's pushMode. Because the tokenizer's
// generic whitespace handler runs before mode rules, whitespace (including
// newlines) inside comments is skipped by the normal whitespace loop, and the
// remaining characters are consumed by COMMENT_BODY's `.`.

mode COMMENT;

COMMENT_OPEN_NESTED : '(*' -> skip, pushMode(COMMENT) ;
COMMENT_CLOSE       : '*)' -> skip, popMode ;
COMMENT_BODY        : .    -> skip ;
