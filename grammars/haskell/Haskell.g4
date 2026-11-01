// Haskell grammar for racklr (authored, pass 1).
//
// This is NOT the upstream antlr/grammars-v4 Haskell grammar: that pair
// (HaskellLexer.g4 + HaskellParser.g4) depends on the Java `HaskellBaseLexer`
// layout preprocessor, which racklr's generator cannot execute (lexer actions
// are dropped, so OCURLY/SEMI/VOCURLY/CCURLY tokens are never produced). See
// grammars/haskell/README.md for the acquisition attempt and the evidence.
//
// This file is an authored pass-1 subset of the Haskell 2010 rule set, written
// to run on racklr's generated lexer/parser. The rule and token structure is
// derived from the upstream grammar and the Haskell 2010 report; upstream is
// BSD-3-Clause, Copyright (c) 2020 Evgeniy Slobodkin (license reproduced in
// grammars/haskell/README.md).
//
// Layout contract (the same one grammars/koka/Koka.g4 uses): racklr's generated
// lexer inserts synthetic NEWLINE / INDENT / DEDENT tokens for line starts,
// and the exact token names NEWLINE, OPEN_PAREN, CLOSE_PAREN, OPEN_BRACK,
// CLOSE_BRACK, OPEN_BRACE, CLOSE_BRACE are required by that machinery. NEWLINE
// is dropped inside brackets/braces (Haskell implicit line joining), so
// multi-line lists, tuples and record braces lex as a contiguous token stream.
//
// Pass-1 scope (see docs/plans/haskell-pipeline.md):
//   - module header, exports, imports, top-level type signatures and bindings
//   - let/where, if/then/else, case with patterns and guards, do-notation
//   - data (positional and named-field constructors), newtype, deriving
//   - typeclass declarations and instances
//   - expressions: literals, variables, application, lambdas, operators,
//     lists, tuples, parenthesised expressions, qualified names
//   - types: constructors, variables, applications, arrows, lists, tuples,
//     and a one-level `C a =>` context for the Phase-4 effect mapping
//
// Deliberate gaps (Phase 2): GADTs, pattern synonyms, type/data families,
// foreign imports, Template Haskell/quasiquotes, fixity-aware parsing
// (operators use a fixed precedence ladder, not the declared fixity),
// multi-constructor record layouts, list/cons/tuple patterns, sections,
// and the hard layout cases (layout keywords on their own line, continuation
// lines that do not begin with a layout keyword).

grammar Haskell;

// ════════════════════════════════════════════════════════════════════
// Module level
// ════════════════════════════════════════════════════════════════════

file
    : NEWLINE* module_header? NEWLINE* topdecls EOF
    ;

topdecls
    : (topdecl | NEWLINE)*
    ;

module_header
    : MODULE modid exports? WHERE
    ;

exports
    : OPEN_PAREN (export_item (COMMA export_item)*)? CLOSE_PAREN
    ;

export_item
    : VARID
    | CONID export_subs?
    | MODULE modid
    ;

export_subs
    : OPEN_PAREN (DOUBLE_DOT | VARID (COMMA VARID)*)? CLOSE_PAREN
    ;

modid
    : CONID (DOT CONID)*
    ;

topdecl
    : import_decl
    | data_decl
    | newtype_decl
    | class_decl
    | instance_decl
    | fixity_decl
    | type_sig
    | value_decl
    ;

// ── Imports ─────────────────────────────────────────────────────────

import_decl
    : IMPORT QUALIFIED? modid (AS modid)? import_spec?
    ;

import_spec
    : HIDING? OPEN_PAREN (import_item (COMMA import_item)*)? CLOSE_PAREN
    ;

import_item
    : VARID
    | CONID export_subs?
    ;

// ── Fixity declarations ─────────────────────────────────────────────

fixity_decl
    : fixity prec? ops
    ;

fixity
    : INFIX
    | INFIXL
    | INFIXR
    ;

prec
    : DECIMAL
    ;

ops
    : op (COMMA op)*
    ;

// ── Algebraic data types, newtypes ──────────────────────────────────

data_decl
    : DATA CONID tyvar* data_body
    ;

tyvar
    : VARID
    ;

data_body
    : EQUALS constr (PIPE constr)* deriving_list?
    | NEWLINE INDENT data_item (NEWLINE data_item)* NEWLINE? DEDENT
    ;

data_item
    : EQUALS constr
    | PIPE constr
    | DERIVING class_ref (COMMA class_ref)*
    ;

deriving_list
    : DERIVING class_ref (COMMA class_ref)*
    ;

class_ref
    : CONID
    | OPEN_PAREN CONID (COMMA CONID)* CLOSE_PAREN
    ;

newtype_decl
    : NEWTYPE CONID tyvar* EQUALS constr deriving_list?
    ;

constr
    : CONID field* (OPEN_BRACE field_decl (COMMA field_decl)* CLOSE_BRACE)?
    ;

field
    : atype
    ;

field_decl
    : VARID DOUBLE_COLON hs_type
    ;

// ── Typeclasses and instances ───────────────────────────────────────

class_decl
    : CLASS CONID tyvar* (WHERE class_body)?
    ;

class_body
    : NEWLINE INDENT method_sig (NEWLINE method_sig)* NEWLINE? DEDENT
    | method_sig (SEMI method_sig)*
    ;

method_sig
    : VARID DOUBLE_COLON hs_type
    ;

instance_decl
    : INSTANCE class_ref hs_type+ (WHERE instance_body)?
    ;

instance_body
    : NEWLINE INDENT decl (NEWLINE decl)* NEWLINE? DEDENT
    | decl (SEMI decl)*
    ;

decl
    : type_sig
    | value_decl
    ;

// ── Top-level bindings ──────────────────────────────────────────────

type_sig
    : VARID DOUBLE_COLON hs_type
    ;

value_decl
    : funlhs EQUALS rhs where_clause?
    ;

funlhs
    : VARID apat*
    | OPEN_PAREN op CLOSE_PAREN apat*
    ;

rhs
    : expr
    | NEWLINE INDENT expr NEWLINE? DEDENT
    ;

where_clause
    : WHERE decl (SEMI decl)*
    | NEWLINE INDENT WHERE decl (NEWLINE decl)* NEWLINE? DEDENT
    | NEWLINE INDENT WHERE NEWLINE INDENT decl (NEWLINE decl)* NEWLINE? DEDENT DEDENT
    ;

// ════════════════════════════════════════════════════════════════════
// Types
// ════════════════════════════════════════════════════════════════════

hs_type
    : context? type0
    ;

context
    : constraint FAT_ARROW
    | OPEN_PAREN constraint (COMMA constraint)* CLOSE_PAREN FAT_ARROW
    ;

constraint
    : CONID atype+
    ;

// Right-associative arrows, then left-associative application.
type0
    : btype ARROW type0
    | btype
    ;

btype
    : btype atype
    | atype
    ;

atype
    : CONID
    | VARID
    | OPEN_PAREN hs_type (COMMA hs_type)* CLOSE_PAREN
    | OPEN_PAREN CLOSE_PAREN
    | OPEN_BRACK hs_type CLOSE_BRACK
    ;

// ════════════════════════════════════════════════════════════════════
// Expressions
// ════════════════════════════════════════════════════════════════════

expr
    : lambda_expr
    | let_expr
    | if_expr
    | case_expr
    | do_expr
    | op_expr
    ;

lambda_expr
    : BACKSLASH apat+ ARROW expr
    ;

if_expr
    : IF expr THEN expr ELSE expr
    | IF expr NEWLINE THEN expr NEWLINE ELSE expr
    ;

case_expr
    : CASE expr OF case_alt
    | CASE expr OF case_alts
    ;

case_alts
    : NEWLINE INDENT case_alt (NEWLINE case_alt)* NEWLINE? DEDENT
    ;

case_alt
    : pat (PIPE expr)* ARROW expr
    ;

do_expr
    : DO NEWLINE INDENT do_stmt (NEWLINE do_stmt)* NEWLINE? DEDENT
    | DO do_stmt (SEMI do_stmt)*
    ;

do_stmt
    : pat LARROW expr
    | LET decl (SEMI decl)*
    | expr
    ;

let_expr
    : LET let_binds IN expr
    | LET let_binds NEWLINE IN expr
    | LET NEWLINE INDENT let_binds NEWLINE? DEDENT IN expr
    | LET let_binds NEWLINE INDENT let_binds NEWLINE? DEDENT NEWLINE? IN expr
    ;

let_binds
    : decl (SEMI decl)*
    ;

// Fixed operator precedence ladder (pass 1; declared fixity is ignored).
op_expr : op_expr OROR op2 | op2 ;
op2     : op2 AMPAMP op3 | op3 ;
op3     : op3 rel_op op4 | op4 ;
rel_op  : EQEQ | NEQ | LT | LTE | GT | GTE ;
op4     : op4 add_op op5 | op5 ;
add_op  : PLUS | MINUS | CONCAT ;
op5     : op5 mul_op op6 | op6 ;
mul_op  : STAR | SLASH | PERCENT | CARET ;
op6     : op6 DOLLAR op7 | op7 ;
op7     : op7 DOT op8 | op8 ;
op8     : op8 SYMBOL op9 | op9 ;
op9     : minus_expr | app_expr ;
minus_expr : MINUS op9 ;
app_expr : app_expr atom | atom ;

op
    : SYMBOL
    | PLUS | MINUS | STAR | SLASH | PERCENT | CARET
    | EQEQ | NEQ | LT | LTE | GT | GTE | OROR | AMPAMP
    | CONCAT | DOLLAR | COLON
    ;

atom
    : literal
    | qvar
    | qcon
    | OPEN_PAREN expr (COMMA expr)* CLOSE_PAREN
    | OPEN_PAREN CLOSE_PAREN
    | OPEN_PAREN op CLOSE_PAREN
    | OPEN_BRACK list_expr? CLOSE_BRACK
    ;

list_expr
    : expr (COMMA expr)*
    ;

qvar
    : CONID DOT qvar
    | VARID
    ;

qcon
    : CONID DOT qcon
    | CONID
    ;

literal
    : DECIMAL
    | OCTAL
    | HEX
    | FLOAT
    | CHAR
    | STRING
    ;

// ════════════════════════════════════════════════════════════════════
// Patterns
// ════════════════════════════════════════════════════════════════════

pat
    : CONID apat*
    | apat
    ;

apat
    : VARID
    | WILDCARD
    | literal
    | CONID
    | OPEN_PAREN pat CLOSE_PAREN
    | OPEN_PAREN CLOSE_PAREN
    ;

// ════════════════════════════════════════════════════════════════════
// Lexer
// ════════════════════════════════════════════════════════════════════

// Layout: the newline plus the next line's leading indentation. The generated
// lexer post-processes this into NEWLINE/INDENT/DEDENT.
NEWLINE : ('\r'? '\n' | '\r') [ \t]* ;

// Structural tokens whose exact names the layout machinery depends on.
OPEN_PAREN  : '(' ;
CLOSE_PAREN : ')' ;
OPEN_BRACK  : '[' ;
CLOSE_BRACK : ']' ;
OPEN_BRACE  : '{' ;
CLOSE_BRACE : '}' ;

// Keywords. Declared before VARID/CONID so a tie on match length resolves to
// the keyword (maximal munch, then first rule wins).
MODULE   : 'module' ;
WHERE    : 'where' ;
IMPORT   : 'import' ;
QUALIFIED : 'qualified' ;
AS       : 'as' ;
HIDING   : 'hiding' ;
DATA     : 'data' ;
NEWTYPE  : 'newtype' ;
TYPE     : 'type' ;
CLASS    : 'class' ;
INSTANCE : 'instance' ;
DERIVING : 'deriving' ;
LET      : 'let' ;
IN       : 'in' ;
DO       : 'do' ;
CASE     : 'case' ;
OF       : 'of' ;
IF       : 'if' ;
THEN     : 'then' ;
ELSE     : 'else' ;
INFIX    : 'infix' ;
INFIXL   : 'infixl' ;
INFIXR   : 'infixr' ;

// Reserved operators and punctuation.
DOUBLE_COLON : '::' ;
ARROW        : '->' ;
LARROW       : '<-' ;
FAT_ARROW    : '=>' ;
EQUALS       : '=' ;
PIPE         : '|' ;
BACKSLASH    : '\\' ;
AT           : '@' ;
TILDE        : '~' ;
DOT          : '.' ;
COMMA        : ',' ;
SEMI         : ';' ;
COLON        : ':' ;
DOUBLE_DOT   : '..' ;

// Common operators, most specific first (maximal munch, then first rule).
OROR   : '||' ;
AMPAMP : '&&' ;
EQEQ   : '==' ;
NEQ    : '/=' ;
LTE    : '<=' ;
GTE    : '>=' ;
LT     : '<' ;
GT     : '>' ;
PLUS   : '+' ;
MINUS  : '-' ;
STAR   : '*' ;
SLASH  : '/' ;
PERCENT : '%' ;
CARET  : '^' ;
CONCAT : '++' ;
DOLLAR : '$' ;

// Literals.
FLOAT   : [0-9]+ '.' [0-9]+ ([eE] [+-]? [0-9]+)?
        | [0-9]+ [eE] [+-]? [0-9]+ ;
HEX     : '0' [xX] [0-9a-fA-F]+ ;
OCTAL   : '0' [oO] [0-7]+ ;
DECIMAL : [0-9]+ ;
CHAR    : '\'' ( '\\' . | ~['\\\r\n] ) '\'' ;
STRING  : '"' ( '\\' . | ~["\\\r\n] )* '"' ;

// Identifiers: lower/underscore starts bind values, upper starts constructors.
WILDCARD : '_' ;
VARID    : [a-z_] [a-zA-Z0-9_']* ;
CONID    : [A-Z] [a-zA-Z0-9_']* ;

// Any other operator symbol (e.g. <>, >>=, <*>). Declared after the reserved
// and common operators so equal-length matches resolve to those rules.
SYMBOL : [!#$%&*+./<=>?@\\^|~:\-]+ ;

// Comments. `--` starts a line comment, `{- -}` a block comment.
COMMENT       : '--' ~[\r\n]* -> skip ;
BLOCK_COMMENT : '{-' .*? '-}' -> skip ;
