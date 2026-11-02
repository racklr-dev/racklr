// Koka 3.x grammar for racklr (authored port of the upstream grammar).
//
// Source of truth: the Koka compiler's own grammar and lexer in
// `koka-lang/koka` — `doc/spec/grammar/parser.y` (parser) and
// `src/Syntax/Lexer.x` (tokens). Koka's layout is *not* Python layout:
// the upstream `src/Syntax/Layout.hs` pass inserts `{`, `;`, `}` from
// column positions, with expression-continuation exceptions. That pass is
// injected here as data: `racklr/gend-parser/language-policy.rkt`
// (`koka-layout-policy`), selected by the option below, so this grammar is
// written against explicit braces and semicolons exactly like upstream.
//
//   options { layout = koka; }
//
// Conventions:
//   - `token` names are the upstream grammar's terminal names; soft keywords
//     (`inline`, `tail`, `fip`, `div`, `linear`, `value`, `open`, `lazy`,
//     `behind`, `finally`, `initially`, `js`, `cs`, ...) are IDENT tokens and
//     the parser matches them by value (`'inline'`).
//   - Layout tokens never reach the parser (the policy removes NEWLINE and
//     inserts OPEN_BRACE/SEMI/CLOSE_BRACE).
//   - Comments are skipped by the lexer; Koka's layout pass therefore does not
//     see them (upstream checks comment-in-indentation errors, which this port
//     does not reproduce).

grammar Koka;

options { layout = koka; }

// ── Program ──────────────────────────────────────────────────────────

file : moduleheader? body EOF ;

moduleheader : pubmod MODULE modulepath ;

// The layout pass wraps an indented module body in braces; an unindented one
// is a plain sequence of imports and declarations.
body : OPEN_BRACE semis imports CLOSE_BRACE | semis imports ;

imports
  : (impdecl semis)* (extimpdecl semis)* (declaration semis)* ;

impdecl
  : pubmod '@open'? IMPORT modulepath ((EQ modulepath) | importconds)? ;

importconds : OPEN_BRACE semis (importcond semis)* CLOSE_BRACE ;
importcond  : targetguard modulepath ;
targetguard : importbackend? importattrs? ;
importbackend : 'c' | 'js' | 'cs' | 'default' ;
importattrs : OPEN_BRACK guardattrs CLOSE_BRACK ;
guardattrs  : guardattr (COMMA guardattr)* COMMA? ;
guardattr   : IDENT EQ guardval (PIPE guardval)* ;
guardval    : IDENT | STRING | INT ;

externkw : EXTERN | 'external' ;

extimpdecl : externkw IMPORT extimpbody | externkw 'include' extimpbody ;

funkw : FUN | 'function' ;

extimpbody
  : EQ extimpentry
  | OPEN_BRACE semis (extimpentry semis)* CLOSE_BRACE ;

extimpentry
  : externtarget? extattrs? (IDENT (STRING | RAWSTRING) | STRING | RAWSTRING
                             | OPEN_BRACE semis (extimppair semis)* CLOSE_BRACE) ;

extimppair : IDENT EQ (STRING | RAWSTRING) ;

externtarget : IDENT ;
extattrs     : importattrs ;

// ── Declarations ─────────────────────────────────────────────────────

declaration : fixitydecl | extimpdecl | topdecl ;

fixitydecl : pubmod fixity oplist1 ;
fixity     : INFIX INT | INFIXL INT | INFIXR INT ;
oplist1    : identifier (COMMA identifier)* COMMA? ;

topdecl
  : pubmod abstractmod typedecl
  | pubmod puredecl
  | pubmod aliasdecl
  | pubmod externdecl
  ;

semis       : SEMI* ;


pubmod      : PUB | 'public' | 'private' | ;
abstractmod : ABSTRACT? ;

// ── Type / struct / effect declarations ──────────────────────────────

typedecl : typedef | structdecl | effectdecl ;

typedef : typemod TYPE typeid typeparams kannot typebody ;

typemod    : structmod | 'open' | 'extend' | 'co' | 'div' | 'lazy' | ;
structmod  : 'value' | 'reference' | ;

structdecl : structmod STRUCT typeid typeparams kannot conparams ;

effectdecl : effecthead (effectbody | operation) ;

effecthead
  : NAMED? 'scoped'? effectmod (EFFECT | 'context' | 'ambient') varid? typeparams kannot ('in' ktype)? ;

effectmod : ('div' | 'rec') | 'linear' ('div' | 'rec')? | ;

effectbody : OPEN_BRACE semis operations CLOSE_BRACE ;

operations : (operation semis)* ;

operation
  : pubmod VAL identifier typeparams COLON tatomic
  | pubmod FUN identifier typeparams OPEN_PAREN pparameters CLOSE_PAREN COLON tatomic
  | pubmod controlmod CTL identifier typeparams OPEN_PAREN pparameters CLOSE_PAREN COLON tatomic
  | pubmod controlmod 'control' identifier typeparams OPEN_PAREN pparameters CLOSE_PAREN COLON tatomic
  ;

controlmod : FINAL | RAW | ;

typeid
  : OPEN_PAREN commas CLOSE_PAREN
  | OPEN_BRACK CLOSE_BRACK
  | LT GT
  | LT PIPE GT
  | varid
  | CTX
  ;

commas  : COMMA* ;
commas1 : COMMA+ ;

typebody : OPEN_BRACE semis constructors CLOSE_BRACE | ;

constructors : (constructor semis)* ;

constructor
  : pubmod conmod con conid typeparams conparams (ARROW blockexpr)?
  ;

conmod : 'lazy' | ;

con : CON | ;

conparams
  : OPEN_PAREN parameters CLOSE_PAREN
  | OPEN_BRACE semis sconparams CLOSE_BRACE
  | ;

sconparams : (parameter semis)* ;

// ── Pure declarations ────────────────────────────────────────────────

aliasdecl : ALIAS typeid typeparams kannot EQ ktype ;

puredecl
  : inlinemod VAL binder EQ blockexpr
  | inlinemod fipmod funkw qidentifier funbody
  ;

inlinemod : 'inline' | 'noinline' | ;

fipmod : tailmod ('fip' | 'fbip')? fiplimit? ;
tailmod : 'tail'? ;

fiplimit
  : OPEN_PAREN INT CLOSE_PAREN
  | OPEN_PAREN '_' CLOSE_PAREN
  | ;

fundecl : identifier funbody ;

binder : qidentifier (COLON ktype)? ;

funbody
  : typeparams OPEN_PAREN pparameters CLOSE_PAREN bodyexpr
  | typeparams OPEN_PAREN pparameters CLOSE_PAREN COLON tresult qualifier block
  ;

annotres : COLON tresult | ;

// ── External declarations ────────────────────────────────────────────

externdecl : inlinemod fipmod externkw inlinemod fipmod qidentifier externtype externbody ;

externtype
  : COLON typescheme
  | typeparams OPEN_PAREN parameters CLOSE_PAREN annotres
  ;

externbody
  : OPEN_BRACE semis externstats CLOSE_BRACE
  | ;

externstats : (externstat semis)* ;

externstat
  : externinline (STRING | RAWSTRING)
  | externtarget extattrs? externinline (STRING | RAWSTRING)
  ;

externinline : 'inline' | ;

// ── Statements / expressions ─────────────────────────────────────────

block : OPEN_BRACE semis statements CLOSE_BRACE ;

statements : (statement semis)* ;

statement
  : decl
  | withstat IN blockexpr
  | withstat
  | returnexpr
  | basicexpr
  ;

decl
  : FUN fundecl
  | VAL apattern EQ blockexpr
  | VAR binder ASSIGN blockexpr
  ;

expr : withexpr | block | returnexpr | valexpr | basicexpr ;

bodyexpr   : blockexpr | ARROW blockexpr ;
blockexpr  : expr ;

basicexpr
  : ifexpr
  | matchexpr
  | handlerexpr
  | fnexpr
  | opexpr
  ;

matchexpr
  : MATCH ntlexpr OPEN_BRACE semis matchrules CLOSE_BRACE
  | 'lazy' MATCH ntlexpr OPEN_BRACE semis matchrules CLOSE_BRACE
  ;

matchrules : (matchrule semis)* ;

matchrule
  : patterns1 PIPE expr ARROW blockexpr
  | patterns1 ARROW blockexpr
  | patterns1 blockexpr
  ;

patterns1 : pattern (COMMA pattern)* COMMA? ;

fnexpr     : FN funbody | FUN funbody ;
returnexpr : RETURN expr ;

ifexpr
  : IF ntlexpr THEN blockexpr elifs
  | IF ntlexpr THEN blockexpr
  | IF ntlexpr RETURN expr
  | IF ntlexpr blockexpr ELSE blockexpr
  | IF ntlexpr blockexpr
  ;

elifs : ELIF ntlexpr THEN blockexpr elifs | ELSE blockexpr ;

valexpr : VAL apattern EQ blockexpr IN expr ;

// ── Operators / application ──────────────────────────────────────────

opexpr     : opexpr qoperator prefixexpr | prefixexpr ;
prefixexpr : '!' prefixexpr | '~' prefixexpr | appexpr ;

appexpr
  : appexpr OPEN_PAREN arguments CLOSE_PAREN
  | appexpr OPEN_BRACK arguments CLOSE_BRACK
  | appexpr DOT name
  | appexpr DOT OPEN_PAREN arguments CLOSE_PAREN
  | appexpr block
  | appexpr fnexpr
  | atom
  ;

// Non-trailing-lambda expression: a matcher/if/handler scrutinee must not
// swallow a following block (which is that construct's own body).
ntlexpr       : ntlopexpr ;
ntlopexpr     : ntlopexpr qoperator ntlprefixexpr | ntlprefixexpr ;
ntlprefixexpr : '!' ntlprefixexpr | '~' ntlprefixexpr | ntlappexpr ;

ntlappexpr
  : ntlappexpr OPEN_PAREN arguments CLOSE_PAREN
  | ntlappexpr OPEN_BRACK arguments CLOSE_BRACK
  | ntlappexpr DOT name
  | ntlappexpr DOT OPEN_PAREN arguments CLOSE_PAREN
  | atom
  ;

atom
  : name
  | literal
  | mask
  | OPEN_PAREN aexprs CLOSE_PAREN
  | OPEN_BRACK cexprs CLOSE_BRACK
  | ctxexpr
  | ctxhole
  | etahole
  ;

name : qidentifier | qconstructor | qimplicit ;

literal : INT | FLOAT | CHAR | STRING | RAWSTRING ;

mask    : (MASK | 'inject') behind LT targuments1 GT ;
behind  : 'behind' | ;
ctxexpr : CTX atom ;
ctxhole : HOLE ;
etahole : '_' ;

arguments : | argument (COMMA argument)* COMMA? ;

argument
  : aexpr
  | identifier EQ expr
  | qimplicit EQ expr
  ;

// ── Parameters ───────────────────────────────────────────────────────

parameters : | parameter (COMMA parameter)* COMMA? ;

parameter
  : PUB? borrow paramid? COLON QUESTION? ktype (EQ expr)?
  | PUB? borrow qimplicit COLON QUESTION? ktype (EQ expr)?
  ;

borrow : '^' | ;
paramid : identifier | wildcard ;

pparameters : | pparameter (COMMA pparameter)* COMMA? ;

pparameter
  : borrow pattern (COLON ktype (EQ expr)? | EQ expr)?
  | borrow qimplicit (COLON ktype)?
  ;

// ── Annotated / list expressions ─────────────────────────────────────

aexprs : | aexpr (COMMA aexpr)* COMMA? ;
aexpr  : expr annot ;
annot  : COLON typescheme | ;

cexprs : | aexpr (COMMA aexpr)* COMMA? ;

// ── Handlers ─────────────────────────────────────────────────────────

handlerexpr
  : override HANDLER witheff 'scoped'? opclauses
  | override HANDLE witheff 'scoped'? ntlexpr opclauses
  | NAMED HANDLER witheff 'scoped'? opclauses
  | NAMED HANDLE witheff 'scoped'? ntlexpr opclauses
  ;

override : OVERRIDE | ;
witheff  : LT anntype GT | ;

withstmt_binder : qidentifier (COLON ktype)? | wildcard | OPEN_PAREN apatterns CLOSE_PAREN ;

withstat
  : WITH basicexpr
  | WITH withstmt_binder (LARROW | EQ) basicexpr
  | WITH override witheff opclause
  | WITH withstmt_binder (LARROW | EQ) witheff opclause
  | WITH override witheff opclauses
  | WITH withstmt_binder (LARROW | EQ) witheff opclauses
  ;

withexpr : withstat IN blockexpr ;

opclauses : OPEN_BRACE semis opclausexs CLOSE_BRACE ;

opclausexs : (opclausex semis)* ;

opclausex
  : 'finally' bodyexpr
  | 'initially' OPEN_PAREN opparam CLOSE_PAREN bodyexpr
  | opclause
  ;

opclause
  : VAL qidentifier EQ blockexpr
  | VAL qidentifier COLON ktype EQ blockexpr
  | FUN qidentifier opparams bodyexpr
  | controlmod CTL qidentifier opparams bodyexpr
  | controlmod 'control' qidentifier opparams bodyexpr
  | RETURN OPEN_PAREN opparam CLOSE_PAREN bodyexpr
  ;

opparams : OPEN_PAREN opparamlist CLOSE_PAREN ;
opparamlist : | opparam (COMMA opparam)* COMMA? ;
opparam : paramid (COLON ktype)? ;

// ── Patterns ─────────────────────────────────────────────────────────

pattern
  : identifier AS pattern
  | identifier
  | conid OPEN_PAREN patargs CLOSE_PAREN
  | qconid OPEN_PAREN patargs CLOSE_PAREN
  | conid
  | qconid
  | OPEN_PAREN apatterns CLOSE_PAREN
  | OPEN_BRACK apatterns CLOSE_BRACK
  | literal
  | wildcard
  ;

patargs : | patarg (COMMA patarg)* COMMA? ;
patarg  : identifier EQ apattern | apattern ;

apatterns : | apattern (COMMA apattern)* COMMA? ;
apattern  : pattern annot ;

// ── Types ────────────────────────────────────────────────────────────

typescheme : someforalls tarrow qualifier ;

ktype : someforalls tarrow qualifier ;

someforalls
  : SOME typeparams1 FORALL typeparams1
  | SOME typeparams1
  | FORALL typeparams1
  | EXISTS typeparams1
  | ;

typeparams : typeparams1 | ;
typeparams1 : LT tbinders GT ;
tbinders : | tbinder (COMMA tbinder)* COMMA? ;
tbinder : varid kannot ;

qualifier : WITH OPEN_PAREN predicates1 CLOSE_PAREN | ;
predicates1 : predicate (COMMA predicate)* COMMA? ;
predicate : typeapp ;

tarrow : tatomic (ARROW tresult)? ;

tresult : tatomic tbasic? ;

tatomic
  : tbasic
  | LT targuments1 PIPE tatomic GT
  | LT targuments GT
  ;

tbasic
  : typeapp
  | OPEN_PAREN tparams CLOSE_PAREN
  | OPEN_BRACK anntype CLOSE_BRACK
  ;

typeapp : typecon (LT targuments GT)? ;

typecon
  : varid
  | qvarid
  | wildcard
  | OPEN_PAREN commas1 CLOSE_PAREN
  | OPEN_BRACK CLOSE_BRACK
  | OPEN_PAREN ARROW CLOSE_PAREN
  | CTX
  ;

tparams : | tparam (COMMA tparam)* COMMA? ;
tparam  : (identifier | qimplicit) COLON anntype | anntype ;

targuments  : | anntype (COMMA anntype)* COMMA? ;
targuments1 : anntype (COMMA anntype)* COMMA? ;

anntype : ktype kannot ;

kannot : DCOLON kind | ;

kind
  : OPEN_PAREN kinds1 CLOSE_PAREN ARROW katom
  | katom ARROW kind
  | katom
  ;

kinds1 : kind (COMMA kind)* COMMA? ;
katom  : conid ;

// ── Names ────────────────────────────────────────────────────────────

modulepath : IDENT (DOT IDENT)* | QID ;

qoperator : op ;

qidentifier : qvarid | QIDOP | identifier ;
identifier  : varid | IDOP ;
varid       : IDENT ;
wildcard    : WILDCARD ;
qimplicit   : QUESTION qidentifier ;

qvarid       : QID ;
qconstructor : conid | qconid ;
qconid       : QCONID ;
conid        : CONID ;

op : OP | GT | LT | PIPE | ASSIGN ;

// ── Lexer ────────────────────────────────────────────────────────────

// Layout: a line break plus the following line's leading indentation. The
// Koka layout policy consumes these and inserts braces/semicolons, so the
// parser never sees NEWLINE.
NEWLINE : ('\r'? '\n' | '\r') [ \t]* ;

// Comments: a line comment, and a block comment that nests.
LINE_COMMENT : '//' ~[\r\n]* -> skip ;
BLOCK_COMMENT_EMPTY : '/**/' -> skip ;
BLOCK_COMMENT_START : '/*' [-$%&*+~!\\^=#.:|<>/]* -> pushMode(COMMENT), skip ;

// Keywords (hard reserved words).
MODULE   : 'module' ;
IMPORT   : 'import' ;
AS       : 'as' ;
PUB      : 'pub' ;
ABSTRACT : 'abstract' ;
TYPE     : 'type' ;
ALIAS    : 'alias' ;
EFFECT   : 'effect' ;
STRUCT   : 'struct' ;
CON      : 'con' ;
FORALL   : 'forall' ;
EXISTS   : 'exists' ;
SOME     : 'some' ;
FUN      : 'fun' ;
FN       : 'fn' ;
VAL      : 'val' ;
VAR      : 'var' ;
EXTERN   : 'extern' ;
IF       : 'if' ;
THEN     : 'then' ;
ELSE     : 'else' ;
ELIF     : 'elif' ;
MATCH    : 'match' ;
RETURN   : 'return' ;
WITH     : 'with' ;
IN       : 'in' ;
HANDLE   : 'handle' ;
HANDLER  : 'handler' ;
MASK     : 'mask' ;
CTL      : 'ctl' ;
FINAL    : 'final' ;
RAW      : 'raw' ;
OVERRIDE : 'override' ;
NAMED    : 'named' ;
CTX      : 'ctx' ;
HOLE     : 'hole' ;
INFIX    : 'infix' ;
INFIXL   : 'infixl' ;
INFIXR   : 'infixr' ;

// Punctuation. Multi-character tokens before the generic operator rule.
ARROW  : '->' ;
LARROW : '<-' ;
ASSIGN : ':=' ;
DCOLON : '::' ;
OPEN_BRACE  : '{' ;
CLOSE_BRACE : '}' ;
OPEN_PAREN  : '(' ;
CLOSE_PAREN : ')' ;
OPEN_BRACK  : '[' ;
CLOSE_BRACK : ']' ;
SEMI   : ';' ;
COMMA  : ',' ;
COLON  : ':' ;
DOT    : '.' ;
EQ     : '=' ;
PIPE   : '|' ;
LT     : '<' ;
GT     : '>' ;
QUESTION : '?' ;

// Identifiers. A qualified name (`std/core/list`) is one token: in Koka `/`
// is the module separator, and arithmetic division needs spaces around it.
IDENT
  : [@]? [a-z] [-a-zA-Z0-9_@'?]* ;

CONID
  : [@]? [A-Z] [-a-zA-Z0-9_@'?]* ;

QID
  : ([@]? [a-z] [-a-zA-Z0-9_@'?]* '/')+ [@]? [a-z] [-a-zA-Z0-9_@'?]* ;

QCONID
  : ([@]? [a-z] [-a-zA-Z0-9_@'?]* '/')+ [@]? [A-Z] [-a-zA-Z0-9_@'?]* ;

QIDOP
  : ([@]? [a-z] [-a-zA-Z0-9_@'?]* '/')+ OPEN_PAREN_IDOP ;

IDOP : OPEN_PAREN_IDOP ;

fragment OPEN_PAREN_IDOP
  : '(' [@]? [-a-zA-Z0-9_@'?]* [-$%&*+~!\\^=#.:|<>/]+ ')' ;

WILDCARD
  : [@]? '_' [-a-zA-Z0-9_@'?]* ;

// Numbers: Koka allows a sign on a literal, `_` digit separators, hex
// integers and hex floats.
FLOAT
  : '-'? [0-9] ([_]? [0-9])* '.' [0-9] ([_]? [0-9])* ([eE] [-+]? [0-9]+)?
  | '-'? [0-9] ([_]? [0-9])* [eE] [-+]? [0-9]+
  | '-'? '0' [xX] [0-9a-fA-F] ([_]? [0-9a-fA-F])*
         ( '.' [0-9a-fA-F] ([_]? [0-9a-fA-F])* )? [pP] [-+]? [0-9]+
  | '-'? '0' [xX] [0-9a-fA-F] ([_]? [0-9a-fA-F])*
         '.' [0-9a-fA-F] ([_]? [0-9a-fA-F])*
  ;

INT
  : '-'? ( '0' [xX] [0-9a-fA-F] ([_]? [0-9a-fA-F])*
          | '0'
          | [1-9] ([_]? [0-9])* )
  ;

CHAR : ['] ('\\' [xXuU] [0-9a-fA-F]+ | '\\' . | ~['\r\n]) ['] ;

STRING : '"' ('\\' . | ~["\r\n])* '"' ;

// Raw strings keep their delimiters: `r"..."`, `r#"..."#"`, ... and may span
// lines (`.?` is non-greedy; `.` matches any character). The grammar matcher
// cannot back-reference an arbitrary `#` count, so up to three hashes are
// supported (every corpus use has zero or one); more is a documented limit.
RAWSTRING
  : 'r' '###' '"' .*? '"' '###'
  | 'r' '##' '"' .*? '"' '##'
  | 'r' '#' '"' .*? '"' '#'
  | 'r' '"' .*? '"'
  ;

// A user operator is a run of symbol characters. Single-character operators
// that are syntax above were matched first (equal length ties go to the
// earlier rule).
OP : [-$%&*+~!\\^=#.:|<>/]+ ;

// ── Comment mode (nested block comments) ─────────────────────────────

mode COMMENT;

CM_NEST : '/*' -> pushMode(COMMENT), skip ;
CM_END  : '*/' -> popMode, skip ;
CM_TEXT : ~[*/]+ -> skip ;
CM_CHAR : [*/] -> skip ;
