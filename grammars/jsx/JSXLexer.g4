/*
 * Cleaned JSX lexer — standalone, no semantic predicates.
 * Mode-stack brace counting replaces ProcessOpenBrace/ProcessCloseBrace.
 * Shared token types (<, </, {) use identical names across modes.
 */

lexer grammar JSXLexer;

// ── Unnamed default mode: entry point for JSX parsing ───────────────
JsxOpen              : '<'  -> pushMode(JSX_OPENING);
JsxClose             : '</' -> popMode, pushMode(JSX_CLOSING);
JsxOpenBrace         : '{'  -> pushMode(EXPRESSION);
HtmlChardata         : ~[<{]+;
WhiteSpaces          : [\t\n\r ]+ -> channel(HIDDEN);

// ── JSX_OPENING: inside an opening tag ──────────────────────────────

mode JSX_OPENING;

JsxOpeningEnd        : '>'  -> popMode, pushMode(JSX_CHILDREN);
JsxOpeningSlashEnd   : '/>' -> popMode;
JsxOpeningOpenBrace  : '{'  -> pushMode(EXPRESSION);
JsxName              : [a-zA-Z_][a-zA-Z0-9_\-.]*;
JsxAssign            : '=';
JsxString            : '"' (~["] | '\\' .)* '"'
                     | '\'' (~['] | '\\' .)* '\'';
JsxOpeningWhiteSpaces : [\t\n\r\u000B\u000C\u0020\u00A0]+ -> channel(HIDDEN);

// ── JSX_CHILDREN: children content ──────────────────────────────────

mode JSX_CHILDREN;

// Same token types as default mode:
JsxOpen              : '<'  -> pushMode(JSX_OPENING);
JsxClose             : '</' -> popMode, pushMode(JSX_CLOSING);
JsxOpenBrace         : '{'  -> pushMode(EXPRESSION);
// Same text token:
HtmlChardata         : ~[<{]+;
WhiteSpaces          : [\t\n\r ]+ -> channel(HIDDEN);

// ── JSX_CLOSING: inside a closing tag ───────────────────────────────

mode JSX_CLOSING;

JsxClosingEnd        : '>'  -> popMode;
JsxClosingName       : [a-zA-Z_][a-zA-Z0-9_\-.]*;
JsxClosingWhiteSpaces : [\t\n\r ]+ -> channel(HIDDEN);

// ── EXPRESSION: embedded JavaScript expressions ─────────────────────

mode EXPRESSION;

ExpressionOpenBrace  : '{'  -> pushMode(EXPRESSION);
ExpressionCloseBrace : '}'  -> popMode;
ExpressionText       : ~[{}]+;
ExpressionWhiteSpaces : [\t\n\r ]+ -> channel(HIDDEN);
