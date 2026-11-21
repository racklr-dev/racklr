# JavaScript ANTLR4 grammar (vendored, cleaned and hardened)

`JavaScriptLexer.g4` and `JavaScriptParser.g4` are the JavaScript front end's
grammar. They are derived from
[antlr/grammars-v4](https://github.com/antlr/grammars-v4) `javascript/javascript`
(the checkout vendored in the repository root's untracked `grammars-v4/` tree,
at that checkout's HEAD `923a1a9`), published under the MIT license reproduced
in the file headers.

They are **not** verbatim upstream: racklr's ANTLR4 parser generator is a
recursive-descent generator with ordered choice, greedy optionals and no
semantic predicates or parser base classes (see the README's "Grammar generator limitations"), so the grammar is the "cleaned" subset that generator
can execute. It is tracked here (like `grammars/python3/`, `grammars/rust/`
and `grammars/koka/`) so a clean checkout runs the JavaScript front end without
the untracked vendored tree; it was previously an untracked
`grammars-v4/javascript/javascript-cleaned/` copy.

## Local modifications

Cleaning (before this front end was tracked):

1. Semantic predicates (`{this.notLineTerminator()}?`,
   `{this.notOpenBraceAndNotFunction()}?`, `{this.n("static")}?`, …) removed;
   the parser generator ignores actions/predicates.
2. The `superClass = JavaScriptParserBase;` hook and the labelled alternatives
   the generator does not need were dropped; rules the generator could not
   express as ordered choice were restructured.
3. `eos` became `SemiColon | EOF | ;` after dropping the
   `lineTerminatorAhead`/`closeBrace` predicates.

Hardening for real-world source (issue #122, measured by
`tools/js-ts-corpus.rkt` over express/axios — see
`docs/js-ts-real-world-battery.md`):

1. `classElement`: replaced `(Static | identifier)? methodDefinition | …` with
   `Static methodDefinition | methodDefinition | Static fieldDefinition |
   fieldDefinition | Static block | emptyStatement_`. The greedy optional prefix
   consumed a plain method or field name and then failed, so only `static`
   members parsed; plain methods, public fields and constructors did not.
2. `singleExpression`: added `singleExpression '?.' arguments
   # OptionalArgumentsExpression` so `obj?.()` parses (only `obj?.prop` did).
3. `singleExpression`: added `Import '.' identifier # ImportMetaExpression`
   (`import.meta.url`) and `'#' identifierName # PrivateNameExpression`
   (the `#field in obj` brand check).
4. `assignmentOperator`: added `'&&='` and `'||='` (the lexer now tokenizes
   `AndAssign` / `OrAssign`; previously they lexed as `&&`/`=` and failed).

Template-literal lexing uses the shared `TEMPLATE` lexer-mode bookkeeping in
`racklr/gend-parser/language-policy.rkt`; nested templates are handled by its
per-nesting brace-frame stack (a scalar depth left the scanner in template mode
after a nested closing backtick).
