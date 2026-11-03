# TypeScript ANTLR4 grammar (vendored, cleaned and hardened)

`TypeScriptLexer.g4` and `TypeScriptParser.g4` are the TypeScript front end's
grammar. They are derived from
[antlr/grammars-v4](https://github.com/antlr/grammars-v4)
`javascript/typescript` (the checkout vendored in the repository root's
untracked `grammars-v4/` tree, at that checkout's HEAD `923a1a9`), published
under the MIT license reproduced in the file headers.

They are **not** verbatim upstream: racklr's ANTLR4 parser generator is a
recursive-descent generator with ordered choice, greedy optionals and no
semantic predicates or parser base classes, so the grammar is the "cleaned"
subset that generator can execute. It is tracked here (like
`grammars/python3/`, `grammars/rust/` and `grammars/koka/`) so a clean
checkout runs the TypeScript/TSX front ends without the untracked vendored
tree; it was previously an untracked
`grammars-v4/javascript/typescript-cleaned/` copy. TypeScript emits through the
JavaScript emitter and re-parses through the JavaScript parser
(`racklr-test/examples-roundtrip-test.rkt`), so its constructs only need to
lower into the shared UIR, not to survive as TypeScript text.

## Local modifications

Cleaning (before this front end was tracked):

1. Semantic predicates and parser base-class hooks removed; alternatives with
   predicates were restructured so ordered choice accepts them.
2. Left-shift/right-shift tokens are handled as literals / split tokens where
   the lexer cannot distinguish generics (`Map<string, Map<string, string>>`);
   see the lexer's comment on `RightShiftArithmetic`.

Hardening for real-world source (issue #122, measured by
`tools/js-ts-corpus.rkt` over the axios corpus — see
`docs/js-ts-real-world-battery.md`):

1. `assignmentOperator`: added `'&&='` and `'||='`; the lexer now tokenizes
   `AndAssign` / `OrAssign` (previously they split into `&&`/`=` and
   `||`/`=` and failed to parse).
2. `singleExpression`: added `singleExpression '?.' arguments
   # OptionalArgumentsExpression` so `obj?.()` parses.

Additional TypeScript/TSX grammar hardening from the same corpus run is
recorded below as it lands.
