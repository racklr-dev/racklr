# OCaml ANTLR4 grammar (authored Pass 1)

`OCamlLexer.g4` and `OCamlParser.g4` are **authored from scratch**, not
vendored. There is no OCaml grammar in
[antlr/grammars-v4](https://github.com/antlr/grammars-v4): the upstream tree
has no `ocaml/` directory. That was checked directly at acquisition time:

```
$ gh api repos/antlr/grammars-v4/contents/ocaml
{"message":"Not Found","documentation_url":"https://docs.github.com/rest/repos/contents#get-repository-content"}gh: Not Found (HTTP 404)
```

The plan (`docs/plans/ocaml-pipeline.md`, "Current State") offered two options:
author `OCamlLexer.g4`/`OCamlParser.g4`, or adapt the third-party
`tree-sitter-ocaml` grammar. This directory takes the authoring route: porting
tree-sitter's grammar would carry its own license and node model into a
repository whose generator is not tree-sitter, and the generator cannot execute
large parts of a full OCaml grammar anyway (see "Generator constraints" below).
`tree-sitter-ocaml` was considered as the adapt option, not copied; the files
here are original text, so no third-party license applies.

## Reference used

The syntax follows the [OCaml 5.2 manual](https://ocaml.org/manual/5.2/)
chapters on lexical conventions, expressions, patterns, type and module
expressions. The Pass-1 fixture set in `docs/plans/ocaml-pipeline.md` defines
the coverage target; the grammar intentionally stops at that set rather than
attempting all of OCaml.

Tests: `racklr-test/gen-ocaml-test.rkt` loads the parser grammar through
`gen-and-load` with the tracked relative path `../grammars/ocaml/`, which also
resolves `options { tokenVocab = OCamlLexer; }` from this directory. The suite
asserts that the resolved path is under `grammars/ocaml/` and never under the
untracked `grammars-v4/` tree (including that the tracked `OCamlLexer.g4` sits
beside the parser), so it runs in a clean checkout with no `grammars-v4/`
present.

## Generated cost (observed 2026-11-01)

Recorded so a future reader knows the compile cost of the authored pair, using
the same `generate-parser-module` path as the tests:

| part | source size | rules | generated Racket module |
| --- | --- | --- | --- |
| `OCamlLexer.g4` | 172 lines | 75 lexer + 5 fragment + 1 mode | merged into the parser's generated module via `tokenVocab` |
| `OCamlParser.g4` | 243 lines | 63 parser rules | 88,267 bytes / 717 lines (~86 KB); generation ~5 ms |

The generated module carries both halves (the parser's `tokenVocab` lexer is
merged), so ~86 KB is the full pair's cost.

## Why a lexer/parser pair and not a combined grammar

The plan names the pair, and the generator resolves `tokenVocab` through the
parser's source path (`generate-parser-module #:source-path`), the same path the
tracked Python3 front end uses. Keeping the lexer separate makes the mode stack
for nested comments and the token name surface explicit.

## Coverage (Pass-1 fixtures from the plan)

| Fixture | Rules that own it |
| --- | --- |
| `let ... in`, `let rec ... and` | `let_in_expr`, `let_definition`, `let_binding`, `let_lhs`/`let_arg` |
| `fun`, `function` lambdas | `fun_expr`, `fun_params`, `function_expr` |
| calls, incl. labeled args | `app_expr`, `argument`, `labeled_argument`, `postfix_expr` |
| operators and precedence | `tuple_expr` down to `unary_expr` (one rule per manual precedence level) |
| `if`/`then`/`else` | `if_expr` |
| `match`, patterns, `when` guards | `match_expr`, `match_cases`, `match_case`, `pattern` down to `atomic_pattern` |
| `type ... = A \| B of int` | `type_definition`, `type_binding`, `variant_type`, `constructor_decl` |
| record types and record expressions | `record_type`, `record_expr`, `record_fields`, `record_field` |
| `module M = struct ... end`, `open` | `module_definition`, `module_expr`, `open_definition` |
| OCaml 5 effect declaration | `extension_definition` (`type _ Effect.t += ...`) |
| `perform` / handler shape | ordinary `app_expr` + `record_expr` + `match_expr` (the plan's "no new UIR" point: effect usage is a call, a record and a match) |

## Deliberate Pass-1 approximations

- No expression sequence rule. `;` sequences are only parsed inside
  `begin ... end` and parentheses (`seq`). A general `expr; expr` layer would be
  greedily consumed inside `[1; 2]` and `{ x = 1; y = 2 }`, where `;` is a
  separator; the plan does not need sequences for its fixture set.
- `constructor_decl` accepts `of` and `:` on both variants and extensions, so
  GADT-style effect constructors and ordinary variants share one rule.
- An arrow type's right side is a `type_expr`, so `int -> int -> int` is
  right-associative; `type_app` is a flat application chain, so `int Effect.t`
  and `(int, string) result` parse as applications.
- `type _ Effect.t += ...` accepts an optional type parameter block, including
  the bare `_` wildcard parameter, and only module-qualified constructor names.
- One extra form is accepted for the plan's handler fixture:
  `fun (type a) -> ...` (locally abstract type parameter). It is a syntax
  concession; the lowering pass still has to interpret it.

## Generator constraints this grammar works around

racklr's generator is a PEG-style recursive-descent generator, not ANTLR4.
The grammar is written for its actual execution model:

- Inside a sequence, elements commit greedily and do not backtrack. Alternatives
  of a rule are tried with longest-match (ties to grammar order), so
  `let f x =` prefers `IDENT pattern+` over the single-pattern base case, and
  `let x = 1 in ...` prefers `let_in_expr` over the shorter `let_definition`.
- Direct left recursion is rewritten into an accumulator loop. That rewrite
  wraps the base case in a node **of the same rule tag**, and inline groups
  such as `(PLUS | MINUS)` become `group` CST nodes. The tests' structural
  assertions read the generated shape explicitly; lowering should treat a
  same-tag singleton wrapper as transparent.
- `~(...)` token sets, semantic predicates and actions are ignored by the
  generator, so the grammar uses none of them.
- The lexer is maximal-munch with ties to the first-defined rule, which is why
  keywords precede `IDENT`, `UNDERSCORE` precedes `IDENT`, and `CHAR_LITERAL`
  precedes `QUOTE_IDENT` (the `'a'` three-character tie).
- `[^...]` class negation is not implemented by the generated `cc-match`, so
  strings/chars use `~[...]` instead.
- Nested comments cannot be a regular lexer rule. A two-mode stack does it:
  every `(*` pushes `COMMENT` and every `*)` pops, so nesting depth is the mode
  stack depth.

## Known limits (follow-up work, not part of Phase 1)

- No error recovery: parsing is all-or-nothing, and the generated entry rule
  raises `match-define: no matching clause for #f` instead of returning `#f`
  when the start rule fails at offset 0. `check-reject` in `racklr/gen-test`
  catches both forms; a lowering caller should use the same guard.
- An **unterminated** `(* ... *)` comment is not reported: the tokenizer reaches
  EOF in `COMMENT` mode and returns EOF, so the comment is silently dropped.
  A real front end needs a lexer-level EOF-in-COMMENT check before lowering.
- Comment tokens are `skip`ped, and the generator now advances the recorded
  line/column across skipped text, so positions after a comment are correct
  today (verified 2026-11-01: `x (* c *) y` reports `y` at column 11). This was
  a generator limitation when the grammar was authored; the tokenizer
  assertions above are the regression guard.
- Unsupported and listed as Phase-2 gaps: functors and first-class modules,
  `sig`/`val`/`include`, objects/classes/`object ... end`, polymorphic variants
  (`` `Foo ``), GADTs beyond the ordinary variant subset, `@@`/`|>`/`|>>`,
  mutation `<-`, `while`/`for` loops, `try ... with` and exceptions, array
  literals `[| ... |]` and array indexing, inline record variants
  (`A of { x : int }`), record field puns in patterns beyond `{ x }`, attributes
  (`[@...]`), quoted-string extensions `{| ... |}`, raw identifiers `\#x`,
  byte literals and integer/float width suffixes. Labeled-argument *types*
  (`~x:int -> ...`) are not parsed; labeled call and parameter syntax is.
- String/char literals are tokenized but not unescaped in Pass 1; numeric
  escapes (`\123`, `\x41`, `\o101`) are consumed as part of the literal.
- Negative literals are a prefix `-` applied to an atom, not one token, matching
  the manual's grammar; a lowering pass needs to fold `-` + literal.

## Refresh / extension

There is no vendored source to re-copy. To check whether an upstream grammar
appeared:

```
gh api repos/antlr/grammars-v4/contents/ocaml   # 404 = still absent
```

If one appears, diff it against this file and decide per rule whether the
generator can execute it; do not replace this grammar wholesale without running
`gen-ocaml-test.rkt` and updating the limits above.

To extend Pass 1: add the token(s) to `OCamlLexer.g4` (order matters only for
maximal-munch ties), add rules to `OCamlParser.g4`, add one tokenizer assertion
and one parser fixture per construct in `racklr-test/gen-ocaml-test.rkt`, then
run it from `racklr-test/` (with `PLTCOLLECTS` pointed at the racklr workspace
root so the workspace generator is loaded, and under `flock` because generated
parser modules are written as `gen-tmp-*.rkt` in the working directory):

```
cd racklr-test
PLTCOLLECTS="<workspace-root>:" flock /tmp/racklr-gen-test.lock \
  racket gen-ocaml-test.rkt
```

The suite must exit 0 and print `... 0 failure(s) 0 error(s)`.
