# JavaScript/TypeScript front end: real-world corpus capability battery (express + axios)

Goal: the JavaScript and TypeScript front ends absorb real-world framework and
library source cleanly, the way the Python front end was hardened against
Django. The measurement is the JS/TS slice of the cross-language capability
battery, committed as `tools/js-ts-corpus.rkt`; this document records the
before/after numbers, the failure taxonomy and the fixes.

The motivating report is issue #122: over vendored `expressjs/express` and
`axios/axios`, 40% of express JavaScript files and 61% of axios JavaScript
files either raised during absorb or lowered with `?`-prefixed unhandled-node
symbols. The two causes turned out to be separate: most `error` rows were
parser gaps on ordinary modern JavaScript (class members, optional calls,
logical assignment, nested templates), and most `fallback` rows were missing
destructuring lowering. A third class — `reparse-fail` — was invalid *emitted*
JavaScript: statement-`if`s were rendered as expression ternaries and named
function parameters after the first were dropped.

## Corpus

- `/tmp/battery-corpus/express` — expressjs/express `43fca52d28a4`
  (2026-11-02), 143 selected files (142 `.js`, 1 `.json`).
- `/tmp/battery-corpus/axios` — axios/axios `7324e94d8a7b` (2026-11-02),
  275 selected files (226 `.js`, 30 `.ts`, 19 `.json`).

The corpus is not vendored into this repository. The scanner selects
`.js`/`.mjs`, `.ts`, `.tsx` and `.json`, skipping `node_modules`, `.git`,
`compiled`, `__pycache__`, `.venv`, `venv`, `dist`, `build` and `vendor`.
`.cjs` is deliberately not selected: it is the CommonJS configuration path
tracked separately (#112), and the historical battery left it out. The current
axios clone has two files more than the revision the issue measured
(226 `.js` vs 225, 30 `.ts` vs 29), so the number of files differs slightly
from the issue text while the corpus is the same upstream project.

## Reproduce

```bash
# from the repository root, checkout on PLTCOLLECTS
PLTCOLLECTS="$(pwd):" racket tools/js-ts-corpus.rkt \
    /tmp/battery-corpus/express /tmp/battery-corpus/axios \
    --json /tmp/js-ts-after.json [--timeout-ms 120000]
```

Full mode classifies each file as `ok`, `tokenize-error`, `parse-fail`,
`lower-error`, `fallback` (a `?`-prefixed unhandled-node symbol survives in
the UIR; the `?.` and `??` operator encodings are not fallbacks),
`emit-error`, `emit-empty`, `reparse-fail` or `timeout` (a single-file
pipeline exceeded the `--timeout-ms` watchdog; the scan continues).
`--parse-only` stops after parse and reports `tokenize-error` / `parse-fail` /
`parse-ok`. Exit code 0 means every selected file is ok; 1 means at least one
blocker; 2 is a usage error. The per-file result set is written only when
`--json PATH` is given.

The generated parsers come from the tracked `grammars/javascript/`,
`grammars/typescript/` and `grammars/jsx/` grammars; a clean checkout needs no
vendored `grammars-v4/` tree for the JS/TS front ends.

## Whole-corpus totals

BEFORE was measured on the untouched `ddb2915` tree (the ticket branch's base)
with this same tool, corpus and watchdog, so the comparison is like for like;
the previous untracked `grammars-v4/javascript/*-cleaned` grammars were copied
into the tracked layout for that run. The issue's own 2026-11-01 numbers were
`express javascript: 32 error / 25 fallback / 85 ok` and
`axios javascript: 91 error / 46 fallback / 88 ok`; the same failure classes
show up here with stage separation (the battery's single `error` bucket is our
`parse-fail` + `reparse-fail`), at slightly different file counts because the
axios corpus gained two files since then.

| status | express | axios |
| ------ | ------- | ----- |
| ok | 91 | 117 |
| fallback | 25 | 51 |
| reparse-fail | 26 | 32 |
| parse-fail | 1 | 71 |
| lower-error | 0 | 2 |
| emit-error / emit-empty | 0 | 0 |
| tokenize-error | 0 | 2 |
| timeout | 0 | 0 |

AFTER (this branch, same corpus/tool/watchdog):

| status | express | axios |
| ------ | ------- | ----- |
| ok | 142 | 248 |
| fallback | 0 | 7 |
| reparse-fail | 1 | 16 |
| parse-fail | 0 | 1 |
| lower-error | 0 | 0 |
| emit-error / emit-empty | 0 | 0 |
| tokenize-error | 0 | 2 |
| timeout | 0 | 1 |

JavaScript is the issue's headline: express JavaScript goes from 91/142 clean
(64.1%) to 142/142 (100%), and axios JavaScript from 103/226 clean (45.6%) to
225/226 (99.6%; the one remaining row is the watchdog timeout below).
Per-language AFTER totals (TypeScript and JSON are carried by the same
scanner):

| language | files | ok | fallback | reparse-fail | parse-fail | tokenize-error | timeout |
| -------- | ----- | -- | -------- | ------------ | ---------- | -------------- | ------- |
| javascript | 226 | 225 | 0 | 0 | 0 | 0 | 1 |
| typescript | 30 | 22 | 7 | 1 | 0 | 0 | 0 |
| json | 19 | 1 | 0 | 15 | 1 | 2 | 0 |

The axios totals are the merge of per-subdirectory runs of the same tool over
the same tree (the whole directory in one process exceeds the harness job
lifetime on the largest test files); every file appears in exactly one shard.
The TypeScript rows are from a TypeScript-only run of the same tool over the
same 30 files with the same watchdog (the first shard runs predate the last
TypeScript grammar fixes).

## Fixes

### Grammar: the JS/TS grammars are now tracked

The front ends previously developed against untracked, locally-edited
`grammars-v4/javascript/{javascript,typescript,jsx}-cleaned/` copies (the
"cleaned" = semantic-predicate-free ANTLR sources the generated-parser
generator can consume). That made the front ends unreproducible from a clean
checkout and made this corpus work impossible to commit. The grammars are now
tracked as `grammars/javascript/`, `grammars/typescript/` and `grammars/jsx/`
(the same layout as `grammars/python3/`, `grammars/rust/` and `grammars/koka/`),
and every test/tool reference points at the tracked copies.

### Grammar gaps found by the corpus

- **Class members.** `classElement` began with a greedy optional
  `(Static | identifier)?` before the method/field rules. The generated parser
  has no lookahead, so it consumed a plain method name as the optional prefix
  and then failed: `class A { m() {} }` did not parse, while
  `class A { static m() {} }` did. The rule is now
  `Static methodDefinition | methodDefinition | Static fieldDefinition |
  fieldDefinition | Static block | emptyStatement_`, which parses methods,
  fields, static fields, static blocks, constructors, getters/setters and
  generators.
- **Optional calls.** `obj?.()` had no production; only `obj?.prop` did. An
  ordered suffix alternative `singleExpression '?.' arguments` adds it, and
  the lowering keeps it as a first-class optional call (`optcall`).
- **Logical assignment.** `&&=` and `||=` were split into `&&`/`=` and
  `||`/`=` by the lexer and were missing from `assignmentOperator`; both grammars
  now tokenize and parse them.
- **Nested template literals.** The template-mode bookkeeping in the generated
  lexer used one scalar brace depth, which the inner `${` of a nested template
  reset; the outer `}` was then miscounted and the scanner stayed in template
  mode (`` `a${`b${c}`}` `` tokenized the trailing `;` as template text). The
  bookkeeping is now a per-nesting frame stack.
- **Fragment rules were token candidates in lexer modes.** A `fragment` rule
  textually after a `mode` declaration belongs to that mode's CST node, and the
  generator listed it as a token matcher in the mode. In the `TEMPLATE` mode the
  `DecimalIntegerLiteral` fragment then beat the one-character
  `TemplateStringAtom` on a digit run, so `` `a ${code}. 1000.` `` produced a
  numeric-literal token in template text and the file did not parse. Mode
  sections now list only token rules; fragments stay matcher helpers.
- **`import.meta` and private-name expressions.** `import.meta.url` and
  `#field in obj` brand checks had no expression production; both are added.

### Lowering gaps found by the corpus

- **Destructuring declarations and `for-of`/`for-in` bindings.** `const {a,
  b: c = 1, ...rest} = o` and `for (const [k, v] of xs)` lowered
  `assignable` to a `?` placeholder. `racklr/lower-javascript/pattern.rkt`
  lowers binding patterns structurally (`object-pattern`, `array-pattern`,
  `bind-rename`, `bind-default`, `bind-rest`, `bind-computed`,
  `pattern-hole`) and `emit-javascript.rkt` renders them back to JS binding
  syntax, so the binding structure survives the round trip instead of being
  dropped. This was the single largest fallback class.
- **Named-function parameters.** `formalParameterList` after the first
  parameter sits in a repetition group; iterating the rule's direct children
  dropped every later parameter (`function (req, res, next)` emitted
  `function(req)`). Parameter collection now walks repetition groups.
- **Statement-`if` emission.** A statement `if` lowered to `uir-if` with
  non-block branches, and the emitter's ternary form of `uir-if` then rendered
  `err ? throw err : null` / `!x ? return v : null` — invalid JavaScript, so
  the emitted text did not re-parse. Statement branches are now wrapped in
  blocks so the emitter keeps the statement form.
- **Keyword property names.** `promise.catch(...)`, `module.new` and other
  reserved-word property names lowered to `?` because the name helper only
  looked for an `identifier` child; it now falls back to the property token
  text.
- **Anonymous class expressions and operator emission.** `class {}` lowered to
  a `?` name that emitted `class ? {}`; an empty name now emits `class {}`.
  The emitter's infix vocabulary gained `?.`, `??`, `in`, `instanceof`,
  `&`, `^`, `|`, `<<`, `>>`, `>>>`; without them those operators emitted as
  pseudo-calls (`in(a, b)`) that did not re-parse.
- **Arrow-function bodies that are blocks.** `arrowFunctionBody` listed
  `singleExpression` before `functionBody`, so `(fn) => { if (fn) { ... } }`
  parsed its block as an *object literal* (the checked-in generated parser has
  no lookahead); the body lowered to an object with a `?`-free but wrong
  structure and the emitted text did not re-parse. A `{` after `=>` is always a
  block body per the spec, so `functionBody` is now the first alternative.
- **Empty `catch` blocks.** An empty `catch (e) {}` lowered to `uir-null` and
  the emitter dropped the clause, emitting `try { ... }` without catch or
  finally (invalid JS). A present catch clause now lowers to an empty block and
  emits as `catch (e) { }` / `catch { }`.
- **Regex literal vs division.** A JavaScript lexer cannot decide `/` from the
  character alone; the generated tokenizer lexed from the first `/` to the next
  one as a regular-expression literal, so `total ? loaded / total : x` (and any
  line with two divisions) failed. The generator now accepts a host
  `lexer-rule-guard` (data in the language policy) and the JS/TS policy rejects
  a regex match when the previous emitted token can end an expression, letting
  maximal munch fall through to `Divide`.
- **Compound assignment into a member.** `sponsorsByTier[tier] ||= []` lowered to
  an infix call, but the emitter's infix vocabulary only had `+= -= *= /= **=`;
  the other compound assignments (`%=`, shifts, bitwise, `&&=`, `||=`, `??=`)
  emitted as pseudo-calls (`||=(x, y)`), which did not re-parse. All compound
  assignment operators are now in the infix vocabulary.
- **`import.meta`, dynamic `import()` and private names.** `import.meta` as an
  expression hit the binary-infix path and crashed (`cst-node-children` on the
  `Import` token — 14 axios files); `import('...')` was silently dropped;
  `#field` in a brand check was lowered as a unary operator. All three now have
  their own lowering (`dot(import, meta)`, `dynamic-import`, `private-name`)
  and emitter forms.
- **For-in/for-of binding kind.** The loop's `var`/`let`/`const` was dropped, so
  `for (const [k, v] of xs)` emitted `for ([k, v] of xs)` and re-lowered to a
  different UIR. The binding now keeps its kind (`for-binding`) so the emitted
  loop is `for (const ...)`.

### TypeScript / TSX

The TSX preprocess used by the TypeScript/TSX front ends rewrites type
declarations before parsing; it was corrupting the source rather than the
parser rejecting it. `racklr/lower-tsx/hooks.rkt` now targets only the brace
that opens a declaration body (so `type T = A | (string & {})` is untouched),
skips matches inside comments/strings (prose ending in "type" no longer injects
a `;` into a later annotation), only terminates a type declaration when it does
not continue (`&`, `|`, `[]`, `)` …), inserts member separators only at bracket
depth 0 (multi-line call signatures survive), erases type-only
`import type`/`export type` statements the grammar has no production for, and
adds import ASI safely. The TypeScript grammar also gained the productions the
corpus needed: conditional types, labeled tuple elements, `declare`
function/class, type predicates ordered before plain type references, full
types after `as` (including the erased empty-bracket suffix for
`(x as A.B[])[0]`), and `&&=`/`||=`. TypeScript lowering now erases ambient
`declare` declarations to the documented erased-type marker, lowers class
members beyond plain property methods (constructors, accessors, index
signatures, static blocks — this also fixed the `cst-node-children … #f`
crash on two files), renders statement-`if`s as `if/else`, emits named
`export { a }` as an export instead of an object literal, and lowers
`instanceof`/`in` as infix operators.

TypeScript result on axios: BEFORE 13/30 ok (8 parse-fail, 2 lower-error,
6 fallback, 1 reparse-fail); AFTER 22/30 ok (0 parse-fail, 0 lower-error,
7 fallback, 1 reparse-fail). The tsx path has no corpus file; its suites
(`gen-jsx-test`, `tsx-integration-test`) stay green. Residual TS gaps:
8 files whose lowering still leaves `?` symbols when recovering names
(`cjs-typing.ts`, `esm-index.ts`, four bun/deno smoke files) and
`index.d.ts` — its 22 KB emitted JavaScript does not re-parse.

## Remaining gaps

- **JavaScript: one watchdog timeout, no uncovered constructs.** The last
  JavaScript row that is not `ok` is `tests/unit/adapters/http.test.js`, which
  exceeds the 120 s per-file watchdog (the single-file pipeline is parse plus
  re-parse of a very large, densely nested file). It is a parser-performance
  limit of the generated recursive-descent parser, not a grammar/lowering gap;
  every other `.js` file in both corpora is `ok`.
- **TypeScript: 7 fallback files and one reparse failure.** Six files
  (`cjs-typing.ts`, `esm-index.ts`, and four bun/deno smoke tests) still leave
  `?` symbols when a lowered declaration recovers a binding name, and the
  22 KB emitted JavaScript for `index.d.ts` does not re-parse. These are the
  remaining TypeScript lowering-completeness gaps.
- **JSON front end (out of scope, tracked by #96).** 18 of the 19 canonical
  axios JSON files fail: the checked-in generated JSON tokenizer requires a
  doubled backslash for its escape rule, so a string containing an escaped
  quote (`\"...\"`, which `package.json` scripts and lock files use) fails to
  tokenize; `devcontainer.json` is JSONC and does not parse. The JSON front end
  is not part of the JS/TS ask and its regeneration is tracked as follow-up
  #96.

- **JSON front end (out of scope, tracked by #96).** The two failing canonical
  axios JSON files contain escaped quotes (`\"...\"`) inside strings; the
  checked-in generated JSON tokenizer requires a doubled backslash for its
  escape rule. The JSON front end is not part of the JS/TS ask and its
  regeneration is tracked as follow-up #96.
- **Parser performance.** Parse time grows faster than linearly with file
  size: the largest express test files (several hundred lines with dense
  expression nesting) can take tens of seconds to parse and double that to
  re-parse, which is why the scanner carries a per-file watchdog and why a
  handful of large files are reported `timeout` at a 120s budget rather than
  `ok`. This is a parser-generator backtracking cost, not a coverage gap; the
  file still absorbs when given the budget.

## Provenance

- Tracked grammars before hardening (the untracked cleaned copies the front
  ends used), sha256:
  - `JavaScriptParser.g4` `bff23846c3a557ec01aa075284b045454b4b20fe0b476f92c7b42cccf90ce63f`
  - `TypeScriptParser.g4` `6c62466324c162327bd7420278ca0f04ecb824b6cbcb8f9561655d35b118d931`
  - `JSXParser.g4` `6652df368ec7d46e727d0f3489f0ac55f119ef4b93fd3f467becb0e4d2600338`
- Tracked grammars after hardening, sha256:
  - `grammars/javascript/JavaScriptParser.g4` `879352e5b3d7225d4984d971291d32cf2831531524f24c4249d86103d807bffe`
  - `grammars/typescript/TypeScriptParser.g4` `e7b1abd5fe40bbe2d9a260efde716b1974ed22e2bbf3eecc833e05a1042492cd`
  - `grammars/jsx/JSXParser.g4` `6652df368ec7d46e727d0f3489f0ac55f119ef4b93fd3f467becb0e4d2600338`
- Corpus revisions: express `43fca52d28a422c8be48fd489cbd4f2d6152bab6`,
  axios `7324e94d8a7b5156a4baf3c86c8b5daa242daee3`.
- BEFORE was run from a pristine `git worktree` at `ddb2915` with
  `tools/js-ts-corpus.rkt` copied in and the pre-hardening cleaned grammars in
  the tracked layout; AFTER from this branch. Both used `--timeout-ms 120000`
  and the same corpus directories.
