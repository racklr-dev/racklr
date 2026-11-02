# Koka front end: real-world corpus capability battery (koka-lang/koka 3.3.0)

Goal: real-world Koka 3.x source parses through the tracked Koka grammar and,
where the lowering/emission layer covers the construct, round-trips through UIR
(absorb -> emit -> re-parse). The measurement is the Koka slice of the
capability battery, committed as `tools/koka-corpus.rkt`; this document is the
record of the before/after numbers and the failure taxonomy.

**Status (2026-11-01): AFTER captured — `ok=1031/1144` (90.1%) full
absorb/emit/re-parse, `parse-ok=1035/1144` (90.5%) parse-only; BEFORE was
`ok=12/1144`. The grammar rewrite (`grammars/koka/Koka.g4`, a Koka 3.x
lexer/parser port with the `koka` layout policy in
`racklr/gend-parser/language-policy.rkt`) and the lowering/emission
modernization have landed; the after numbers below come from one
`--parse-only` run and one full run of the same tool on the frozen tree. The
remaining 113 full-pipeline failures are 109 legacy/experimental parse gaps
(no `lib/std` file), 2 re-parse failures and 2 watchdog timeouts — see the
failure tables and "Remaining lowering gaps".

Corpus: `/tmp/koka-corpus/koka` (koka-lang/koka dev branch, version 3.3.0,
revision `d23ee7cd73737c1da7a7a5fa837a2806facc3d6b`, **1144 `.kk` files**, all
subdirectories included). The corpus is not vendored into this repository;
`.git`, `node_modules`, `compiled`, `.koka`, `build`, `vendor` and dot
directories are skipped by the scanner.

Measurement provenance (AFTER, 2026-11-01): grammar
`sha256:cc9887fba4d664c7d1747072c8f02570fc46f22d9f6b152b53b03261e3d8024f`
(the frozen after tree; the lowering/emitter/lexer/tests were frozen with it and
their hashes were re-verified unchanged during the runs). `tools/koka-corpus.rkt`
(sha256 `238ca81cc7473881009162aa61da8813e2ad2bb29461347b2ebe304a70da4cf1`, with
the per-file `--timeout-ms` watchdog) was run with `PLTCOLLECTS="<checkout>:"`
from a scratch directory; the parser is generated into a private temp dir. The
per-file JSON artifacts stay outside the repository. The parse-only run is one
process over the whole corpus; the full run is the same tool over the same tree
split into 8 balanced, disjoint file sets (143 files each) run in parallel and
merged from the per-file rows (1144 total) — the single-process full scan
exceeded the harness job lifetime, and the tool is file-scoped, so the merge is
exact.

## Reproduce

```
# from the repository root, checkout on PLTCOLLECTS
PLTCOLLECTS="$(pwd):" racket tools/koka-corpus.rkt /tmp/koka-corpus/koka \
    [--parse-only] [--json /tmp/koka-<label>.json] [--timeout-ms 60000]
```

Full mode classifies each file as `ok`, `tokenize-error`, `parse-fail`,
`lower-error`, `fallback` (a `?`-prefixed unhandled-node symbol survives in the
UIR), `emit-error`, `emit-empty`, `reparse-fail`, or `timeout` (a single-file
pipeline exceeded the `--timeout-ms` watchdog, default 60000 ms; the scan
continues). `--parse-only` stops after parse and reports `tokenize-error` /
`parse-fail` / `parse-ok` / `timeout` — the grammar measurement, independent of
lowering coverage. Exit code 0 means every file in the selected mode is ok; 1
means at least one blocker; 2 is a usage error. The per-file result set is
written only when `--json PATH` is given.

`gen-and-load-layout` resolves the grammar's `options { layout = koka; }`
declaration to the Koka brace/semicolon layout policy in
`racklr/gend-parser/language-policy.rkt`, so callers pass no policy argument.

## Whole-corpus totals

BEFORE (2026-11-01; measured with the current `tools/koka-corpus.rkt` against a
frozen pre-rewrite checkout produced by `git archive HEAD`, so the rewrite
cannot change the before numbers):

| status | files |
| ------ | ----- |
| ok | 12 |
| tokenize-error | 542 |
| parse-fail | 590 |
| lower-error | 0 |
| fallback | 0 |
| emit-error | 0 |
| emit-empty | 0 |
| reparse-fail | 0 |

That reproduces the ticket exactly: 12/1144 files pass;
590 fail because the generated parse entry raises
`match-define: no matching clause for #f` (the grammar's `file` rule returned
#f), and 542 fail in the tokenizer. Note that no file reaches the lowering
layer and no file lowers with a `?` fallback: the front end never gets that far.

Failure messages, grouped (before):

| status | count | message |
| ------ | ----- | ------- |
| tokenize-error | 541 | `tokenize: no matching rule in mode default at L:C` |
| tokenize-error | 1 | `insert-indents: unindent does not match any outer indentation level` |
| parse-fail | 590 | `match-define: no matching clause for #f` |

AFTER (2026-11-01, frozen tree; one `--parse-only` run and one full run of the
same tool over the same corpus, per-file JSONs kept outside the repository).
Remaining failure messages are grouped after the tables.

Parse-only AFTER (grammar/tokenizer measurement, independent of lowering):

| status | files |
| ------ | ----- |
| parse-ok | 1035 |
| parse-fail | 109 |
| tokenize-error | 0 |
| files | 1144 |

Parse-only parse-fail classes (109 files, all one generic message
`match-define: no matching clause for #f`; the generated parser entry returns
#f, so the parser gives no token position): 93 under `test/` — 36
`test/misc` (instance/evidence/ambients experiments), 17 `test/algeff`, 10
`test/cgen`, 10 `test/overload`, 4 `test/async`, 4 `test/perf`, 3 `test/syntax`,
2 `test/finally`, 2 `test/lib`, 2 `test/type`, 1 each `test/bayes`,
`test/medium`, `test/parc`; 14 under `lib/v1` (legacy demos/async); 2 under
`samples`. No `lib/std` file remains in the parse-fail set and no file fails in
the tokenizer.

Full AFTER (parse -> lower -> emit -> re-parse):

| status | files |
| ------ | ----- |
| ok | 1031 |
| tokenize-error | 0 |
| parse-fail | 109 |
| lower-error | 0 |
| fallback | 0 |
| emit-error | 0 |
| emit-empty | 0 |
| reparse-fail | 2 |
| timeout | 2 |
| files | 1144 |

Remaining failure messages (AFTER), grouped, one sample file each:

| status | count | message | sample |
| ------ | ----- | ------- | ------ |
| parse-fail | 109 | `match-define: no matching clause for #f` (generated parser entry returned #f) | `test/misc/...` (legacy/experimental set above) |
| reparse-fail | 2 | `match-define: no matching clause for #f` (emitted text did not re-parse) | `test/algeff/reactive1.kk` |
| timeout | 2 | `pipeline exceeded 60000 ms` (single-file watchdog) | `test/static/shadow3a.kk` |

A first AFTER run had 21 `emit-error`s (one `first: contract violation ...
'()` class) caused by one lowering gap: the anonymous-handler sugar
`with { return (x) { ... }; fun get() { ... } }` was parsed but not lowered as a
handler, so emission saw an empty locals list. That is fixed
(`racklr/lower-koka/handler.rkt` + `racklr/emit-koka/expr.rkt`), and this table
is the re-run after the fix: `emit-error=0`. The 2 `reparse-fail`s are `test/algeff/reactive1.kk` and
`test/mask/inject-behind1.kk`. The 2 `timeout`s are `test/static/shadow3a.kk`
and `shadow3b.kk`: both parse in ~1 ms and lower in 0 ms, but effect inference
(`annotate-effects`, called by `emit-koka`) does not terminate on their nested
local `fun id` shadowing the enclosing `fun id`. That is an engine-side
effects-inference iteration issue outside this ticket's grammar/lowering/emitter
layer, exposed by these two test programs; the per-file watchdog reports it
(default 60 s, `--timeout-ms`) instead of hanging the scan. Files that exceed
the watchdog are reported separately and are not counted as `ok`.

Comparison: BEFORE `ok=12/1144` (1.0%); AFTER `ok=1031/1144` (90.1%), with
`parse-fail=109`, `reparse-fail=2`, `timeout=2`, and no
tokenizer/lowerer/emitter/fallback/empty-emit failures. Parse-only BEFORE
`parse-ok=12/1144` — the old lexer rejected 542 files before the parser ran —
AFTER `parse-ok=1035/1144` (90.5%).

## Failure taxonomy (before)

The tracked pass-1 grammar was a hand-written toy, not a port of the
authoritative Koka grammar. The two failure modes follow directly:

1. **Tokenizer: unknown characters (541 files).** The lexer only knew a handful
   of keywords/operators and a single-line `"..."` string with no escapes. Real
   3.3.0 source uses identifiers with `-` and `@`, multi-character and symbolic
   operators outside `== != + - * /`, floats/hex numbers/char literals/raw
   strings (`r"..."`, `r#"..."#`, including multi-line), `::` kind annotations,
   and a much larger keyword set. Any of these produced
   `no matching rule in mode default`.
2. **Layout mismatch (1 file).** The old grammar used the Python
   NEWLINE/INDENT/DEDENT model. Koka's layout is brace/semicolon insertion with
   expression-continuation exceptions (a line ending in an operator, or a line
   starting with a closer/`else`/`.`/`=`, continues the previous expression),
   and dedent columns need not match an enclosing Python-style level. One corpus
   file hit `unindent does not match any outer indentation level` before
   reaching the parser.
3. **Parser: grammar too narrow (590 files).** Everything that did tokenize
   failed in the `file` rule: no module/import path with `/`, no `pub`, braces,
   semicolons, match, `elif`, type parameters, effect rows/declarations,
   handlers, externs, structs, fixity, literals beyond INT and `"..."`.

The reference port target is the authoritative upstream grammar and layout:
`/tmp/koka-corpus/koka/doc/spec/grammar/{parser.y,lexer.l}` and
`/tmp/koka-corpus/koka/src/Syntax/{Lexer.x,Layout.hs,Parse.hs}`.

## Fixtures: `racklr-test/koka-fixtures/`

`racklr-test/koka-real-world-test.rkt` is the committed regression guard. Every
fixture must tokenize without raising and parse to a CST (non-#f). Fixtures named
in its round-trip list must also lower, emit and re-parse with no `?`-fallback
UIR node, and the emitted text must contain per-fixture anchors (declaration
names, types, constructors) so a degenerate lowering that silently drops
constructs cannot pass as a round trip. On the after tree all 16 fixtures are
round-trip classified; the parse-only list is empty (an empty list is explicit,
not a skipped check). The two lists are exhaustive and disjoint, so a fixture
cannot be silently skipped.

All fixtures are original samples written for this test, not copied from the
corpus. Construct coverage:

| fixture | constructs |
| ------- | ---------- |
| `01-module-imports.kk` | module header, slash-qualified paths, private + `pub` imports, `pub fun` with effect row |
| `02-functions-signatures.kk` | type parameters, typed params, tuple/function types, `total`/`<exn,div>`/open `e` effect rows, `console` atom |
| `03-bindings.kk` | typed and inferred top-level `val`, local `val`, mutable `var` with `:=` |
| `04-patterns.kk` | constructor/literal/wildcard/`as`/tuple/list patterns, boolean guards; note Koka 3.x has no or-patterns (`\|` starts a guard) |
| `05-types.kk` | polymorphic `type` with named constructor fields, `struct`, simple and effect-row aliases |
| `06-effects.kk` | block `effect` with `fun`/`ctl` operations, one-line `effect fun`/`effect val`, `named effect` |
| `07-handlers.kk` | `with handler`, `handle(...)`, single-operation `with ctl`, `named handler`, `resume` |
| `08-externs.kk` | `extern import` (c/js targets), extern declaration with inline bodies, `@`-prefixed extern name |
| `09-control-flow.kk` | `if`/`elif`/`else`, braced `match`, `return` inside `if` |
| `10-lambdas.kk` | `fn` expression/braced bodies, typed lambda param, trailing lambda argument, dot chaining |
| `11-operators-fixity.kk` | `pub infix*` declarations, user-defined operator functions `(+++)`/`(***)` |
| `12-literals.kk` | digit separators, hex int, float/exponent, char and escape literals, escaped string, raw string, `r#"..."#`, multi-line raw string |
| `13-continuations.kk` | multi-line bracket continuation, line-ending operator continuation, leading-dot chaining |
| `14-comments.kk` | nested block comments, line comments before/in/after declarations |
| `15-kinds.kk` | `::` kind annotations (`S`, `H -> X`, `E`), `@`-prefixed identifiers |
| `16-program.kk` | mixed application-shaped module: imports, ADT, effect, handler, pipeline |

## Remaining lowering gaps

The after measurement leaves no `lower-error`, no `fallback` (no `?`-unhandled
UIR node) and no `emit-empty`, and every fixture is a clean round trip. The
remaining non-`ok` files fall into four groups, none of them a tokenizer or
lowering gap:

1. **Parse-only gaps, 109 files** — legacy/experimental syntax only: 93 in
   `test/` (`misc` instance/evidence/ambients experiments, `algeff`, `cgen`,
   `overload`, `async`, `perf`, `syntax`, ...), 14 in `lib/v1` (legacy demos)
   and 2 samples. No `lib/std` file remains. These are tracked as grammar
   coverage, not lowering work.
2. **Re-parse failures, 2 files** — `test/algeff/reactive1.kk`,
   `test/mask/inject-behind1.kk`: emitted text does not parse. These are
   emitter output-shape gaps for reactive/behind-mask constructs.
3. **Watchdog timeouts, 2 files** — `test/static/shadow3a.kk` / `shadow3b.kk`:
   both parse (~1 ms) and lower (0 ms), but effect inference (`annotate-effects`
   inside `emit-koka`) does not terminate on their nested local `fun id`
   shadowing the enclosing `fun id`. This is an engine-side effects-inference
   iteration issue outside the Koka grammar/lowering/emitter layer, exposed by
   these two programs; the per-file watchdog (default 60 s, `--timeout-ms`)
   reports it instead of hanging the scan. Raise `--timeout-ms` to observe the
   non-termination directly.

Intentional loss in the current emitter (verified by the fixture anchors, so it
cannot hide behind a green battery): a string whose value is multi-line or long
is emitted as a raw string and other strings as escaped ordinary strings, so
the source's exact raw-string delimiter (its hash count) is not preserved while
the string value is. Handler operation kinds are preserved (`fun`/`ctl`/
`final ctl`/`raw ctl`), so no handler kind is lost. Both are re-parsed and
counted `ok` by the battery.
