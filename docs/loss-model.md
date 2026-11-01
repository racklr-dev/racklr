# Loss model and round-trip verification

Absorb then emit is identity only within a declared loss model. Two languages
with different concurrency, memory, or type semantics cannot round-trip
perfectly, and the tool must not hide the difference. This document describes
the loss model racklr declares today, how the round-trip harness enforces it,
and where a violation is recorded.

- Declaration: `racklr/loss-model.rkt`
- Verifier: `racklr/loss/verify.rkt` (plus `sexp.rkt`, `normalize.rkt`,
  `features.rkt`, `effects.rkt`, `report.rkt`)
- Harness: `racklr-test/examples-roundtrip-test.rkt`
- Machine-readable result: `loss-artifacts/round-trip.json` (build output,
  gitignored; the JSON artifact is the first-class record of a violation)

## The vocabulary

For each supported source→target pair the model lists rules. A rule names a
*feature* and a *disposition*:

- `preserved` — the feature must survive the round trip unchanged. A difference
  is a violation.
- `approximated` — the feature is allowed to change; the rule's detail says how.
  The verifier records what it observed.
- `refused` — the feature is dropped on purpose. If the target UIR still carries
  it, the model is stale and that is a violation.

Features currently understood by the verifier:

| feature | projection |
| --- | --- |
| `definitions` | block-scope declared names (functions, classes, module bindings) |
| `signatures` | function name, arity, parameter names |
| `classes` | class names and method names |
| `literals` | string and number literal inventory |
| `control-flow` | census of `if`/`for-each`/`while`/`try`/`match`/`await`/... nodes |
| `effect-rows` | per-function effect rows in the target's native form |
| `types` | type-level declarations (interfaces, aliases, enums) |
| `jsx` | JSX element tree |
| `statement-shape` | regrouping of statements/assignments/operator chains |
| `runtime-guards` | emitter-injected effect-guard support code |
| `imports` | import shape |
| `formatting` | layout and statement re-flow (not representable in UIR) |
| `comments` | source comments (not representable in UIR) |

## Verification modes

- **exact** — after the model's declared normalizations, the two canonical UIR
  serializations (`uir->sexp`) must be equal. Any residual difference is an
  undeclared loss. Used for same-language pairs (and it is the strongest check
  the tool can make).
- **projected** — a lossy one-way translation (TypeScript/TSX → JavaScript).
  Only the features the model declares `preserved` must match; declared
  approximations are recorded but do not fail the pair.

A model also lists *normalizations* the verifier applies to both sides before
comparing, each corresponding to a declared loss:

| normalization | effect |
| --- | --- |
| `annotate-effects` | runs effect inference on the absorbed UIR — what the emitters do at emit time |
| `drop-generated` | removes emitter-injected declarations and imports (the effect-guard runtime) |
| `drop-effect-guards` | removes effect-guard annotations from the structure; rows are checked separately |
| `drop-parens` | parentheses are not semantic |
| `erase-js-types` | erases TypeScript type syntax for the JavaScript target |

The normalizer never removes anything the model does not declare, and it will
not drop a generated name the source itself binds (a user `__helper` survives).

## Supported pairs

Every source language the harness can absorb has a model, and the harness fails
if a language is absorbed without one. Pairs with no example in
`docs/examples/` are listed with zero corpus coverage in the artifact rather
than silently assumed.

| pair | mode | notable preserved | notable approximated / refused |
| --- | --- | --- | --- |
| python → python | exact | definitions, signatures, classes, literals, control flow, effect rows | guard runtime approximated; comments refused; layout approximated |
| javascript → javascript | exact | definitions, signatures, classes, literals, control flow, effect rows | guard runtime approximated; comments refused; layout approximated |
| clojure → clojure | exact | definitions, signatures, literals, control flow, effect rows | comments refused; layout approximated |
| json → json | exact | keys and string/number/boolean/null values | effect rows and comments refused; layout approximated |
| koka → koka | exact | definitions, signatures, literals, control flow, effect rows (names) | comments refused; layout approximated |
| typescript → javascript | projected | definitions, signatures, classes, effect rows | types refused; statement shape, literals, guard runtime, layout approximated |
| tsx → javascript | projected | definitions, signatures, effect rows | JSX approximated; types and comments refused; literals, control flow, statement shape, guard runtime, layout approximated |

## Effect rows in the target's native form

"Effect rows preserved" means the row survives translation in the target's own
vocabulary, and the verifier re-absorbs it:

- Python: emitted as `@__withEffects({...})` decorators; the verifier decodes
  the row record back into a canonical row.
- JavaScript/TypeScript/TSX: emitted as `__withEffects({...}, fn)`; bound names
  are mapped back to their rows.
- Clojure: emitted as the `{:__effects__ ...}` defn attr-map.
- Koka: carried in the return-type row (names only on both sides).
- JSON: rows are refused (no function concept).

Rendering richer per-target vocabulary is the job of the P3.2 work; when that
changes, the model's `effect-rows` rules are the place to record the change.

## What a violation is

The verifier emits, as plain transparent structs, a `loss-observation` per rule
and a `loss-violation` for:

1. exact mode: normalized UIRs differ (`structure` violation with canonical
   diff paths as evidence);
2. a `preserved` feature's projection changed;
3. a rule declared `refused` but the target UIR still carries the feature
   (stale model).

`loss-artifacts/round-trip.json` records the model digest, every pair's declared
rules and corpus coverage, every file's per-feature observations, and every
violation with evidence. The harness prints the violations and exits non-zero.
A loss is surfaced as an artifact rather than as a `PASS` line that hides it.

To resolve a violation: fix the emitter or absorber (preferred — the round-trip
harness found both a JSON safe-codepoint lexer bug and two JavaScript absorb
gaps: `export default function` and multi-parameter arrow functions), or update
the declared rule deliberately. The model digest in the artifact makes the
model edit and the result reviewable together.

## Limits

- The harness verifies `docs/examples/**`; the model is a contract about the
  declared pair, and the corpus is the evidence. Zero-coverage pairs are
  reported in the artifact.
- `formatting` and `comments` are not representable in UIR, so they are
  declared but cannot be observed by the verifier.
- TypeScript and TSX front ends and the JavaScript front end lower emitted
  assignment and operator chains differently. The model declares that under
  `statement-shape` and does not require structural identity for those pairs;
  declarations, signatures, classes, and effect rows still are checked.
- The JSON front end ships a generated parser; its escaped-codepoint character
  class was corrected by hand (see the comment in `racklr/gen-json-parser.rkt`).
