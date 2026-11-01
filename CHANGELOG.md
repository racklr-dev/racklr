# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- Effect-edge provenance is first-class in the graph substrate. Inference
  (`infer-effects`, `infer-effects-all`) optionally records every classifying
  call site (tokens, receiver type, classification kind) and the inferred call
  graph (`#:provenance-record`); `racklr/graph/provenance.rkt` projects it into
  the typed hypergraph as `fn-effect`, `grounds`, and `call-site` edges.
  `provenance-paths` returns the exact caller→callee chain for a given
  function/effect edge, and the projection refuses an effect edge that no
  ground can reach.
- `racklr-test/provenance-test.rkt` covering direct, chained, receiver-typed,
  unresolved, declared, recursive, and cross-file provenance, plus the
  no-unprovenanced-edge invariant, a cross-check between the record-level and
  atom-level reachability checks, and a projection-bug case (a dropped
  call-site link).
- `tools/provenance-corpus.rkt` runs the same completeness check over a real
  Python corpus (default `django/django/core`) and reports the counts without
  touching the shared baseline.
- The UIR schema is now self-describing data: `UIR-SCHEMA` in
  `racklr/uir/schema-data.rkt` is a UIR value declaring every node kind, field,
  and codec, and the hand-written, versioned kernel
  (`racklr/uir/kernel.rkt`, `schema-io.rkt`, `schema-check.rkt`,
  `schema-macro.rkt`) generates the node kinds from it. The schema validates
  against its own declarations and round-trips through its own serialization
  using only the hand-written kernel layouts. Adding a node kind is a
  schema-value edit.
- `typed-param` carries an optional parameter-kind payload slot
  (`#f`, `kw-only`, `var-positional`, `var-keyword`) expressed as a trailing
  defaulted schema field (`param-kind` codec). UIR payload version 2 adds the
  slot; the v1 -> v2 migration is the identity, so version-1 payloads stay
  readable. This is the UIR-layer part of the parameter-marker work landing
  separately; constructors are exact-arity, so call sites pass `#f` explicitly.
- Declared per-pair loss model (`racklr/loss-model.rkt`): every supported
  source→target pair states what absorb→emit preserves, approximates, and
  refuses, plus the normalizations the verifier applies before comparing.
- Round-trip verifier (`racklr/loss/`): `exact` mode (normalized UIRs must be
  equal) and `projected` mode (lossy one-way translations), effect-row
  re-absorption from each target's native form, and `loss-observation` /
  `loss-violation` records with canonical diff evidence.
- `racklr-test/examples-roundtrip-test.rkt` now verifies absorb→emit→re-absorb
  within the declared model and writes `loss-artifacts/round-trip.json`
  (model digest, pair coverage, per-file observations, violations with
  evidence); undeclared loss fails the harness.
- `racklr-test/loss-model-test.rkt` covering model integrity, normalizations,
  feature projections, native effect-row extraction, and violation
  classification.
- `docs/loss-model.md` documenting the declared model and the artifact.
- Round-trip examples for JSON, JavaScript, and Koka; the Python effects
  example exercises effect-row carrying.
- Tracked modern Python 3 grammar in `grammars/python3/` (`Python3Lexer.g4`,
  `Python3Parser.g4`). Adds assignment expressions (`:=`, PEP 572),
  positional-only parameters (`/`, PEP 570) in `def` and `lambda`, and PEP 695
  `type` alias statements, including generic aliases.
- UIR nodes `uir-walrus` and `uir-type-alias`, with Python lowering, emission
  and sexp/JSON serialization. `emit-python` renders `(name := value)` and
  `type Name[T] = value`.
- `racklr-test/modern-python-test.rkt` covering parse, lower and emit
  round-trips for the new syntax.
- Effect algebra (`racklr/effects/algebra/`): provenance-carrying rows as sets
  of (effect, evidence) pairs, the `join` / `sequence` / `choose` / `scope`
  combinators, a generic monotone fixpoint driver, and row subtyping with
  handler interpretation (the bridge checker's capability table generalized to
  rows).
- `racklr-test/effects-algebra-property-test.rkt`: property tests for
  associativity and idempotence of join, monotonicity and least-fixpoint laws,
  the subtyping laws (reflexivity, transitivity, bottom, join as LUB), and
  handler interpretation.
- `racklr-test/effects-algebra-prohibitions-test.rkt`: the declared-row
  prohibition agreement. Algebra rows canonicalize `must-not` to absence, so
  `declared-prohibited-effects` reads a prohibition from the UIR row and the
  tests pin that `row-subtype?` still agrees with `effect-subtype?` for rows
  carrying `must-not` on either side of `≤`.
- One emit contract (`racklr/emit-contract.rkt`) and a single dispatch
  (`racklr/emit.rkt`): every target emitter takes UIR plus an optional explicit
  effect row, and `(emit target uir row)` renders any supported target from
  that same input, forwarding target-specific keyword options. An explicit row
  is reified in the target's native vocabulary (Python/JavaScript guards, Koka
  effect annotation, Clojure metadata) or refused, never silently dropped;
  omitting the row keeps the previous one-argument call shape.
- `racklr-test/emit-contract-test.rkt` covering the target table, the native
  row rendering for every row-capable target, refusal for the targets without a
  native row vocabulary, and the unchanged row-less call shape.
- `tools/scan-python.rkt`: full-tree Python front-end scan over a directory
  tree (`racket tools/scan-python.rkt django/django`). It generates the parser
  from the tracked `grammars/python3/` grammar and reports `ok` /
  `INDENT-ERROR` / `PARSE-FAIL` / `LOWER-ERROR` / `FALLBACK` per file, with an
  exit code that gates on zero blockers. Re-run over the 907-file Django
  corpus: 907 `ok`, 0 blockers.
- `racklr-test/scan-python-test.rkt` covering the scanner's bucket
  classification and fallback detection on a fixture tree.

### Changed

- Effect inference's fixpoint is now expressed in the algebra:
  `infer.rkt` and `whole-program.rkt` run `algebra/least-fixpoint` over
  `algebra/join`, and `rows.rkt` is the algebra adapter. The refactor is
  behavior-preserving: the `type-graph.rkt django/django/core` baseline is
  unchanged (`sha256:14bac7860833329849c4d81740ecccd01870b72cb8355f947f70956e179623ba`).
- `docs/django-frontend-catalogue.md` is now the re-run record, not a list of
  open blockers: all twelve front-end blockers are marked resolved, the latest
  whole-tree scan is 907/907 `ok` with 0 blockers, and the measurement command
  runs from the tracked grammar instead of the untracked `grammars-v4/` tree.
- UIR node kinds are generated from the schema value instead of hand-written
  struct definitions (`racklr/uir/types.rkt` is now the loader/validator for
  the schema). The generated structs are `#:transparent` and exported through
  `struct-out`, so `match` and `struct-copy` work on the public bindings.
- `json->uir` now restores `symbol`-typed fields as symbols rather than
  strings (method visibility, effect-entry modality, effect-policy kind,
  node-kind kind, schema-field name/type, and parameter-kind slots). JSON round
  trips now hold for every schema-declared kind.
- `examples-roundtrip-test.rkt` pipelines now return the absorbed, emitted, and
  re-absorbed forms, and the harness resolves each source language to its
  declared loss-model pair. Adds a Koka round-trip pipeline.
- JavaScript front end: `export default function`, `export default async
  function`, and `export default class` re-absorb into UIR instead of lowering
  to `null`; multi-parameter arrow functions keep every parameter.
- JSON front end: corrected the generated parser's safe-codepoint character
  class, which rejected `u`, `A`–`Z`, and `0`–`9` inside JSON strings.
- Python front-end test suites load the tracked `grammars/python3/` grammar
  instead of the vendored `grammars-v4/python/python3/` copy, so the parser the
  tests exercise is the one the repository owns.
- Emitted effect rows now have one owner of their spelling
  (`racklr/emit-effects.rkt`): Python/JS/TS object literals, Clojure `defn`
  metadata, and Koka typed effect rows, with a documented `@effects`
  annotation for fields a target cannot spell. Python and JS gain a
  `# @effects {...}` / `/** @effects {...} */` annotation next to their runtime
  guard; Koka and Clojure emit inferred rows instead of discarding them.
- The Python/JS runtime guards' scope read/write vocabulary is rendered from the
  classifier's DATA verb tables (`racklr/effects/guard-vocabulary.rkt`) instead
  of hand-written lists, and a scope encoding other than `reads=...` or
  `writes=...` now fails at guard construction rather than silently no-oping.

### Notes

- Koka's typed effect row is names-only: `may` and `must` both render as `<db>`
  and the modality distinction survives only in the documented `// @effects`
  comment, so a Koka reader should not read the typed row as stronger than the
  contract row. A row on a Koka function with no declared result type is emitted
  as the documented comment alone, because the grammar only accepts `<effects>`
  in front of a result type.

### Fixed

- `json->uir` previously could not read a `typed-param` payload at all: the
  tag was missing from the JSON tag set, so any function with parameters failed
  to round-trip through JSON. The tag set is now derived from the schema
  layouts, covering every declared kind.

## [0.1.0rc1] - 2026-06-23

### Added

- JSX→React.createElement transpilation pipeline:
  - Cleaned JSX grammar (`grammars-v4/javascript/jsx-cleaned/`)
  - `lower-jsx.rkt` — CST→UIR lowering (7 lowering functions)
  - `emit-jsx.rkt` — UIR→React.createElement emitter
  - 4 new UIR struct types: `uir-element`, `uir-attribute`, `uir-jsx-expr`, `uir-text-node`
  - 42 JSX tests (`gen-jsx-test.rkt`) covering tokenize, parse, lower, and emit

## [0.1.0] - 2025-06-14

### Added

- Standard Racket collection layout: source in `racklr/`, tests in `racklr-test/`
- Package metadata (`info.rkt`) with multi-collection declaration
- GPL-3.0-or-later license (`LICENSE`)
- `.gitignore` entries for generated parser artifacts, Racket bytecode, and vendored grammars
- `CHANGELOG.md` (this file)

### Changed

- Branch renamed from `master` to `main`
- All inter-module `(require "file.rkt")` references converted to collection-style `(require racklr/file)` paths
- Generated parser template now emits `(require racklr/tree)` instead of `(require "tree.rkt")`
- Grammar file paths in tests now use `../grammars-v4/` prefix (tests moved one level deeper)

### Removed

- Flat-file project structure (all `.rkt` files previously in repository root)
