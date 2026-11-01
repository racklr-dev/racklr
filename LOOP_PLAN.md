# LOOP_PLAN — the loop's phase tracker (updated 2026-11-01)

`ROADMAP.md` defines the phases, their definitions of done, and the risks they
retire. This file records the loop's progress against them: what has landed,
what is verified, what is in flight, and what the next levers are. It is a
snapshot at the commit; `git log` and the open pull-request list are the live
state.

## Status

- **P0.1 — Version the UIR schema and add a migration path: DONE (verified)**
  - `racklr/uir.rkt` is a thin facade (50 lines); versioning lives in
    `racklr/uir/version.rkt` (91). Serialization moved with P2.1 into
    `racklr/uir/schema-io.rkt` (312), driven by the schema value in
    `racklr/uir/schema-data.rkt` (174) over the bootstrap kernel
    `racklr/uir/kernel.rkt` (195). All files under the 400-line obligation.
  - Versioned envelope `(uir VERSION payload)` for JSON and explicit sexp entry
    points; bare `uir->sexp`/`sexp->uir` stay version-free (in-memory graph
    representation). `UIR-KERNEL-VERSION` (2) versions the hand-written
    bootstrap, and `UIR-SCHEMA-VERSION` is derived from the schema value.
  - Migration chain `register-uir-migration!` / `migrate-uir-payload` turns a
    struct field-count change into a controlled event. `json->uir` reads old
    payloads and migrates before parsing; future versions raise.
  - Tests: `test-versioned-sexp` and `test-schema-migration` in
    `racklr-test/uir-test.rkt`; the suite passes (re-run 2026-11-01).
- **P0.2 — receiver-type dataflow: DONE (verified)** (epic child drg-5140)
  - The dataflow (locals + self-attrs + MRO + manager attrs) already existed;
    the remaining audit relocated every residual library name table out of
    engine code:
    - `pure-roots.rkt`: `python-builtins`, `pure-dunder-names`,
      `str-method-names`, `pure-container-method-names` → four new
      `effect-tokens` fields in `library-tokens.rkt`; the four predicates now
      read the injected lexicon (optional `tokens` arg).
    - `imports/effect-tag.rkt`: `db-read-methods`, `db-write-methods`,
      `io-read-verbs`, `io-write-verbs`, the inline io "other verbs" list, and
      `builtin-class-methods` → `imports/library-config.rkt`; the engine reads
      them via require, `builtin-class-methods` re-exported for the contract.
    - Fixed a typed-op gap: `imported-effect`'s class-handle fallback now
      derives ops via `method-ops` (class→module→domain), matching
      `receiver-effect`, instead of a coarse `(effect-cap dom '())`.
    - New test `test-imported-receiver-dataflow`: an imported class receiver
      with no blessed caps entry resolves to typed `db<QuerySet>.read`.
  - Audit DOD met: no `(set "…` / `(hash '…` name-table literals remain in
    classification logic outside `library-tokens.rkt` / `library-config.rkt`.
    The only in-engine name sets left are `synthetic-callees` (front-end
    language constructs, not library tokens) and `__handle` (emitter primitive).
- **P0.3 — Django corpus to acceptance bar: IN PROGRESS** (epic child drg-1bd3)
  - Acceptance bar (ROADMAP.md): 907/907 parse, every function carries a row,
    zero unknown leaves, provenance on every edge.
  - **Parse criterion MET**: `racket tools/scan-python.rkt django/django`
    reports `files=907, ok=907`, every other bucket empty, 0 blockers. The scan
    runs through the tracked `grammars/python3/` grammar, so it is a command
    anyone can rerun (#97).
  - **Row criterion MET**: every named function carries an inferred row. Last
    full-corpus measurement (2026-10): 8244/8244 named functions; the P6.1
    compiler corpus is 2498/2498.
  - **Provenance criterion MET**: P4.2 makes provenance a first-class graph
    edge — every effect edge reaches a ground, and the projection refuses an
    effect edge that has none (it runs the completeness check over the corpus).
  - **Remaining gap: the unknown-elimination obligation.** The gate answers
    `baseline COVERAGE-FAILED sha256:14bac786…` (exit 3): the graph matches the
    pin, and unknown functions are 250 across 66 files on the
    `django/django/core` probe (#73). The full 907-file unknown census now
    OOMs the harness at 4 GB, so the shared core probe is the tracked bar; the
    last full-corpus census was 5014 unknown functions / 514 files (2026-10).
    This is a multi-lever effort, not a single change.
  - Increment landed 2026-10 (multi-level receiver chains):
    - `whole-program.rkt`: multi-level receiver-chain resolution. New
      `receiver-chain-class` (walks `a.b.c` get-chains) + `class-attr-lookup`
      (per-class attribute graph walk with MRO + cycle guard). A
      `self.stream.file.write(...)` chain now resolves through two object
      boundaries to typed `io<File>.write`. `classify-call-all` threads the
      merged `class-attrs` env through it.
    - Test `test-receiver-type-nested-attr` in `effects-infer-test.rkt`.
    - Measured: core 575 unknown (unchanged); full corpus 5014 unknown. The
      change has little corpus surface because 3-level `self.a.b.m(` chains are
      rare (~8 sites via grep). It is correct but not the high-leverage lever.
  - Increment landed 2026-10 (module-qualified supers + stdlib/framework token
    lexicon + self.* stdlib-parent fallback):
    - `class-resolve.rkt` / `collect.rkt`: super refs are now
      `(first-sym . full-dotted-sym)` pairs so `class Serializer(base.Serializer)`
      resolves to the real base, not a self-loop (basehttp super() sites were
      silently self-edges before).
    - `library-tokens.rkt` split: receiver-TYPE tables moved to
      `library-stdlib-types.rkt` (both < 400 lines). Added `timezone.now`/`localtime`
      → time, `timezone.make_aware/is_aware/...` → pure, and pure bare Django/ctypes
      helpers (`capfirst`, `get_language`, `safe_repr`, `Decimal`, `c_void_p`, `path`,
      `make_hashable`, ...).
    - `classify-all.rkt` (split out of `whole-program.rkt`, both < 400 lines): new
      `self-stdlib-fallback` resolves `self.m()` inherited from a NON-corpus parent
      (`unittest.TestCase.fail/assertEqual` → pure) through the stdlib-type table.
    - Tests: `test-self-stdlib-parent-fallback`, `test-datetime-now-is-time`
      (was wrongly asserted pure).
    - Measured: core gate `unknown` 87→86 files, 415→406 functions (time 33→48,
      now correct clock reads); full corpus direct-unknown call sites 5666→5445,
      unknown functions 2981→2913. Suites green (racklr infer/imports/contract/
      python-annot/python-emit + effervesce library + check/*).
  - The unknown surface, tracked by shape (PR #85's census of the core probe;
    after: 214 unknown call sites, 236 unknown functions, 65 files):
    - `param` (36): framework callbacks, `*args` spread, un-annotated external
      callers. Needs tuple/spread parameter typing and container element-type
      dataflow (the `Page(*args, **kwargs)` paginator pattern).
    - `bare-call` (53) / `self-method` (11): higher-order callables reached only
      as parameters or closure values (`middleware_method`, `send`, `receive`,
      `start_response`, `converter`). Correctly `unknown` today; needs Phase 1
      row holes / effect polymorphism.
    - `local / call-result` (32): callee return types that project to no single
      class (dynamic imports, `or`-fallbacks, `*args` spread) or method-return
      types not yet in `stdlib-type-result-projections-data`.
    - `local / loop-var` (12) and `attr` (10): element-type dataflow and
      attribute copies of untracked receivers (`for m in re.finditer(...)`,
      `_meta.pk` chains).
    - `local / literal` (6): module-global literal registries (`_caches = {}`)
      read through `setdefault` — needs module-scope assignment tracking.
    - `chained` (17): imported-module method chains whose RESULT type is not in
      the data lexicon (`connection.ops.get_db_converters`,
      `graph.forwards_plan`).
    - Bare stdlib/framework functions still missing from the DATA lexicons
      (`urlsplit`, `iscoroutinefunction`, `defaultdict`, `functools.*`, Django
      helpers `import_string`, `get_resolver`, `deconstructible`, ...).
  - In flight: PR #85 projects receiver types through imported stdlib classes
    and results (its base: 249 → 214 unknown call sites, 250 → 236 unknown
    functions, 66 → 65 files); PR #81 covers optional-chain/`this` calls and
    unresolved `self.m()` leaves (drg-c128); PR #80 is the real-context unknown
    census that aims the next receiver-type levers.

## Phase 1 work landed

- **P1.5 — Effect algebra + property tests: DONE (verified)**
  - `racklr/effects/algebra/` is a standalone typed algebra: provenance-carrying
    rows as sets of (effect, evidence) pairs (`row.rkt`, 181 lines),
    `join` / `sequence` / `choose` / `scope` over them (`combinators.rkt`, 237),
    a generic monotone fixpoint driver (`fixpoint.rkt`, 133), and row subtyping
    with handler interpretation (`subtyping.rkt`, 105). `main.rkt` is the
    facade; all files under the 400-line obligation.
  - The inference fixpoint now runs on the algebra: `infer.rkt` and
    `whole-program.rkt` call `algebra/least-fixpoint` over `algebra/join`, and
    `rows.rkt` is the inference-facing adapter. The refactor is
    behavior-preserving — the shared type-graph baseline is unchanged
    (`sha256:14bac786…`).
  - `effects-algebra-property-test.rkt` covers join associativity/idempotence,
    fixpoint monotonicity and least-fixpoint laws, the subtyping laws, handler
    interpretation, and a differential check against the pre-refactor union.
    `effects-algebra-prohibitions-test.rkt` pins that `row-subtype?` agrees
    with `effect-subtype?` for declared rows carrying `must-not`, and adds
    `declared-prohibited-effects` so a contract check can never read a
    prohibition as "no statement".
  - Still open for Phase 1: effect polymorphism (row holes) and the Pass-4
    contract check over the new relation (`docs/effects-proposal.md` §11). The
    row holes are exactly what P0.3's higher-order `bare-call` / `self-method`
    unknowns need.

## Phase 2 work landed

- **P2.1 — Express the UIR schema as self-describing data: DONE (verified)**
  (epic child drg-c985)
  - `racklr/uir/schema-data.rkt` holds `UIR-SCHEMA`, a UIR value: a
    `uir-schema` of `uir-node-kind` declarations, each recording its tag, its
    constructor, and one `uir-schema-field` per field (name + codec/type).
    `racklr/uir/types.rkt` is now 17 lines: require the schema, validate it,
    let the generator synthesise the node kinds.
  - The bootstrap is a hand-written, versioned fixed-point kernel:
    `racklr/uir/kernel.rkt` (bootstrap node kinds `schema`/`node-kind`/
    `schema-field`, layouts, codecs, introspection), `schema-io.rkt` (generic
    UIR<->sexp and JSON reader/writer), `schema-check.rkt` (self-validation
    and schema extension), `schema-macro.rkt` (schema-driven struct,
    predicate, accessor, and layout generation). `UIR-KERNEL-VERSION` versions
    the trust root; the schema value declares the kernel version it requires
    and `version.rkt` derives `UIR-SCHEMA-VERSION` from the schema value.
  - `check-schema!` validates the schema value under the declarations it itself
    contains, and checks the kernel-owned declarations against the
    hand-written `KERNEL-LAYOUTS`. The schema value also round-trips through
    its own serialization using only the kernel layouts: the trust root reads
    and writes the schema without the generated vocabulary.
  - Schema fields carry a default marker (`required` or `false`); only a
    trailing suffix may be defaulted. The payload omits trailing fields at
    their default and the reader fills them back in, so `typed-param`'s
    optional `kind` slot is expressible as data (payload v2, identity v1 -> v2
    migration) while every generated constructor stays exact-arity. This
    pre-lands the UIR-layer part of the parameter-marker PR so either order
    merges without dropping the kind field.
  - DOD proofs in `racklr-test/uir-test.rkt`: the schema is UIR data and round
    trips (sexp, kernel-only sexp, JSON); every declared kind has a generated
    layout; an extended schema value adds a `demo` node kind whose
    constructor, predicate, accessors, sexp and JSON codecs all work with no
    core struct edit; kernel/schema version mismatch, duplicate kind tags, and
    unknown field codecs are refused.
  - The JSON reader is now schema-aware: it restores `symbol`-typed fields
    instead of leaving them strings, so JSON round trips now hold for every
    declared kind (previously `method` visibility, `effect-entry` modality and
    other symbol-valued fields did not round-trip through JSON).
  - Verification observed (recorded 2026-11-01): `racklr-test/uir-test.rkt`
    passes; all 332 previously provided bindings still exist with identical
    arities; `raco make` of every tracked `.rkt` in racklr and effervesce exits
    0; racklr suite matrix 40/42 pass (only the two documented pre-existing
    parse failures `gen-python3-test` and `lower-python-test` remain);
    effervesce suites 10/10 pass; the shared baseline is unchanged at
    `sha256:14bac7860833329849c4d81740ecccd01870b72cb8355f947f70956e179623ba`
    (no graph diff).
- **P2.2 — Move all language knowledge into injectable lexicon data: in flight**
  (drg-5a4f, PR #84).
- **P2.3 — New-front-end proof (Rust): in flight** (PR #88). Phase 2's
  definition of done is "a new front end is a grammar plus a lowering hook plus
  token-lexicon data with zero core edits"; P2.2 and P2.3 are the two halves of
  that proof and neither has merged yet.

## Phase 3 work landed

- **P3.1 — One emit contract and single dispatch: DONE (verified)**
  (#74, drg-ef26)
  - `racklr/emit-contract.rkt` (168 lines) declares the call shape (UIR plus an
    optional explicit effect row in, target source out), the native
    row-reification strategies, and the shared entry-row attachment;
    `racklr/emit.rkt` (109) is the single dispatch over the target table.
  - An explicit row is reified in the target's native vocabulary where one
    exists (Python/JS `__withEffects` guards, Koka effect annotation, Clojure
    `:__effects__` metadata) and refused with a contract error where it does
    not (JSX, DOM JavaScript, JSON, Effervesce), so it is never silently
    dropped. Omitting the row keeps the previous one-argument call shape.
  - `emit-contract-test.rkt` covers the target table, the same UIR-plus-row
    input rendered by every row-capable target, refusal for row-less targets,
    the empty-row declaration, keyword-option forwarding, and the unchanged
    row-less shape.
- **P3.2 — Render effect rows in each target's native vocabulary: DONE
  (verified)** (#79, drg-dd4c)
  - `racklr/emit-effects.rkt` is the single owner of the spelling: Python/JS/TS
    object literals, Clojure `defn` metadata, Koka typed effect rows, with a
    documented `@effects` comment for fields a target cannot spell. `must-not`
    is never granted as a typed Koka effect and `unknown` never materializes as
    a concrete effect.
  - Python and JS add `# @effects {...}` / `/** @effects {...} */` next to the
    runtime guard; Koka and Clojure emit inferred rows instead of discarding
    them; JSDoc absorption re-reads the emitted `__withEffects(row, fn)` guard.
    Koka's `may`/`must` flattening and the other per-target losses are
    documented in README/CHANGELOG.
  - The Python/JS runtime guards' scope read/write vocabulary now comes from
    the classifier's DATA verb tables (`effects/guard-vocabulary.rkt`), and an
    unsupported scope encoding fails at guard construction instead of
    silently no-oping.
  - Tests: `effects-render-test.rkt` plus the per-target
    `effects-{python,ts,jsdoc,clojure,koka}-annot-test` suites.
- **P3.3 — Explicit loss model and round-trip verification: in flight**
  (PR #83).

## Phase 4 work landed

- **P4.1 — Merge the six graphs into one typed hypergraph substrate: DONE
  (verified)** (#75, drg-8a16)
  - `racklr/hypergraph.rkt` is a thin facade over `racklr/hypergraph/*`: the
    dependency-free kernel (atoms, lattices, per-slot algebra, pattern query,
    materialize, flat closure, Atomese), the effect-row instance, the type +
    capability instance, the six-graph typed-edge schema, the single instance
    with typed queries, and the six projectors. Every new production module is
    under 400 lines.
  - A function vertex is the same atom in the call, effect, and module graphs.
    `schema.rkt` declares edge type -> graph kind + arity + slot node types, so
    `hg-links` (by kind), `hg-edges` (by type), `hg-out`/`hg-in` (by
    direction), and `hg-ill-typed-edges` (schema validation) work over the
    whole substrate.
  - `hypergraph-test.rkt` builds one instance from real data (materialized
    UIR, walked call edges, inferred rows, type relations, capability
    declarations, module imports) and exits nonzero on failure; it previously
    printed FAILURE but exited 0. The shared baseline pin is unchanged.
- **P4.2 — Effect-edge provenance as first-class graph edges: DONE (verified)**
  (#77, drg-80c7)
  - Inference records one provenance ground per classifying call site (tokens,
    receiver type, classification kind) and the inferred call graph;
    `racklr/graph/provenance.rkt` projects rows into the typed hypergraph as
    `fn-effect`, `grounds`, and `call-site` edges. `provenance-paths` returns
    the exact caller -> callee chain for a function/effect edge, and the
    projection refuses an effect edge that no ground can reach.
  - `provenance-test.rkt` covers direct, propagated, cross-file,
    receiver-typed, unresolved, declared, recursive, and multi-ground
    provenance, the no-unprovenanced-edge invariant, and the
    record-vs-atom cross-check; `tools/provenance-corpus.rkt` runs the same
    completeness check over a real corpus (`django/django/core` by default)
    without touching the shared baseline.
  - This discharges P0.3's "provenance on every edge" criterion.
- **P4.3 — Contextual scoping and query unification: in flight** (PR #87,
  drg-3a3e).

## Phase 5 work (in flight)

No Phase 5 increment has merged to `main`; the `effervesce` submodule pointer
stays at `93b7b73`. The four increments are open pull requests, three of them
drafts:

- P5.1 live image as a long-lived process — draft PR #90 (drg-b3fd)
- P5.2 durable journal with replay — draft PR #89 (drg-6134)
- P5.3 loop as data (UIR workflow manifest with gated rewrites) — PR #92
  (drg-2553)
- P5.4 the effect gate (accept / isolate / rollback) — draft PR #91 (drg-61f1)

## Phase 6 work landed

- **P6.1 — Lower the compiler into UIR with inferred effect rows: DONE
  (verified)** (#95, drg-70d2)
  - A two-pass Racket front end (`racklr/lower-racket.rkt` plus
    `racklr/lower-racket/*`) absorbs the compiler's own modules; cross-file
    calls resolve through require/provide to mangled `<component>::<name>`
    call-graph nodes, so core inference sees real edges instead of ambiguous
    bare names. `racklr/compiler-uir.rkt` drives discovery, lowering,
    `infer-effects-all`, exact row write-back, and a report;
    `tools/compiler-effects.rkt` is the CLI view; `racklr-test/
    compiler-uir-test.rkt` is the definition-of-done gate (41 checks).
  - `racklr/effects/library-racket-tokens.rkt` (DATA) carries 332 pure and 90
    effectful Racket procedures; the dedicated `racket-builtins` field keeps
    Racket names out of the Python/JS value-constructor table that receiver
    typing reads. Classifier semantics are unchanged and the shared baseline is
    identical.
  - Observed: 137 compiler modules (95 racklr including the new front end, 42
    effervesce) lower to UIR; 2498 named functions, 2498 inferred rows, every
    function carrying its row after write-back; 718 functions (28.7%, 534 call
    sites) remain tracked `unknown` for higher-order callbacks and dynamic
    resolution, never guessed.
- **P6.2 — Emit the compiler into a target language: in flight** (PR #94,
  drg-fab8).
- **P6.3 — The emitted tower runs and passes the gate: in flight** (PR #93).

## Gate, measurement, and foundation work landed

- **Baseline is content-addressed** (#72, nc-AE7E):
  `baseline/type-graph.snapshot` plus `baseline/type-graph.sha256`
  (`sha256:14bac786…`), generated and checked by `tools/baseline-check.rkt` /
  `tools/baseline-snapshot.rkt` over a dependency-free SHA-256
  (`tools/sha256.rkt`). Drift prints the exact atom lines that moved;
  `--update` is the deliberate re-pin whose snapshot diff is the review.
  `baseline-check-test.rkt` pins the format, the path/order independence, and
  the exact diff.
- **Pass/fail is separate from coverage** (#73, nc-156C): the gate prints one of
  `baseline OK` (exit 0), `baseline DRIFT` (1), `baseline COVERAGE-FAILED` (3),
  or `baseline GATE-FAILED` (4). Coverage is cross-checked against the graph's
  named `fn-effect-row` witnesses, and known-bad non-vacuity controls in
  `tools/gate-controls.rkt` must reject before any verdict is reported. On the
  pinned graph the verdict is `COVERAGE-FAILED`: unknown functions 250 / files
  66, exit 3.
- **Modern Python 3 grammar** (#71): the tracked `grammars/python3/` grammar
  adds walrus `:=` (PEP 572), positional-only `/` (PEP 570), and PEP 695
  `type` aliases, lowered to the new `uir-walrus` / `uir-type-alias` nodes and
  emitted back. The Python front-end suites load the tracked grammar;
  `modern-python-test.rkt` covers parse, lower and emit round-trips.
- **Full-tree Python scan** (#97): `tools/scan-python.rkt` walks a directory
  tree through the tracked grammar and reports ok / INDENT-ERROR / PARSE-FAIL /
  LOWER-ERROR / FALLBACK per file, with an exit code that gates on zero
  blockers. Re-run over the pinned Django corpus: 907/907 ok.
  `scan-python-test.rkt` pins the bucket classification.
- **BendTT kernel in Koka** (#98, nc-0B63): `bendtt/` learns the BendTT kernel
  and checks it live — the total weak-head evaluator, lazy conversion, and the
  affinity/descent live check; `koka -e bendtt/meta.kk` prints the paper's 8
  verdicts and `bendtt/check.sh` asserts them. Not a roadmap phase; a research
  spike, with the Racket baseline unchanged.

## Verification observed (2026-11-01)

Re-run for this plan update from the ticket worktree with `PLTCOLLECTS` pointing
at it (13 suites, all exit 0, no FAILURE/ERROR in their output): `uir`,
`effects-algebra-property`, `effects-algebra-prohibitions`, `emit-contract`,
`provenance`, `hypergraph`, `effects-render`, `modern-python` (12/12),
`scan-python`, `baseline-check`, `effects-infer`, `effects-imports`,
`effects-contract`.

The shared baseline pin is unchanged: `baseline/type-graph.sha256` still holds
`sha256:14bac7860833329849c4d81740ecccd01870b72cb8355f947f70956e179623ba`. The
baseline gate itself was not re-run for this documentation-only change (the
`django` submodule and the vendored `grammars-v4/` tree are not present in this
checkout); the last recorded verdict is `baseline COVERAGE-FAILED
sha256:14bac786…`, unknown functions 250, unknown files 66, exit 3 (#73).

## Known failures (pre-existing or environmental)

- `gen-python3-test` and `lower-python-test` exit 1 with
  `match-define: no matching clause for #f` in the generated parser's
  `single_input` path for compound statements (the known Python3
  compound-statement limitation). The modern grammar (#71) rewrote the grammar
  but the failure points are unchanged; it needs its own bead.
- `effects-clojure-test` and `effects-jsdoc-annot-test` exit 1 in
  `parse-g4-file` (`cannot open input file`): the vendored `grammars-v4/` tree is
  gitignored and absent, so any suite that regenerates a parser from a `.g4`
  file outside the tracked `grammars/python3/` and `grammars/koka/` trees
  cannot run here. Environmental.
- `esbuild-integration-test` exits 1 with a system error because `esbuild` is
  not installed in this environment. Environmental.

## How work lands

One ticket, one feature branch (`agent/<ticket>`), one pull request: the agent
commits as `racklr-agent` and opens the PR; a person merges it. The agent never
merges a PR or enables auto-merge. Everything in this file is a snapshot at the
commit, so check `git log` and the open-PR list for the live state.

## Next

P0.3 (`drg-1bd3`) is still the long pole. The gate says so honestly
(`COVERAGE-FAILED`, exit 3), and "zero unknown leaves" is a multi-lever effort,
not a single change. Next levers, highest value first:

1. Land the receiver-type projections already in flight: imported stdlib
   classes and results (#85), optional-chain/`this` calls and unresolved
   `self.m()` leaves (#81), and the real-context census (#80).
2. Parameter typing and container element-type dataflow for the `param` (36),
   `local / loop-var` (12) and `attr` (10) families — the `*args` / `**kwargs`
   paginator pattern.
3. Effect polymorphism (row holes) for the higher-order `bare-call` (53) /
   `self-method` (11) families. Phase 1 left this open, and P0.3 needs it.
4. DATA-lexicon additions for the remaining `chained` results and bare
   stdlib/framework names — in `library-tokens.rkt` /
   `library-stdlib-types.rkt` / `library-config.rkt`, never a method-name table
   in engine code.
5. Re-pin the baseline only when a graph change is intended; the snapshot diff
   is the review.

The rest of the roadmap is represented by open PRs (P2.2/P2.3, P3.3, P4.3,
P5.1-5.4, P6.2/P6.3). Landing them clears the roadmap except P0.3's
unknown-elimination obligation; Phase 5 stays red until its submodule PRs merge
and the `effervesce` pointer bumps. Also open, outside the phase increments:
#76 (TS/TSX subscript lowering so emitted JS re-parses) and #78 (keyword-only
and rest-arity parameter markers in Python `def` lowering).
