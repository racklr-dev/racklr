# LOOP_PLAN — Phase 0 (Make the foundation load-bearing)

## Status

- **P0.1 — Version the UIR schema and add a migration path: DONE (verified)**
  - `racklr/uir.rkt` is a thin facade (183 lines); serialization and versioning
    live in `racklr/uir/serialize.rkt` (350) and `racklr/uir/version.rkt` (94).
    All files under the 400-line obligation.
  - Versioned envelope `(uir VERSION payload)` for JSON and explicit sexp entry
    points; bare `uir->sexp`/`sexp->uir` stay version-free (in-memory graph
    representation).
  - Migration chain `register-uir-migration!` / `migrate-uir-payload` turns a
    struct field-count change into a controlled event. `json->uir` reads old
    payloads and migrates before parsing; future versions raise.
  - Tests: `test-versioned-sexp` and `test-schema-migration` added to
    `racklr-test/uir-test.rkt`. `raco setup racklr` exits 0.
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
  - Verified 2026-10: **907/907 parse, 0 failed; 8244/8244 named functions carry
    a row** (fn-effect-row links = 8244). The two structural criteria are MET.
  - Remaining gap: **unknown = 5014 functions / 514 files** (the "zero unknown
    leaves" criterion). This is a multi-lever effort, not a single change.
  - Increment landed this iteration (correct, tested, no regression, but ~no
    aggregate movement):
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
  - The real unknown surface (needs its own plan, not this iteration):
    - `obj.method(...)` on arbitrary receivers (parameters, `self.a` attrs whose
      assignment is not a tracked constructor). Needs parameter/return-type
      dataflow — the "type projection" still unimplemented in `type-graph.rkt`.
    - Stdlib constructor returns (`self.log = logging.getLogger()`,
      `f = open(...)`, `socket.socket()`) — receiver-type dataflow only tracks
      in-tree `KnownClass(...)` constructors today.
    - Bare stdlib/framework functions missing from the DATA lexicons
      (`urlsplit`, `iscoroutinefunction`, `defaultdict`, `functools.*`, Django
      helpers `import_string`, `get_resolver`, `deconstructible`, ...).

## Verification observed (2026-10)

- `racket uir-test.rkt` → "All uir tests passed", exit 0.
- `raco setup racklr` → exit 0.
- Full-suite gate (10 suites): 9 PASS, 1 FAIL — see next.

## Known failures (pre-existing, not caused by P0.1)

- `lower-python-test` exits 1: `gen-tmp-1.rkt:1220` `match-define: no matching
  clause for #f` inside `parse` → `check-py`. Fails identically against HEAD;
  P0.1 does not touch the Python lowerer or the generated parser. Needs its own
  bead.
- `effects-clojure-test` and `effects-jsdoc-annot-test` exit 1 with
  `open-input-file: cannot open input file ... grammars-v4/...`. The vendored
  grammar tree is gitignored and absent from this checkout, so any suite that
  regenerates a parser from a `.g4` file cannot run. Unrelated to effects work.

## Note on git state

P0.2 landed on a feature branch with the operator's go-ahead to open and merge
PRs (gh CLI, bot identity `allen-munsch`). The four untracked trees
(`atomspace/`, `chaining/`, `effervesce-out/`, `link-grammar/`) predate this
work and are left untouched.

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

## Next

P0.3 (`drg-1bd3`) is the active task but "zero unknown leaves" is a multi-lever
effort. Next lever to pick (highest value first): parameter/return-type
dataflow for arbitrary receivers (the unimplemented "type projection" in
`type-graph.rkt`), then stdlib-constructor tracking in `receiver-type.rkt`,
then DATA-lexicon additions for bare stdlib/framework names. Each is its own
bead, not a single change.
