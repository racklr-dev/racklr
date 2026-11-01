# Real-context unknown census

`tools/unknown-census.rkt` is the aiming instrument for the `unknown`
elimination obligation: it reports every call site the **real** classification
still leaves `unknown`, attributed to the receiver-type-dataflow lever that
would resolve it, and it emits the exact sites in a deterministic TSV.

## Why not the empty-context census

Two diagnostics can count unknowns, and they disagree by an order of magnitude:

- **Empty context** (`effervesce/tools/unknown-histogram.rkt`): classifies each
  call callee with *no known names*, so a call to another function in the tree,
  a `self.m()` edge, or an imported module handle counts as `unknown`. It
  reports 1463 non-edge call sites on `django/django/core`.
- **Real context** (`tools/unknown-census.rkt`): builds the module table,
  export graph, derived `Class.method` capabilities, type relations, and MRO,
  then runs the whole-program effect fixpoint (`infer-effects-all`). Only sites
  the real context cannot resolve are counted: 215 call sites on the same
  corpus at the same revision.

The empty-context number overcounts the surface and cannot aim work; a lever
chosen against it would be chosen against phantoms. This census is the real
surface. (The empty-context histogram is kept as a rough calibration probe,
not as an aiming tool.)

## Run

From the repo root, with the racklr collection on `PLTCOLLECTS` (the same
requirement as `effervesce/tools/type-graph.rkt`), and with the gitignored
`grammars-v4/` tree present for the Python front end:

```sh
PLTCOLLECTS="$(pwd):" racket tools/unknown-census.rkt django/django/core
PLTCOLLECTS="$(pwd):" racket tools/unknown-census.rkt django/django --tsv /tmp/full-census.tsv
```

Options:

- `--top N` — methods listed per lever (default 10)
- `--shape SHAPE` — restrict `--sites` lines to one receiver shape
- `--lever LEVER` — restrict `--sites` lines to one dataflow lever
- `--sites` — print per-site lines
- `--tsv PATH` — write the deterministic per-site TSV

A module's dotted name comes from its PACKAGE CHAIN, not from the scan root:
`path->module-path` walks the `__init__.py` chain up from each file, so a
repository checkout whose package lives under `src/` (`…/flask/src/flask/app.py`)
resolves as `flask.app` and the corpus's own imports resolve, while a scan root
that is itself a package directory (`django/django/core`) keeps the historical
dotted name (`django.core…`) and the pinned Django baseline is unchanged.

## The lever taxonomy

Every unknown call site is attributed to the receiver-type-dataflow mechanism
that would resolve it (`census-lever-of` in `racklr/effects/census.rkt`):

| lever | receiver shape / binding | what resolves it |
| --- | --- | --- |
| `return-type-projection` | `call-result`, local bound from a call | project the return class of `f(...)` so `f(...).m()` resolves |
| `parameter-dataflow` | `param` | propagate parameter types from the concrete call chain |
| `self-attr-typing` | `self-attr` | type `self.attr` from its constructor/annotation and MRO |
| `element-type-dataflow` | local `loop-var` / `comp-var` | project iterable element types into loop/comprehension variables |
| `attribute-alias-typing` | local `attr` / `alias` | carry a receiver type through an attribute copy or alias |
| `mro-edge-resolution` | `self-method` | resolve `self.m()`/`cls.m()` through the class MRO to an edge |
| `multi-level-chain` | `chained` | walk a multi-level `a.b.m()` receiver chain to a class |
| `higher-order-row-hole` | `bare-call` whose callee names an enclosing parameter | type a callback parameter as a row hole at its call sites |
| `bare-call-lexicon` | `bare-call` to a free/global/imported name | add the missing bare stdlib/framework name to the DATA lexicon |
| `import-handle-lexicon` | `symbol` | tag the imported module/object handle's effect domain |
| `literal-constructor-typing` | local `literal` | give container/stdlib literals a receiver type |
| `unbound-binding-resolution` | local `unbound` | bind a module-global/import/closure name before typing the receiver |
| `exception-type-dataflow` | local `exception` | type the caught exception binding |
| `function-value-typing` | `bare-call` or receiver from a local `fn` | type a function value held in a local before calling it |
| `local-constructor-tracking` | other local | track the local's concrete constructor (`KnownClass(...)`) |
| `unclassified` | `var` / `other` | shape the census does not yet attribute to a lever |

## Aiming work

1. Run the census on the corpus that matters (`django/django/core` while a
   lever is in flight; `django/django` for the full-bar number).
2. Read the lever histogram highest-first, open the TSV, and filter to that
   lever:
   ```sh
   awk -F'\t' 'NR==1 || $7=="parameter-dataflow"' /tmp/full-census.tsv
   ```
3. Land one lever. Re-run the same command and diff the TSV (and the site /
   function / file counters) against the baseline below: a lever that does not
   move real sites did not move the goal.

`higher-order?` in the TSV is #t only for a callback parameter (row-hole
work); a bare call to a function-valued local is the separate
`function-value-typing` lever (binding-kind `fn`), and a bare free/imported
name stays `bare-call-lexicon` (binding-kind `unbound`), so the three
`bare-call` cases are not conflated. The `component` column is the dotted module path (for example
`django.core.cache.backends.base`), so the TSV does not depend on the checkout
directory and diffs cleanly across machines.

TSV fields are **space-normalized, not escaped**: every control character in a
field (tab, newline, carriage return, ...) is replaced by a space. The file is
a diff artifact, not a lossless serialization of arbitrary identifiers.

## Baseline (observed 2026-11-01, racklr `11eb4a2` + this change)

`django/django/core`:

```
files scanned: 112   parsed: 112   failed: 0
functions: 979 (in 101 files with inferred functions)
unknown call sites: 215   unknown functions: 236   unknown files: 65

receiver shapes: local 63, bare-call 53, param 36, self-attr 23,
                 chained 18, call-result 11, self-method 11
local binding kinds (of 63): call-result 32, loop-var 12, attr 10,
                 literal 6, comp-var 2, unbound 1

levers:
  return-type-projection   55   parameter-dataflow       36
  self-attr-typing         23   element-type-dataflow    22
  multi-level-chain        18   attribute-alias-typing   15
  higher-order-row-hole    15   mro-edge-resolution      11
  literal-constructor-typing 10 bare-call-lexicon         9
  unbound-binding-resolution 1
```

`django/django` (907 files, the acceptance-bar corpus):

```
files scanned: 907   parsed: 907   failed: 0
functions: 9503 (in 570 files with inferred functions)
unknown call sites: 3911   unknown functions: 3612   unknown files: 449

receiver shapes: local 1108, param 733, self-attr 639, bare-call 503,
                 chained 327, call-result 312, self-method 285, other 4
local binding kinds (of 1108): call-result 437, unbound 231, loop-var 192,
                 attr 155, comp-var 59, literal 21, exception 9, alias 4

levers:
  return-type-projection    811    parameter-dataflow        733
  self-attr-typing          639    multi-level-chain         327
  bare-call-lexicon         299    mro-edge-resolution       285
  element-type-dataflow     272    unbound-binding-resolution 231
  attribute-alias-typing    226    higher-order-row-hole      41
  literal-constructor-typing 34    exception-type-dataflow     9
  unclassified                4
```

One full-corpus run takes on the order of 8-15 min wall including parser
generation (452-908 s observed on this machine) and peaks at ~275 MB RSS, so
the census fits a small harness memory budget.

The independently pinned `effervesce/tools/unknown-shapes.rkt` reports the
same 215 sites / 236 functions / 65 files and the same shape histogram on
`django/django/core`, and the same 3911 / 3612 / 449 and the same shape and
local-binding-kind histograms on `django/django`, so the census agrees with
the diagnostic already used by the loop.

The effect-graph gate is unchanged by this diagnostic. On
`django/django/core`, `racket tools/baseline-check.rkt` reports
`baseline COVERAGE-FAILED sha256:94fcb2b2b58254f4162c4ebf80bb3ee6876048d4c61e12b6e65c86f5d7d11263`:
the graph **matches the pin** (the pass/fail half is green — no drift) while the
Phase-0.3 unknown-elimination obligation is still open (236 unknown functions /
65 unknown files, `<= 0` required). The census in this document is the aiming
instrument for that open obligation, not a claim that it is discharged. The
same run reports 112 parsed / 0 failed, 979 named functions, db 169/54,
network 9/6, inherits 126, fn-effect-row 979.

Re-run the two commands above to reproduce the numbers; a lever PR should
update this block (or the committed baseline it points at) in the same change
that moves the number.

## Non-Django Python (Flask + Requests)

The same instrument/contract is measured against real non-Django frameworks:
shallow checkouts of `pallets/flask` and `psf/requests` scanned as
`racket effervesce/tools/type-graph.rkt flask requests` (the invocation used in
issue #109; 120 files including the frameworks' own tests, examples and docs).
The corpus is external and not vendored, so these numbers are a spot-check of
the generalization, not a pinned gate; the pinned Django baseline remains the
regression gate.

Before the non-Django Python lexicon data and the package-chain module paths
(racklr `0a26a01`): 120 parsed / 0 failed, 1890 named functions, 1183 effectful,
998 unknown functions in 83 files, `entries with typed ops: 0 / 1936`.

After:

- 120 parsed / 0 failed, 1890 named functions, 1119 effectful,
  781 unknown functions in 76 files,
  `entries with typed ops: 52 / 1832` — e.g.
  `network<Session>.read` (`Session.get`), `network<Session>.request`,
  `network<Flask>.run`, `network<FlaskClient>.test_client`.
- The real-context census over the frameworks' own source only
  (`flask/src/flask` + `requests/src/requests`, 43 files) moves the same way:
  unknown call sites 385 → 215, unknown functions 284 → 197, and the
  `bare-call-lexicon` lever 121 → 45 sites, `unbound-binding-resolution`
  91 → 42.
- The residual unknowns are still the receiver-dataflow levers above
  (`return-type-projection` 40 sites, `multi-level-chain` 30,
  `attribute-alias-typing` 17, `parameter-dataflow` 13, …); the framework
  lexicon removes the `bare-call-lexicon` / `import-handle-lexicon` share of
  them, it does not eliminate the dataflow work.

The pinned Django baseline is unaffected by the framework data: `baseline
COVERAGE-FAILED sha256:be9db70ab78cbbb6810ce8559735d9d6e6e0d2b36d7b9925f7e387414babeac2`
still reports the graph matches the pin (231 unknown functions / 64 unknown
files, the open Phase-0.3 obligation).

## Determinism

Sites are sorted by component, enclosing function, exact site string, shape and
op; the aggregations sort by count then printed key; the TSV header is fixed.
Two runs over the same corpus produce byte-identical TSV, so a lever's effect
is a diff, not a remembered number.
