# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- Module-receiver tracking: memcache / redis client handles and their methods
  resolve to `cache` instead of `db` / `unknown`. `racklr/effects/receiver-module.rkt`
  (new) is the MODULE half of receiver dataflow: a tracked module receiver
  (`pylibmc` / `pymemcache` / `memcache` / `redis`) resolves a module-qualified
  class read to the module's class-ref DATA type (`self._lib.Redis` → `Redis`,
  `self._lib.DriverInfo` → `DriverInfo`) and falls back to the dotted module
  marker so the module domain still classifies; `classify-all.rkt` treats such a
  construction as pure construction, and `receiver-type.rkt`'s
  `value-class-of` / `call-result-type` project it. `library-stdlib-types.rkt`
  adds the redis `Pipeline` receiver type plus the `Redis.pipeline` result
  projection (pipeline command calls are cache round trips). In the same
  increment `library-dialect.rkt` declares `kw-spread` (the `**expr`
  argument/dict-unpack encoding) a synthetic callee: it is a language construct,
  not a runtime-dispatched call, so `f(..., **options)` no longer counts as an
  unknown call site. Measured on the pinned `django/django/core` probe: unknown
  call sites 305 -> 202, unknown functions 292 -> 223, unknown files 73 -> 62
  (100 `kw-spread` sites + 3 module-receiver projections resolved, 0 rows
  added). The full package goes 4947 -> 3764 call sites / 4000 -> 3549
  functions / 478 -> 453 files, with no site gaining `unknown`. `db` falls
  (core 165 -> 164 functions) as `RedisCacheClient`'s spurious `db` class
  capability and `RedisCacheClient.set_many`'s `db` entry are removed; `cache`
  is unchanged because earlier increments already classified these methods
  `cache`. The four residual memcache/redis unknowns are caller-supplied
  `serializer` / `pool_class` OR-fallback receivers and stay fail-closed. The
  type-graph baseline is re-pinned to the reduced graph
  (`sha256:f96a641b...` -> `sha256:291dca50...`); the gate still answers
  `baseline COVERAGE-FAILED` with the Phase-0.3 obligation open.
- JS/TS front-end real-world hardening (issue #122): the JavaScript and
  TypeScript front ends now round-trip real express/axios source without
  absorb/emit/re-parse failures (one large-file parser timeout is quarantined
  and never counted as ok; issue #133). The
  front ends' grammars are tracked (`grammars/javascript/`,
  `grammars/typescript/`, `grammars/jsx/` — same layout as `grammars/python3/`
  and `grammars/koka/`), replacing the untracked, locally-edited
  `grammars-v4/javascript/*-cleaned/` copies the tests used to depend on, and
  `tools/js-ts-corpus.rkt` (+ `tools/js-ts-corpus-lib.rkt`) is the committed
  corpus scorer: for every `.js`/`.mjs`/`.ts`/`.tsx`/`.json` file it runs
  tokenize -> parse -> lower -> emit -> re-parse and classifies the first
  blocking stage (`ok`, `tokenize-error`, `parse-fail`, `lower-error`,
  `fallback`, `emit-error`, `emit-empty`, `reparse-fail`, `timeout`), with a
  per-file watchdog, `--parse-only` and `--json` modes. Grammar fixes: class
  elements no longer swallow a plain method/field name in the greedy optional
  prefix; `obj?.()` (optional call), `&&=`/`||=` (logical assignment),
  `import.meta` and `#name in obj` (private-name brand check) parse; the
  generated lexer's template-literal bookkeeping is a per-nesting frame stack
  so nested templates tokenize (`` `a${`b${c}`}` ``), and the generator no
  longer lists `fragment` rules as token candidates inside lexer modes (digits
  in template text were tokenized as numeric literals). Lowering fixes:
  destructuring declarations and for-in/for-of bindings lower to structural
  patterns (`racklr/lower-javascript/pattern.rkt`) that the JavaScript emitter
  renders back as binding syntax instead of a `?` placeholder (the largest
  fallback class); named-function parameters after the first are no longer
  dropped (repetition groups); statement-`if`s keep statement form instead of
  emitting invalid `cond ? return x : null`; reserved-word property names
  (`p.catch`, `x.new`), anonymous class expressions, block-bodied arrow
  functions (the body no longer mis-parses as an object literal) and empty
  `catch` blocks lower correctly;
  `import.meta`, dynamic `import()` and `#field` brand checks lower instead of
  crashing or being dropped; `?.`, `??`, `in`, `instanceof`, `&`, `^`, `|`,
  `<<`, `>>`, `>>>` and every compound assignment are emitted as operators;
  and `for-of`/`for-in` binding kinds are preserved. TypeScript/TSX: the
  preprocess that inserts type separators no longer corrupts type sources
  (declaration-body-targeted, comment/string-safe, continuation-aware,
  bracket-depth-aware member separators), type-only imports are erased, and the
  grammar/lowering add conditional types, labeled tuple elements, `declare`
  declarations, type predicates, full types after `as`, class members beyond
  property methods, named exports and `instanceof`/`in` infix lowering. Corpus
  result: express JavaScript 91/142 -> 142/142 ok and axios JavaScript
  103/226 -> 225/226 ok, plus one quarantined 120 s watchdog timeout
  (`axios/tests/unit/adapters/http.test.js`, parse alone > 120 s; a
  failing bucket, never counted as ok, tracked as issue #133), axios
  TypeScript 13/30 -> 22/30; the before/after tables and the remaining gaps are
  recorded in `docs/js-ts-real-world-battery.md`.
- Call-result projection through the whole-program type relations:
  `racklr/effects/return-type.rkt` adds `call-result-return-class` (a call's
  result class from the corpus return relation, then the receiver-type-dataflow
  half `x.m()` for a receiver whose class the relations track, then the
  constructor/stdlib/imported result type), `call-target-keys` (method targets
  resolved through tracked receivers, complementing the syntactic
  `self.m()`/`super().m()`/`KnownClass.m()` resolution) and `return-class-of`
  (moved from `receiver-type.rkt`; a returned call now projects too). The
  assign, parameter and return relations and the receiver chain share one rule,
  and `infer-receiver-types` gains an injectable `#:call-result-class`
  callback so an assignment from a method on a tracked receiver projects
  without a require cycle. `receiver-type.rkt` also seeds stdlib/module-marker
  receiver types the relations propagate (a relation-derived `open` result must
  seed like an annotation). The projection fails closed on a mixed return and a
  method call projects only its most-derived dispatch key, so an unprojected
  override is never shadowed by a superclass projection. The relation join
  keeps #118's widening driver: the return round still reads a previous-round
  snapshot, and termination comes from the widening, not a round cap. On
  current `main` this reduces Django-core unknown call sites 316 -> 305,
  unknown functions 304 -> 292, unknown files 75 -> 73, and the full package
  4969 -> 4947 call sites / 4018 -> 4000 functions (22 removed rows, none
  added); the type-graph baseline is re-pinned to the reduced graph
  (`sha256:78cc470a...` -> `sha256:995a25ab...`). Full-package numbers and the
  remaining levers are recorded in `docs/unknown-census.md`.
- Non-Django Python effect generalization: the Python lexicon now carries the
  non-Django web-framework surface as DATA, and the import graph canonicalizes
  modules by their package chain rather than by the scan root.
  - `racklr/effects/imports/resolve.rkt` derives a scanned file's dotted module
    name from its `__init__.py` package chain, so a repository checkout whose
    package lives under `src/` (`…/flask/src/flask/app.py`) lowers as
    `flask.app` and the corpus's own imports resolve. A scan root that is itself
    a package directory keeps the historical dotted name, so the pinned Django
    type-graph baseline is unchanged (verified: same sha256, same counters).
  - `racklr/effects/imports/library-config.rkt` adds the Flask, Requests,
    Werkzeug, Click, Jinja2, `itsdangerous`, `blinker`, `markupsafe`,
    `urllib3`, `charset_normalizer`, `idna`, `certifi` and `dotenv` import-tag
    rules with a documented fail-closed split (network/io/pure per module, plus
    public re-export member rules for `from flask import …` / `from requests
    import …`), and pure rules for the generic Python helpers they import
    (`typing`, `functools`, `platform`, `urllib.parse`). `requests.models`
    stays untagged on purpose: the receiver-TYPE table types `Response` body
    parsing (pure) vs stream consumption (io) precisely.
  - The lexicon gains a per-domain operation vocabulary
    (`lexicon-domain-op-vocabulary`, supplied by the host lexicon), and
    `method-ops` derives typed operations for more than db/io from it. The
    Python lexicon supplies network read/write/neutral verbs, so receiver-type
    dataflow now yields `network<Session>.read` / `network<Flask>.wsgi_app`
    instead of the coarse `ops=()`. The JavaScript lexicon supplies an empty
    table, proving the injection seam is host data, not a Python special case.
  - `racklr-test/effects-non-django-python-test.rkt` gates the package-chain
    module paths (src-layout checkout, package-root scan, docs/script fallback),
    the framework/pure rules, the injected network vocabulary, typed ops through
    receiver-type dataflow for `requests.Session` and `flask.Flask`, and a
    whole-pipeline run of real source using both frameworks.
- Inference-side modality, budget and grounded scope for effect rows (issue
  #111): a body's row is now composed through the effect algebra instead of a
  flat union. A direct classified effect is a `must` obligation at its call
  site; `sequence` keeps it, `choose` keeps it only when every branch must,
  and loop bodies/comprehensions/short-circuit operands demote it to `may`
  (`repeatable`), a `try` body demotes it (`weaken-must`), and a callee's
  `must` propagates only through an unconditional call. Budgets are static
  invocation upper bounds (direct site 1, branches max, sequences sum, loops and
  call-graph cycles unbounded via SCC widening). Scope is inferred
  (`reads=<resource>` / `writes=<resource>`) only when the call chain grounds
  the direction (typed op verb or the guard vocabulary's DATA verb tables) and
  the resource (typed op receiver class, or a known-class receiver root),
  composing through the algebra's `scope-lub`. The shared evaluator
  (`racklr/effects/invocation-row.rkt`, `invocation-scope.rkt`) drives both
  single-file and whole-program inference; declared whole-program method
  capabilities seed the fixpoint. On the `django/django/core` probe the effect
  set and the unknown counts are unchanged (231 unknown functions / 64 files,
  1238 entries), while modalities now include `must`, 321 entries carry a
  finite budget and 15 carry a grounded scope. Write-back keeps its existing
  runtime behavior: an inferred `must` is emitted as `may` (an analysis
  obligation is not a runtime obligation when a caller-installed handler may
  interpret the effect), while a hand-written row keeps its declared modality;
  budget/ops/scope still guard.

- `racklr-test/effects-modality-budget-test.rkt` covering guaranteed vs
  conditional modality, early-return reachability, sequential/loop/branch
  budgets, `must` propagation, recursive SCC widening, `unknown` preservation,
  grounded/conflicting/unrooted scopes, and the new region combinators.

 (P0.3))

- Koka 3.x front end rebuilt for real-world source. `grammars/koka/Koka.g4` is
  an authored port of the Koka compiler's own grammar
  (`doc/spec/grammar/parser.y`) and lexer (`src/Syntax/Lexer.x`), and Koka's
  layout pass (`src/Syntax/Layout.hs`) is injected as data: `koka-layout-policy`
  in `racklr/gend-parser/language-policy.rkt`, selected by a grammar
  declaration `options { layout = koka; }` that `gen-and-load-layout` resolves
  (an unknown policy name is an error). The grammar covers `pub` visibility,
  modules and conditional/target-guarded/`@open` imports, extern
  declarations/imports/includes, type/struct/alias/effect declarations with
  type parameters and `::` kinds, fixity, functions and lambdas with pattern
  parameters, implicit `?x` parameters, `match` with guards and arrowless case
  bodies, `if`/`elif`/`else`, effect handlers (`handler`/`handle`/`named`/
  `override`/`scoped`, `fun`/`ctl`/`final`/`raw`/`return`/`finally`/
  `initially` clauses), `mask`/`inject` (multi-effect lists), `ctx`/`hole`,
  named arguments, multi-line raw strings and nested block comments. Lowering
  and emission are rewritten against the new CST (`racklr/lower-koka/`,
  `racklr/emit-koka/`; public call shapes unchanged) with additive UIR node
  kinds (`struct`, `fixity`, `extern-value/fun/import`, `handler`,
  `handler-op`, `mask`, `named-arg`, `char`, `type-params`, `import-alias`,
  `ascribe`, `index`, `implicit-ref`) and no existing-kind field change or
  schema-version bump. The committed `tools/koka-corpus.rkt` scorer plus
  `racklr-test/koka-fixtures/` and `koka-real-world-test.rkt` measure the front
  end over a real checkout; on koka-lang/koka 3.3.0 dev (1144 `.kk` files) the
  full absorb -> emit -> re-parse battery goes from 12/1144 `ok` to 1031/1144
  and parse-only coverage from 12/1144 to 1035/1144, with no tokenizer or
  lowering failures. Remaining limits and the before/after taxonomy are
  recorded in `docs/koka-real-world-battery.md` and the loss model in
  `docs/plans/koka-pipeline.md`.

- Stable site origins for effect-edge provenance (nc-4A19):
  `racklr/effects/origin.rkt` defines `prov-origin` — file, owner, structural
  receiver chain (from the UIR callee, never a rendered expression), and a
  call-node positional discriminator (the line stand-in while UIR carries no
  source positions, after which a recorded line joins the identity). Every
  ground and call hop carries its origin; `make-callee-site` /
  `make-declared-site` take `#:file`/`#:line`/`#:position` and `infer-effects`
  gains an additive `#:source-file`, so per-file and whole-program inference
  agree on a site identity. The projection names `site` atoms by
  `file::owner@position:receiver-chain`, making the readable expression a
  `site-expression` display atom, and adds `site-file`, `site-line` (when
  known) and `site-chain` atoms; `provenance-origins` returns the sorted site
  ids for run-to-run drift diffs. Origin identity excludes the inferred effect,
  kind, coverage, tokens, and receiver class, so a site whose classification
  changes keeps its identity; two identical expressions at different positions
  are now distinct grounds. `tools/provenance-corpus.rkt` uses repo-relative
  component names and reports the distinct-origin count plus a sha256
  fingerprint (with `--origins` to dump the ids).
- `racklr-test/provenance-origin-test.rkt` covering origin fields and identity,
  identical-expression distinctness, cross-run stability, effect/kind and
  literal-value independence, the structural receiver chain, graph display
  separation, whole-program and declared origins, the line forward path, drift
  localisation, per-file/whole-program agreement, and record/graph agreement.
- Effect-row subtyping under a row-valued handler interpretation:
  `racklr/effects/algebra/subtyping` generalizes the bridge checker's
  capability → effect-names table to typed rows, so a handler can bound the
  modality, budget and ops of the effects it interprets, and handler
  chains compose (`Store via Db`, `Db via db`). The identity interpretation
  recovers `row-subtype?`, the name-set table is recovered exactly by the
  general relation, and `racklr-test/effects-subtyping-handlers-test.rkt` pins
  both plus the bridge checker's verdicts as this subtyping check.
- Confidence and coverage labels on effect edges, and transitive effect
  profiles. Every provenance ground now carries a coverage label
  (`universal` for-all structural evidence, `census-param` for a worked
  instance resolved through parameter dataflow, `local-literal` for one
  resolved from a tracked local/attribute/call-result value); the
  classification kind maps to the confidence label (`inferred` / `declared` /
  `unknown`). `racklr/effects/labels.rkt` owns the two weakest-link orders and
  the evidence derivation, and `racklr/graph/profile.rkt` exposes
  `transitive-profile` / `transitive-profiles`: per function, every reachable
  effect edge with weakest-link confidence and coverage plus the exact witness
  chains, and the unresolved leaves as data (`effect-profile-obligations`)
  rather than a boolean flag.
- The provenance graph gains a `site-coverage` atom per ground, and its atom
  vocabulary, reusable index, and completeness check are split into
  `racklr/graph/provenance-atoms.rkt`, `provenance-index.rkt`, and
  `provenance-check.rkt` (re-exported from `racklr/graph/provenance.rkt`).
- `tools/provenance-corpus.rkt` reports ground-site coverage and, with
  `--profiles`, the per-corpus transitive-profile report (complete vs
  incomplete functions, entries by effect and label, open obligations).
- `racklr-test/effect-profile-test.rkt` covering the label orders and
  weakest-link combination, coverage derivation in both inference drivers,
  declared grounds, transitive witnesses, mixed-confidence aggregation, and
  the unknown-obligation data structure.

- Injectable host lexicons for effect classification (P2.2). A lexicon bundles
  token tables, the host dialect (implicit receiver names, exception and
  constructor conventions, front-end construct encodings), import-tag rules and
  the operation vocabulary. `infer-effects`, `infer-effects-all`,
  `classify-call` and `classify-call-all` take it as `#:lexicon`; the default is
  the Python/Django lexicon, assembled from the DATA modules
  (`library-tokens.rkt`, `library-pure-names.rkt`, `library-dialect.rkt`,
  `imports/library-config.rkt`).
- `racklr/effects/lexicons/javascript.rkt`: a complete non-Python lexicon as
  data, proving a JavaScript host classifies with zero engine edits. Like the
  Python lexicon's starting point it is name-table-driven (`fs.readFile` → io,
  `console.log` → io, `JSON.parse` → pure), so it is the injection proof, not
  the receiver-type-dataflow milestone; JS unknown leaves will need the
  receiver-type dataflow work (P0.3/P4.x) applied to JS.
- `racklr/gend-parser/language-policy.rkt`: the absorb layer's host grammar
  policy (layout token post-pass, line-break/template-mode token names,
  template-rule bookkeeping hooks, preferred entry rule) as injectable data;
  `generate-parser-module` takes it as `#:policy`.
- `racklr-test/effects-lexicon-test.rkt`: end-to-end lexicon-injection tests
  plus grep guards that fail if library names, quoted-symbol/string literals,
  or host-grammar policy leak back into the core engine; the guards derive
  their forbidden sets from the lexicons' DATA and derive their scan targets
  from the directories, so a module split cannot silently shrink the scan.
- `fastapi/` vendors the backend of the official FastAPI
  full-stack template (release 0.10.0, 27 Python files / 1,611 lines) as a
  substantial real host-code sample, with an Effervesce spec (`app.fizz`) and
  `tools/bridge-python.rkt`, a tracked-grammar counterpart of
  `effervesce/tools/verify-dir.rkt` that runs the four checkers over a
  directory of Python source per file (so repeated top-level names do not
  collapse effect rows). `racklr-test/bridge-fastapi-sample-test.rkt` pins the
  result: every file parses and lowers, the bridge finds no uncovered effect,
  and the unresolved calls are reported as `bridge-unverified`.
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
- The self-emit path (P6.2): `racklr/self-emit.rkt` takes a compiler-as-UIR
  program, emits it through the P3 contract (`emit`), writes the generated
  source, runs it where the target runtime is wired up, and reports whether the
  effect rows survived in the target's native vocabulary. The self-image is a
  registry entry or a serialized UIR file (s-expression or JSON), so a lowered
  compiler self-image plugs in with no driver change.
  `tools/emit-self.rkt` is the CLI (`--image kernel --target python --run`),
  and `racklr/self/seed/` is the runnable seed self-image: a compiler written
  entirely as a UIR program (`tokenize`, `parse`, `token_atom`, `emit_expr`,
  `emit_program`, `compile_source`, `main`) with an explicit effect row on
  every function (seven named functions: six pure stages, `main` with the
  `io` row). Emitted into Python it is a standalone compiler whose own output is runnable Python, and
  its rows travel as `@__withEffects({...})` with `__perform(...)` runtime
  guards.
- `racklr-test/self-seed-test.rkt`, `racklr-test/self-emit-test.rkt` and
  `racklr-test/self-emit-integration-test.rkt`: the seed program's shape and
  rows, the driver/CLI and the native-row guarantee, the serialized-UIR seam,
  and the end-to-end proof (emit the compiler, run it, run the program it
  compiles) with real `python3` execution.
- `docs/self-emit.md` documents the self-emit design, the P6.1 seam and the
  honest loss model (Python is the runnable target today).
- Python `def` parameter lists now populate the `typed-param` kind slot with
  the calling convention the source spells: keyword-only parameters after a
  bare `*` (`kw-only`), `*args` (`var-positional`) and `**kwargs`
  (`var-keyword`). `lower-parameters` tracks the markers so a default stays on
  its own parameter, and `emit-python` renders the markers back for functions
  and methods.
- `racklr-test/lower-python-params-test.rkt` covering lowering, emit
  round-trips, class methods, composition with `/`, and serialization of the
  star markers.

- The P6.3 self-emission checker: `tools/self-emit.rkt` (oracle, goldens,
  verify --tower, tower, gate, selftest) with the canonical UIR-plus-row
  comparison document (`tools/self-emit/canonical.rkt`), the original-tower
  oracle (`oracle.rkt`), the shared gate runner (`gate.rkt`) that reports the
  baseline's separate pass/fail and coverage verdicts, and the P6.2 seam
  (`tower.rkt`) that runs the emitted compiler and compares translations and
  effect rows with the original UIR self-image. `docs/self-emit-verification.md`
  states the emitted-tower contract and the acceptance transcript.

### Changed

- Effect inference's fixpoint is now expressed in the algebra:
  `infer.rkt` and `whole-program.rkt` run `algebra/least-fixpoint` over
  `algebra/join`, and `rows.rkt` is the algebra adapter. The refactor is
  behavior-preserving: the `type-graph.rkt django/django/core` baseline is
  unchanged (`sha256:14bac7860833329849c4d81740ecccd01870b72cb8355f947f70956e179623ba`).
- The classification engine is host-neutral and fully injectable: the lexical
  receiver-type/classifier helpers moved to `receiver-resolve.rkt` (splitting
  `classify-all.rkt` under the 400-line obligation), and the annotation surface
  is an injected hook (`annotation-hooks.rkt`) rather than a require of one
  host's annotation module. The default remains the Python/Django lexicon, so
  classification results are unchanged (`baseline OK`).
- The parser generator no longer embeds Python layout code or
  JavaScript/TypeScript template-literal rule names; existing front ends'
  generated parsers are byte-identical.
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
- JSON front end: corrected the checked-in generated parser's safe-codepoint
  character class (a stopgap; regeneration tracked in #96), which rejected `u`,
  `A`–`Z`, and `0`–`9` inside JSON strings.
- `gend-parser` now decodes `\uXXXX` escapes in string literals (character
  classes already did), so generated parsers match the grammar's characters
  instead of the escape text. `gend-parser-test.rkt` pins escaped class and
  literal behavior.
- The loss-model coverage gate now requires every model to declare every loss
  feature exactly once (explicitly `refused` when it does not apply), and
  `verify-round-trip` rejects an incomplete model instead of silently verifying
  only the declared dimensions.
- Loss-model normalizations rebuild serialized `fn`/`typed-param`/`ann-set!`
  nodes through shape helpers that preserve appended schema fields, so a
  parallel schema addition (such as the `typed-param` kind) cannot be silently
  dropped.
- Loss verification follows the emit contract (`racklr/emit-effects.rkt`):
  Python/JavaScript rows are checked through their runtime guards, Clojure
  through the `:__effects__` metadata map, and Koka through the typed row, with
  the model's `effect-rows` details naming the documented annotations as well.
- The loss verifier drops emitter-injected guard declarations even when an
  emitter wraps them in `const`/`let`/`var` statement calls, so the new guard
  runtime is not mistaken for source definitions.
- TypeScript lowering stores numeric literals in `uir-number` as strings (seven
  sites in `lower-typescript/{stmt,expr}.rkt`), as the schema requires; emitted
  TSX for rest/array extraction and `void` previously failed schema-driven
  serialization with `uir->sexp: string field: 0`.
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
- Tracked pass-1 Haskell grammar `grammars/haskell/Haskell.g4`, an authored
  subset of the Haskell 2010 rule set written for the generator's
  NEWLINE/INDENT/DEDENT layout contract. The vendored `antlr/grammars-v4`
  `haskell/` pair generates but cannot parse: its `OCURLY`/`VOCURLY`/`SEMI`/
  `CCURLY` tokens come from the Java `HaskellBaseLexer` layout preprocessor,
  whose lexer actions racklr compiles as no-ops, and its `NEWLINE` rule does not
  carry the next line's indentation, so no `INDENT`/`DEDENT` is inserted.
  `grammars/haskell/README.md` records the acquisition evidence, the pass-1
  limits, and the upstream BSD-3-Clause attribution.
- Tracked pass-1 OCaml grammar pair `grammars/ocaml/OCamlLexer.g4` +
  `grammars/ocaml/OCamlParser.g4`, authored from the OCaml 5.2 manual because
  `antlr/grammars-v4` ships no OCaml grammar. Nested `(* ... *)` comments use
  the generator's lexer mode stack; `grammars/ocaml/README.md` records the
  generator constraints worked around and the pass-1 limits.
- `racklr-test/gen-haskell-test.rkt` (15 test cases) and
  `racklr-test/gen-ocaml-test.rkt` (25 test cases) load the tracked grammars and
  assert tokenizer, layout/precedence, and Phase-1 fixture parsing plus reject
  cases. Each suite also asserts the loaded grammar path is under tracked
  `grammars/<lang>/` and never the gitignored `grammars-v4/` tree, so it runs in
  a clean checkout; both READMEs record the authored grammars' generated-module
  cost. Both suites exit 0 with zero failures or errors.

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
- The generated lexer advances line and column across a token that contains
  line breaks (multi-line triple-quoted strings, `NEWLINE` tokens, multi-line
  comments), so tokens after a multi-line docstring report their real source
  position instead of the line the literal opened on. Covered by
  `racklr-test/python-multiline-strings-test.rkt`, which also pins the
  catalogue's multi-line triple-quoted string reproducers as a single STRING
  token through parse, lower and emit.

### Fixed

- TypeScript/TSX subscript expressions (`posts[0]`, `posts[0].title`,
  `rows[i][j]`, mixed binary/ternary tails, postfix `++`/`--`) no longer lower
  to a null expression. The parser folds the postfix chain after an identifier
  into a trailing `singleExpression`; the TypeScript lowering rebuilds that
  suffix as ordinary CST and routes it back through the normal expression
  lowering. Before the fix, subscript declarations such as
  `const heroPost = allPosts[0];` dropped the initializer and emitted
  `const heroPost;` (rejected by a JavaScript parser), and subscript
  assignments such as `x = rows[i];` emitted the wrong JavaScript `x = null;`.
  Known remaining gaps, unchanged by this fix: multi-element brackets (the
  comma operator, `a[0, 1]`) and optional chaining after a folded subscript
  (`a[0]?.b`) still lower to `uir-null` because the reconstruction cannot
  rebuild those parser shapes without fabricating tokens.
- Python `def` parameter lists no longer drop the bare `*` keyword-only
  separator or the `*`/`**` prefixes of rest-arity parameters. Previously
  `def f(a, *, b=None)` lowered and re-emitted as `def f(a, b = None)`, which
  silently accepts a second positional argument; the emitted signature now
  keeps the source calling convention.

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
