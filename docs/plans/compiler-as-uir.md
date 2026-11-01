# Plan: the compiler as a UIR program (P6.1)

## Definition of Done

GitHub issue #38, P6.1. The compiler's own modules lower to UIR, every function
carries an inferred effect row, and `racklr/effects/infer.rkt` runs over that UIR
with no crash and reports the compiler's own rows. Nothing is special-cased in
core inference: the compiler is just another program to the effect system.

Verification is executable in `racklr-test/compiler-uir-test.rkt`; the
human-facing view is `tools/compiler-effects.rkt`.

## Architecture

The pipeline is the ordinary absorb → effect path, with the compiler corpus as
the input:

```
racklr/**.rkt + effervesce/**.rkt
  → racklr/lower-racket.rkt            (two-pass Racket front end)
      pass 1: scan  — module id, requires, provides, defines, structs
      pass 2: lower — datums → UIR with a resolved callee per call site
  → racklr/compiler-uir.rkt            (this driver)
      discovery  — compiler source paths under <root>/racklr + <root>/effervesce
      lowering   — lower-racket-modules paths #:root
      inference  — infer-effects-all over (component . uir-module) pairs
      annotation — write each inferred row into every named uir-fn effects slot
      report     — counts, histogram, per-component functions, unknown census
  → racklr/effects/infer.rkt           (reused unchanged: the whole-program
                                        fixpoint, classifier, token lexicon)
```

### Two-pass absorb and module identity

The front end scans every module before lowering any, so a call to a name
imported from another compiler module resolves to the module that defines it.
Component id = source path relative to the checkout root without `.rkt`
(e.g. `racklr/effects/infer`). Top-level defined names are emitted mangled as
`<component>::<name>`. That is the whole cross-file trick: a mangled callee is a
globally unique symbol, so the existing classifier resolves it as a call-graph
edge through `fn-defs`/`known` and the fixpoint propagates the callee's row. The
one cast is in the name the front end emits; core inference is unchanged.

Nested/local functions are named `uir-fn` nodes (and therefore inference nodes
with their own rows). A local definition or anonymous function gets a unique
symbol (`helper@3`), bound in the lexical environment before the body is
lowered, so mutually recursive or forward-referenced helpers are real
call-graph edges instead of bare names and two same-named helpers cannot merge
into one row. Lexically bound values (parameters, `let` bindings) stay bare: a
call through one is a higher-order call and is deferred to `unknown`.

## Effect rows and the lexicon

The Racket front end lowers calls, not receiver types, so classification comes
from the token data layer. Racket evidence lives only in
`racklr/effects/library-racket-tokens.rkt` (DATA: 332 pure procedures, 90
effectful: 55 io, 11 process, 6 time, 18 network). The pure names enter their
own `racket-builtins` field of the injected `effect-tokens`, consumed by one
generic predicate (`pure-racket-builtin-callee?`) in `pure-roots.rkt`, and the
effect names enter the shared effect tables (`io-builtins`, `time-methods`,
`network-names`, `stdlib-root-effects`). The dedicated pure field matters: the
Python/JS path reads the shared `pure-stdlib-bare-names` table as a
language-level value-constructor list for receiver typing, so a Racket name such
as `values` or `filter` must not turn a Django `.values()`/`.filter()` call
into a pure constructor. That split is the only core touch this ticket makes
(additive injected-config plumbing; no classifier semantics changed) and it
keeps the shared pinned type-graph output byte-identical.

Racket raising primitives (`error`, `raise-*`) are language constructs, not
domain effects: the front end lowers them to the UIR raise construct, so they
do not need a bare-name token in the shared default lexicon (where `error()`
could also name an arbitrary host-code callable).

Everything outside those sets stays a tracked `unknown` entry with modality
`unknown` — including higher-order calls through parameters (`expr-fn`,
`body-fn` callbacks in the front end), `apply`, and dynamic names
(`dynamic-require`, eval). Unknown is deferred, never guessed.

## Verification

```sh
# library/CLI/test compile against this checkout's collection
PLTCOLLECTS="$(pwd):" raco make racklr/compiler-uir.rkt \
    tools/compiler-effects.rkt racklr-test/compiler-uir-test.rkt

# the DoD gate: prints observed counts + histogram, ends in a PASS line, exits
# non-zero on any failed check (rackunit top-level checks do not set exit codes)
cd racklr-test && PLTCOLLECTS="$(pwd)/..:" racket compiler-uir-test.rkt

# human view: report, then every module's functions with their rows
PLTCOLLECTS="$(pwd):" racket tools/compiler-effects.rkt [--root DIR]
```

The suite asserts, against the live corpus: ≥120 discovered modules and every
file lowers to a `uir-module`; ≥1000 named functions and every named function
has an inferred row key and, after annotation, an actual `uir-effect-row` in its
effects slot (and that the attached row equals the inferred row — nothing
dropped or invented); inference and lowering are deterministic across runs;
the report's histogram, unknown counts and per-component counts agree with an
independent pass over the rows; every row entry has a tracked modality; and
three rows are checked against source-level ground truth:

| component / function | ground truth |
| --- | --- |
| `racklr/g4-parse` `parse-g4-file` | `io` (`file->string`) |
| `racklr/esbuild-bundle` `esbuild-bundle` | `process` (`system`) + `io` |
| `racklr/emit-json` `write-json-string` | `io` (`display` to a port) |

One labelled constant, `UNKNOWN-FUNCTION-RATIO-LIMIT`, is a ratchet on the
tracked-unknown share, set from observed output with a small margin plus its
rationale in the comment. It is not a target: passing means "no regression
beyond the recorded unknown surface", not "the compiler is fully classified".

## Current unknown surface and next levers

Observed at P6.1 integration (137 program modules; 2498 named functions, 2498
rows, every function annotated; effect histogram `unknown:718 io:145
process:18`; entry modalities `unknown:718 may:163`; 718 unknown functions =
28.7%, 534 unknown call sites). The remaining tracked-unknown functions come
from

- higher-order calls through parameters and `apply` (`expr-fn`/`body-fn`
  callbacks, active-parameter `parameterize`, named-let loops),
- Racket library procedures not in the token lexicon (uncertain or non-stdlib
  names are deliberately left out rather than guessed),
- dynamically resolved names (`dynamic-require`, eval, `namespace-variable-value`).

Levers, in dataflow order: extend the Racket token lexicon from the real call
sites (`tools/compiler-effects.rkt` lists them per function); teach the front
end to resolve more lexical bindings (named-let loops are already named `fn`
nodes); keep `unknown` deferred for genuinely dynamic calls. Do not add a
name table to engine code — library knowledge stays in the data modules.

## Non-goals

- No changes to core inference, the classifier's decisions, or the effect
  contract. The only core edits are additive injected-config plumbing: a
  dedicated `racket-builtins` field on `effect-tokens` and one predicate clause
  in the pure-name chain, with the shared baseline re-run and unchanged.
- No re-implementation of Racket semantics: macro bodies and syntax objects
  lower as syntax data, not executed code.
- No runtime guards or emission of the compiler back to Racket (absorb only in
  P6.1; emit identity is separate work).
