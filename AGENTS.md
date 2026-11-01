# AGENTS.md

Project rules for racklr and effervesce. The authoritative contract is
`STYLE_GUIDE.md`. Read it before any change. `README.md` documents the
architecture, `docs/` holds the design documents (`uir-schema.md`,
`effects-proposal.md`), and `LOOP_PLAN.md` / `ROADMAP.md` track the work.

## What this is

racklr lowers Python3, TypeScript, TSX, JavaScript, JSX, Clojure, and Koka
source into one universal intermediate representation (UIR) and emits target
code from it. effervesce infers a typed effect row for every function and
method, grounded in the concrete call chain, and checks that graph against
declared capability contracts.

## Obligations (non-negotiable)

- Every function and method carries an effect row. `unknown` is a tracked
  state with an elimination obligation, never a silent default.
- Effect keys come from the call chain via receiver-type dataflow
  (class to module to domain), never from a method-name table in engine code.
- Library-specific tokens live only in `racklr/effects/library-tokens.rkt` and
  `racklr/effects/imports/library-config.rkt`. Engine code carries none.
- Every effect edge carries provenance you can point at.
- UIR is the single source of truth.

## Modularity

- No `.rkt` file over 400 lines. Split by responsibility, not size.
- Respect layer boundaries: absorb, uir, effect, check, graph, emit.
- A new front end is a grammar plus a lowering hook plus token-lexicon data.
  It must not edit core modules.
- Keep public call shapes stable. Add injectable config as an optional
  argument with a default, not a breaking change.

## Test gate

Run from the repo root unless noted. `raco test` is unreliable here: zero exit
is not success. Grep the output for FAILURE and ERROR and read the real exit
code.

- `raco setup racklr` then `raco setup effervesce`, both exit 0.
- Racklr suites: `cd racklr-test && racket <suite>-test.rkt`.
- Effervesce suites: `cd effervesce && racket <suite>-test.rkt` (must run from
  `effervesce/`).
- Shared baseline and coverage verdict: `racket tools/baseline-check.rkt`
  prints exactly one verdict. Pass/fail and coverage are separate, so a run
  that matches the pin is not green while its unknown-elimination obligation is
  open.
  - `baseline OK sha256:<hash>`, exit 0: the graph matches the pin and the
    unknown-elimination obligation (Phase-0.3 bar: zero unknown leaves) is
    discharged.
  - `baseline DRIFT`, exit 1: the graph moved; the changed counters and the
    exact atom lines added or removed are printed.
  - `baseline COVERAGE-FAILED sha256:<hash>`, exit 3: the graph matches the pin
    but unknown effect leaves remain.
  - `baseline GATE-FAILED`, exit 4: a known-bad non-vacuity control was not
    rejected, or the summary counters disagree with the named `fn-effect-row`
    witnesses. Never green in this state.

  The hash pins `baseline/type-graph.snapshot`, the canonical serialization of
  the type-graph output; a mismatch prints the exact graph diff. Drift is a
  regression until proven otherwise, and re-pinning is a deliberate `--update`
  whose snapshot diff is the review.

## Working rules

- Confirm the baseline before touching anything. Record the hash and the
  verdict: `baseline DRIFT` is a regression, while `baseline COVERAGE-FAILED`
  with the recorded unknown counts is the open Phase-0.3 obligation, not a
  reason to re-pin. Record both again after the change.
- Report only what you observed. Say plainly when you could not verify
  something. Never manufacture a green result or suppress a failing check.
- Fix the underlying representation, not the symptom.
- Do not commit. Commits are the operator's job, GPG-signed. Leave the working
  tree uncommitted and say what changed. Multi-step work lives on a dedicated
  feature branch; stage specific files, never `git add -A`.
