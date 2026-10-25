# STYLE_GUIDE

The working contract for racklr and effervesce. Read this before a change. Paste the prompt template at the bottom into every session.

The guide exists for one reason: the work happens in parallel and each thread must go deep without breaking the contract the other threads rely on. Every rule below is either a hard obligation the system enforces, or a way to keep parallel threads from colliding.

## Obligations

These are the non-negotiable contracts. A change that breaks one is not done, no matter how green its own tests are.

- Every function and method carries an effect row (db, io, network, process, cache, llm, time, email). Unknown is a tracked state with an elimination obligation, never a silent default.
- Effect keys are grounded in the concrete call chain. Classification is a property of the receiver's class, resolved class to module to domain. No method-name heuristics in engine code.
- Library-specific tokens live only in the two DATA modules: `racklr/effects/library-tokens.rkt` and `racklr/effects/imports/library-config.rkt`. Engine code carries none.
- Every effect edge carries provenance. You can point at the exact tokens that made something `db`, or it is not classified.
- UIR is the single source of truth. Absorb then emit is identity within the declared loss model, not a hidden one.
- The checkers that grade host code are themselves gradeable. The gate is the contract.

## Working expectations

- Confirm the baseline is green before touching anything. Record the baseline numbers you observed, then record them again after.
- Work in a plan-execute-verify loop. State the goal in one line, list the concrete steps, do one step, verify it, then move on.
- Report only what you observed. Say plainly when you could not verify something.
- A zero exit is not success. `raco test` is unreliable here. Grep the output for FAILURE and ERROR and read the real exit code.
- Do not trust a note that claims a gate passed. Re-run the gate and capture real output.
- Commits are the operator's job, GPG-signed. Never commit. Leave the working tree uncommitted and say what changed.
- Multi-step work lives on a dedicated feature branch. Stage specific files, never `git add -A`.

### Parallel work

Parallel threads stay out of each other's way by owning disjoint file sets.

- Decompose by layer: absorb, uir, effect, check, graph, emit. One thread owns one layer's files.
- No two threads edit the same `.rkt` file in the same iteration. If a change crosses a boundary, one thread owns it or the threads sequence on it.
- Each layer has its own test suite. A thread's gate is its layer's suites, plus the shared baseline when it touches uir, effects, or graph.
- Integrate by re-running the full shared baseline, not by merging unverified work.

### Going deep

- Follow the call chain to the root cause. An unknown leaf is a bug to eliminate, not a classification to accept.
- Fix the underlying representation, not the symptom. The `uir-string` pitfall is the canonical case: fix the accessor's fallthrough branch, do not special-case downstream.
- Do not paper over a failure with a heuristic. If a token cannot be classified, make the dataflow resolve it, do not add a name table.

## Modularity

- No `.rkt` file over 400 lines. Split by responsibility, not by mechanical size.
- Respect the layer boundaries: absorb (grammars plus lowering), uir, effect, check, graph, emit. A module stays in its layer.
- Language knowledge is data, not core code. Adding a front end is a grammar plus a lowering hook plus a token lexicon, and it must not edit core modules.
- Keep public call shapes stable. Add injectable config as an optional argument with a default, the way `direct-effect-of-callee` and `pure-stdlib-callee?` take `[tokens default-effect-tokens]`. Existing consumers keep their single-argument call.

## Testability

The gate. Run it from the right working directory.

- `raco setup racklr` then `raco setup effervesce`, both exit 0.
- Racklr suites: `cd racklr-test && racket <suite>-test.rkt`.
- Effervesce suites: `cd effervesce && racket <suite>-test.rkt`. These must run from `effervesce/`; `check/verify-test.rkt` opens its fixture relative to the current directory.
- The shared baseline: `racket effervesce/tools/type-graph.rkt django/django/core` must print `112 parsed, 0 failed, db 52 files, network 2, unknown 83, inherits 126`. A drift from these numbers is a regression until proven otherwise.
- Two Python3 front-end suites are known to fail at the parse stage (`gen-python3-test.rkt`, `lower-python-test.rkt`) because of the `single_input` compound-statement limitation. Documented, not to be "fixed" as part of unrelated work.
- No false greens. Never suppress a failing check to manufacture green.
- No silent skips. A test that skips most of its assertions is a bug in the test, not a pass.
- One test per behavior. Assert the typed op (`db<User>.read`), not just that a row is non-empty.
- Property tests for the effect algebra: associativity of join, monotonicity of the fixpoint, subtyping laws.

## Self-improving guidelines

- When you solve a non-obvious error, save the pitfall to memory or a skill immediately, with the exact symptom and the fix.
- When a skill is stale, patch it now, not after the next time it misleads you.
- Drive the unknown count down and keep the number in the baseline. An unknown leaf is an open obligation, not a fact of life.
- Re-run, do not re-trust. A repeated note that claims verification without captured output is a signal to run the gate again yourself.
- The system that checks code is itself checked. That reflexivity is the point, so keep the checker's own code under the same contract.

## Coding style

- Use `if`/`else`, not `when`, for a single-armed check.
- `printf` and `format` take `~a` and `~s`, never `%s`.
- Any function that harvests a name out of UIR returns a plain string on every branch. Unwrap a `uir-string` field with `uir-string-value`; audit the fallthrough branches of `cond`s over `uir-symbol?`/`uir-var?`/`uir-get?` mixes.
- No library-name literals in engine code. Comments may name a library; classification logic may not.
- Make structs `#:transparent` where they serialize, and treat a field-count change as a schema change with a migration.

## The prompt template

Paste this block at the start of every session.

---

You are working on racklr and effervesce. Racklr lowers Python3, TypeScript, TSX, JavaScript, JSX, Clojure, and Koka source into one UIR. Effervesce infers a typed effect row (db, io, network, process, cache, llm, time, email) for every function and method, grounded in the concrete call chain, and checks that graph against declared capability contracts.

Obligations, in force for every change:

- Every function and method carries an effect row. Unknown is a tracked state with an elimination obligation, never a silent default.
- Effect keys come from the call chain via receiver-type dataflow, class to module to domain, never from a method-name table in engine code.
- Library-specific tokens live only in `racklr/effects/library-tokens.rkt` and `racklr/effects/imports/library-config.rkt`. Engine code carries none.
- Every effect edge carries provenance you can point at.
- UIR is the single source of truth.

Workflow:

1. Confirm the baseline is green before you touch anything.
2. State the goal, then the smallest change that meets it.
3. Change one thing, run the gate, read the result, then move on.
4. Report only what you observed. Say plainly when you could not verify.

Modularity:

- No `.rkt` file over 400 lines. Split by responsibility.
- A new front end is grammar plus lowering hook plus lexicon data. It must not edit core modules.
- Keep public call shapes stable. Add injectable config as an optional argument, not a breaking change.

Test gate:

- `raco setup racklr`
- `raco setup effervesce`
- Racklr suites: `cd racklr-test && racket <suite>-test.rkt`
- Effervesce suites: `cd effervesce && racket <suite>-test.rkt` (must run from `effervesce/`)
- Baseline: `racket effervesce/tools/type-graph.rkt django/django/core` must print `112 parsed, 0 failed, db 52 files, network 2, unknown 83, inherits 126`.
- `raco test` is unreliable. Zero exit is not success. Grep output for FAILURE and ERROR and read the real exit code.

Coding style:

- `if`/`else`, not `when`, for a single-armed check.
- `printf` and `format` take `~a` and `~s`, never `%s`.
- Any function that harvests a name out of UIR returns a plain string on every branch. Unwrap a `uir-string` field with `uir-string-value`.

Self-improvement:

- When you solve a non-obvious error, save the pitfall to memory or a skill.
- When a skill is stale, patch it immediately.
- Do not trust a prior note that claims a gate passed. Re-run it and capture the output.

Commit:

- Do not commit. Commits are the operator's job. Leave the working tree uncommitted and say what changed.

Now begin.
