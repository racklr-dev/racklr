# FastAPI sample: the full-stack template backend

This directory vendors a real FastAPI service — the backend of
[fastapi/full-stack-fastapi-template][upstream], the official FastAPI
full-stack template — so the Effervesce bridge checker can be run over a
substantial, unmodified host-code sample instead of a toy file.

The app is a FastAPI + SQLModel service: JWT login, users/items/private routers,
startup database probes, alembic migrations, and the email/password-reset
utilities. `racklr-test/bridge-fastapi-sample-test.rkt` and
`tools/bridge-python.rkt` run the bridge over it.

## Provenance

- Upstream: <https://github.com/fastapi/full-stack-fastapi-template>
- Release: `0.10.0`, commit `d40de23896d27d15c17a7bf9649123fd167a0aa8`
- Vendored verbatim: `backend/app/` (27 Python files / 1,611 lines, plus the
  alembic template and the email templates it reads), the upstream root
  `LICENSE` (MIT, © 2019 Sebastián Ramírez), and `backend/pyproject.toml` kept
  as `upstream-pyproject.toml`.
- Not vendored: `backend/tests/`, `backend/scripts/`, `frontend/`, the
  deployment (compose/CI) files, and `backend/alembic.ini`. The app tree does
  not need them to parse, lower, or infer.
- The release is pinned at `0.10.0` because `0.11.0` moved the backend to
  Python 3.14 and uses PEP 758 `except A, B:` clauses, which the tracked
  `grammars/python3/` front end does not parse yet. This is source material,
  not a runnable app: no Python dependencies are installed.

## What the bridge checks

`app.fizz` describes the backend as one `Api` component that needs three
capabilities: `Db`, `Io`, and `Time`. `tools/bridge-python.rkt` parses and
lowers every file with the tracked grammar, infers the effect row of every
function, and checks each row against the component's needs through the
interpretation table `Db -> db`, `Io -> io`, `Time -> time`.

Latest run:

```text
bridge-python: fastapi/app — 27 files (27 ok, 0 failed)
functions: 70 (53 with an unresolved call)
effects: db=23 io=8 time=3 unknown=53
spec: 1 component Api
type/policy/model: 0 findings
bridge: 0 violations, 20 unverified file findings
...
ok
```

Every effect the inference could ground is covered by the spec (`0`
violations). 53 of the 70 functions call something the inference cannot resolve
to a row — libraries such as `jwt`, `pwdlib`, `jinja2`, `emails`, `sentry_sdk`,
and FastAPI's `Depends` — so the bridge reports those files as
`bridge-unverified` rather than counting them as covered. That is the tracked
`unknown` state (see `AGENTS.md`), not a pass.

## Run it

From the repository root:

```sh
./fastapi/check.sh

# or directly
racket tools/bridge-python.rkt fastapi/app fastapi/app.fizz

# the test suite carries the same assertions
cd racklr-test && racket bridge-fastapi-sample-test.rkt
```

`tools/bridge-python.rkt` runs the bridge once per file rather than merging the
tree into one program: five alembic migrations define `upgrade`/`downgrade`,
and `create_user`/`create_item`/`main`/`init` repeat, so a merged program would
key the rows by name and silently drop the overwritten definitions' effects.
The per-file rows are still unioned for the reported effect histogram.

[upstream]: https://github.com/fastapi/full-stack-fastapi-template
