#!/usr/bin/env bash
# Run the Effervesce bridge over the vendored FastAPI sample.
#
#   ./fastapi/check.sh
#
# Exits 0 only when every vendored file parses and lowers with the tracked
# Python3 grammar and the bridge finds no effect the spec does not declare.
set -euo pipefail

cd "$(dirname "$0")/.."

# Resolve racklr/ and effervesce/ from this checkout first, so the run does not
# depend on a linked Racket package pointing at another clone.
export PLTCOLLECTS="$PWD:${PLTCOLLECTS:-}"

racket tools/bridge-python.rkt fastapi/app fastapi/app.fizz
