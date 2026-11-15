#!/usr/bin/env bash
# formalize.sh <python-package-dir> — one command, to fixpoint:
# infer every leaf -> LLM corrects the complete effect set -> write DATA ->
# re-census (unknown must be 0) -> emit the .fizz contract.
set -euo pipefail

DIR="${1:?usage: formalize.sh <python-package-dir>}"
HERE="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(cd "$HERE/.." && pwd)"
export PLTCOLLECTS="$ROOT:$ROOT/effervesce:"

python3 "$HERE/formalize.py" "$DIR"
racket "$HERE/unknown-census.rkt" "$DIR" --tsv /tmp/_formalize.tsv >/dev/null 2>&1
racket "$HERE/unknown-census.rkt" "$DIR" 2>/dev/null | grep -E 'unknown functions|unknown files' || true
echo '--- fizz ---'
racket "$HERE/emit-fizz.rkt" "$DIR" 2>/dev/null | sed -n '/^system /,$p' > /tmp/_f.fizz
echo '--- gherkin ---'
racket "$HERE/emit-gherkin.rkt" /tmp/_f.fizz 2>/dev/null | grep -vE 'WARNING|accumulator'
