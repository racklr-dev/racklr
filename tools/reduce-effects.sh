#!/usr/bin/env bash
# reduce-effects.sh <python-package-dir> — run the reducer loop's discovery half:
# census the package's unknown bare-call names, import-join them, classify them
# through classify-effects.sh, and print a reviewable DATA diff.
#
# Requires PLTCOLLECTS to resolve the racklr + effervesce collections (set here
# from the repo layout), and DEEPSEEK_API_KEY/CLASSIFY_KEY in the environment
# for the classifier stage.
set -euo pipefail

DIR="${1:?usage: reduce-effects.sh <python-package-dir>}"
HERE="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(cd "$HERE/.." && pwd)"
export PLTCOLLECTS="$ROOT:$ROOT/effervesce:"

TSV=$(mktemp)
trap 'rm -f "$TSV"' EXIT

# The census prints parser-generation warnings to stderr; keep stdout clean.
racket "$HERE/unknown-census.rkt" "$DIR" --tsv "$TSV" >/dev/null 2>&1
python3 "$HERE/reduce-effects.py" "$DIR" "$TSV"
