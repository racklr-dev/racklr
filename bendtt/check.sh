#!/usr/bin/env bash
# Verify the Koka BendTT kernel against the paper's verdicts.
#
# The ticket's acceptance command is `koka -e bendtt/meta.kk`; this script
# runs it from the repository root and asserts every expected line, so the
# check can be run in CI or by hand. Requires Koka 3.2.9 on PATH (or $KOKA).
set -euo pipefail

cd "$(dirname "$0")/.."

KOKA=${KOKA:-koka}
if ! command -v "$KOKA" >/dev/null 2>&1; then
  echo "bendtt: koka not found (set KOKA=/path/to/koka)" >&2
  exit 2
fi

# Koka prints compile progress on stdout; keep it for failures only.
out=$("$KOKA" -e bendtt/meta.kk 2>&1) || {
  echo "bendtt: koka failed" >&2
  printf '%s\n' "$out" >&2
  exit 1
}

fail=0
check() {
  if ! printf '%s\n' "$out" | grep -qxF "$1"; then
    echo "bendtt: missing expected line: $1" >&2
    fail=1
  fi
}

# 1. the paper's verdicts (live check)
check "R: accepted"
check "add: accepted"
check "ack: accepted"
check "twice: accepted"
check "app: affinity-rejected"
check "map: affinity-rejected"
check "two: affinity-rejected"
check "loop: descent-rejected"
# 2. conversion: a lambda+ converts as a lambda, not as a lambda of quantity 0
check "conv lam+ ~ lam: convertible"
check "conv lam+ ~ lam-: distinct"
check "conv lam ~ lam: convertible"
check "conv beta: convertible"
check "conv mixed: convertible"
check "conv def unfolds: convertible"
# 3. the untyped measure ignores dead q=0 parts
check "measure ann (type weighs 0): ok"
check "measure let-0 (value weighs 0): ok"
check "measure app-0 (arg weighs 0): ok"
check "measure tup-0 (first field weighs 0): ok"
check "measure let-1 (live value): ok"
check "measure fuel = size (add): ok"
check "measure fuel = size (ack): ok"
# 4. summary
check "verdicts 8/8, conversion 6/6, measure 7/7"
check "ALL CHECKS PASS"

if [ "$fail" -ne 0 ]; then
  echo "bendtt: FAILED (see lines above); full output:" >&2
  printf '%s\n' "$out" >&2
  exit 1
fi

# The kernel's non-console functions must all be total: Koka's interface for
# the module must not contain a divergence effect.
kki=$( { for d in . .koka "$HOME/.koka"; do
           [ -d "$d" ] && find "$d" -name 'bendtt_kernel.kki'
         done; } 2>/dev/null | xargs -r ls -t 2>/dev/null | head -1 || true)
if [ -n "$kki" ] && grep -q 'div' "$kki"; then
  echo "bendtt: FAILED: a kernel function was inferred with the div effect" >&2
  grep 'div' "$kki" >&2
  exit 1
fi
if [ -z "$kki" ]; then
  echo "bendtt: note: inferred interface not found, skipped the div scan" >&2
fi

echo "bendtt: OK"
