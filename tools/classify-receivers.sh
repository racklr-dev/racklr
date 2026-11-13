#!/usr/bin/env bash
# classify-receivers.sh — module/domain classifier for the racklr reducer's
# receiver-type stage (return-type-projection, self-attr, unbound-binding).
#
# stdin : a JSON array of unresolved method call sites:
#   [{"site": "proxy_manager.connection_from_url", "receiver": "proxy_manager",
#     "method": "connection_from_url", "function": "HTTPAdapter.get_connection",
#     "hint": "urllib3", "sites": 3}, ...]
# stdout: a JSON array in the same order:
#   [{"module": "urllib3", "domain": "network", "confidence": 0.93,
#     "reason": "connection pool"}, ...]
#
# `module` is the Python package that provides the receiver's TYPE (the module
# to tag with an effect rule), not the method. Domains: pure, io, network, db,
# time, process, dynamic.
#
# Key: DEEPSEEK_API_KEY / CLASSIFY_KEY env only (see classify-effects.sh).
set -euo pipefail

KEY="${CLASSIFY_KEY:-${DEEPSEEK_API_KEY:-}}"
if [ -z "$KEY" ]; then
  echo "classify-receivers.sh: set DEEPSEEK_API_KEY (or CLASSIFY_KEY); the key is never hardcoded." >&2
  exit 2
fi
MODEL="${CLASSIFY_MODEL:-deepseek-flash}"

TMP=$(mktemp)
trap 'rm -f "$TMP"' EXIT
cat > "$TMP" <<'PY'
import json, sys, urllib.request

key, model = sys.argv[1], sys.argv[2]
items = json.load(sys.stdin)
if not isinstance(items, list):
    raise SystemExit("stdin must be a JSON array")

system = (
    "You classify unresolved Python method call sites for a static effect system. "
    "For each site, identify the MODULE (Python package) that provides the receiver's "
    "TYPE — the module to tag with an effect rule — and the effect domain of the method call. "
    "Domain is exactly one of: pure, io, network, db, time, process, dynamic. "
    "pure = no side effect (string parsing, hashing, encoding, in-memory data structure). "
    "io = file or stdio read/write. network = HTTP, socket, remote call. "
    "db = database query/execute. time = clock read, sleep, timer. "
    "process = subprocess, os exec, signal. dynamic = runtime-dispatched callback. "
    "Reply with ONLY a JSON array, one object per input item in the same order: "
    '{"module": "...", "domain": "...", "confidence": 0.0, "reason": "one short clause"}. '
    "No prose, no markdown fences."
)

def classify(items):
    req = {
        "model": model,
        "messages": [
            {"role": "system", "content": system},
            {"role": "user", "content": json.dumps(items)},
        ],
        "temperature": 0,
        "max_tokens": 16000,
    }
    data = json.dumps(req).encode()
    r = urllib.request.Request(
        "https://api.deepseek.com/v1/chat/completions",
        data=data,
        headers={"Authorization": f"Bearer {key}", "Content-Type": "application/json"},
    )
    body = urllib.request.urlopen(r, timeout=300).read()
    msg = json.loads(body)["choices"][0]["message"]["content"]
    msg = msg.strip()
    if msg.startswith("```"):
        msg = msg.split("\n", 1)[1]
        if msg.rstrip().endswith("```"):
            msg = msg.rstrip()[:-3]
        msg = msg.strip()
    return json.loads(msg)

# The model reasons per item, so batch small and concatenate in order.
out = []
for start in range(0, len(items), 10):
    out.extend(classify(items[start:start + 10]))
json.dump(out, sys.stdout, indent=2)
PY
python3 "$TMP" "$KEY" "$MODEL"
