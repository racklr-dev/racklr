#!/usr/bin/env bash
# classify-functions.sh — function-level effect-domain classifier for the
# reducer's declared-function-effects stage (the residual the receiver-type
# engine cannot resolve).
#
# stdin : a JSON array of functions with their unresolved call sites:
#   [{"name": "requests.adapters::HTTPAdapter.send",
#     "sites": ["conn.urlopen", "resp.read"], "sites_n": 2}, ...]
# stdout: a JSON array in the same order:
#   [{"name": "...", "domain": "network", "confidence": 0.9, "reason": "..."}, ...]
#
# Domain vocabulary: pure, io, network, db, time, process, dynamic.
# Key: DEEPSEEK_API_KEY / CLASSIFY_KEY env only (see classify-effects.sh).
set -euo pipefail

KEY="${CLASSIFY_KEY:-${DEEPSEEK_API_KEY:-}}"
if [ -z "$KEY" ]; then
  echo "classify-functions.sh: set DEEPSEEK_API_KEY (or CLASSIFY_KEY); the key is never hardcoded." >&2
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
    "You classify the side-effect domain of Python functions for a static effect system, "
    "given each function's unresolved call sites. "
    "Domain is exactly one of: pure, io, network, db, time, process, dynamic. "
    "pure = no side effect (string parsing, hashing, encoding, in-memory data). "
    "io = file or stdio read/write. network = HTTP, socket, remote call. "
    "db = database query/execute. time = clock read, sleep, timer. "
    "process = subprocess, os exec, signal. "
    "dynamic = a runtime-dispatched callback whose effect cannot be named statically. "
    "Reply with ONLY a JSON array, one object per input item in the same order: "
    '{"name": "...", "domain": "...", "confidence": 0.0, "reason": "one short clause"}. '
    "No prose, no markdown fences."
)

def classify(chunk):
    req = {
        "model": model,
        "messages": [
            {"role": "system", "content": system},
            {"role": "user", "content": json.dumps(chunk)},
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

out = []
for start in range(0, len(items), 10):
    out.extend(classify(items[start:start + 10]))
json.dump(out, sys.stdout, indent=2)
PY
python3 "$TMP" "$KEY" "$MODEL"
