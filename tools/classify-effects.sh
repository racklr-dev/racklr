#!/usr/bin/env bash
# classify-effects.sh — one-shot effect-domain classifier for the racklr reducer.
#
# stdin : a JSON array of unknown call sites:
#   [{"name": "urlparse", "import": "from urllib.parse import urlparse", "sites": 20}, ...]
# stdout: a JSON array in the same order:
#   [{"name": "urlparse", "domain": "pure", "confidence": 0.98, "reason": "URL string parsing"}, ...]
#
# Domain vocabulary: pure, io, network, db, time, process, dynamic.
# Backend: DeepSeek (OpenAI-completions wire). The key is read ONLY from the
# environment (DEEPSEEK_API_KEY, or CLASSIFY_KEY to override); it is never
# hardcoded and never read from a committed file. Override the model with
# CLASSIFY_MODEL.
#
# Exit 0 on a parsed JSON answer; non-zero on any transport/parse failure.
set -euo pipefail

# The key arrives ONLY from the environment, never from this file or any
# committed path. CLASSIFY_KEY is the script-specific override; DEEPSEEK_API_KEY
# is the standard name the workstation config already documents.
KEY="${CLASSIFY_KEY:-${DEEPSEEK_API_KEY:-}}"
if [ -z "$KEY" ]; then
  echo "classify-effects.sh: set DEEPSEEK_API_KEY (or CLASSIFY_KEY) in the environment; the key is never hardcoded." >&2
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
    "You classify Python call sites by their side-effect domain for a static effect system. "
    "Domain is exactly one of: pure, io, network, db, time, process, dynamic. "
    "pure = no side effect (string parsing, hashing, encoding, type casting, dataclass/value construction). "
    "io = file or stdio read/write/seek. network = HTTP, socket, remote call. "
    "db = database query/execute/commit. time = clock read, sleep, timer. "
    "process = subprocess, os exec, signal, atexit. "
    "dynamic = a runtime-dispatched callback whose effect cannot be named statically. "
    "Reply with ONLY a JSON array, one object per input item in the same order: "
    '{"name": "...", "domain": "...", "confidence": 0.0, "reason": "one short clause"}. '
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
