#!/usr/bin/env bash
# classify-dynamic.sh — decompose the `dynamic` deferral into sub-effects.
# For a function currently carrying `dynamic`, name the CONCRETE effect(s) the
# runtime-dispatched call actually performs, or `callback` when it is a
# polymorphic callback whose effect is the CALLER's (a type variable).
#
# stdin : [{"name": "httpx._client::AsyncClient._send_handling_auth",
#           "domains": ["dynamic", "network"], "sites": []}, ...]
# stdout: same order, [{"name": ..., "domains": [...], "confidence": ...,
#           "reason": ...}]
#
# Key: DEEPSEEK_API_KEY / CLASSIFY_KEY env only.
set -euo pipefail

KEY="${CLASSIFY_KEY:-${DEEPSEEK_API_KEY:-}}"
if [ -z "$KEY" ]; then
  echo "classify-dynamic.sh: set DEEPSEEK_API_KEY (or CLASSIFY_KEY); the key is never hardcoded." >&2
  exit 2
fi
MODEL="${CLASSIFY_MODEL:-deepseek-flash}"

TMP=$(mktemp)
trap 'rm -f "$TMP"' EXIT
cat > "$TMP" <<'PY'
import json, sys, time, urllib.request

key, model = sys.argv[1], sys.argv[2]
items = json.load(sys.stdin)
if not isinstance(items, list):
    raise SystemExit("stdin must be a JSON array")

system = (
    "You decompose the `dynamic` effect of Python functions into sub-effects. "
    "Each input is a function with its current effect domains (which include `dynamic` "
    "for a runtime-dispatched call). Name the CONCRETE effect(s) the runtime-dispatched "
    "call actually performs. Use only: pure, io, network, db, time, process, callback. "
    "callback means a polymorphic callback whose effect is the CALLER's (a type variable) — "
    "use it ONLY when the effect genuinely depends on what the caller passes. "
    "Remove `dynamic`: replace it with the concrete effect(s) or `callback`. "
    "Keep the function's other, already-correct domains. "
    "Reply with ONLY a JSON array, one object per input item in the same order: "
    '{"name": "...", "domains": ["...", "..."], "confidence": 0.0, "reason": "one short clause"}. '
    "No prose, no markdown fences."
)

def classify(chunk):
    last = None
    for attempt in range(3):
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
        msg = json.loads(body)["choices"][0]["message"]["content"] or ""
        msg = msg.strip()
        if msg.startswith("```"):
            msg = msg.split("\n", 1)[1]
            if msg.rstrip().endswith("```"):
                msg = msg.rstrip()[:-3]
            msg = msg.strip()
        try:
            return json.loads(msg)
        except Exception as e:
            last = e
            time.sleep(1.0)
    raise last

out = []
for start in range(0, len(items), 3):
    out.extend(classify(items[start:start + 3]))
json.dump(out, sys.stdout, indent=2)
PY
python3 "$TMP" "$KEY" "$MODEL"
