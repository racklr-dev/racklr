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
    "You CORRECT the side-effect domain set of Python functions for a static effect system. "
    "Each input gives a function name and its current inferred domains (which may contain "
    "false positives from a coarse name table, e.g. `get`/`set`/`pop` misread as db on a dict, "
    "or a runtime-dispatched call lumped as dynamic). "
    "Return the TRUE complete effect set. "
    "Each domain is one of: pure, fs, stdio, io, http, socket, websocket, sql, nosql, vector, db, cache, llm, email, clock, sleep, time, process, env, rng, queue, callback. "
    "pure = no side effect (string parsing, hashing, encoding, in-memory data). "
    "fs = filesystem read/write (open, os file ops, pathlib, zip). stdio = console/stream/logging output. io = general I/O ONLY when fs vs stdio is unclear. "
    "http = HTTP client/server. socket = raw socket/ssl. websocket = websocket. "
    "sql = relational DB (sqlalchemy, psycopg, Django ORM). nosql = document/key-value DB (pymongo, motor). vector = vector store (chromadb, pinecone, faiss). db = database ONLY when sql/nosql/vector is unclear. "
    "cache = redis/memcache/cache store. llm = model inference (openai/anthropic/langchain). email = SMTP send. "
    "clock = read the clock. sleep = delay. time = clock/sleep/timer ONLY when clock vs sleep is unclear. "
    "process = subprocess, os exec, signal. env = environment WRITE (os.putenv, dotenv; reads are pure). rng = randomness (random, secrets, uuid4). queue = message queue (kafka, celery, SQS/SNS). "
    "callback = a polymorphic callback whose effect is the CALLER's (use only when the "
    "effect genuinely depends on what the caller passes). "
    "pure means the EMPTY set. Do not use `dynamic`: decompose it into concrete effects "
    "or `callback`. "
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
            import time
            time.sleep(1.0)
    raise last

out = []
for start in range(0, len(items), 3):
    out.extend(classify(items[start:start + 3]))
json.dump(out, sys.stdout, indent=2)
PY
python3 "$TMP" "$KEY" "$MODEL"
