# Effect vocabulary widening + lemmatization expansion

Status: APPROVED (iteration 1). One command `tools/formalize.sh <dir>` turns a
Python package into a `.fizz` contract of its effect lemmas. This plan widens
the effect vocabulary that command can speak, validates it on `langchain_core`,
then extends the same loop to TypeScript/JavaScript. Tracked on the nodecode
taskboard under one epic with V1a/V1b/V1c children.

## Why widen first

The vocabulary is the contract's alphabet. Three places disagree today:

- **Engine tokens** (`racklr/effects/library-tokens.rkt`): the meta categories
  are `llm, db, network, cache, io, process, time, email` (+ `pure`/`dynamic`/
  `unknown`). The JS lexicon (`racklr/effects/lexicons/javascript.rkt`) has
  only `io, network, process, cache, time`.
- **LLM correction prompt** (`tools/classify-functions.sh`): `pure, io,
  network, db, time, process, callback`. It is MISSING `cache`, `llm`, `email`,
  so those effects cannot survive the correction pass and get coerced or
  dropped.
- **Contract emitter** (`tools/emit-fizz.rkt`): generic, one capability per
  domain. No subtyping, so a fine domain cannot satisfy a coarse `needs`.

Widening before picking libraries means the test libraries actually exercise
the new alphabet instead of being squashed into it.

## Vocabulary design (umbrella + leaves)

The algebra already has subtyping (`effect-subtype?`, proven transitive in the
Coq kernel). The design is two tiers: keep the coarse categories as umbrellas,
add fine leaves that SUBTYPE them, so `http <: network`, `sql <: db`, `fs <: io`.
A component declaring `needs network` accepts an `http` implementation; a
component declaring `needs http` is precise. The operator asked for a ROBUST
vocabulary: v1 implements the full table, not a minimal subset.

| Leaf | Umbrella | Python roots (examples) | JS/TS roots (examples) |
|---|---|---|---|
| `pure` | — | (existing) | (existing) |
| `io` | — | print, input, stdio | console, process.stdout |
| `fs` | io | open, pathlib, os.read/write, shutil | node:fs, fs.readFile/writeFile |
| `network` | — | (umbrella) | (umbrella) |
| `http` | network | httpx, requests, aiohttp, urllib | fetch, axios, XMLHttpRequest |
| `socket` | network | socket, ssl | net, tls, dgram |
| `websocket` | network | websockets, websocket | WebSocket |
| `db` | — | (umbrella) | (umbrella) |
| `sql` | db | sqlalchemy, psycopg, asyncpg, sqlmodel | pg, mysql2, better-sqlite3 |
| `nosql` | db | pymongo, motor | mongodb |
| `vector` | db | chromadb, pinecone, faiss, pgvector | — |
| `cache` | — | redis, memcache, cachetools | ioredis, localStorage |
| `llm` | — | openai, anthropic, cohere, ollama, groq, langchain, llamaindex, agno, langgraph | openai, anthropic SDKs |
| `email` | — | smtplib, sendgrid | nodemailer |
| `time` | — | time, datetime | Date, setTimeout |
| `sleep` | time | time.sleep, asyncio.sleep | setTimeout, sleep |
| `process` | — | subprocess, os.system, signal | child_process |
| `env` | — | os.environ, os.getenv, dotenv | process.env |
| `rng` | — | random, secrets, uuid | Math.random, crypto.randomUUID |
| `crypto` | — | hashlib, hmac, cryptography, paramiko | node:crypto |
| `queue` | — | boto3 SQS/SNS, kafka, celery, kombu, pika | amqplib, kafkajs |
| `gpu` | — | torch.cuda, jax, cupy | webgpu |

## Where the vocabulary lives (all six must change together)

1. `racklr/effects/library-tokens.rkt` — add roots/methods for the new leaves,
   split existing (`io` -> `fs`, `network` -> `http`/`socket`, `db` ->
   `sql`/`nosql`/`vector`, `time` -> `sleep`).
2. `racklr/effects/lexicons/javascript.rkt` — add `llm`, `db`, `email` and the
   new leaves for TS/JS.
3. `tools/classify-functions.sh` — expand the prompt's vocabulary to match, and
   spell each domain's boundary (the `racklr-formalize-loop` lesson: db only
   for real DB ops, dynamic decomposed, never a coarse fallback).
4. `tools/emit-fizz.rkt` — emit the umbrella/subtype relation so a fine
   capability satisfies a coarse `needs`.
5. effervesce check bridge + capability vocabulary — accept the new domains;
   subtyping across the bridge.
6. `racklr/effects/algebra/subtyping.rkt` + property tests — declare the order
   and prove it non-vacuously (`effects-algebra-subtyping-handlers-test`,
   `effects-guard-vocabulary-test`).

Every change DRIFTs the baseline; re-pin deliberately with the snapshot diff as
review, never silently.

## Test libraries

1. **langchain_core — PRIMARY (confirmed).** Pin a fixed commit (exact tag
   recorded when fetched). Exercised: `llm` (model calls), `http` (provider
   REST), `vector`/`db` (vector stores), `fs` (loaders), `cache`
   (in-memory/redis), `env` (config), `time`/`sleep` (rate limiting), `rng`.
2. **boto3 — s3 + dynamodb + sqs/sns clients (secondary hardening).**
   Exercised: `http` (AWS API), `queue` (SQS/SNS), `nosql`/`db` (DynamoDB),
   `fs` (S3 object I/O).
3. **redis-py (secondary hardening).** Exercised: `cache` + `socket` + `time`
   (TTL), and the cache-vs-network distinction.

STRETCH (after the leaves land): **paramiko** (`crypto`, `socket`, `fs`,
`process`), **pandas** (`fs` read/write + pure precision, and it stresses the
C-extension wall for the 0-unknown gate).

For each: `tools/formalize.sh <dir>` -> census 0 unknown -> `emit-fizz` -> review
the contract for false positives and misses, then tighten the classifier
prompt's domain boundaries and re-run. The libraries are the spec; the
vocabulary is wrong until the contracts read correctly.

## TS/JS lemmatize (folded into V1a)

Front ends (`lower-javascript`, `lower-typescript`, `lower-tsx`), the JS
lexicon, and the absorb/lower/emit battery (`tools/js-ts-corpus.rkt`, express +
axios) already exist. What is missing is the INFERENCE side of the loop:
`tools/provenance-corpus.rkt` / `fx-rows.rkt` scan only `.py` and classify with
the Python default lexicon. The host-dispatch lands in V1a so the vocabulary
ships for both hosts at once:

1. Generalize `fx-rows.rkt` / `provenance-corpus.rkt` to dispatch on host
   (`.py` -> Python lexicon; `.ts`/`.tsx`/`.js`/`.mjs` -> JS lexicon through the
   existing TS/JS front ends).
2. A TS/JS classifier prompt — same vocabulary, host boundaries (fetch/axios =
   network, node:fs = io, process.env = env, Date/setTimeout = time,
   Math.random = rng, node:crypto = crypto).
3. `formalize.sh`/`formalize.py` dispatch on host and write a per-host DATA
   json (the `library-reduced-function-effects.json` shape).
4. Test libraries (V1c): **axios** (network), **express** (network/io/process),
   **ioredis** (cache), **pg** (sql).

## Phasing and definition of done

- **V1a — align the vocabulary (both hosts).** New leaves in engine tokens +
  JS lexicon + LLM prompt (Python and JS) + emit-fizz + subtyping order, AND
  the TS/JS host-dispatch in `fx-rows`/`provenance-corpus`/`formalize.sh` so the
  vocabulary lands for Python and TS/JS at once. DOD: a small multi-domain
  fixture (langchain-shaped, Python) plus an axios-shaped JS fixture classify
  with the new leaves, census 0 unknown, subtyping property tests green.
- **V1b — validate on langchain_core.** `tools/formalize.sh` over a pinned
  `langchain_core` checkout -> census 0 unknown -> `.fizz`. DOD: the contract
  reads correctly (no false positives/misses) and every exercised leaf is
  backed by a real provenance site.
- **V1c — TS/JS libraries.** axios + an express subset formalized to `.fizz`.
  Secondary (same method): boto3 s3+dynamodb+sqs/sns and redis-py to harden
  queue/nosql/cache.

## Decisions (confirmed by operator)

1. Umbrella + subtyping (`http <: network`) — CONFIRMED.
2. Python test library: `langchain_core`, pinned at a fixed commit (exact tag
   recorded when fetched).
3. `fs` as a distinct leaf — CONFIRMED, and the vocabulary must be robust
   enough to cover most cases with precision (v1 = the full table above).
4. Order: vocab first, with the TS/JS host-dispatch folded INTO V1a so the
   vocabulary lands for both hosts at once; then langchain_core; then TS/JS
   libraries. Work tracked on the nodecode taskboard.
