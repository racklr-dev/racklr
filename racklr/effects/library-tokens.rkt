#lang racket

;; racklr/effects/library-tokens.rkt
;;
;; The host lexicon's TOKEN TABLES: the concrete call-chain names (module
;; roots, method names, dotted fields, value constructors, receiver types)
;; that map to the meta effect categories (llm / db / network / cache / io /
;; process / time / email), plus the host `dialect` (naming / encoding
;; conventions). This is DATA, kept apart from the classification engine so
;; racklr/effervesce carry no library-specific tokens in code.
;;
;; The shape is host-neutral: effect roots are a domain→roots table ordered by
;; `effect-root-priority`, bare-name and method effects are name→domain
;; tables, the name-only method fallback is a pair of method→domain tables
;; (one gated by a flag), and the conventions travel in `dialect`. The DEFAULT
;; instance is the Python/Django lexicon; supply another host's tokens (see
;; racklr/effects/lexicons/javascript.rkt) and every optional
;; `[tokens default-effect-tokens]` argument classifies with that host.
;;
;; Each table is consulted by `direct-effect-of-callee` / `pure-stdlib-callee?`
;; in effect-roots.rkt and by the pure-name predicates in pure-roots.rkt. The
;; default instance covers the stdlib and the frameworks racklr is exercised
;; against (Django et al.).

(provide
  effect-tokens? effect-tokens
  effect-tokens-effect-roots effect-tokens-effect-root-priority
  effect-tokens-bare-name-effects effect-tokens-method-effects
  effect-tokens-unambiguous-method-effects effect-tokens-ambiguous-method-effects
  effect-tokens-ambiguous-method-effects-enabled?
  effect-tokens-root-method-effects effect-tokens-builtin-type-roots
  effect-tokens-stdlib-module-roots effect-tokens-stdlib-root-effects
  effect-tokens-stdlib-type-methods
  effect-tokens-pure-stdlib-roots effect-tokens-pure-stdlib-methods
  effect-tokens-pure-stdlib-bare-names effect-tokens-pure-op-names
  effect-tokens-pure-constructor-names effect-tokens-pure-builtin-names
  effect-tokens-protocol-method-names effect-tokens-string-method-names
  effect-tokens-pure-method-names
  effect-tokens-module-result-types effect-tokens-receiver-modules
  effect-tokens-module-class-refs effect-tokens-type-result-projections
  effect-tokens-module-element-types
  effect-tokens-stdlib-method-element-types
  effect-tokens-class-attr-element-types
  effect-tokens-class-attr-types
  effect-tokens-dynamic-class-factories
  effect-tokens-dynamic-class-instance-types
  effect-tokens-mapping-value-methods
  effect-tokens-mapping-projection-methods
  effect-tokens-element-identity-bare-names
  effect-tokens-pure-native-builtin-names
  effect-tokens-dialect
  default-effect-tokens
  ;; The builtin(s) whose CALL RESULT is a bound-method alias: `getattr(obj,
  ;; "name", default)` yields obj.name as a VALUE (a bound method when it is a
  ;; method). This is Python-host DATA kept here, not an engine table; a host
  ;; whose lowerer has no such construct leaves this set empty.
  bound-method-factories)

(require racket/hash
         racklr/effects/library-stdlib-types
         racklr/effects/library-pure-names
         racklr/effects/library-racket-tokens
         racklr/effects/library-dialect)

;; The token tables of a host lexicon. Name sets are sets of strings; tables
;; are hashes keyed as documented per field. `dialect` carries the host's
;; naming / encoding conventions (lexicon-types.rkt).
;; Builtin callee names whose result is a bound-method alias (Python's
;; `getattr(obj, "name", default)`). The receiver-type dataflow records such a
;; result as a (receiver-class . method-name) alias so a later bare call of the
;; local resolves as `obj.name(...)`. DATA: a non-Python host supplies an empty
;; set (or its own equivalent) through this same export.
(define bound-method-factories (set "getattr"))

;; The token tables of a host lexicon. Name sets are sets of strings; tables
;; are hashes keyed as documented per field. `dialect` carries the host's
;; naming / encoding conventions (lexicon-types.rkt).
(struct effect-tokens
  (effect-roots effect-root-priority bare-name-effects method-effects
   unambiguous-method-effects ambiguous-method-effects
   ambiguous-method-effects-enabled?
   root-method-effects
   builtin-type-roots stdlib-module-roots
   stdlib-root-effects
   stdlib-type-methods
   pure-stdlib-roots pure-stdlib-methods pure-stdlib-bare-names pure-op-names
   pure-constructor-names pure-builtin-names protocol-method-names
   string-method-names pure-method-names
   module-result-types receiver-modules
   module-class-refs type-result-projections
   module-element-types stdlib-method-element-types class-attr-element-types
   class-attr-types
   dynamic-class-factories
   dynamic-class-instance-types
   mapping-value-methods
   mapping-projection-methods
   element-identity-bare-names
   ;; Host-NATIVE pure procedures (the compiler's own Racket front end, from
   ;; library-racket-tokens.rkt). A dedicated table: it must never feed the
   ;; Python/JS value-constructor table receiver typing reads, so a Racket
   ;; `values` cannot make a Django `.values()` call look like a pure
   ;; constructor. A different host supplies its own native pure names here.
   pure-native-builtin-names
   dialect)
  #:transparent)

(define default-effect-tokens
  (effect-tokens
   ;; ── effect roots: every call rooted at one of these names is the domain ──
   (hash
    'llm (set "llm" "openai" "anthropic" "cohere" "ollama" "groq" "agno"
              "langgraph" "langchain" "llamaindex")
    'sql (set "sql" "sqlalchemy" "sqlmodel" "asyncpg" "psycopg" "sqlite3"
              "transaction")
    'nosql (set "pymongo" "motor")
    'vector (set "chromadb" "pinecone" "faiss" "pgvector")
    'db (set "db" "database")
    'http (set "httpx" "requests" "aiohttp" "urllib" "urllib3" "fetch" "axios"
               "http")
    'socket (set "socket" "ssl")
    'websocket (set "websockets" "websocket")
    'cache (set "redis" "memcache" "cache")
    'env (set "dotenv")
    'rng (set "random" "secrets")
    'queue (set "kafka" "celery" "kombu" "pika" "rabbitmq" "pulsar"))
   ;; Root domains are tried in this order (the strongest root wins).
   '(llm sql nosql vector db http socket websocket cache env rng queue)
   ;; ── bare-name effects: a bare callee name that is an effect on its own ──
   (hash-union
    (hash
    ;; http client libraries called as bare names
    "fetch" 'http
    "axios" 'http
    ;; NOTE: Next.js cache verbs (revalidatePath/revalidate_path/invalidate/
    ;; clearCache/purgeCache) are JS-host names, not Python; they were leaking
    ;; into the Python/Django default and mis-classifying cache off-corpus.
    ;; They belong in the JS lexicon, not here.
    ;; bare builtins: filesystem I/O vs console/logging output, plus framework
    ;; logging / deprecation-warning helpers imported bare
    ;; (`django.utils.log.log_response`).
    "open" 'fs
    "print" 'stdio
    "input" 'stdio
    "warn_about_external_use" 'stdio
    "log_response" 'stdio
    "NamedTemporaryFile" 'fs
    "TemporaryFile" 'fs
    "TemporaryDirectory" 'fs
    "mkstemp" 'fs
    "mkdtemp" 'fs
    ;; netrc (reads ~/.netrc) and bare file-position ops (seek/tell).
    "netrc" 'fs
    "seek" 'fs
    "tell" 'fs
    ;; eval / exec run arbitrary code in-process (the Django shell command
    ;; feeds user input to exec); code execution is the process domain, the
    ;; same bucket as os.system / subprocess. compile is pure (see
    ;; pure-builtin-names).
    "eval" 'process
    "exec" 'process
    ;; asgiref.sync.sync_to_async: runs a sync callable on a thread-pool
    ;; executor (the async bridge), so the scheduling is an io (umbrella) effect.
    "sync_to_async" 'io
    ;; sleep vs clock reads, as bare names (`time.sleep` / `from time import
    ;; sleep`, clock reads `time.time`/`time.monotonic`, Django's now/tz_now).
    "sleep" 'sleep
    "time" 'clock
    "monotonic" 'clock
    "monotonic_ns" 'clock
    "perf_counter" 'clock
    "perf_counter_ns" 'clock
    "process_time" 'clock
    "thread_time" 'clock
    "time_ns" 'clock
    "tz_now" 'clock
    "now" 'clock
    "localtime" 'clock
    "gmtime" 'clock
    ;; environment writes and nondeterminism, as bare names (reads stay pure)
    "putenv" 'env
    "uuid4" 'rng)
    ;; Racket standard-library I/O / time / network names
    ;; (library-racket-tokens.rkt); the shared bare-name table.
    (for/hash ([n (in-set racket-io-builtins)]) (values n 'io))
    (for/hash ([n (in-set racket-time-builtins)]) (values n 'time))
    (for/hash ([n (in-set racket-network-builtins)]) (values n 'network))
    ;; both tables agree where they overlap; keep the first
    #:combine (λ (a b) a))
   ;; ── method effects: a dotted method name that is an effect on its own ──
   ;; file/stream I/O or logging output regardless of receiver; `log` is
   ;; deliberately absent: it collides with `math.log`.
   (hash-union
    (hash
    "open" 'fs "read" 'fs "readline" 'fs "readlines" 'fs "read1" 'fs
    "readinto" 'fs "readinto1" 'fs "write" 'fs "writelines" 'fs "flush" 'fs
    "seek" 'fs "tell" 'fs "truncate" 'fs "fileno" 'fs "debug" 'stdio "info" 'stdio
    "warning" 'stdio "warn" 'stdio "error" 'stdio "critical" 'stdio "exception" 'stdio
    ;; dotted methods that send email
    "send_mail" 'email "send_mass_mail" 'email "mail_admins" 'email
    "mail_managers" 'email
    ;; sleep / clock reads reached as dotted methods (`time.sleep`,
    ;; `datetime.now`, Django `timezone.now`)
    "sleep" 'sleep "time" 'clock "monotonic" 'clock "monotonic_ns" 'clock
    "perf_counter" 'clock "perf_counter_ns" 'clock "process_time" 'clock
    "thread_time" 'clock "time_ns" 'clock "tz_now" 'clock "now" 'clock
    "localtime" 'clock "gmtime" 'clock)
    ;; Racket clock reads, dotted like any other method name
    ;; (library-racket-tokens.rkt).
    (for/hash ([n (in-set racket-time-builtins)]) (values n 'time))
    #:combine (λ (a b) a))
   ;; ── name-only method fallback: unambiguous ORM operations ──
   ;; Strongly-ORM method names: unambiguous QuerySet / Manager / Model DB
   ;; operations that classify name-only even when the receiver cannot be
   ;; resolved (`obj.prefetch_related`, `qs.values_list`).
   (hash
    "prefetch_related" 'sql "select_related" 'sql "values_list" 'sql
    "get_or_create" 'sql "bulk_create" 'sql "refresh_from_db" 'sql
    "in_bulk" 'sql "db_for_read" 'sql "db_for_write" 'sql
    "create_deterministic_function" 'sql)
   ;; ── ambiguous-or-generic method fallback: gated by the flag ──
   ;; Generic-English ORM verbs: `get`/`update`/`filter`/`save`/... collide
   ;; with non-DB receivers, so they classify as db ONLY when the host enables
   ;; this table (the Django-ORM dialect flag, default #t) or with receiver
   ;; evidence, never name-only in a non-django corpus.
   (hash
    "all" 'sql "annotate" 'sql "aggregate" 'sql "alias" 'sql "as_manager" 'sql
    "count" 'sql "dates" 'sql "datetimes" 'sql "db_manager" 'sql "defer" 'sql
    "difference" 'sql "distinct" 'sql "earliest" 'sql "exclude" 'sql
    "exists" 'sql "explain" 'sql "extra" 'sql "filter" 'sql "first" 'sql
    "get" 'sql "intersection" 'sql "iterator" 'sql "last" 'sql "latest" 'sql
    "none" 'sql "only" 'sql "order_by" 'sql "raw" 'sql "reverse" 'sql
    "union" 'sql "using" 'sql "values" 'sql "bulk_update" 'sql "create" 'sql
    "delete" 'sql "update" 'sql "update_or_create" 'sql "save" 'sql
    "execute" 'sql "executemany" 'sql "fetchone" 'sql "fetchall" 'sql
    "fetchmany" 'sql "commit" 'sql "rollback" 'sql "cursor" 'sql
    "create_function" 'sql "create_aggregate" 'sql)
   #t
   ;; ── root-qualified method effects ──
   ;; `root.method` → effect, where the effect depends on the method, not only
   ;; the root (`os.remove` → io, `os.system` → process, `sys.exit` →
   ;; process, `warnings.warn` → io).
   (hash
    'os (hash
         ;; environment write (`os.putenv`); reads (`os.getenv`) stay pure
         "putenv" 'env
         ;; `os.*` filesystem operations
         "remove" 'fs "unlink" 'fs "rename" 'fs "renames" 'fs "replace" 'fs
         "mkdir" 'fs "makedirs" 'fs "rmdir" 'fs "removedirs" 'fs "chdir" 'fs
         "chmod" 'fs "chown" 'fs "utime" 'fs "link" 'fs "symlink" 'fs
         "readlink" 'fs "listdir" 'fs "scandir" 'fs "stat" 'fs "lstat" 'fs
         "fsync" 'fs "fdatasync" 'fs
         ;; `os.*` filesystem metadata reads and fd (stream) operations
         "exists" 'fs "isfile" 'fs "isdir" 'fs "islink" 'fs "lexists" 'fs
         "samefile" 'fs "sameopenfile" 'fs "samestat" 'fs "getsize" 'fs
         "getmtime" 'fs "getctime" 'fs "getatime" 'fs "access" 'fs
         "walk" 'fs "fwalk" 'fs "realpath" 'fs "fstat" 'fs "fstatvfs" 'fs
         "statvfs" 'fs "getcwd" 'fs "getcwdb" 'fs
         "close" 'fs "fdopen" 'fs "open" 'fs "read" 'fs "write" 'fs
         "dup" 'fs "dup2" 'fs "dup3" 'fs "lseek" 'fs "ftruncate" 'fs
         "fchmod" 'fs "fchown" 'fs "isatty" 'fs "pipe" 'fs "pipe2" 'fs
         "mkfifo" 'fs
         ;; `os.*` process-spawning / process-control operations
         "system" 'process "popen" 'process "popen2" 'process "popen3" 'process
         "popen4" 'process "spawnl" 'process "spawnle" 'process "spawnlp" 'process
         "spawnlpe" 'process "spawnv" 'process "spawnve" 'process "spawnvp" 'process
         "spawnvpe" 'process "execl" 'process "execle" 'process "execlp" 'process
         "execlpe" 'process "execv" 'process "execve" 'process "execvp" 'process
         "execvpe" 'process "fork" 'process "forkpty" 'process "kill" 'process
         "killpg" 'process "startfile" 'process "posix_spawn" 'process
         "posix_spawnp" 'process "umask" 'process "_exit" 'process "abort" 'process)
    'sys (hash "exit" 'process)
    'warnings (hash "warn" 'stdio)
    'glob (hash "glob" 'fs "iglob" 'fs)
    'traceback (hash "print_exc" 'stdio "print_exception" 'stdio "print_last" 'stdio)
    'mail (hash "send" 'email)
    ;; django.utils.timezone clock reads (make_aware/is_aware are pure datetime
    ;; arithmetic and live in pure-stdlib-methods below, not here)
    'timezone (hash "now" 'clock "localtime" 'clock))
   ;; ── roots whose method namespace makes the name-only fallback unsafe ──
   ;; builtin type roots whose method namespace collides with the fallback
   (set "dict" "list" "set" "str" "int" "float" "complex" "bool" "bytes"
        "bytearray" "tuple" "frozenset" "type" "object" "range" "slice"
        "memoryview")
   ;; stdlib module roots whose attribute namespace collides with the fallback
   ;; (`os.path.exists`, `pathlib.Path.exists`)
   (set "os" "sys" "io" "time" "re" "json" "pathlib" "logging" "email"
        "warnings" "traceback" "inspect" "random" "uuid" "importlib" "pickle"
        "shelve" "sqlite3" "csv" "glob" "fnmatch" "tarfile" "zipfile" "gzip"
        "bz2" "lzma" "codecs" "subprocess" "multiprocessing" "shutil" "tempfile"
        "fileinput" "smtplib" "http" "socket" "urllib3" "math" "cmath"
        "itertools" "functools" "operator" "collections" "string" "textwrap"
        "statistics" "types" "typing" "dataclasses" "enum" "abc" "copy" "decimal"
        "fractions" "ipaddress" "struct" "array" "bisect" "heapq" "hashlib" "hmac"
        "base64" "binascii" "unicodedata" "calendar" "keyword" "datetime")
   ;; ── whole-root effects (unambiguous) ──
   (hash-union
    (hash 'subprocess 'process
          'multiprocessing 'process
          'shutil 'fs
          'tempfile 'fs
          'fileinput 'fs
          'smtplib 'email
          'http 'http
          'socket 'socket
          'urllib3 'http)
    ;; Racket process spawn/control names, e.g. (system "make") / (exit 0).
    racket-process-root-effects
    ;; both tables say 'process where they overlap (e.g. 'subprocess)
    #:combine (λ (a b) a))
   stdlib-type-methods-data
   ;; ── pure name tables (data: library-pure-names.rkt) ──
   pure-stdlib-roots-data
   pure-stdlib-methods-data
   pure-stdlib-bare-names-data
   pure-op-names-data
   pure-constructor-names-data
   pure-builtin-names-data
   protocol-method-names-data
   string-method-names-data
   pure-method-names-data
   ;; ── module result types: imported function → its result receiver TYPE ──
   stdlib-module-result-types-data
   ;; ── external module receivers tracked by receiver-type dataflow ──
   ;; a local / param / self-attr bound to one of these modules resolves its
   ;; method calls to the module's effect domain (the memcache client
   ;; libraries: pylibmc, pymemcache, memcache, redis). Mirrors the cache
   ;; effect roots, but as a name→domain table so `self._lib = library`
   ;; (library=pylibmc) tracks `pylibmc` and `self._lib.get(...)` classifies
   ;; as cache instead of the db method-name fallback.
   (hash "pylibmc" 'cache "pymemcache" 'cache "memcache" 'cache "redis" 'cache)
   ;; ── module-qualified CLASS references (`smtplib.SMTP`, `ssl.SSLContext`):
   ;; module path → attribute → receiver TYPE. See library-stdlib-types.rkt.
   stdlib-module-class-refs-data
   ;; ── receiver-type → call → result-type for type-preserving projections
   ;; (pathlib's `/` and `with_suffix` keep the path type).
   stdlib-type-result-projections-data
   ;; ── iterable element types: module fn / receiver method / class attr ──
   stdlib-module-element-types-data
   stdlib-method-element-types-data
   class-attr-element-types-data
   stdlib-class-attr-types-data
   ;; ── bare class factories: a call's RESULT is a runtime-dependent CLASS ──
   dynamic-class-factories-data
   ;; ── instance class of a dynamic-class factory's construction ──
   dynamic-class-instance-types-data
   ;; ── mapping methods that return the value and take a default argument ──
   stdlib-mapping-value-methods-data
   stdlib-mapping-projection-methods-data
   ;; ── bare builtins whose iterated element mirrors their last argument ──
   stdlib-element-identity-bare-names-data
   ;; ── host-NATIVE pure procedures (the Racket corpus front end) ──
   racket-pure-builtins
   ;; ── host naming / encoding conventions ──
   default-dialect))
