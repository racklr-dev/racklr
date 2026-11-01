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
  effect-tokens-pure-native-builtin-names
  effect-tokens-dialect
  default-effect-tokens)

(require racket/hash
         racklr/effects/library-stdlib-types
         racklr/effects/library-pure-names
         racklr/effects/library-racket-tokens
         racklr/effects/library-dialect)

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
    'db (set "db" "database" "sql" "sqlalchemy" "sqlmodel" "asyncpg" "psycopg"
             "transaction")
    'network (set "fetch" "axios" "httpx" "requests" "aiohttp" "urllib")
    'cache (set "redis" "memcache" "cache"))
   ;; Root domains are tried in this order (the strongest root wins).
   '(llm db network cache)
   ;; ── bare-name effects: a bare callee name that is an effect on its own ──
   (hash-union
    (hash
    ;; network / cache client libraries called as bare names
    "fetch" 'network
    "axios" 'network
    "revalidatePath" 'cache
    "revalidate_path" 'cache
    "invalidate" 'cache
    "clearCache" 'cache
    "purgeCache" 'cache
    ;; bare builtins that perform console/file I/O, plus framework logging /
    ;; deprecation-warning helpers imported bare
    ;; (`django.utils.log.log_response`).
    "open" 'io
    "print" 'io
    "input" 'io
    "warn_about_external_use" 'io
    "log_response" 'io
    "NamedTemporaryFile" 'io
    "TemporaryFile" 'io
    "TemporaryDirectory" 'io
    "mkstemp" 'io
    "mkdtemp" 'io
    ;; asgiref.sync.sync_to_async: runs a sync callable on a thread-pool
    ;; executor (the async bridge), so the scheduling is an io effect.
    "sync_to_async" 'io
    ;; method and bare names that perform a time effect regardless of receiver:
    ;; `time.sleep` / `from time import sleep`, clock reads (`time.time`,
    ;; `time.monotonic`), and Django's `now` / `tz_now` timezone clock reads.
    "sleep" 'time
    "time" 'time
    "monotonic" 'time
    "monotonic_ns" 'time
    "perf_counter" 'time
    "perf_counter_ns" 'time
    "process_time" 'time
    "thread_time" 'time
    "time_ns" 'time
    "tz_now" 'time
    "now" 'time
    "localtime" 'time
    "gmtime" 'time)
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
    "open" 'io "read" 'io "readline" 'io "readlines" 'io "read1" 'io
    "readinto" 'io "readinto1" 'io "write" 'io "writelines" 'io "flush" 'io
    "seek" 'io "tell" 'io "truncate" 'io "fileno" 'io "debug" 'io "info" 'io
    "warning" 'io "warn" 'io "error" 'io "critical" 'io "exception" 'io
    ;; dotted methods that send email
    "send_mail" 'email "send_mass_mail" 'email "mail_admins" 'email
    "mail_managers" 'email
    ;; clock reads / sleeps reached as dotted methods (`time.sleep`,
    ;; `datetime.now`, Django `timezone.now`)
    "sleep" 'time "time" 'time "monotonic" 'time "monotonic_ns" 'time
    "perf_counter" 'time "perf_counter_ns" 'time "process_time" 'time
    "thread_time" 'time "time_ns" 'time "tz_now" 'time "now" 'time
    "localtime" 'time "gmtime" 'time)
    ;; Racket clock reads, dotted like any other method name
    ;; (library-racket-tokens.rkt).
    (for/hash ([n (in-set racket-time-builtins)]) (values n 'time))
    #:combine (λ (a b) a))
   ;; ── name-only method fallback: unambiguous ORM operations ──
   ;; Strongly-ORM method names: unambiguous QuerySet / Manager / Model DB
   ;; operations that classify name-only even when the receiver cannot be
   ;; resolved (`obj.prefetch_related`, `qs.values_list`).
   (hash
    "prefetch_related" 'db "select_related" 'db "values_list" 'db
    "get_or_create" 'db "bulk_create" 'db "refresh_from_db" 'db
    "in_bulk" 'db "db_for_read" 'db "db_for_write" 'db
    "create_deterministic_function" 'db)
   ;; ── ambiguous-or-generic method fallback: gated by the flag ──
   ;; Generic-English ORM verbs: `get`/`update`/`filter`/`save`/... collide
   ;; with non-DB receivers, so they classify as db ONLY when the host enables
   ;; this table (the Django-ORM dialect flag, default #t) or with receiver
   ;; evidence, never name-only in a non-django corpus.
   (hash
    "all" 'db "annotate" 'db "aggregate" 'db "alias" 'db "as_manager" 'db
    "count" 'db "dates" 'db "datetimes" 'db "db_manager" 'db "defer" 'db
    "difference" 'db "distinct" 'db "earliest" 'db "exclude" 'db
    "exists" 'db "explain" 'db "extra" 'db "filter" 'db "first" 'db
    "get" 'db "intersection" 'db "iterator" 'db "last" 'db "latest" 'db
    "none" 'db "only" 'db "order_by" 'db "raw" 'db "reverse" 'db
    "union" 'db "using" 'db "values" 'db "bulk_update" 'db "create" 'db
    "delete" 'db "update" 'db "update_or_create" 'db "save" 'db
    "execute" 'db "executemany" 'db "fetchone" 'db "fetchall" 'db
    "fetchmany" 'db "commit" 'db "rollback" 'db "cursor" 'db
    "create_function" 'db "create_aggregate" 'db)
   #t
   ;; ── root-qualified method effects ──
   ;; `root.method` → effect, where the effect depends on the method, not only
   ;; the root (`os.remove` → io, `os.system` → process, `sys.exit` →
   ;; process, `warnings.warn` → io).
   (hash
    'os (hash
         ;; `os.*` filesystem operations
         "remove" 'io "unlink" 'io "rename" 'io "renames" 'io "replace" 'io
         "mkdir" 'io "makedirs" 'io "rmdir" 'io "removedirs" 'io "chdir" 'io
         "chmod" 'io "chown" 'io "utime" 'io "link" 'io "symlink" 'io
         "readlink" 'io "listdir" 'io "scandir" 'io "stat" 'io "lstat" 'io
         "fsync" 'io "fdatasync" 'io
         ;; `os.*` filesystem metadata reads and fd (stream) operations
         "exists" 'io "isfile" 'io "isdir" 'io "islink" 'io "lexists" 'io
         "samefile" 'io "sameopenfile" 'io "samestat" 'io "getsize" 'io
         "getmtime" 'io "getctime" 'io "getatime" 'io "access" 'io
         "walk" 'io "fwalk" 'io "realpath" 'io "fstat" 'io "fstatvfs" 'io
         "statvfs" 'io "getcwd" 'io "getcwdb" 'io
         "close" 'io "fdopen" 'io "open" 'io "read" 'io "write" 'io
         "dup" 'io "dup2" 'io "dup3" 'io "lseek" 'io "ftruncate" 'io
         "fchmod" 'io "fchown" 'io "isatty" 'io "pipe" 'io "pipe2" 'io
         "mkfifo" 'io
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
    'warnings (hash "warn" 'io)
    'glob (hash "glob" 'io "iglob" 'io)
    'traceback (hash "print_exc" 'io "print_exception" 'io "print_last" 'io)
    'mail (hash "send" 'email)
    ;; django.utils.timezone clock reads (make_aware/is_aware are pure datetime
    ;; arithmetic and live in pure-stdlib-methods below, not here)
    'timezone (hash "now" 'time "localtime" 'time))
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
          'shutil 'io
          'tempfile 'io
          'fileinput 'io
          'smtplib 'email
          'http 'network
          'socket 'network
          'urllib3 'network)
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
   ;; ── host-NATIVE pure procedures (the Racket corpus front end) ──
   racket-pure-builtins
   ;; ── host naming / encoding conventions ──
   default-dialect))
