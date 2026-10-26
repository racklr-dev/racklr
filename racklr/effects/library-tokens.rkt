#lang racket

;; racklr/effects/library-tokens.rkt
;;
;; The effect-token lexicon: the concrete call-chain tokens (module roots,
;; method names, dotted fields) that map to the meta effect categories
;; (llm / db / network / cache / io / process / time / email). This is DATA,
;; kept apart from the classification engine so racklr/effervesce carry no
;; library-specific tokens in code. Swap the whole lexicon by injecting a
;; different `effect-tokens` struct.
;;
;; Each field is a name table consulted by `direct-effect-of-callee` /
;; `pure-stdlib-callee?` in effect-roots.rkt and by the pure-name predicates in
;; pure-roots.rkt. The default instance covers the stdlib and the frameworks
;; racklr is exercised against (Django et al.).

(provide
  effect-tokens? effect-tokens
  effect-tokens-llm-roots effect-tokens-db-roots
  effect-tokens-network-roots effect-tokens-network-names
  effect-tokens-cache-roots effect-tokens-cache-names
  effect-tokens-orm-methods effect-tokens-orm-generic-methods
  effect-tokens-orm-generic-by-name?
  effect-tokens-builtin-type-roots effect-tokens-stdlib-module-roots
  effect-tokens-io-builtins effect-tokens-stdlib-root-effects
  effect-tokens-io-methods effect-tokens-email-methods
  effect-tokens-time-methods effect-tokens-root-method-effects
  effect-tokens-stdlib-type-methods
  effect-tokens-pure-stdlib-roots effect-tokens-pure-stdlib-methods
  effect-tokens-framework-exception-names effect-tokens-declarative-names
  effect-tokens-pure-stdlib-bare-names effect-tokens-gettext-names
  effect-tokens-pure-config-method-names
  effect-tokens-python-builtins effect-tokens-pure-dunder-names
  effect-tokens-str-method-names effect-tokens-pure-container-method-names
  effect-tokens-module-result-types effect-tokens-receiver-modules
  default-effect-tokens)

(require racklr/effects/library-stdlib-types)

;; The lexicon of tokens the engine classifies. All fields are name sets (or a
;; symbol→effect hash for root-qualified stdlib effects / per-root method sets).
(struct effect-tokens
  (llm-roots db-roots network-roots network-names cache-roots cache-names
   orm-methods orm-generic-methods orm-generic-by-name?
   builtin-type-roots stdlib-module-roots
   io-builtins stdlib-root-effects io-methods email-methods
   time-methods root-method-effects stdlib-type-methods
   pure-stdlib-roots pure-stdlib-methods
   framework-exception-names declarative-names
   pure-stdlib-bare-names gettext-names pure-config-method-names
   python-builtins pure-dunder-names str-method-names pure-container-method-names
   module-result-types receiver-modules)
  #:transparent)

(define default-effect-tokens
  (effect-tokens
   ;; library roots whose every call is an llm effect
   (set "llm" "openai" "anthropic" "cohere" "ollama" "groq" "agno"
        "langgraph" "langchain" "llamaindex")
   ;; library roots whose every call is a db effect
   (set "db" "database" "sql" "sqlalchemy" "sqlmodel" "asyncpg" "psycopg"
        "transaction")
   ;; library roots / bare names whose every call is a network effect
   (set "fetch" "axios" "httpx" "requests" "aiohttp" "urllib")
   (set "fetch" "axios")
   ;; library roots / bare names whose every call is a cache effect
   (set "redis" "memcache" "cache")
   (set "revalidatePath" "revalidate_path" "invalidate" "clearCache" "purgeCache")
   ;; Strongly-ORM method names: unambiguous QuerySet / Manager / Model DB
   ;; operations that classify name-only even when the receiver cannot be
   ;; resolved (`obj.prefetch_related`, `qs.values_list`). The generic-English
   ;; ORM verbs (get, update, filter, save, create, delete, count, exists,
   ;; first, last, all, none, only, values, execute, commit, defer, distinct,
   ;; difference, intersection, union, reverse, using, extra, dates, datetimes,
   ;; earliest, latest, raw, explain, alias, annotate, aggregate, exclude,
   ;; order_by, iterator, plus the SQL-cursor verbs fetchone/fetchall/... )
   ;; are DELIBERATELY absent: they collide with non-DB receivers, so they
   ;; classify only with receiver evidence (receiver-type dataflow resolving
   ;; the receiver to a db-domain class/module), never by name alone.
   (set "prefetch_related" "select_related" "values_list" "get_or_create"
        "bulk_create" "refresh_from_db" "in_bulk" "db_for_read"
        "db_for_write" "create_deterministic_function")
   ;; Generic-English ORM verbs: `get`/`update`/`filter`/`save`/... collide
   ;; with non-DB receivers, so they classify as db ONLY under the django-orm
   ;; dialect flag (default #t) or with receiver evidence, never name-only in
   ;; a non-django corpus.
   (set "all" "annotate" "aggregate" "alias" "as_manager" "count" "dates"
        "datetimes" "db_manager" "defer" "difference" "distinct" "earliest"
        "exclude" "exists" "explain" "extra" "filter" "first" "get"
        "intersection" "iterator" "last" "latest" "none" "only" "order_by"
        "raw" "reverse" "union" "using" "values" "bulk_update" "create"
        "delete" "update" "update_or_create" "save" "execute" "executemany"
        "fetchone" "fetchall" "fetchmany" "commit" "rollback" "cursor"
        "create_function" "create_aggregate")
   #t
   ;; builtin type roots whose method namespace collides with the ORM heuristic
   (set "dict" "list" "set" "str" "int" "float" "complex" "bool" "bytes"
        "bytearray" "tuple" "frozenset" "type" "object" "range" "slice"
        "memoryview")
   ;; stdlib module roots whose attribute namespace collides with the ORM
   ;; heuristic (`os.path.exists`, `pathlib.Path.exists`)
   (set "os" "sys" "io" "time" "re" "json" "pathlib" "logging" "email"
        "warnings" "traceback" "inspect" "random" "uuid" "importlib" "pickle"
        "shelve" "sqlite3" "csv" "glob" "fnmatch" "tarfile" "zipfile" "gzip"
        "bz2" "lzma" "codecs" "subprocess" "multiprocessing" "shutil" "tempfile"
        "fileinput" "smtplib" "http" "socket" "urllib3" "math" "cmath"
        "itertools" "functools" "operator" "collections" "string" "textwrap"
        "statistics" "types" "typing" "dataclasses" "enum" "abc" "copy" "decimal"
        "fractions" "ipaddress" "struct" "array" "bisect" "heapq" "hashlib" "hmac"
        "base64" "binascii" "unicodedata" "calendar" "keyword" "datetime")
   ;; bare builtins that perform console/file I/O, plus framework logging /
   ;; deprecation-warning helpers imported bare (`django.utils.log.log_response`).
   (set "open" "print" "input" "warn_about_external_use" "log_response"
        "NamedTemporaryFile" "TemporaryFile" "TemporaryDirectory"
        "mkstemp" "mkdtemp"
        ;; asgiref.sync.sync_to_async: runs a sync callable on a thread-pool
        ;; executor (the async bridge), so the scheduling is an io effect.
        "sync_to_async")
   ;; module roots whose every call is an effect (unambiguous)
   (hash 'subprocess 'process
         'multiprocessing 'process
         'shutil 'io
         'tempfile 'io
         'fileinput 'io
         'smtplib 'email
         'http 'network
         'socket 'network
         'urllib3 'network)
   ;; dotted methods that perform file/stream I/O or logging output regardless
   ;; of receiver. `log` is deliberately absent: it collides with `math.log`.
   (set "open" "read" "readline" "readlines" "read1" "readinto" "readinto1"
        "write" "writelines" "flush" "seek" "tell" "truncate" "fileno"
        "debug" "info" "warning" "warn" "error" "critical" "exception")
   ;; dotted methods that send email
   (set "send_mail" "send_mass_mail" "mail_admins" "mail_managers")
   ;; method and bare names that perform a time effect regardless of receiver:
   ;; `time.sleep` / `from time import sleep`, clock reads (`time.time`,
   ;; `time.monotonic`), and Django's `now` / `tz_now` timezone clock reads.
   (set "sleep" "time" "monotonic" "monotonic_ns" "perf_counter"
        "perf_counter_ns" "process_time" "thread_time" "time_ns"
        "tz_now" "now" "localtime" "gmtime")
   ;; root-qualified method effects: `root.method` → effect, where the effect
   ;; depends on the method, not only the root (`os.remove` → io, `os.system` →
   ;; process, `sys.exit` → process, `warnings.warn` → io).
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
   stdlib-type-methods-data
   ;; whole-module pure roots
   (set "math" "cmath" "itertools" "functools" "operator" "collections" "string"
        "textwrap" "statistics" "types" "typing" "dataclasses" "enum" "abc" "copy"
        "decimal" "fractions" "ipaddress" "struct" "array" "bisect" "heapq"
        "hashlib" "hmac" "base64" "binascii" "unicodedata" "calendar" "keyword"
        "datetime" "chain" "posixpath" "ntpath" "genericpath")
   ;; modules with a mix of pure and effectful members: only the listed methods
   ;; are pure
   (hash 'json (set "dumps" "loads" "JSONEncoder" "JSONDecoder")
         're   (set "compile" "match" "search" "fullmatch" "findall" "finditer"
                    "split" "sub" "subn" "escape" "template")
         'os   (set "join" "dirname" "basename" "splitext" "split" "splitdrive"
                    "normpath" "normcase" "isabs" "commonprefix" "commonpath"
                    "relpath" "expanduser" "expandvars" "abspath"
                    "get" "strerror" "getenv" "getenvb")
         'sys  (set "exc_info" "isatty" "getrecursionlimit" "getswitchinterval"
                    "getdefaultencoding" "getfilesystemencoding")
         'pickle (set "dumps" "loads")
         'pathlib (set "Path" "PurePath" "PosixPath" "WindowsPath")
         'django (set "setup" "get_version")
         'zlib (set "compress" "decompress" "crc32" "adler32")
         ;; django.utils.timezone datetime arithmetic (no clock read): pure
         'timezone (set "make_aware" "is_aware" "make_naive" "is_naive"
                        "get_current_timezone" "get_default_timezone"
                        "get_fixed_timezone" "activate" "deactivate"
                        "override")
         ;; asyncio / inspect runtime predicates (no domain effect); the
         ;; effectful members (asyncio.run, asyncio.create_subprocess, ...)
         ;; are deliberately absent
         'asyncio (set "iscoroutine" "iscoroutinefunction" "get_running_loop"
                        "get_event_loop")
         'inspect (set "isbuiltin" "isfunction" "ismethod" "isclass" "ismodule"
                        "isroutine" "isgenerator" "isasyncgen" "iscoroutine"
                        "iscoroutinefunction" "getfullargspec" "getdoc"))
   ;; framework exception classes that don't end in "Error"/"Exception"/
   ;; "Warning" (so the suffix heuristic misses them); constructing one to
   ;; raise performs no effect
   (set "ImproperlyConfigured" "Http404" "PermissionDenied" "SuspiciousOperation"
        "DisallowedHost" "DisallowedRedirect" "FieldDoesNotExist"
        "ObjectDoesNotExist" "MultipleObjectsReturned" "EmptyResultSet"
        "ViewDoesNotExist" "NoReverseMatch" "AppRegistryNotReady" "BadSignature"
        "PageNotAnInteger" "EmptyPage" "DoesNotExist" "SuspiciousMultipartForm"
        "SuspiciousFileOperation" "RequestAborted" "RequestDataTooBig"
        "TooManyFieldsSent" "Resolver404" "MiddlewareNotUsed"
        ;; nested exception subclass constructors that don't carry an
        ;; Error/Exception suffix (`DeserializationError.WithData(...)`)
        "WithData")
   ;; declarative framework constructors that are pure class instantiation
   ;; (model fields and query-expression objects)
   (set
    "AutoField" "BigAutoField" "SmallAutoField" "BigIntegerField" "BinaryField"
    "BooleanField" "CharField" "DateField" "DateTimeField" "DecimalField"
    "DurationField" "EmailField" "FileField" "FilePathField" "FloatField"
    "ForeignKey" "GenericIPAddressField" "IPAddressField" "ImageField"
    "IntegerField" "JSONField" "ManyToManyField" "ManyToManyRel" "ManyToOneRel"
    "NullBooleanField" "OneToOneField" "OneToOneRel" "PositiveBigIntegerField"
    "PositiveIntegerField" "PositiveSmallIntegerField" "SlugField"
    "SmallIntegerField" "TextField" "TimeField" "URLField" "UUIDField"
    "F" "Q" "Value" "Case" "When" "Subquery" "Exists" "OuterRef" "Func"
    "Coalesce" "Concat" "Count" "Sum" "Avg" "Min" "Max" "StdDev" "Variance"
    "ExpressionWrapper" "CombinedExpression" "DurationValue" "Greatest" "Least")
   ;; bare stdlib/ctypes/functools/urllib/collections/inspect value constructors,
   ;; decorators, and pure helpers imported directly (`from functools import
   ;; partial`, `from urllib.parse import urljoin`) that lower to a bare callee.
   ;; Includes Django pure helpers (`import_string` matches the existing
   ;; `__import__` pure-builtin treatment; `get_resolver` and `deconstruct*`
   ;; build or introspect values without a domain effect).
   (set "partial" "wraps" "lru_cache" "cached_property" "byref" "POINTER"
        "urlsplit" "urljoin" "urlencode" "quote" "unquote" "parse_qs"
        "parse_qsl" "defaultdict" "iscoroutinefunction" "deconstructible"
        "deconstruct" "import_string" "get_resolver"
        ;; stdlib value constructors / pure helpers imported bare
        "import_module" "Path" "PurePath" "PosixPath" "WindowsPath"
        "BytesIO" "StringIO" "OrderedDict" "Counter" "deque" "namedtuple"
        "md5" "sha1" "sha256" "sha384" "sha512" "signature" "Signature"
        "Lock" "RLock" "Event" "Condition" "Semaphore" "BoundedSemaphore"
        "SimpleNamespace" "Header" "Address"
        ;; Django pure helpers imported bare
        "force_bytes" "force_str" "get_urlconf" "set_urlconf" "color_style"
        "no_style" "punycode" "django_file_prefixes" "safe_join"
        "validate_file_name" "qn" "quote_name" "get_runner" "get_docs_version"
        "get_migration_by_prefix" "get_public_serializer_formats"
        "MigrationLoader" "MigrationWriter" "MigrationOptimizer"
        ;; pure Django text/translation/url helpers imported bare
        "capfirst" "get_text_list" "get_language" "check_for_language"
        "to_locale" "path" "make_hashable"
        ;; unittest.util / ctypes / decimal bare pure helpers and constructors
        "safe_repr" "c_void_p" "string_at" "Decimal"
        ;; Django pure helpers imported bare from django.utils.* that are
        ;; outside a django/core-only scan: regex_helper._lazy_re_compile
        ;; (lazy re.compile wrapper) and deprecation.deprecate_posargs (a
        ;; decorator factory applied at import time).
        "_lazy_re_compile" "deprecate_posargs"
        ;; Django checks registry: `register = registry.register` is a module
        ;; alias to CheckRegistry.register (in-memory registration), so bare
        ;; `register(...)` / `@register(...)` performs no domain effect.
        "register"
        ;; bare pure helpers imported from math / ctypes
        "ceil" "sizeof" "OVERLAPPED")
   ;; gettext translation-lookup aliases (pure string mapping, no effect)
   (set "_" "gettext" "gettext_lazy" "gettext_noop" "ngettext" "ngettext_lazy"
        "pgettext" "pgettext_lazy" "npgettext" "npgettext_lazy"
        "ugettext" "ugettext_lazy" "ungettext" "ungettext_lazy")
   ;; framework config/registration methods (argparse / xml.sax) that mutate no
   ;; external state of interest
   (set "add_argument" "add_subparsers" "set_defaults" "parse_args"
        "parse_known_args" "addQuickElement" "startElement" "endElement"
        "characters" "ignorableWhitespace"
        ;; Django app-registry introspection and output-style formatting
        "get_app_config" "get_app_configs" "get_models" "get_model"
        "get_field"
        ;; Django terminal color-style roles (pure ANSI string formatters;
        ;; methods are attached dynamically to the Style object, so they can
        ;; only be classified by the framework's own role vocabulary)
        "MIGRATE_HEADING" "MIGRATE_LABEL" "SUCCESS" "NOTICE" "ERROR"
        "WARNING" "SQL_KEYWORD" "SQL_FIELD" "SQL_COLTYPE" "SQL_TABLE"
        "COMMAND_HEADER" "COMMAND_HIGHLIGHT" "COMMAND_DATA" "ERROR_OUTPUT"
        ;; Django model-field / check-message value conversion and read-only
        ;; predicates: `field.to_python(v)` / `field.get_internal_type()`
        ;; convert a stored value to Python / name the column type without a
        ;; domain effect; `e.is_silenced()` / `e.is_serious()` read flags on a
        ;; CheckMessage. These classes live outside a django/core-only scan
        ;; (django.db.models / django.core.checks), so their methods resolve
        ;; through this data table rather than a receiver-type edge.
        "to_python" "get_internal_type" "is_silenced" "is_serious"
        ;; cross-tree Django introspection / value-building methods on
        ;; migration graphs, URL resolvers, serializers, and db backends
        "leaf_nodes" "resolve_error_handler" "get_by_natural_key"
        "adapt_datetimefield_value")
   ;; ── pure name tables (relocated from pure-roots.rkt) ─────────────
   ;; Side-effect-free Python builtins (lower to uir-var callees). Effectful
   ;; builtins (open/input/print/eval/exec/compile/breakpoint/help) are
   ;; deliberately absent so they stay `unknown` rather than being marked pure.
   (set "abs" "all" "any" "ascii" "bin" "bool" "bytearray" "bytes" "callable"
        "chr" "classmethod" "complex" "delattr" "dict" "dir" "divmod"
        "aiter" "anext"
        "enumerate" "filter" "float" "format" "frozenset" "getattr" "globals"
        "hasattr" "hash" "hex" "id" "int" "isinstance" "issubclass" "iter"
        "len" "list" "locals" "map" "max" "memoryview" "min" "next" "object"
        "oct" "ord" "pow" "property" "range" "repr" "reversed" "round" "set"
        "setattr" "slice" "sorted" "staticmethod" "str" "sum" "super" "tuple"
        "type" "vars" "zip" "__import__")
   ;; Protocol ("dunder") method names on an arbitrary receiver. `__call__`,
   ;; `__enter__`, `__exit__`, and `__getattr__` are deliberately absent: they
   ;; dispatch to arbitrary (possibly effectful) code and stay `unknown`.
   (set "__init__" "__new__" "__repr__" "__str__" "__format__"
        "__eq__" "__ne__" "__lt__" "__le__" "__gt__" "__ge__" "__hash__"
        "__bool__" "__len__" "__contains__" "__iter__" "__next__"
        "__getitem__" "__setitem__" "__delitem__" "__copy__" "__deepcopy__"
        "__init_subclass__" "__set_name__")
   ;; String-literal method names (`"".join(x)`); the uir-string base is what
   ;; disambiguates them from an arbitrary `obj.join(...)`.
   (set "join" "format" "format_map" "split" "rsplit" "splitlines" "strip"
        "lstrip" "rstrip" "lower" "upper" "capitalize" "title" "swapcase"
        "casefold" "replace" "translate" "zfill" "center" "ljust" "rjust"
        "expandtabs" "count" "find" "rfind" "index" "rindex" "startswith"
        "endswith" "isalnum" "isalpha" "isascii" "isdecimal" "isdigit"
        "isidentifier" "islower" "isnumeric" "isprintable" "isspace" "istitle"
        "isupper" "encode" "partition" "rpartition" "removeprefix" "removesuffix"
        "maketrans")
   ;; Pure collection-method names on an arbitrary receiver. Deliberately omits
   ;; every name that collides with a Django QuerySet/RelatedManager operation
   ;; (`get`, `update`, `filter`, ...): those reach `direct-effect-of-callee`
   ;; first and must classify `db`, so anything still carrying one here is an
   ;; arbitrary object and stays `unknown`.
   (set "append" "extend" "insert" "pop" "popitem" "sort" "index" "keys"
        "items" "setdefault" "copy" "fromkeys" "discard"
        "getvalue" "isidentifier" "seekable"
        "issubset" "issuperset" "isdisjoint"
        ;; pure string/bytes methods on arbitrary receivers
        "decode" "encode" "format" "format_map" "isoformat" "lower" "upper"
        "strip" "lstrip" "rstrip" "split" "rsplit" "splitlines" "replace"
        "startswith" "endswith" "find" "rfind" "rindex" "join" "capitalize"
        "title" "swapcase" "casefold" "center" "ljust" "rjust" "zfill"
        "partition" "rpartition" "removeprefix" "removesuffix" "translate"
        "maketrans" "isalnum" "isalpha" "isascii" "isdigit" "islower"
        "isnumeric" "isprintable" "isspace" "istitle" "isupper"
        ;; pure regex / email / ordered-collection methods on arbitrary receivers
        "match" "search" "fullmatch" "findall" "finditer" "escape"
        "as_string" "as_bytes" "move_to_end" "quote_name")
   stdlib-module-result-types-data
   ;; External module receivers tracked by receiver-type dataflow: a local /
   ;; param / self-attr bound to one of these modules resolves its method calls
   ;; to the module's effect domain (the memcache client libraries: pylibmc,
   ;; pymemcache, memcache, redis). Mirrors `cache-roots`, but as a
   ;; name->domain table so `self._lib = library` (library=pylibmc) tracks
   ;; `pylibmc` and `self._lib.get(...)` classifies as cache instead of the
   ;; ORM-name `get`->db bias.
   (hash "pylibmc" 'cache "pymemcache" 'cache "memcache" 'cache "redis" 'cache)))
