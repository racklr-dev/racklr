#lang racket

;; racklr/effects/library-pure-names.rkt
;;
;; The default (Python/Django) PURE name tables as DATA: names whose call is
;; side-effect-free (no domain effect) as far as the effect algebra is
;; concerned. Split out of library-tokens.rkt to keep that module under the
;; 400-line obligation. A different host supplies its own pure-name tables
;; through its lexicon; the classifier reads them from the injected tokens, so
;; none of these names appear in engine code.

(provide
 pure-stdlib-roots-data
 pure-stdlib-methods-data
 pure-stdlib-bare-names-data
 pure-op-names-data
 pure-constructor-names-data
 pure-builtin-names-data
 protocol-method-names-data
 string-method-names-data
 pure-method-names-data)

;; ── whole-module pure roots ──
(define pure-stdlib-roots-data
  (set "math" "cmath" "itertools" "functools" "operator" "collections" "string"
       "textwrap" "statistics" "types" "typing" "dataclasses" "enum" "abc" "copy"
       "decimal" "fractions" "ipaddress" "struct" "array" "bisect" "heapq"
       "hashlib" "hmac" "base64" "binascii" "unicodedata" "calendar" "keyword"
       "datetime" "chain" "posixpath" "ntpath" "genericpath"))

;; ── mixed modules: only the listed methods are pure ──
(define pure-stdlib-methods-data
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
                      "iscoroutinefunction" "getfullargspec" "getdoc")))

;; ── bare stdlib / framework / value-constructor pure names ──
;; bare stdlib/ctypes/functools/urllib/collections/inspect value constructors,
;; decorators, and pure helpers imported directly (`from functools import
;; partial`, `from urllib.parse import urljoin`) that lower to a bare callee.
;; Includes Django pure helpers (`import_string` matches the existing
;; `__import__` pure-builtin treatment; `get_resolver` and `deconstruct*`
;; build or introspect values without a domain effect).
(define pure-stdlib-bare-names-data
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
       ;; namedtuple class factories assigned to module-level names
       ;; (`EmailAlternative = namedtuple(...)`, `URLPattern = ...`):
       ;; constructing one is pure value construction.
       "EmailAlternative" "EmailAttachment" "URLPattern"
       ;; bare pure helpers imported from math / ctypes
       "ceil" "sizeof" "OVERLAPPED"))

;; ── pure op names (also dotted): translation-lookup aliases ──
(define pure-op-names-data
  (set "_" "gettext" "gettext_lazy" "gettext_noop" "ngettext" "ngettext_lazy"
       "pgettext" "pgettext_lazy" "npgettext" "npgettext_lazy"
       "ugettext" "ugettext_lazy" "ungettext" "ungettext_lazy"))

;; ── pure constructor names ──
;; framework exception classes that don't end in an exception suffix (so the
;; suffix heuristic misses them); constructing one to raise performs no
;; effect. Plus declarative framework constructors that are pure class
;; instantiation (model fields and query-expression objects).
(define pure-constructor-names-data
  (set
   "ImproperlyConfigured" "Http404" "PermissionDenied" "SuspiciousOperation"
   "DisallowedHost" "DisallowedRedirect" "FieldDoesNotExist"
   "ObjectDoesNotExist" "MultipleObjectsReturned" "EmptyResultSet"
   "ViewDoesNotExist" "NoReverseMatch" "AppRegistryNotReady" "BadSignature"
   "PageNotAnInteger" "EmptyPage" "DoesNotExist" "SuspiciousMultipartForm"
   "SuspiciousFileOperation" "RequestAborted" "RequestDataTooBig"
   "TooManyFieldsSent" "Resolver404" "MiddlewareNotUsed"
   ;; nested exception subclass constructors that don't carry an
   ;; Error/Exception suffix (`DeserializationError.WithData(...)`)
   "WithData"
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
   "ExpressionWrapper" "CombinedExpression" "DurationValue" "Greatest" "Least"))

;; ── pure builtin names ──
;; Side-effect-free Python builtins (lower to uir-var callees). Effectful
;; builtins (open/input/print/eval/exec/compile/breakpoint/help) are
;; deliberately absent so they stay `unknown` rather than being marked pure.
(define pure-builtin-names-data
  (set "abs" "all" "any" "ascii" "bin" "bool" "bytearray" "bytes" "callable"
       "chr" "classmethod" "complex" "delattr" "dict" "dir" "divmod"
       "aiter" "anext"
       "enumerate" "filter" "float" "format" "frozenset" "getattr" "globals"
       "hasattr" "hash" "hex" "id" "int" "isinstance" "issubclass" "iter"
       "len" "list" "locals" "map" "max" "memoryview" "min" "next" "object"
       "oct" "ord" "pow" "property" "range" "repr" "reversed" "round" "set"
       "setattr" "slice" "sorted" "staticmethod" "str" "sum" "super" "tuple"
       "type" "vars" "zip" "__import__"))

;; ── protocol method names ──
;; Protocol ("dunder") method names on an arbitrary receiver. `__call__`,
;; `__enter__`, `__exit__`, and `__getattr__` are deliberately absent: they
;; dispatch to arbitrary (possibly effectful) code and stay `unknown`.
(define protocol-method-names-data
  (set "__init__" "__new__" "__repr__" "__str__" "__format__"
       "__eq__" "__ne__" "__lt__" "__le__" "__gt__" "__ge__" "__hash__"
       "__bool__" "__len__" "__contains__" "__iter__" "__next__"
       "__getitem__" "__setitem__" "__delitem__" "__copy__" "__deepcopy__"
       "__init_subclass__" "__set_name__"))

;; ── string-method names ──
;; String-literal method names (`"".join(x)`); the uir-string base is what
;; disambiguates them from an arbitrary `obj.join(...)`.
(define string-method-names-data
  (set "join" "format" "format_map" "split" "rsplit" "splitlines" "strip"
       "lstrip" "rstrip" "lower" "upper" "capitalize" "title" "swapcase"
       "casefold" "replace" "translate" "zfill" "center" "ljust" "rjust"
       "expandtabs" "count" "find" "rfind" "index" "rindex" "startswith"
       "endswith" "isalnum" "isalpha" "isascii" "isdecimal" "isdigit"
       "isidentifier" "islower" "isnumeric" "isprintable" "isspace" "istitle"
       "isupper" "encode" "partition" "rpartition" "removeprefix" "removesuffix"
       "maketrans"))

;; ── pure method names (arbitrary receivers) ──
;; Pure collection / configuration method names on an arbitrary receiver.
;; Deliberately omits every name that collides with a DB receiver operation:
;; those classify `db` first, so anything still carrying one here is an
;; arbitrary object and stays `unknown`.
(define pure-method-names-data
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
       "as_string" "as_bytes" "move_to_end" "quote_name"
       ;; framework config/registration methods (argparse / xml.sax) that
       ;; mutate no external state of interest
       "add_argument" "add_subparsers" "set_defaults" "parse_args"
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
       ;; CheckMessage.
       "to_python" "get_internal_type" "is_silenced" "is_serious"
       ;; cross-tree Django introspection / value-building methods on
       ;; migration graphs, URL resolvers, serializers, and db backends
       "leaf_nodes" "resolve_error_handler" "get_by_natural_key"
       "adapt_datetimefield_value"))
