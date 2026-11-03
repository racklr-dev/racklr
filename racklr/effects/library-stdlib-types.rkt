#lang racket

;; racklr/effects/library-stdlib-types.rkt
;;
;; Receiver-TYPE -> method -> effect tables for call-result chains
;; (`Path(...).is_absolute`, `md5(...).hexdigest`, `open(...).close`,
;; `signature(...).bind`, and `super().__init__` on a stdlib parent). The
;; receiver type is the class of a call's RESULT; the engine carries that
;; class through a `.attr` chain via receiver-type dataflow and then looks
;; the method up HERE (class -> method -> domain) rather than in a bare
;; method-name rule.
;;
;; Split out of library-tokens.rkt to keep that module under the 400-line
;; obligation. This is pure DATA (library-specific tokens), not engine code:
;; add a framework class here without touching the classifier.

(provide stdlib-type-methods-data
         stdlib-module-result-types-data
         stdlib-module-class-refs-data
         stdlib-type-result-projections-data)

(define stdlib-type-methods-data
  (hash
   ;; pathlib (pure to construct; filesystem methods are io)
   "Path" (hash "is_absolute" 'pure "is_relative_to" 'pure "joinpath" 'pure
                "with_name" 'pure "with_suffix" 'pure "relative_to" 'pure
                "parts" 'pure "suffix" 'pure "suffixes" 'pure "name" 'pure
                "stem" 'pure "parent" 'pure "parents" 'pure "drive" 'pure
                "root" 'pure "anchor" 'pure "as_posix" 'pure "as_uri" 'pure
                "match" 'pure "is_dir" 'io "is_file" 'io "is_symlink" 'io
                "exists" 'io "resolve" 'io "absolute" 'io "stat" 'io "lstat" 'io
                "read_text" 'io "read_bytes" 'io "write_text" 'io "write_bytes" 'io
                "open" 'io "iterdir" 'io "glob" 'io "rglob" 'io "expanduser" 'io
                "unlink" 'io "mkdir" 'io "rmdir" 'io "rename" 'io "replace" 'io
                "touch" 'io "chmod" 'io "hardlink_to" 'io "symlink_to" 'io
                "samefile" 'io "cwd" 'io "home" 'io "readlink" 'io)
   "PurePath" (hash "is_absolute" 'pure "joinpath" 'pure "with_name" 'pure
                    "with_suffix" 'pure "relative_to" 'pure "parts" 'pure
                    "suffix" 'pure "name" 'pure "stem" 'pure "parent" 'pure
                    "parents" 'pure "drive" 'pure "root" 'pure "anchor" 'pure
                    "as_posix" 'pure "as_uri" 'pure "match" 'pure "is_dir" 'io
                    "is_file" 'io "is_symlink" 'io "exists" 'io "resolve" 'io
                    "stat" 'io "lstat" 'io)
   "PosixPath" (hash "is_absolute" 'pure "joinpath" 'pure "with_name" 'pure
                    "parts" 'pure "suffix" 'pure "name" 'pure "parent" 'pure
                    "as_posix" 'pure "match" 'pure "is_dir" 'io "is_file" 'io
                    "exists" 'io "resolve" 'io "read_text" 'io "read_bytes" 'io
                    "write_text" 'io "write_bytes" 'io "open" 'io "iterdir" 'io
                    "glob" 'io "rglob" 'io)
   "WindowsPath" (hash "is_absolute" 'pure "joinpath" 'pure "with_name" 'pure
                      "parts" 'pure "suffix" 'pure "name" 'pure "parent" 'pure
                      "match" 'pure "is_dir" 'io "is_file" 'io "exists" 'io
                      "resolve" 'io "read_text" 'io "read_bytes" 'io "write_text" 'io
                      "write_bytes" 'io "open" 'io "iterdir" 'io "glob" 'io "rglob" 'io)
   ;; hashlib (pure value objects)
   "md5" (hash "hexdigest" 'pure "digest" 'pure "update" 'pure "copy" 'pure
               "digest_size" 'pure "block_size" 'pure "name" 'pure)
   "sha1" (hash "hexdigest" 'pure "digest" 'pure "update" 'pure "copy" 'pure
                "digest_size" 'pure "block_size" 'pure "name" 'pure)
   "sha256" (hash "hexdigest" 'pure "digest" 'pure "update" 'pure "copy" 'pure
                  "digest_size" 'pure "block_size" 'pure "name" 'pure)
   "sha384" (hash "hexdigest" 'pure "digest" 'pure "update" 'pure "copy" 'pure
                  "digest_size" 'pure "block_size" 'pure "name" 'pure)
   "sha512" (hash "hexdigest" 'pure "digest" 'pure "update" 'pure "copy" 'pure
                  "digest_size" 'pure "block_size" 'pure "name" 'pure)
   ;; hmac (pure keyed-hash value object)
   "hmac" (hash "hexdigest" 'pure "digest" 'pure "update" 'pure "copy" 'pure
                "digest_size" 'pure "block_size" 'pure "name" 'pure)
   ;; datetime (pure value: formatting and arithmetic are pure; clock reads are
   ;; the module's `now`/`utcnow`/`today`, not methods on the result instance)
   "datetime" (hash "strftime" 'pure "strptime" 'pure "date" 'pure "time" 'pure
                    "timetz" 'pure "replace" 'pure "astimezone" 'pure
                    "isoformat" 'pure "ctime" 'pure "timetuple" 'pure
                    "utctimetuple" 'pure "weekday" 'pure "isoweekday" 'pure
                    "isocalendar" 'pure "timestamp" 'pure "toordinal" 'pure
                    "utcoffset" 'pure "dst" 'pure "tzname" 'pure
                    "fromtimestamp" 'pure "fromordinal" 'pure "combine" 'pure)
   ;; inspect (pure introspection)
   "signature" (hash "bind" 'pure "bind_partial" 'pure "parameters" 'pure
                     "return_annotation" 'pure "replace" 'pure)
   "Signature" (hash "bind" 'pure "bind_partial" 'pure "parameters" 'pure
                     "return_annotation" 'pure "replace" 'pure)
   ;; builtins.open -> file object (io)
   "open" (hash "close" 'io "read" 'io "write" 'io "seek" 'io "tell" 'io
                "flush" 'io "fileno" 'io "readline" 'io "readlines" 'io
                "writelines" 'io "truncate" 'io "readable" 'io "writable" 'io
                "seekable" 'io "isatty" 'io
                ;; context-manager protocol on a tracked file object:
                ;; __enter__ returns self (pure), __exit__ closes it (io)
                "__enter__" 'pure "__exit__" 'io)
   ;; zipfile (constructing one reads the archive header -> io)
   "ZipFile" (hash "__init__" 'io "read" 'io "open" 'io "extract" 'io
                   "extractall" 'io "close" 'io "namelist" 'io "infolist" 'io
                   "getinfo" 'io "printdir" 'io "testzip" 'io)
   ;; json encoder (pure)
   "JSONEncoder" (hash "__init__" 'pure "default" 'pure "encode" 'pure
                       "iterencode" 'pure)
   ;; argparse (pure construction/registration; printing and exit are effects)
   "HelpFormatter" (hash "__init__" 'pure "add_arguments" 'pure "add_usage" 'pure
                         "format_help" 'pure "format_usage" 'pure)
   "ArgumentParser" (hash "__init__" 'pure "add_argument" 'pure "add_subparsers" 'pure
                          "parse_args" 'pure "parse_known_args" 'pure
                          "format_help" 'pure "format_usage" 'pure
                          "print_help" 'io "print_usage" 'io "error" 'io "exit" 'process)
   ;; pyexpat (constructing is pure; parsing a file is io)
   "_ExpatParser" (hash "__init__" 'pure "Parse" 'pure "ParseFile" 'io
                        "SetBase" 'pure "GetBase" 'pure)
   ;; xml.sax.saxutils / django.utils.xmlutils SimplerXMLGenerator: the
   ;; writer wraps a destination stream, so its callbacks emit to it.
   "SimplerXMLGenerator" (hash "startDocument" 'io "endDocument" 'io
                               "startElement" 'io "endElement" 'io
                               "characters" 'io "ignorableWhitespace" 'io
                               "processingInstruction" 'io "skippedEntity" 'io
                               "startPrefixMapping" 'io "endPrefixMapping" 'io
                               "startDTD" 'io "endDTD" 'io
                               "startCDATA" 'io "endCDATA" 'io)
   ;; socketserver / wsgiref (thread and socket bookkeeping)
   "ThreadingMixIn" (hash "__init__" 'pure "process_request" 'pure
                          "process_request_thread" 'pure "server_close" 'io)
   "WSGIServer" (hash "__init__" 'pure "close_request" 'io
                      "process_request_thread" 'pure "get_request" 'io
                      "handle_error" 'io "serve_forever" 'process)
   ;; wsgiref request/server handlers Django's basehttp subclasses; their
   ;; methods live outside the scanned corpus, so self/super calls resolve
   ;; here by the external parent's bare class name.
   "WSGIRequestHandler" (hash "log_date_time_string" 'pure "get_stderr" 'pure
                              "parse_request" 'io "send_error" 'io
                              "get_environ" 'pure "get_app" 'pure)
   "ServerHandler" (hash "__init__" 'pure "cleanup_headers" 'io "get_stdin" 'pure
                         "send_headers" 'io "finish_response" 'io "close" 'io)
   ;; builtin `object` base: every class without another base inherits these
   ;; protocol dunders, so `super().__init__` on `object` is pure.
   "object" (hash "__init__" 'pure "__new__" 'pure "__str__" 'pure "__repr__" 'pure
                   "__eq__" 'pure "__hash__" 'pure "__ne__" 'pure "__lt__" 'pure
                   "__le__" 'pure "__gt__" 'pure "__ge__" 'pure "__getattribute__" 'pure
                   "__setattr__" 'pure "__delattr__" 'pure "__sizeof__" 'pure
                   "__reduce__" 'pure "__reduce_ex__" 'pure "__subclasshook__" 'pure
                   "__init_subclass__" 'pure)
   ;; exception protocol (constructing/setting an exception is pure)
   "BaseException" (hash "__init__" 'pure "__str__" 'pure "__repr__" 'pure)
   "Exception" (hash "__init__" 'pure "__str__" 'pure "__repr__" 'pure)
   "ImproperlyConfigured" (hash "__init__" 'pure)
   "DefusedXmlException" (hash "__init__" 'pure)
   ;; unittest.TestCase assertions inherited by Django's SimpleTestCase (a
   ;; stdlib class outside the scanned corpus): pure observation/registration,
   ;; resolved via the self-stdlib-fallback in whole-program.rkt.
   "TestCase" (hash "fail" 'pure "assertEqual" 'pure "assertNotEqual" 'pure
                    "assertTrue" 'pure "assertFalse" 'pure "assertIs" 'pure
                    "assertIsNot" 'pure "assertIsNone" 'pure "assertIsNotNone" 'pure
                    "assertIn" 'pure "assertNotIn" 'pure "assertRaises" 'pure
                    "assertRaisesRegex" 'pure "assertWarns" 'pure "assertWarnsRegex" 'pure
                    "assertAlmostEqual" 'pure "assertNotAlmostEqual" 'pure
                    "assertGreater" 'pure "assertGreaterEqual" 'pure
                    "assertLess" 'pure "assertLessEqual" 'pure
                    "assertRegex" 'pure "assertNotRegex" 'pure
                    "assertCountEqual" 'pure "assertMultiLineEqual" 'pure
                    "assertSequenceEqual" 'pure "assertListEqual" 'pure
                    "assertTupleEqual" 'pure "assertSetEqual" 'pure
                    "assertDictEqual" 'pure "assertLogs" 'pure
                    "subTest" 'pure "addTypeEqualityFunc" 'pure
                    "_formatMessage" 'pure "countTestCases" 'pure)
   ;; builtin collections (pure methods, disambiguated by receiver type from
   ;; the ORM name heuristic: dict.get is pure, QuerySet.get is db)
   "dict" (hash "get" 'pure "keys" 'pure "values" 'pure "items" 'pure "update" 'pure
                "pop" 'pure "popitem" 'pure "setdefault" 'pure "clear" 'pure
                "copy" 'pure "fromkeys" 'pure)
   "list" (hash "append" 'pure "extend" 'pure "insert" 'pure "pop" 'pure "remove" 'pure
                "clear" 'pure "index" 'pure "count" 'pure "sort" 'pure "reverse" 'pure
                "copy" 'pure)
   "set" (hash "add" 'pure "remove" 'pure "discard" 'pure "pop" 'pure "clear" 'pure
               "union" 'pure "intersection" 'pure "difference" 'pure "update" 'pure
               "issubset" 'pure "issuperset" 'pure "isdisjoint" 'pure "copy" 'pure
               "symmetric_difference" 'pure)
   "str" (hash "join" 'pure "format" 'pure "format_map" 'pure "split" 'pure "rsplit" 'pure
               "strip" 'pure "lstrip" 'pure "rstrip" 'pure "lower" 'pure "upper" 'pure
               "replace" 'pure "startswith" 'pure "endswith" 'pure "find" 'pure
               "index" 'pure "count" 'pure "encode" 'pure "partition" 'pure
               "rpartition" 'pure "removeprefix" 'pure "removesuffix" 'pure "zfill" 'pure
               "center" 'pure "ljust" 'pure "rjust" 'pure "translate" 'pure
               "maketrans" 'pure "capitalize" 'pure "title" 'pure "swapcase" 'pure
               "casefold" 'pure)
   ;; http response objects (requests / httpx): the body is already fetched, so
   ;; parsing it is pure; streaming/reading consumes the socket -> io
   "Response" (hash "json" 'pure "text" 'pure "content" 'pure "status_code" 'pure
                   "headers" 'pure "url" 'pure "encoding" 'pure "links" 'pure
                   "cookies" 'pure "elapsed" 'pure "reason" 'pure "history" 'pure
                   "request" 'pure "apparent_encoding" 'pure "raise_for_status" 'pure
                   "is_redirect" 'pure "is_permanent_redirect" 'pure
                   "iter_content" 'io "iter_lines" 'io "iter_bytes" 'io "iter_raw" 'io
                   "raw" 'io "close" 'io "read" 'io)
   "HTTPResponse" (hash "read" 'io "readline" 'io "readlines" 'io "readinto" 'io
                        "getheader" 'pure "getheaders" 'pure "geturl" 'pure
                        "getcode" 'pure "status" 'pure "info" 'pure "msg" 'pure
                        "version" 'pure "reason" 'pure "closed" 'pure "close" 'io)
   ;; ── type-projection chains (P0.3) ───────────────────────────────
   ;; Objects whose CONSTRUCTION is resolved through the import graph
   ;; (`smtplib.SMTP()` is a module-qualified class reference, `re.match(...)`
   ;; a module function with a result type, `Path(...) / x` an operator call
   ;; that preserves the receiver type). The method table below is what turns
   ;; that tracked receiver type into an effect, so `self.connection.sendmail`
   ;; classifies network instead of `unknown`.
   "SMTP" (hash "starttls" 'network "login" 'network "sendmail" 'network
                "quit" 'network "close" 'network "ehlo" 'network "noop" 'network
                "connect" 'network "mail" 'network "rcpt" 'network
                "data" 'network "docmd" 'network "send_message" 'network
                "has_extn" 'network "rset" 'network "set_debuglevel" 'pure)
   "SSLContext" (hash "load_cert_chain" 'io "load_verify_locations" 'io
                     "set_default_verify_paths" 'io "wrap_socket" 'network
                     "set_ciphers" 'pure "check_hostname" 'pure
                     "set_alpn_protocols" 'pure "set_ecdh_curve" 'pure
                     "verify_mode" 'pure "minimum_version" 'pure
                     "maximum_version" 'pure)
   "socket" (hash "close" 'io "send" 'network "sendall" 'network "recv" 'network
                  "recv_into" 'network "connect" 'network "connect_ex" 'network
                  "listen" 'network "accept" 'network "shutdown" 'network
                  "bind" 'io "setsockopt" 'pure "settimeout" 'pure
                  "gettimeout" 'pure "fileno" 'pure "makefile" 'pure
                  "getsockname" 'pure "getpeername" 'pure "detach" 'pure)
   "Logger" (hash "debug" 'io "info" 'io "warning" 'io "warn" 'io "error" 'io
                 "critical" 'io "fatal" 'io "exception" 'io "log" 'io
                 "setLevel" 'pure "getEffectiveLevel" 'pure "isEnabledFor" 'pure
                 "addHandler" 'pure "removeHandler" 'pure "hasHandlers" 'pure)
   "Match" (hash "group" 'pure "groups" 'pure "groupdict" 'pure "start" 'pure
                 "end" 'pure "span" 'pure "expand" 'pure "__getitem__" 'pure)
   "Pattern" (hash "match" 'pure "search" 'pure "fullmatch" 'pure
                   "findall" 'pure "finditer" 'pure "split" 'pure
                   "sub" 'pure "subn" 'pure)
   ;; email.generator writes the MIME tree to its destination stream.
   "Generator" (hash "flatten" 'io "write" 'io "clone" 'pure "close" 'io)
   ;; builtin in-memory containers: mutation touches no external resource.
   "OrderedDict" (hash "get" 'pure "keys" 'pure "values" 'pure "items" 'pure
                       "update" 'pure "pop" 'pure "popitem" 'pure
                       "setdefault" 'pure "clear" 'pure "copy" 'pure
                       "fromkeys" 'pure "move_to_end" 'pure)
   "defaultdict" (hash "get" 'pure "keys" 'pure "values" 'pure "items" 'pure
                       "update" 'pure "pop" 'pure "popitem" 'pure
                       "setdefault" 'pure "clear" 'pure "copy" 'pure
                       "fromkeys" 'pure)
   "BytesIO" (hash "read" 'pure "readline" 'pure "readlines" 'pure
                   "write" 'pure "writelines" 'pure "getvalue" 'pure
                   "seek" 'pure "tell" 'pure "truncate" 'pure "close" 'pure
                   "flush" 'pure "readable" 'pure "writable" 'pure
                   "seekable" 'pure)
   "StringIO" (hash "read" 'pure "readline" 'pure "readlines" 'pure
                    "write" 'pure "writelines" 'pure "getvalue" 'pure
                    "seek" 'pure "tell" 'pure "truncate" 'pure "close" 'pure
                    "flush" 'pure "readable" 'pure "writable" 'pure
                    "seekable" 'pure)
   ;; executors schedule work; the submitted callable's own effects are its own
   ;; rows, so scheduling/waiting here is pure.
   "ThreadPoolExecutor" (hash "submit" 'pure "map" 'pure "shutdown" 'pure
                              "__enter__" 'pure "__exit__" 'pure)
   "ProcessPoolExecutor" (hash "submit" 'pure "map" 'pure "shutdown" 'pure
                               "__enter__" 'pure "__exit__" 'pure)
   "Future" (hash "result" 'pure "exception" 'pure "done" 'pure "cancel" 'pure
                  "add_done_callback" 'pure "running" 'pure)
   "TaskGroup" (hash "create_task" 'pure "__aenter__" 'pure "__aexit__" 'pure)
   ;; xml.dom.pulldom streams events from an already-open source: reading the
   ;; stream is io, expanding a parsed node is in-memory work.
   "DOMEventStream" (hash "getEvent" 'io "expandNode" 'pure "next" 'io)
   "Document" (hash "toxml" 'pure "toprettyxml" 'pure "writexml" 'io
                    "getElementsByTagName" 'pure "appendChild" 'pure)
   ;; redis-py client objects: every command is a cache round trip.
   "Redis" (hash "set" 'cache "get" 'cache "delete" 'cache "expire" 'cache
                 "persist" 'cache "exists" 'cache "incr" 'cache "decr" 'cache
                 "mget" 'cache "mset" 'cache "pipeline" 'cache "flushdb" 'cache
                 "keys" 'cache "ttl" 'cache "setex" 'cache "hset" 'cache
                 "hget" 'cache "publish" 'cache "eval" 'cache)
   ;; redis-py Pipeline objects queue commands and run them as cache round
   ;; trips when executed; `Redis.pipeline` projects to this type.
   "Pipeline" (hash "execute" 'cache "expire" 'cache "mset" 'cache
                    "get" 'cache "set" 'cache "delete" 'cache "incr" 'cache
                    "decr" 'cache "persist" 'cache "exists" 'cache
                    "flushdb" 'cache "mget" 'cache "hset" 'cache
                    "hget" 'cache "publish" 'cache "eval" 'cache
                    "keys" 'cache "ttl" 'cache "setex" 'cache)
   "ConnectionPool" (hash "from_url" 'pure "disconnect" 'io "reset" 'io)
   "DriverInfo" (hash "add_upstream_driver" 'pure)))

;; module-path -> (function-name -> result-type-name) for stdlib/framework
;; functions whose RESULT is a receiver the next call can flow through
;; (`r = httpx.get(...)` then `r.json()`). This is receiver-TYPE data, the
;; mirror of stdlib-type-methods-data: the function's module is resolved through
;; the import graph (like imported-class-ref), then its result type is looked
;; up HERE — never in engine code.
(define stdlib-module-result-types-data
  (hash
   "httpx" (hash "get" "Response" "post" "Response" "put" "Response"
                  "patch" "Response" "delete" "Response" "head" "Response"
                  "options" "Response" "request" "Response" "stream" "Response")
   "requests" (hash "get" "Response" "post" "Response" "put" "Response"
                     "patch" "Response" "delete" "Response" "head" "Response"
                     "options" "Response" "request" "Response")
   "urllib.request" (hash "urlopen" "HTTPResponse")
   ;; os.fdopen wraps a raw fd into a text file object (io-capable receiver).
   "os" (hash "fdopen" "open")
   ;; datetime.now()/today()/... construct a datetime whose methods are pure
   ;; (strftime etc.); the result flows through the stdlib-type-methods table.
   "datetime" (hash "now" "datetime" "utcnow" "datetime" "today" "datetime"
                    "fromtimestamp" "datetime" "fromordinal" "datetime"
                    "combine" "datetime" "strptime" "datetime")
   "hmac" (hash "new" "hmac")
   ;; Result types of stdlib module FUNCTIONS whose value is a receiver the next
   ;; call flows through (`m = re.match(...)` then `m.groups()`,
   ;; `m = logging.getLogger(...)` then `m.info(...)`). Keys are the module path
   ;; the import graph resolved for the binding root.
   "re" (hash "match" "Match" "search" "Match" "fullmatch" "Match"
              "compile" "Pattern")
   "ssl" (hash "create_default_context" "SSLContext")
   "logging" (hash "getLogger" "Logger")
   "socket" (hash "socket" "socket" "create_connection" "socket")
   "wsgiref.simple_server" (hash "make_server" "WSGIServer")
   "xml.dom.pulldom" (hash "parse" "DOMEventStream")
   "xml.dom.minidom" (hash "parse" "Document" "parseString" "Document")))

;; module-path -> (attribute-name -> receiver-TYPE-name) for module-qualified
;; CLASS references (`smtplib.SMTP`, `ssl.SSLContext`, `concurrent.futures.
;; ThreadPoolExecutor`). A member access whose root binding resolves through the
;; import graph to a module handle is a CLASS VALUE when this table names the
;; attribute; the result flows through stdlib-type-methods-data exactly like a
;; `from module import Class` constructor. This is receiver-TYPE data for the
;; import graph, mirroring stdlib-module-result-types-data for module functions.
;;
;; Only entries here are treated as module-qualified class references: an
;; unknown attribute of an external module is NOT guessed from capitalization,
;; so a mis-guessed class (a common constant or namespace) cannot be tracked.
(define stdlib-module-class-refs-data
  (hash
   "smtplib" (hash "SMTP" "SMTP" "SMTP_SSL" "SMTP")
   "ssl" (hash "SSLContext" "SSLContext")
   "socket" (hash "socket" "socket")
   "email.generator" (hash "Generator" "Generator" "BytesGenerator" "Generator")
   "collections" (hash "OrderedDict" "OrderedDict" "defaultdict" "defaultdict")
   "concurrent.futures" (hash "ThreadPoolExecutor" "ThreadPoolExecutor"
                              "ProcessPoolExecutor" "ProcessPoolExecutor"
                              "Future" "Future")
   "asyncio" (hash "TaskGroup" "TaskGroup" "Task" "Task")
   "io" (hash "BytesIO" "BytesIO" "StringIO" "StringIO")
   "xml.dom.pulldom" (hash "DOMEventStream" "DOMEventStream")
   "xml.dom.minidom" (hash "Document" "Document")
   "redis" (hash "Redis" "Redis" "ConnectionPool" "ConnectionPool"
                 "DriverInfo" "DriverInfo")))

;; receiver-TYPE -> call -> result-TYPE for CALLS that preserve the receiver
;; type: a binary operator (`po_path = Path(dirpath) / f` lowers to
;; `(call (symbol "/") (list (call Path ...) (str f)))`) or a value-returning
;; method (`mo_path = po_path.with_suffix(".mo")`). Without this table the
;; result has no type, so a later `.stat()` / `.parent` stays `unknown`.
;; pathlib joins/suffix operations return the same path type.
(define stdlib-type-result-projections-data
  (hash
   ;; redis-py: `client.pipeline()` returns the command queue, whose command
   ;; methods are cache round trips like the client's.
   "Redis" (hash "pipeline" "Pipeline")
   "Path" (hash "/" "Path" "joinpath" "Path" "with_suffix" "Path"
                     "with_name" "Path" "with_stem" "Path" "relative_to" "Path"
                     "resolve" "Path" "absolute" "Path" "expanduser" "Path"
                     "parent" "Path")
        "PosixPath" (hash "/" "PosixPath" "joinpath" "PosixPath"
                          "with_suffix" "PosixPath" "with_name" "PosixPath"
                          "relative_to" "PosixPath" "resolve" "PosixPath"
                          "absolute" "PosixPath" "parent" "PosixPath")
        "WindowsPath" (hash "/" "WindowsPath" "joinpath" "WindowsPath"
                            "with_suffix" "WindowsPath" "with_name" "WindowsPath"
                            "relative_to" "WindowsPath" "resolve" "WindowsPath"
                            "absolute" "WindowsPath" "parent" "WindowsPath")
        "PurePath" (hash "/" "PurePath" "joinpath" "PurePath"
                         "with_suffix" "PurePath" "with_name" "PurePath"
                         "relative_to" "PurePath" "parent" "PurePath")
        "PurePosixPath" (hash "/" "PurePosixPath" "joinpath" "PurePosixPath"
                              "with_suffix" "PurePosixPath"
                              "with_name" "PurePosixPath"
                              "relative_to" "PurePosixPath" "parent" "PurePosixPath")
        "PureWindowsPath" (hash "/" "PureWindowsPath" "joinpath" "PureWindowsPath"
                                "with_suffix" "PureWindowsPath"
                                "with_name" "PureWindowsPath"
                                "relative_to" "PureWindowsPath"
                                "parent" "PureWindowsPath")))
