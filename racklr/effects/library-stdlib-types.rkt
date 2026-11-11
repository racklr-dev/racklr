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
         stdlib-type-result-projections-data
         stdlib-module-element-types-data
         stdlib-method-element-types-data
         class-attr-element-types-data
         declared-param-types
         declared-return-types
         declared-element-types
         declared-param-element-types)

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
                         "add_mutually_exclusive_group" 'pure "add_argument_group" 'pure
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
                         "send_headers" 'io "finish_response" 'io "close" 'io
                         "run" 'io)
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
   "ConnectionPool" (hash "from_url" 'pure "disconnect" 'io "reset" 'io)
   "DriverInfo" (hash "add_upstream_driver" 'pure)
   ;; django.http.request.HttpRequest methods inherited by WSGIRequest
   ;; (_mark_post_parse_error sets an in-memory flag; pure)
   "HttpRequest" (hash "_mark_post_parse_error" 'pure "_mark_post_parse_error_flag" 'pure
                       "get_host" 'pure "get_port" 'pure "get_full_path" 'pure
                       "get_full_path_info" 'pure "build_absolute_uri" 'pure
                       "get_signed_cookie" 'pure "is_secure" 'pure "is_ajax" 'pure
                       "get_raw_uri" 'pure "get_content_type" 'pure
                       "_get_scheme" 'pure "_get_raw_host" 'pure)
   ;; ── external framework / stdlib classes typed by declared param/return
   ;; tables (django/django/core scans their use sites, not their bodies) ──
   ;; datetime.timedelta (pure value: field reads and arithmetic are pure)
   "timedelta" (hash "total_seconds" 'pure "days" 'pure "seconds" 'pure
                     "microseconds" 'pure)
   ;; float/int methods (pure numeric predicates)
   "int" (hash "is_integer" 'pure "as_integer_ratio" 'pure "bit_length" 'pure
                "to_bytes" 'pure "from_bytes" 'pure "real" 'pure "imag" 'pure
                "conjugate" 'pure "numerator" 'pure "denominator" 'pure)
   "float" (hash "is_integer" 'pure "as_integer_ratio" 'pure "hex" 'pure "fromhex" 'pure
                  "is_finite" 'pure "conjugate" 'pure)
   ;; argparse.Namespace (parsed options object; in-memory reads)
   "Namespace" (hash "_get_kwargs" 'pure "__getattr__" 'pure "__contains__" 'pure)
   ;; django.http.HttpResponse (response body already built in memory; render
   ;; serializes it, close releases streaming resources -> io)
   "HttpResponse" (hash "render" 'pure "content" 'pure "status_code" 'pure
                        "reason_phrase" 'pure "set_cookie" 'pure "delete_cookie" 'pure
                        "get" 'pure "items" 'pure "has_header" 'pure
                        "serialize" 'pure "serialize_headers" 'pure "make_bytes" 'pure
                        "close" 'io "write" 'io "writelines" 'io "flush" 'io
                        "tell" 'io "read" 'io)
   ;; email.charset.Charset (pure charset name/codec lookups)
   "Charset" (hash "get_output_charset" 'pure "body_encode" 'pure "header_encode" 'pure
                    "header_encode_lines" 'pure "get_body_encoding" 'pure)
   ;; email.message.Message (stdlib): pure in-memory header/body reads
   "Message" (hash "get_charset" 'pure "as_bytes" 'pure "as_string" 'pure
                    "get" 'pure "get_all" 'pure "items" 'pure "keys" 'pure
                    "values" 'pure "get_content_type" 'pure "get_content_maintype" 'pure
                    "get_content_subtype" 'pure "get_filename" 'pure
                    "get_param" 'pure "get_params" 'pure "is_multipart" 'pure
                    "walk" 'pure "attach" 'pure "set_charset" 'pure)
   ;; xml.dom Node / Element (in-memory DOM reads/mutation are pure; the
   ;; file/network read happened when the document was parsed)
   "Node" (hash "getAttribute" 'pure "hasAttribute" 'pure "setAttribute" 'pure
                 "removeAttribute" 'pure "getAttributeNode" 'pure "nodeName" 'pure
                 "nodeValue" 'pure "childNodes" 'pure "firstChild" 'pure
                 "lastChild" 'pure "parentNode" 'pure "appendChild" 'pure
                 "removeChild" 'pure "getElementsByTagName" 'pure
                 "getElementsByTagNameNS" 'pure "expandNode" 'pure
                 "toxml" 'pure "toprettyxml" 'pure)
   ;; django.db.models.Field (external to django/django/core): reading a field's
   ;; value from an in-memory model instance / formatting it is pure.
   "Field" (hash "value_from_object" 'pure "value_to_string" 'pure "attname" 'pure
                "db_type" 'pure "get_internal_type" 'pure "formfield" 'pure
                "name" 'pure "get_attname" 'pure "get_attname_column" 'pure)
   ;; django.db.migrations.operations.Operation (external): describe() renders
   ;; an in-memory human-readable summary of the operation.
   "Operation" (hash "describe" 'pure "migration_name_fragment" 'pure "reduce" 'pure
                  "formatted_description" 'pure "state_forwards" 'pure "state_backwards" 'pure
                  "database_forwards" 'pure "database_backwards" 'pure
                  "references_model" 'pure "references_field" 'pure
                  "allow_migrate_model" 'pure "elidable" 'pure)
   ;; django.db.migrations.migration.Migration (external): suggest_name renders
   ;; a name from the operations; pure string formatting.
   "Migration" (hash "suggest_name" 'pure)
   ;; django.db.models.options.Options: can_migrate consults the DB router
   ;; (db); get_fields/value_to_string read the in-memory registry (pure).
   "django.db.models.options.Options" (hash "get_fields" 'pure "can_migrate" 'db
                                             "value_to_string" 'pure)
   ;; django.db.migrations.state.ProjectState (external): in-memory migration
   ;; state mutation (pure).
   "ProjectState" (hash "clone" 'pure "clear_delayed_apps_cache" 'pure)
   ;; django.apps.registry.Apps (external): model-registry mutation is the
   ;; migrate command's model reloading (db domain).
   "Apps" (hash "unregister_model" 'db "render_multiple" 'db "bulk_update" 'db)
   ;; django.db.models.expressions.Expression (external): get_db_converters
   ;; returns field converter functions (pure; DB access happens at the cursor).
   "Expression" (hash "get_db_converters" 'pure "resolve_expression" 'pure)
   ;; django.template.Engine / Template: parsing/rendering are in-memory.
   "Engine" (hash "from_string" 'pure "get_template" 'pure)
   "Template" (hash "render" 'pure)
   ;; http.cookies.SimpleCookie / Morsel (in-memory cookie jar; pure).
   "SimpleCookie" (hash "values" 'pure "items" 'pure "keys" 'pure "get" 'pure
                         "load" 'pure "output" 'pure)
   "Morsel" (hash "OutputString" 'pure "set" 'pure "update" 'pure
                   "key" 'pure "value" 'pure "coded_value" 'pure)
   ;; django.urls.resolvers.RegexURLPattern (namedtuple): _asdict is pure
   ;; (in-memory tuple->dict projection).
   "RegexURLPattern" (hash "_asdict" 'pure)
   ;; django.db.models.base.Model (external): natural_key/_is_pk_set read the
   ;; already-loaded instance state in memory.
   "Model" (hash "natural_key" 'pure "_is_pk_set" 'pure "get_deferred_fields" 'pure
                 "refresh_from_db" 'db "delete" 'db "save" 'db "clean" 'pure
                 "full_clean" 'pure "validate_unique" 'db)
   ;; django.db.backends.base.base.BaseDatabaseWrapper (external): the database
   ;; connection handle; schema_editor() is the entry to schema DDL.
   "BaseDatabaseWrapper" (hash "schema_editor" 'db "cursor" 'db "close" 'io
                               "commit" 'db "rollback" 'db "ensure_connection" 'db
                               "constraint_checks_disabled" 'db
                               "check_constraints" 'db "prepare_database" 'db
                               "check" 'db "ensure_timezone" 'pure)
   "BaseDatabaseSchemaEditor" (hash "create_model" 'db)
   "DatabaseValidation" (hash "check" 'db)
   "DatabaseClient" (hash "runshell" 'process "executable_name" 'pure)
   ;; decimal.Decimal (stdlib): as_tuple() reads the in-memory triple.
   "Decimal" (hash "as_tuple" 'pure "as_integer_ratio" 'pure "is_finite" 'pure
                   "is_nan" 'pure "is_infinite" 'pure "compare" 'pure "copy_abs" 'pure
                   "copy_negate" 'pure "copy_sign" 'pure "quantize" 'pure
                   "to_eng_string" 'pure "to_integral" 'pure "normalize" 'pure)
   ;; os.DirEntry (os.scandir yield): is_dir/is_file/is_symlink/stat touch the
   ;; filesystem (io); name/path/inode read the fetched entry (pure).
   "DirEntry" (hash "is_dir" 'io "is_file" 'io "is_symlink" 'io "stat" 'io
                    "name" 'pure "path" 'pure "inode" 'pure)
   ;; threading.Lock / RLock (in-process synchronization, no domain effect)
   "Lock" (hash "acquire" 'pure "release" 'pure "locked" 'pure
                "__enter__" 'pure "__exit__" 'pure)
   "RLock" (hash "acquire" 'pure "release" 'pure "locked" 'pure
                 "__enter__" 'pure "__exit__" 'pure)
   "Counter" (hash "items" 'pure "update" 'pure "get" 'pure "most_common" 'pure
                   "elements" 'pure "subtract" 'pure "keys" 'pure "values" 'pure
                   "copy" 'pure)
   "deque" (hash "append" 'pure "appendleft" 'pure "pop" 'pure "popleft" 'pure
                  "extend" 'pure "extendleft" 'pure "clear" 'pure "copy" 'pure
                  "count" 'pure "index" 'pure "remove" 'pure "reverse" 'pure
                  "rotate" 'pure)
   ;; django.urls.resolvers.URLResolver (external): resolve() matches the
   ;; request path against the in-memory URL patterns.
   "URLResolver" (hash "resolve" 'pure)
   ;; django.core.mail.backends.base.BaseEmailBackend (external): the email
   ;; backend returned by get_connection; send_messages carries the email
   ;; domain.
   "BaseEmailBackend" (hash "send_messages" 'email "open" 'email "close" 'email)
   ;; PIL.ImageFile.Parser (external): the streaming image parser fed in
   ;; get_image_dimensions; feed reads the in-memory chunk buffer.
   "Parser" (hash "feed" 'pure)
   ;; django.test.runner.DiscoverRunner (external): add_arguments wires the
   ;; argparse surface (pure); run_tests executes the suite — building and
   ;; tearing down test databases (db) — and orchestrates the run (process).
   "DiscoverRunner" (hash "add_arguments" 'pure "run_tests" 'process
                          "setup_databases" 'db "teardown_databases" 'db
                          "build_suite" 'pure "run_checks" 'db)))

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
   "xml.dom.minidom" (hash "parse" "Document" "parseString" "Document")
   ;; django.urls.get_resolver returns the URL resolver the request path is
   ;; matched against (resolve() reads the in-memory URL patterns).
   "django.urls" (hash "get_resolver" "URLResolver")
   ;; django.db.migrations.state.ProjectState.from_apps is a classmethod
   ;; constructor: it builds the migration state for the loaded apps.
   "django.db.migrations.state" (hash "from_apps" "ProjectState")
   ;; django.core.mail.get_connection returns the email backend (its
   ;; send_messages carries the email domain).
   "django.core.mail" (hash "get_connection" "BaseEmailBackend")
   ;; django.test.utils.get_runner returns the DiscoverRunner class the test
   ;; command instantiates (add_arguments is pure; run_tests executes the
   ;; suite, which sets up and tears down test databases).
   "django.test.utils" (hash "get_runner" "DiscoverRunner")
   ;; django.utils.crypto.salted_hmac returns an hmac object (digest/hexdigest
   ;; are pure in-memory crypto).
   "django.utils.crypto" (hash "salted_hmac" "hmac")))

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
                 "DriverInfo" "DriverInfo")
   ;; django.db.models.Expression (SQL expression the DB backend's
   ;; converters operate on; get_db_converters is pure).
   "django.db.models" (hash "Expression" "Expression")
   ;; PIL.ImageFile.Parser: the streaming image parser get_image_dimensions
   ;; feeds image chunks into (feed reads the in-memory buffer).
   "PIL.ImageFile" (hash "Parser" "Parser")))

;; receiver-TYPE -> call -> result-TYPE for CALLS that preserve the receiver
;; type: a binary operator (`po_path = Path(dirpath) / f` lowers to
;; `(call (symbol "/") (list (call Path ...) (str f)))`) or a value-returning
;; method (`mo_path = po_path.with_suffix(".mo")`). Without this table the
;; result has no type, so a later `.stat()` / `.parent` stays `unknown`.
;; pathlib joins/suffix operations return the same path type.
(define stdlib-type-result-projections-data
  (hash "Path" (hash "/" "Path" "joinpath" "Path" "with_suffix" "Path"
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
                                "parent" "PureWindowsPath")
        ;; email.message.Message.get_charset returns the message's charset
        ;; object; the result flows through Charset's method table
        ;; (get_output_charset / body_encode / header_encode).
        "Message" (hash "get_charset" "Charset")
        ;; django.db.migrations.executor.MigrationExecutor: migrate() and
        ;; _create_project_state() return the in-memory ProjectState the next
        ;; call reads (clone / clear_delayed_apps_cache).
        "MigrationExecutor" (hash "migrate" "ProjectState"
                                  "_create_project_state" "ProjectState")
        ;; django.db.backends.base.base.BaseDatabaseWrapper.schema_editor()
        ;; returns the DDL editor (create_model carries the db domain).
        "BaseDatabaseWrapper" (hash "schema_editor" "BaseDatabaseSchemaEditor")
        ;; django.template.Engine.from_string/get_template return a Template
        ;; whose render() serializes the in-memory template.
        "Engine" (hash "from_string" "Template" "get_template" "Template")))

;; Declared param signatures for external-entry-point parameters the corpus call
;; chain could not type: a parameter supplied by a caller OUTSIDE the scanned
;; tree (WSGI/ASGI server, test client, mail backend, database layer) has no
;; in-corpus call site to project it. Keyed (module-path . qualified-name) ->
;; ((param-name . class-name) ...). class-name is a corpus class, an external
;; framework class (resolved through stdlib-type-methods-data above), or a
;; stdlib value type. Call-chain evidence WINS: seeding overlays only gaps.
(define declared-param-types
  (hash
   (cons "django.core.handlers.exception" 'response_for_exception)
   ;; HttpRequest (not the WSGIRequest subclass) so request._mark_post_parse_error
   ;; resolves through the external parent's stdlib method table; WSGIRequest
   ;; is a corpus class whose inherited HttpRequest methods are outside the scan.
   (list (cons 'request 'HttpRequest))
   (cons "django.core.serializers.xml_serializer" 'm2m_convert)
   (list (cons 'n 'Node))
   (cons "django.core.files.storage.filesystem" 'FileSystemStorage._save)
   (list (cons 'content 'File))
   (cons "django.core.serializers.python" 'Serializer._value_from_field)
   (list (cons 'field 'Field))
   (cons "django.core.mail.backends.filebased" 'EmailBackend.write_message)
   (list (cons 'message 'EmailMessage))
   (cons "django.core.management.commands.migrate" 'Command.sync_apps)
   (list (cons 'connection 'BaseDatabaseWrapper))
   (cons "django.core.management.commands.migrate" 'Command.describe_operation)
   (list (cons 'operation 'Operation))
   (cons "django.core.mail.message" 'EmailMessage._add_bodies)
   (list (cons 'msg 'EmailMessage))
   (cons "django.core.paginator" 'BasePaginator._validate_number)
   (list (cons 'number 'float))
   (cons "django.core.signing" 'TimestampSigner.unsign)
   (list (cons 'max_age 'timedelta))
   (cons "django.core.serializers.base" 'Serializer._resolve_natural_key)
   (list (cons 'obj 'Model))
   (cons "django.core.validators" 'DecimalValidator.__call__)
   (list (cons 'value 'Decimal))
   (cons "django.core.serializers.xml_serializer" 'Deserializer._handle_object)
   (list (cons 'node 'Node))
   (cons "django.core.mail.backends.smtp" 'EmailBackend._close_connection)
   (list (cons 'connection 'SMTP))
   (cons "django.core.mail.backends.smtp" 'EmailBackend._send)
   (list (cons 'email_message 'EmailMessage))
   (cons "django.core.mail.backends.console" 'EmailBackend.write_message)
   (list (cons 'message 'EmailMessage))
   (cons "django.core.management.commands.showmigrations" 'Command.add_arguments)
   (list (cons 'parser 'ArgumentParser))
   (cons "django.core.management.commands.dbshell" 'Command.add_arguments)
   (list (cons 'parser 'ArgumentParser))
   (cons "django.core.cache.backends.filebased" 'FileBasedCache._is_expired)
   (list (cons 'f 'open))
   (cons "django.core.serializers.xml_serializer" 'Serializer.start_object)
   (list (cons 'obj 'Model))
   (cons "django.core.mail.message" 'EmailMultiAlternatives._add_bodies)
   (list (cons 'msg 'EmailMessage))
   (cons "django.core.files.storage.memory" 'InMemoryStorage._save)
   (list (cons 'content 'File))
   (cons "django.core.serializers.xml_serializer" 'Serializer.handle_field)
   (list (cons 'field 'Field))
   (cons "django.core.mail" 'send_mass_mail)
   (list (cons 'connection 'BaseEmailBackend))
   (cons "django.core.mail.message" 'EmailMessage._add_attachment)
   (list (cons 'content 'EmailMessage))
   (cons "django.core.management.commands.loaddata" 'Command.load_label)
   (list (cons 'fixture 'open))
   (cons "django.core.validators" 'StepValueValidator.__call__)
   (list (cons 'value 'Decimal))))

;; Declared return types for corpus functions whose body projects no return
;; class through the dataflow (a dynamic import_module(...).Command() result,
;; or a runtime-dependent return): keyed (module-path . qualified-name) ->
;; class-name. Body/call-chain evidence WINS: a return class the fixpoint
;; already projects is never overwritten.
(define declared-return-types
  (hash
   (cons "django.core.management" 'load_command_class) 'BaseCommand
   (cons "django.core.mail" 'get_connection) 'BaseEmailBackend
   (cons "django.core.handlers.exception" 'get_exception_response) 'HttpResponse
   (cons "django.core.handlers.exception" 'handle_uncaught_exception) 'HttpResponse
   (cons "django.core.management.base" 'BaseCommand.create_parser) 'ArgumentParser
   (cons "django.core.mail.message" 'EmailMessage.message) 'Message
   (cons "django.core.serializers.python" 'Deserializer._get_model_from_node) 'Model
   (cons "django.core.serializers.xml_serializer" 'Deserializer._get_model_from_node) 'Model
   ;; get_cookie_signer returns a TimestampSigner (a corpus class in signing.py);
   ;; its unsign() calls the in-memory signer (crypto is pure).
   (cons "django.core.signing" 'get_cookie_signer) 'TimestampSigner))

;; ELEMENT TYPE(S) of a module FUNCTION used as an iterable in a for-loop / with
;; (`for dirpath, dirnames, filenames in os.walk(...)`, `with os.scandir(p) as
;; entries`). The function's module resolves through the import graph (like
;; imported-module-result-type); the value is the element class, or a LIST of
;; element classes for a tuple-yielding generator the loop destructures. This
;; is receiver-TYPE data, kept here — never in engine code.
(define stdlib-module-element-types-data
  (hash
   ;; os.walk yields 3-tuples (dirpath, dirnames, filenames): str, list, list.
   "os" (hash "walk" '(str list list)
               ;; os.scandir yields DirEntry values.
               "scandir" 'DirEntry)
   ;; django.db.connections is the ConnectionHandler singleton; a subscript
   ;; connections[database] yields the BaseDatabaseWrapper for that alias
   ;; (schema_editor/cursor carry the db domain).
   "django.db" (hash "connections" 'BaseDatabaseWrapper)))

;; ELEMENT TYPE of a METHOD call on a tracked stdlib/framework receiver, used
;; when the method's result is ITERATED (`for c in response.cookies.values()`):
;; (receiver-class . method) -> element class. The iterable mirror of
;; stdlib-method-result-types-data; a call classified through this table still
;; projects its receiver class, never a name-only guess.
(define stdlib-method-element-types-data
  (hash
   ;; http.cookies.SimpleCookie is a dict subclass whose values() yields
   ;; Morsel instances (each cookie's value).
   "SimpleCookie" (hash "values" "Morsel" "items" "Morsel")))

;; ELEMENT TYPE of an ATTRIBUTE on a tracked framework class, used when the
;; attribute is ITERATED (`for operation in migration.operations`) or
;; subscripted: (class . attr) -> element class. The attribute mirror of
;; class-attr-types-data (which names the attribute's VALUE class); this names
;; the class of the container's ELEMENTS. A receiver resolved through this
;; table still classifies by the element class's own effect domain.
(define class-attr-element-types-data
  (hash
   ;; django.core.management.commands.loaddata.Command.objs_with_deferred_fields
   ;; is a list of DeserializedObject appended in save_obj(); save_deferred_fields
   ;; writes deferred fields (db).
   "Command" (hash "objs_with_deferred_fields" "DeserializedObject")
   ;; django.db.migrations.migration.Migration.operations / merged_operations
   ;; are lists of Operation; formatted_description renders each (pure).
   "Migration" (hash "operations" "Operation" "merged_operations" "Operation")
   ;; django.db.migrations.graph.MigrationGraph.nodes is a dict of Migration;
   ;; nodes[key] yields the Migration the loader will write (pure formatting).
   "django.db.migrations.graph.MigrationGraph" (hash "nodes" "Migration")
   ;; django.db.migrations.loader.MigrationLoader.applied_migrations is a dict
   ;; of MigrationRecord namedtuples (whose .applied is a datetime).
   "MigrationLoader" (hash "applied_migrations" "MigrationRecord")))

;; Declared ELEMENT types for iterable-returning calls whose element class no
;; corpus assignment can project: keyed (module-path . qualified-name) →
;; element spec (a class symbol — often a module-qualified marker so an
;; external framework class resolves to its module's effect domain — or a list
;; for tuple destructuring). The qualified-name for a call is the corpus
;; Class.method / bare function name, or the imported binding's handle name
;; (`engines.all` for `from django.template import engines` then
;; `engines.all()`). This is the iterable mirror of declared-return-types.
(define declared-element-types
  (hash
   ;; `for engine in engines.all()` in checks/templates.py: Engine lives in
   ;; django.template (pure rendering).
   (cons "django.template" 'engines.all) 'django.template.engine.Engine
   ;; `for model in apps.get_models()` in checks/model_checks.py: the model
   ;; class lives in django.db (db).
   (cons "django.apps" 'apps.get_models) 'django.db.models.base.Model
   ;; `for conn in connections.all()` in runserver.py: DatabaseWrapper lives
   ;; in django.db (db).
   (cons "django.db" 'connections.all) 'django.db.backends.base.base.BaseDatabaseWrapper
   ;; `for future in concurrent.futures.as_completed(futures)`:
   ;; concurrent.futures.Future (result() waits on the worker -> process).
   (cons "concurrent" 'as_completed) 'Future
   ;; `for field_node in getChildrenByTagName(node, "field")`: xml.dom Node
   ;; (getAttribute/expandNode are pure in-memory DOM reads).
   (cons "django.core.serializers.xml_serializer" 'getChildrenByTagName) 'Node))

;; Declared ELEMENT types for a PARAMETER (or local) that is iterated in a
;; for-loop whose element class no corpus assignment can project: keyed
;; (module-path . qualified-name) → ((param-name . element-class) ...) — the
;; iterable-param mirror of declared-param-types. A `for message in messages`
;; loop where `messages` is an external entry-point param
;; (`EmailBackend.send_messages`, `Command.format_json`, `Command.create_table`)
;; has no corpus call site to type the iterable, so the loop variable's element
;; class is seeded from this table instead. Call-chain evidence WINS: a param
;; whose element type the fixpoint already projects is never overwritten.
(define declared-param-element-types
  (hash
   ;; EmailBackend.send_messages iterates the messages it will redirect;
   ;; message.message() builds the stdlib Message (already declared return).
   (cons "django.core.mail.backends.locmem" 'EmailBackend.send_messages)
   (list (cons 'messages 'EmailMessage))
   ;; Command.format_json iterates the resolved URL patterns; _asdict() is the
   ;; namedtuple->dict projection (pure).
   (cons "django.core.management.commands.listurls" 'Command.format_json)
   (list (cons 'url_patterns 'RegexURLPattern))
   ;; Command.create_table iterates the model fields; f.db_type() renders the
   ;; SQL column type (pure formatting).
   (cons "django.core.management.commands.createcachetable" 'Command.create_table)
   (list (cons 'fields 'Field))))
