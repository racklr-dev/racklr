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
         stdlib-module-result-types-data)

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
   ;; inspect (pure introspection)
   "signature" (hash "bind" 'pure "bind_partial" 'pure "parameters" 'pure
                     "return_annotation" 'pure "replace" 'pure)
   "Signature" (hash "bind" 'pure "bind_partial" 'pure "parameters" 'pure
                     "return_annotation" 'pure "replace" 'pure)
   ;; builtins.open -> file object (io)
   "open" (hash "close" 'io "read" 'io "write" 'io "seek" 'io "tell" 'io
                "flush" 'io "fileno" 'io "readline" 'io "readlines" 'io
                "writelines" 'io "truncate" 'io "readable" 'io "writable" 'io
                "seekable" 'io "isatty" 'io)
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
   ;; socketserver / wsgiref (thread and socket bookkeeping)
   "ThreadingMixIn" (hash "__init__" 'pure "process_request" 'pure
                          "process_request_thread" 'pure "server_close" 'io)
   "WSGIServer" (hash "__init__" 'pure "close_request" 'io
                      "process_request_thread" 'pure "get_request" 'io
                      "handle_error" 'io "serve_forever" 'process)
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
                        "version" 'pure "reason" 'pure "closed" 'pure "close" 'io)))

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
   "urllib.request" (hash "urlopen" "HTTPResponse")))
