#lang racket

;; racklr/effects/imports/library-config.rkt
;;
;; The library-specific effect configuration: which module paths carry which
;; effect domain. This is DATA, kept apart from the generic tagging machinery
;; so the racklr core carries no library-specific tokens. Add a library by
;; adding rules here — no code changes.

(require racklr/effects/imports/effect-tag-types)

(provide effect-tag-config
         db-read-methods db-write-methods io-read-verbs io-write-verbs
         io-other-verbs builtin-class-methods)

;; Default configuration. Extend by adding a library here — no code changes.
(define effect-tag-config
  (list
   (effect-library "django"
                    (list
                          ;; Per-object / per-member rules (most-specific-first):
                          ;; the dotted prefix names the imported object or the
                          ;; called member, so a mixed module resolves without a
                          ;; coarse whole-module guess. `settings` is a lazy
                          ;; registry whose queries are in-memory except the
                          ;; initial env/module read; `apps` queries the loaded
                          ;; model registry (pure); `run_checks` executes
                          ;; registered checks that inspect the codebase (io).
                          (effect-rule "django.conf.settings._setup" 'io)
                          (effect-rule "django.conf.settings" 'pure)
                          (effect-rule "django.apps.apps.check_models_ready" 'pure)
                          (effect-rule "django.core.checks.run_checks" 'io)
                          (effect-rule "django.core.checks.tag_exists" 'pure)
                          (effect-rule "django.core.management.color.supports_color" 'io)
                          (effect-rule "django.middleware.csrf._get_failure_view" 'io)
                          (effect-rule "django.utils.archive" 'io)
                          (effect-rule "django.views.debug" 'io)
                          (effect-rule "django.test.client" 'network)
                          (effect-rule "django.core.mail" 'email)
                          (effect-rule "django.core.cache" 'cache)
                          (effect-rule "django.core.management.base" 'io)
                          (effect-rule "django.core.files" 'io)
                          (effect-rule "django.http" 'io)
                          (effect-rule "django.db" 'db)
                          ;; GIS: GEOS geometry is in-memory C math (pure); GDAL/OGR
                          ;; read and write raster/vector files (io); measure classes
                          ;; are pure arithmetic.
                          (effect-rule "django.contrib.gis.geos" 'pure)
                          (effect-rule "django.contrib.gis.gdal" 'io)
                          (effect-rule "django.contrib.gis.measure" 'pure)
                          ;; Utilities: formatting/translation/signing are in-memory
                          ;; (fail-closed io for serializers, which write streams).
                          (effect-rule "django.utils.formats" 'pure)
                          (effect-rule "django.utils.translation" 'pure)
                          ;; timezone.now/localtime read the clock; make_aware is pure
                          ;; but fail-closed 'time is safe (spurious, never hidden).
                          (effect-rule "django.utils.timezone" 'time)
                          (effect-rule "django.template.loader" 'io)
                          (effect-rule "django.template.loaders" 'io)
                          (effect-rule "django.template.backends" 'io)
                          ;; Template parsing/rendering is in-memory (pure); the
                          ;; file-reading loader + engine backends are io above.
                          (effect-rule "django.template" 'pure)
                          (effect-rule "django.contrib.admindocs.utils" 'pure)
                          (effect-rule "django.core.signals" 'pure)
                          (effect-rule "django.core.signing" 'pure)
                          (effect-rule "django.core.serializers" 'io)
                          ;; Utils: in-memory helpers (string/url/introspection).
                          (effect-rule "django.utils.crypto" 'pure)
                          (effect-rule "django.utils.encoding" 'pure)
                          (effect-rule "django.utils.inspect" 'pure)
                          (effect-rule "django.utils.http" 'pure)
                          (effect-rule "django.utils.ipv6" 'pure)
                          (effect-rule "django.utils.text" 'pure)
                          (effect-rule "django.utils.duration" 'pure)
                          (effect-rule "django.utils.csp" 'pure)
                          (effect-rule "django.utils.asyncio" 'pure)
                          (effect-rule "django.utils.termcolors" 'pure)
                          ;; Utils that touch the filesystem / console / logging.
                          (effect-rule "django.utils._os" 'io)
                          (effect-rule "django.utils.log" 'io)
                          (effect-rule "django.utils.module_loading" 'io)
                          (effect-rule "django.utils.autoreload" 'io)
                          ;; URL resolution is in-memory.
                          (effect-rule "django.urls" 'pure)
                          (effect-rule "django.core.checks.registry" 'pure)
                          (effect-rule "django.test.runner" 'process)
                          ;; auth.authenticate/login hit the DB; fail-closed db.
                          (effect-rule "django.contrib.auth" 'db)))
   (effect-library "sqlparse"
                    (list (effect-rule "sqlparse.cli" 'io)
                          (effect-rule "sqlparse" 'pure)))
   (effect-library "tzdata" (list (effect-rule "tzdata" 'pure)))
   ;; Stdlib top modules used bare in the corpus (fail-closed where mixed).
   (effect-library "inspect" (list (effect-rule "inspect" 'pure)))
   (effect-library "random" (list (effect-rule "random" 'pure)))
   (effect-library "uuid" (list (effect-rule "uuid" 'pure)))
   (effect-library "logging" (list (effect-rule "logging" 'io)))
   (effect-library "zoneinfo" (list (effect-rule "zoneinfo" 'pure)))
   (effect-library "mimetypes" (list (effect-rule "mimetypes" 'pure)))
   (effect-library "getpass" (list (effect-rule "getpass" 'io)))
   (effect-library "signal" (list (effect-rule "signal" 'process)))
   ;; stdlib / third-party modules used bare in the corpus. Fail-closed where a
   ;; module mixes in-memory and stream operations: io is the safe direction
   ;; (spurious, never hidden).
   (effect-library "json" (list (effect-rule "json" 'io)))
   (effect-library "pickle" (list (effect-rule "pickle" 'io)))
   (effect-library "yaml" (list (effect-rule "yaml" 'io)))
   (effect-library "ssl" (list (effect-rule "ssl" 'network)))
   (effect-library "stat" (list (effect-rule "stat" 'pure)))
   (effect-library "fnmatch" (list (effect-rule "fnmatch" 'pure)))
   (effect-library "code" (list (effect-rule "code" 'io)))
   (effect-library "pkgutil" (list (effect-rule "pkgutil" 'io)))
   (effect-library "fcntl" (list (effect-rule "fcntl" 'io)))
   (effect-library "winreg" (list (effect-rule "winreg" 'io)))
   (effect-library "select" (list (effect-rule "select" 'io)))
   (effect-library "traceback" (list (effect-rule "traceback" 'pure)))
   (effect-library "difflib" (list (effect-rule "difflib" 'pure)))
   (effect-library "itertools" (list (effect-rule "itertools" 'pure)))
   (effect-library "shutil" (list (effect-rule "shutil" 'io)))
   (effect-library "email" (list (effect-rule "email" 'pure)))
   (effect-library "pathlib" (list (effect-rule "pathlib" 'io)))
   (effect-library "importlib" (list (effect-rule "importlib.util" 'io)))
   (effect-library "urllib" (list (effect-rule "urllib.request" 'network)))
   (effect-library "xml" (list (effect-rule "xml.dom" 'io)
                                (effect-rule "xml.sax" 'io)))
   (effect-library "readline" (list (effect-rule "readline" 'io)))
   (effect-library "rlcompleter" (list (effect-rule "rlcompleter" 'pure)))
   (effect-library "bpython" (list (effect-rule "bpython" 'io)))
   (effect-library "IPython" (list (effect-rule "IPython" 'io)))
   ;; contextlib's exports are in-memory context-manager helpers.
   (effect-library "contextlib" (list (effect-rule "contextlib" 'pure)))
   ;; asgiref.sync's async_to_sync / sync_to_async are WRAPPER CONSTRUCTORS:
   ;; the direct call builds a closure without running it, so the wrapped
   ;; function's effect materializes at the separate wrapper-invocation site.
   (effect-library "asgiref" (list (effect-rule "asgiref.sync" 'pure)))
   ;; Pillow: Parser parses an in-memory buffer; Image.init imports plugin
   ;; modules from disk (io). Tagged at member granularity so Image.open/
   ;; ImageFile.load stay unclassified rather than being hidden as pure.
   (effect-library "PIL" (list (effect-rule "PIL.Image.init" 'io)
                                (effect-rule "PIL.ImageFile.Parser" 'pure)))
   ;; asyncio.TaskGroup construction is in-memory; the module is otherwise mixed.
   (effect-library "asyncio" (list (effect-rule "asyncio.TaskGroup" 'pure)))
   ;; isort.code sorts imports in a string (pure); isort.file writes files.
   (effect-library "isort" (list (effect-rule "isort.code" 'pure)))))

;; ── primitive-operation vocabulary (per domain) ─────────────────────
;; The db/io verb tables and the builtin-receiver seed environment. This is
;; DATA: library/domain-specific method-name knowledge, kept out of the
;; effect-tag engine (which consumes it only through these bindings).
;;
;; db.read  = SELECT (get/filter/count/exists/fetch*/...)
;; db.write = DML/DDL (update/delete/create/save/execute/commit/...)
;; io: the operation names a RESOURCE (derived from the class's module path)
;;     and a verb (from the method name), e.g. file.read, stdio.write.
;; Methods whose verb is unclassified keep () — sound: the domain is known,
;; the specific operation is not.

(define db-read-methods
  (set "get" "filter" "all" "count" "exists" "first" "last" "iterator"
       "values" "values_list" "dates" "datetimes" "in_bulk" "distinct"
       "order_by" "defer" "only" "select_related" "prefetch_related" "using"
       "alias" "annotate" "aggregate" "fetchone" "fetchall" "fetchmany"
       "latest" "earliest" "none" "explain" "raw" "resolve_expression"))

(define db-write-methods
  (set "create" "update" "delete" "save" "bulk_create" "bulk_update"
       "get_or_create" "update_or_create" "execute" "executemany" "commit"
       "rollback" "insert" "raw_delete" "set" "add" "remove" "clear"
       "create_function" "create_aggregate" "create_deterministic_function"))

(define io-read-verbs  (set "read" "readline" "readlines" "recv" "recvfrom" "readinto" "peek" "readable"))
(define io-write-verbs (set "write" "writelines" "send" "sendall" "sendto"))
(define io-other-verbs
  (set "close" "seek" "tell" "truncate" "fileno" "flush" "readable" "writable" "seekable"))

;; Builtin receiver classes (the seed environment): method names tagged pure.
;; Resolves `dict.get` vs `QuerySet.get` by class identity.
(define builtin-class-methods
  (hash "dict"  (set "get" "keys" "values" "items" "update" "pop" "popitem"
                     "setdefault" "clear" "copy" "fromkeys")
        "list"  (set "append" "extend" "insert" "pop" "remove" "clear" "index"
                     "count" "sort" "reverse" "copy")
        "set"   (set "add" "remove" "discard" "pop" "clear" "union"
                     "intersection" "difference" "symmetric_difference" "update"
                     "issubset" "issuperset" "isdisjoint" "copy")
        "str"   (set "join" "format" "format_map" "split" "rsplit" "strip"
                     "lstrip" "rstrip" "lower" "upper" "replace" "startswith"
                     "endswith" "find" "index" "count")))
