#lang racket

;; racklr/effects/imports/library-config.rkt
;;
;; The library-specific effect configuration: which module paths carry which
;; effect domain. This is DATA, kept apart from the generic tagging machinery
;; so the racklr core carries no library-specific tokens. Add a library by
;; adding rules here — no code changes.

(require racket/hash
         racket/set
         racklr/effects/imports/effect-tag-types)

(provide effect-tag-config
         db-read-methods db-write-methods io-read-verbs io-write-verbs
         io-other-verbs domain-op-vocabulary builtin-class-methods)

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
   (effect-library "isort" (list (effect-rule "isort.code" 'pure)))
   ;; ── non-Django Python: web frameworks and their dependency surface ──
   ;;
   ;; The typed contract is Python-general, not Django-specific, so the lexicon
   ;; carries the non-Django frameworks effect inference is measured against.
   ;; Rules are most-specific-first. A module that mixes in-memory work with
   ;; real I/O is tagged with the effect (fail-closed: a spurious effect is
   ;; preferred to a hidden one), and its pure members get an explicit
   ;; most-specific rule first. Module names are only ever here, never in the
   ;; classifier.
   ;;
   ;; Flask: the application object and test client run the request/response
   ;; lifecycle; helper/templating/console modules touch files and streams;
   ;; registration, configuration, contexts, sessions and signals are in-memory.
   (effect-library "flask"
     (list (effect-rule "flask.app" 'network)
           (effect-rule "flask.testing" 'network)
           (effect-rule "flask.helpers" 'io)
           (effect-rule "flask.templating" 'io)
           (effect-rule "flask.cli" 'io)
           (effect-rule "flask.logging" 'io)
           (effect-rule "flask.debughelpers" 'io)
           (effect-rule "flask.wrappers" 'io)
           (effect-rule "flask.sansio" 'pure)
           (effect-rule "flask.blueprints" 'pure)
           (effect-rule "flask.config" 'pure)
           (effect-rule "flask.ctx" 'pure)
           (effect-rule "flask.globals" 'pure)
           (effect-rule "flask.json" 'pure)
           (effect-rule "flask.sessions" 'pure)
           (effect-rule "flask.signals" 'pure)
           (effect-rule "flask.views" 'pure)
           (effect-rule "flask.typing" 'pure)
           ;; The public re-export surface (`from flask import X`): a package
           ;; attribute is one import away from an unscanned module, so the
           ;; member rules carry the same domains as the submodule rules above.
           ;; `flask.Flask` is the request/response lifecycle object.
           (effect-rule "flask.Flask" 'network)
           (effect-rule "flask.Request" 'io)
           (effect-rule "flask.Response" 'io)
           (effect-rule "flask.send_file" 'io)
           (effect-rule "flask.send_from_directory" 'io)
           (effect-rule "flask.render_template" 'io)
           (effect-rule "flask.render_template_string" 'pure)
           (effect-rule "flask.url_for" 'pure)
           (effect-rule "flask.redirect" 'pure)
           (effect-rule "flask.abort" 'pure)
           (effect-rule "flask.jsonify" 'pure)
           (effect-rule "flask.make_response" 'pure)
           (effect-rule "flask.stream_with_context" 'pure)
           (effect-rule "flask.flash" 'pure)
           (effect-rule "flask.get_flashed_messages" 'pure)
           (effect-rule "flask.has_request_context" 'pure)
           (effect-rule "flask.has_app_context" 'pure)
           (effect-rule "flask.after_this_request" 'pure)
           (effect-rule "flask.copy_current_request_context" 'pure)
           (effect-rule "flask.Blueprint" 'pure)
           (effect-rule "flask.Config" 'pure)
           (effect-rule "flask.Session" 'pure)
           (effect-rule "flask.BlueprintSetupState" 'pure)
           (effect-rule "flask.FlaskGroup" 'io)))
   ;; Werkzeug (Flask's HTTP/WSGI toolkit): server and test client are network,
   ;; stream/middleware/reloader plumbing is io, the data/routing/parsing
   ;; surface is in-memory.
   (effect-library "werkzeug"
     (list (effect-rule "werkzeug.serving" 'network)
           (effect-rule "werkzeug.test" 'network)
           (effect-rule "werkzeug.wsgi" 'io)
           (effect-rule "werkzeug.middleware" 'io)
           (effect-rule "werkzeug.formparser" 'io)
           (effect-rule "werkzeug.debug" 'io)
           (effect-rule "werkzeug._reloader" 'io)
           (effect-rule "werkzeug" 'pure)))
   ;; Click (Flask's CLI toolkit): terminal output/prompting and command
   ;; invocation are effects; decorators, parameter types and parsing are
   ;; in-memory value construction.
   (effect-library "click"
     (list (effect-rule "click.echo" 'io)
           (effect-rule "click.secho" 'io)
           (effect-rule "click.prompt" 'io)
           (effect-rule "click.confirm" 'io)
           (effect-rule "click.pause" 'io)
           (effect-rule "click.edit" 'io)
           (effect-rule "click.getchar" 'io)
           (effect-rule "click.get_binary_stream" 'io)
           (effect-rule "click.get_text_stream" 'io)
           (effect-rule "click.open_file" 'io)
           (effect-rule "click.launch" 'process)
           (effect-rule "click.termui" 'io)
           (effect-rule "click.utils" 'io)
           (effect-rule "click._termui_impl" 'io)
           (effect-rule "click.testing" 'io)
           (effect-rule "click.core" 'io)
           (effect-rule "click.shell_completion" 'io)
           (effect-rule "click.decorators" 'pure)
           (effect-rule "click.exceptions" 'pure)
           (effect-rule "click.formatting" 'pure)
           (effect-rule "click.parser" 'pure)
           (effect-rule "click.types" 'pure)
           (effect-rule "click" 'pure)))
   ;; Jinja2 (Flask's template engine): template LOADING hits the filesystem;
   ;; rendering compiled templates is in-memory.
   (effect-library "jinja2"
     (list (effect-rule "jinja2.Environment.get_template" 'io)
           (effect-rule "jinja2.Environment.select_template" 'io)
           (effect-rule "jinja2.Environment.get_or_select_template" 'io)
           (effect-rule "jinja2.environment.Environment.get_template" 'io)
           (effect-rule "jinja2.environment.Environment.select_template" 'io)
           (effect-rule "jinja2.loaders" 'io)
           (effect-rule "jinja2" 'pure)))
   ;; Flask's remaining dependency surface: signing, events, string markup,
   ;; and .env loading.
   (effect-library "itsdangerous" (list (effect-rule "itsdangerous" 'pure)))
   (effect-library "blinker" (list (effect-rule "blinker" 'pure)))
   (effect-library "markupsafe" (list (effect-rule "markupsafe" 'pure)))
   (effect-library "dotenv" (list (effect-rule "dotenv" 'io)))
   ;; Requests: sessions/adapters/the top-level API send and receive HTTP;
   ;; cookies, structures, hooks, auth preparation and compatibility shims are
   ;; in-memory. `requests.models` is deliberately UNTAGGED: its data objects
   ;; mix pure body parsing (`Response.json`) with stream consumption
   ;; (`Response.iter_content`), which the receiver-TYPE table types precisely.
   (effect-library "requests"
     (list (effect-rule "requests.sessions" 'network)
           (effect-rule "requests.adapters" 'network)
           (effect-rule "requests.api" 'network)
           (effect-rule "requests.utils" 'io)
           (effect-rule "requests.cookies" 'pure)
           (effect-rule "requests.structures" 'pure)
           (effect-rule "requests.hooks" 'pure)
           (effect-rule "requests.exceptions" 'pure)
           (effect-rule "requests.status_codes" 'pure)
           (effect-rule "requests.compat" 'pure)
           (effect-rule "requests._internal_utils" 'pure)
           (effect-rule "requests._types" 'pure)
           (effect-rule "requests.auth" 'pure)
           (effect-rule "requests.certs" 'pure)
           ;; The public re-export surface (`from requests import Session`).
           ;; Sessions are the HTTP client; Request/PreparedRequest are
           ;; in-memory data objects.
           (effect-rule "requests.Session" 'network)
           (effect-rule "requests.Request" 'pure)
           (effect-rule "requests.PreparedRequest" 'pure)
           (effect-rule "requests.packages" 'pure)
           (effect-rule "requests.help" 'pure)))
   ;; urllib3 (Requests' transport): pools/connections/responses are network;
   ;; its configuration/value utilities are in-memory.
   (effect-library "urllib3"
     (list (effect-rule "urllib3.connectionpool" 'network)
           (effect-rule "urllib3.connection" 'network)
           (effect-rule "urllib3.poolmanager" 'network)
           (effect-rule "urllib3.response" 'network)
           (effect-rule "urllib3.util" 'pure)
           (effect-rule "urllib3" 'network)))
   ;; Requests' encoding/identity dependencies: pure in-memory transforms.
   (effect-library "charset_normalizer" (list (effect-rule "charset_normalizer" 'pure)))
   (effect-library "idna" (list (effect-rule "idna" 'pure)))
   (effect-library "certifi" (list (effect-rule "certifi" 'pure)))
   ;; ── generic Python helpers the frameworks import ──
   ;; These stdlib modules are pure in-memory value transforms; their function
   ;; handles are otherwise unresolved when imported from an unscanned module
   ;; (`from typing import cast`, `from urllib.parse import urlparse`).
   (effect-library "typing" (list (effect-rule "typing" 'pure)))
   (effect-library "functools" (list (effect-rule "functools" 'pure)))
   (effect-library "platform" (list (effect-rule "platform" 'pure)))
   (effect-library "urllib"
     (list (effect-rule "urllib.parse" 'pure)
           (effect-rule "urllib.request" 'network)))))

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

;; ── the remaining domains' primitive-operation vocabulary ───────────
;; The db/io tables above are per-domain verb sets because their typed ops are
;; the two Django/standard-stream families the corpus was built on. Other
;; domains are typed by the same mechanism, with the vocabulary as DATA here:
;; `network.get` → network<Session>.read, `network.post` →
;; network<Session>.write, and a neutral verb keeps its own name
;; (`network.request` → network<Session>.request). A domain absent from this
;; hash keeps the coarse `ops=()` capability (the domain is known, the
;; operation is not).
(define network-read-verbs
  (set "get" "head" "options" "read" "recv" "recvfrom"))
(define network-write-verbs
  (set "post" "put" "patch" "delete" "send" "sendall" "write" "writelines"))
(define network-other-verbs
  (set "request" "open" "close" "connect" "disconnect" "stream"
       "run" "test_client" "dispatch_request" "wsgi_app"))

;; Domain → (method name → operation segment) for the domains whose vocabulary
;; lives here. The db/io sets keep their dedicated fields for backward
;; compatibility (their public bindings are part of the lexicon contract).
(define (verbs->typed-ops read-verbs write-verbs other-verbs)
  (hash-union (for/hash ([v (in-set read-verbs)]) (values v "read"))
              (for/hash ([v (in-set write-verbs)]) (values v "write"))
              (for/hash ([v (in-set other-verbs)]) (values v v))
              #:combine (λ (a b) a)))

(define domain-op-vocabulary
  (hash 'network (verbs->typed-ops network-read-verbs network-write-verbs
                                   network-other-verbs)))
