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
                    (list (effect-rule "django.test.client" 'network)
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
   (effect-library "signal" (list (effect-rule "signal" 'process)))))

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
