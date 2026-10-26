#lang racket

(require rackunit
         racklr/uir
         racklr/effects/imports)

;; ── helpers ─────────────────────────────────────────────────────────

(define (tok type value) (cons type value))
(define (toks . xs) xs)

(define (cls name super methods)
  (uir-class (uir-symbol name) (if super (list (uir-var (uir-symbol super))) '())
             '() methods))
(define (mth name)
  (uir-method (uir-symbol name) '() (uir-null) 'public))

(define (fn-def name)
  (uir-fn (uir-symbol name) '() (uir-null) #f #f))

(define (prog . stmts) (uir-block stmts))

;; ── import extraction ───────────────────────────────────────────────

(define (test-extract-import-name)
  (define recs (extract-imports
                (list (tok 'NEWLINE "\n")
                      (tok 'IMPORT "import") (tok 'NAME "django")
                      (tok 'DOT ".") (tok 'NAME "db")
                      (tok 'DOT ".") (tok 'NAME "models")
                      (tok 'NEWLINE "\n"))))
  (check-equal? (length recs) 1)
  (define r (first recs))
  (check-equal? (import-record-form r) 'import)
  (check-equal? (import-record-source r) "django.db.models")
  (check-equal? (import-record-bindings r) (list (cons "django" "django"))))

(define (test-extract-import-alias)
  (define recs (extract-imports
                (list (tok 'IMPORT "import") (tok 'NAME "django")
                      (tok 'DOT ".") (tok 'NAME "db")
                      (tok 'DOT ".") (tok 'NAME "models")
                      (tok 'AS "as") (tok 'NAME "models")
                      (tok 'NEWLINE "\n"))))
  (define r (first recs))
  (check-equal? (import-record-bindings r) (list (cons "models" "models"))))

(define (test-extract-import-from-named)
  (define recs (extract-imports
                (list (tok 'FROM "from") (tok 'NAME "django")
                      (tok 'DOT ".") (tok 'NAME "db")
                      (tok 'DOT ".") (tok 'NAME "models")
                      (tok 'IMPORT "import") (tok 'NAME "QuerySet")
                      (tok 'COMMA ",") (tok 'NAME "Count")
                      (tok 'AS "as") (tok 'NAME "C")
                      (tok 'NEWLINE "\n"))))
  (define r (first recs))
  (check-equal? (import-record-form r) 'from)
  (check-equal? (import-record-source r) "django.db.models")
  (check-equal? (import-record-bindings r)
                (list (cons "QuerySet" "django.db.models.QuerySet")
                      (cons "C" "django.db.models.Count"))))

(define (test-extract-import-from-star)
  (define recs (extract-imports
                (list (tok 'FROM "from") (tok 'NAME "django")
                      (tok 'DOT ".") (tok 'NAME "db")
                      (tok 'IMPORT "import") (tok 'STAR "*")
                      (tok 'NEWLINE "\n"))))
  (define r (first recs))
  (check-equal? (import-record-bindings r) (list (cons "*" "django.db"))))

(define (test-extract-import-relative)
  (define recs (extract-imports
                (list (tok 'FROM "from") (tok 'DOT ".") (tok 'NAME "base")
                      (tok 'IMPORT "import") (tok 'NAME "run")
                      (tok 'NEWLINE "\n"))))
  (define r (first recs))
  (check-equal? (import-record-source r) ".base")
  (check-equal? (import-record-bindings r) (list (cons "run" ".base.run"))))

(define (test-extract-import-from-dot-only)
  (define recs (extract-imports
                (list (tok 'FROM "from") (tok 'DOT ".")
                      (tok 'IMPORT "import") (tok 'NAME "x")
                      (tok 'NEWLINE "\n"))))
  (define r (first recs))
  (check-equal? (import-record-source r) ".")
  (check-equal? (import-record-bindings r) (list (cons "x" ".x"))))

(define (test-extract-import-multiple-statements)
  (define recs (extract-imports
                (list (tok 'IMPORT "import") (tok 'NAME "os")
                      (tok 'NEWLINE "\n")
                      (tok 'IMPORT "import") (tok 'NAME "sys")
                      (tok 'AS "as") (tok 'NAME "s")
                      (tok 'NEWLINE "\n"))))
  (check-equal? (length recs) 2)
  (check-equal? (import-record-source (first recs)) "os")
  (check-equal? (import-record-source (second recs)) "sys")
  (check-equal? (import-record-bindings (second recs)) (list (cons "s" "s"))))

;; ── harvest ─────────────────────────────────────────────────────────

(define (test-harvest-classes-and-functions)
  (define mi (harvest-module "app" (prog (cls "A" #f (list (mth "m")))
                                          (fn-def "helper")) '()))
  (check-true (hash-has-key? (module-info-classes mi) "A"))
  (check-true (hash-has-key? (module-info-functions mi) "helper"))
  (check-true (hash-has-key? (class-info-methods
                              (hash-ref (module-info-classes mi) "A")) "m")))

(define (test-harvest-super-name)
  (define mi (harvest-module "app" (prog (cls "A" "Base" (list (mth "m")))) '()))
  (check-equal? (class-info-super (hash-ref (module-info-classes mi) "A")) '("Base")))

;; ── module-path resolution ──────────────────────────────────────────

(define (test-resolve-absolute)
  (check-equal? (resolve-module-path "django.db.models" "app" (hash "django.db.models" #t) (set))
                "django.db.models"))

(define (test-resolve-relative-sibling)
  (define mods (hash "app" #t "app.base" #t))
  (check-equal? (resolve-module-path ".base" "app" mods (set "app")) "app.base"))

(define (test-resolve-relative-parent)
  ;; Python relative-import semantics: from `pkg.sub` (a module in package
  ;; `pkg`), one leading dot = package `pkg`, two = top level.
  (define mods (hash "util" #t "pkg.util" #t "pkg.sub" #t))
  (check-equal? (resolve-module-path ".util" "pkg.sub" mods (set "pkg")) "pkg.util")
  (check-equal? (resolve-module-path "..util" "pkg.sub" mods (set "pkg")) "util"))

;; ── export resolution + tagging ─────────────────────────────────────

(define (test-resolve-exports-import-class)
  (define mods
    (hash "django.db.models"
          (harvest-module "django.db.models" (prog (cls "QuerySet" #f (list (mth "get")))) '())
          "app"
          (harvest-module "app" (prog) (list (import-record 'from "django.db.models"
                                                            (list (cons "QuerySet" "django.db.models.QuerySet")))))))
  (define ex (resolve-exports mods (set "django.db.models" "app")))
  (define h (hash-ref (hash-ref ex "app") "QuerySet"))
  (check-equal? (handle-kind h) 'class)
  (check-equal? (handle-path h) "django.db.models")
  (check-equal? (handle-name h) "QuerySet"))

(define (test-resolve-exports-transitive)
  ;; a imports from b; b re-exports from c. a's binding resolves transitively.
  (define mods
    (hash "c" (harvest-module "c" (prog (cls "Thing" #f '())) '())
          "b" (harvest-module "b" (prog) (list (import-record 'from "c" (list (cons "Thing" "c.Thing")))))
          "a" (harvest-module "a" (prog) (list (import-record 'from "b" (list (cons "Thing" "b.Thing")))))))
  (define ex (resolve-exports mods (set "c" "b" "a")))
  (define h (hash-ref (hash-ref ex "a") "Thing"))
  (check-equal? (handle-path h) "c"))

(define (test-resolve-exports-external-class)
  ;; `from django.db.migrations.loader import MigrationLoader` where that
  ;; module is OUTSIDE the scanned corpus: synthesize a class handle from the
  ;; PEP 8 Capitalized binding so imported-class-ref / imported-class-cap can
  ;; still resolve it through the library config (django.db → db). A lowercase
  ;; external binding (`connections`) synthesizes an fn handle so a bare call
  ;; resolves to the module's effect domain — never a receiver TYPE, since
  ;; imported-class-ref requires kind 'class.
  (define mods
    (hash "app"
          (harvest-module "app" (prog)
                          (list (import-record 'from "django.db.migrations.loader"
                                               (list (cons "MigrationLoader" "django.db.migrations.loader.MigrationLoader")))
                                (import-record 'from "django.db"
                                               (list (cons "connections" "django.db.connections")))))))
  (define ex (resolve-exports mods (set "app")))
  (define e (hash-ref ex "app"))
  (define h (hash-ref e "MigrationLoader"))
  (check-equal? (handle-kind h) 'class)
  (check-equal? (handle-path h) "django.db.migrations.loader")
  (check-equal? (handle-name h) "MigrationLoader")
  (define c (hash-ref e "connections"))
  (check-equal? (handle-kind c) 'fn)
  (check-equal? (handle-path c) "django.db")
  (check-equal? (handle-name c) "connections"))

(define (test-path-to-module-path)
  (define root (string->path "/r/django"))
  (check-equal? (path->module-path "django" root (string->path "/r/django/__init__.py"))
                "django")
  (check-equal? (path->module-path "django" root (string->path "/r/django/db/__init__.py"))
                "django.db")
  (check-equal? (path->module-path "django" root (string->path "/r/django/db/models/query.py"))
                "django.db.models.query")
  (check-equal? (path->module-path "asgiref" (string->path "/s/asgiref")
                                   (string->path "/s/asgiref/sync.py"))
                "asgiref.sync"))

(define (test-module-effect-tag)
  (check-equal? (module-effect-tag "django.db.models.query") 'db)
  (check-equal? (module-effect-tag "django.http.response") 'io)
  (check-equal? (module-effect-tag "django.core.mail.message") 'email)
  (check-equal? (module-effect-tag "django.core.cache.backends.base") 'cache)
  (check-equal? (module-effect-tag "os.path") #f))

(define (test-effect-tag-config)
  ;; sqlparse is mostly pure, but its CLI entry point does real I/O.
  (check-equal? (module-effect-tag "sqlparse.lexer") 'pure)
  (check-equal? (module-effect-tag "sqlparse.cli") 'io)
  ;; tzdata is version constants + data — definitely effect-free.
  (check-equal? (module-effect-tag "tzdata.zoneinfo") 'pure)
  ;; an unconfigured library stays untagged.
  (check-equal? (module-effect-tag "requests.api") #f)
  ;; the generic resolver honors a custom config independent of the default.
  (define cfg
    (list (effect-library "foo" (list (effect-rule "foo.bar" 'llm)))))
  (check-equal? (resolve-effect-tag cfg "foo.bar.baz") 'llm)
  (check-equal? (resolve-effect-tag cfg "foo.qux") #f)
  ;; rules are most-specific-first within a library.
  (define cfg2
    (list (effect-library "x" (list (effect-rule "x.a.b" 'io)
                                    (effect-rule "x.a" 'db)))))
  (check-equal? (resolve-effect-tag cfg2 "x.a.b.c") 'io)
  (check-equal? (resolve-effect-tag cfg2 "x.a.z") 'db))

(define (test-injectable-effect-tag-config)
  ;; module-effect-tag honors an injected config instead of the default.
  (define cfg (list (effect-library "foo" (list (effect-rule "foo.bar" 'llm)))))
  (check-equal? (module-effect-tag "foo.bar.baz" cfg) 'llm)
  (check-equal? (module-effect-tag "foo.qux" cfg) #f)
  ;; derive-class-method-effects honors an injected config.
  (define mods
    (hash "foo.bar" (harvest-module "foo.bar" (prog (cls "Thing" #f (list (mth "m")))) '())))
  (check-equal? (hash-ref (derive-class-method-effects mods cfg) "Thing.m")
                (effect-cap 'llm '())))

(define (test-derive-class-method-effects)
  (define mods
    (hash "django.db.models" (harvest-module "django.db.models"
                                             (prog (cls "QuerySet" #f
                                                        (list (mth "get") (mth "filter") (mth "update") (mth "bulk_create")))) '())))
  (define eff (derive-class-method-effects mods))
  ;; operation-level, not coarse: read verbs vs write verbs, TYPED by receiver.
  (check-equal? (hash-ref eff "QuerySet.get") (effect-cap 'db '(db<QuerySet>.read)))
  (check-equal? (hash-ref eff "QuerySet.filter") (effect-cap 'db '(db<QuerySet>.read)))
  (check-equal? (hash-ref eff "QuerySet.update") (effect-cap 'db '(db<QuerySet>.write)))
  (check-equal? (hash-ref eff "QuerySet.bulk_create") (effect-cap 'db '(db<QuerySet>.write)))
  ;; an unclassified db verb stays coarse within the domain (sound, not silent).
  (define mods2
    (hash "django.db.models" (harvest-module "django.db.models"
                                             (prog (cls "QuerySet" #f (list (mth "as_manager")))) '())))
  (check-equal? (hash-ref (derive-class-method-effects mods2) "QuerySet.as_manager")
                (effect-cap 'db '())))

(define (test-io-operation-decomposition)
  ;; Same domain (io), different resource + verb from the class's module path.
  (define mods
    (hash "django.core.files.base" (harvest-module "django.core.files.base"
                                                   (prog (cls "File" #f (list (mth "read") (mth "write")))) '())
          "django.http.response" (harvest-module "django.http.response"
                                                 (prog (cls "HttpResponse" #f (list (mth "write")))) '())
          "django.core.management.base" (harvest-module "django.core.management.base"
                                                        (prog (cls "OutputWrapper" #f (list (mth "write") (mth "flush")))) '())))
  (define eff (derive-class-method-effects mods))
  (check-equal? (hash-ref eff "File.write") (effect-cap 'io '(io<File>.write)))
  (check-equal? (hash-ref eff "File.read") (effect-cap 'io '(io<File>.read)))
  (check-equal? (hash-ref eff "HttpResponse.write") (effect-cap 'io '(io<HttpResponse>.write)))
  (check-equal? (hash-ref eff "OutputWrapper.write") (effect-cap 'io '(io<OutputWrapper>.write)))
  (check-equal? (hash-ref eff "OutputWrapper.flush") (effect-cap 'io '(io<OutputWrapper>.flush))))

(define (test-collision-resolves-by-class)
  ;; Same method name `get`, different class, different effect: the class is
  ;; what decides. builtin `dict.get` is pure; `QuerySet.get` is (db read).
  (check-true (set-member? (hash-ref builtin-class-methods "dict") "get"))
  (define mods (hash "django.db.models"
                     (harvest-module "django.db.models" (prog (cls "QuerySet" #f (list (mth "get")))) '())))
  (define eff (derive-class-method-effects mods))
  (check-equal? (hash-ref eff "QuerySet.get") (effect-cap 'db '(db<QuerySet>.read))))

(define (test-typed-op-parts)
  (check-equal? (typed-op-parts 'db<QuerySet>.read) '(db "QuerySet" "read"))
  (check-equal? (typed-op-parts 'io<OutputWrapper>.flush) '(io "OutputWrapper" "flush"))
  (check-false (typed-op-parts 'db.read))
  (check-false (typed-op-parts 'io))
  (check-false (typed-op-parts 'pure)))

(define (test-derive-class-supers)
  (define mods
    (hash "django.db.models" (harvest-module "django.db.models"
                                              (prog (cls "QuerySet" "BaseQuerySet" (list (mth "get")))) '())))
  (check-equal? (hash-ref (derive-class-supers mods) "QuerySet") '("BaseQuerySet")))

(define (test-handle-effect-domain)
  ;; Re-export follow: `from django.utils import autoreload` synthesizes an fn
  ;; handle whose path is the package, but the object lives in the submodule
  ;; that already carries the domain rule.
  (check-equal? (handle-effect-domain "django.utils" "autoreload" "run_with_reloader") 'io)
  (check-equal? (handle-effect-domain "django.utils" "termcolors" "make_style") 'pure)
  (check-equal? (handle-effect-domain "django.utils" "archive" "extract") 'io)
  ;; Per-object method rules: `settings` is a lazy registry (pure queries, io
  ;; setup); the most-specific `_setup` rule wins over the general `settings`.
  (check-equal? (handle-effect-domain "django.conf" "settings" "configure") 'pure)
  (check-equal? (handle-effect-domain "django.conf" "settings" "is_overridden") 'pure)
  (check-equal? (handle-effect-domain "django.conf" "settings" "_setup") 'io)
  ;; Module-handle member rules (name is #f): `from django.core import checks`.
  (check-equal? (handle-effect-domain "django.core.checks" #f "run_checks") 'io)
  (check-equal? (handle-effect-domain "django.core.checks" #f "tag_exists") 'pure)
  ;; External package granularity: Parser is in-memory, Image.init loads
  ;; plugins; asgiref.sync wrapper constructors are pure.
  (check-equal? (handle-effect-domain "PIL" "ImageFile" "Parser") 'pure)
  (check-equal? (handle-effect-domain "PIL" "Image" "init") 'io)
  (check-equal? (handle-effect-domain "asgiref.sync" "async_to_sync" "async_to_sync") 'pure)
  ;; The coarse module domain is still the fallback when no member rule fires.
  (check-equal? (handle-effect-domain "django.db" "connections" "cursor") 'db))

(module+ main
  (test-extract-import-name)
  (test-extract-import-alias)
  (test-extract-import-from-named)
  (test-extract-import-from-star)
  (test-extract-import-relative)
  (test-extract-import-from-dot-only)
  (test-extract-import-multiple-statements)
  (test-harvest-classes-and-functions)
  (test-harvest-super-name)
  (test-resolve-absolute)
  (test-resolve-relative-sibling)
  (test-resolve-relative-parent)
  (test-resolve-exports-import-class)
  (test-resolve-exports-transitive)
  (test-resolve-exports-external-class)
  (test-path-to-module-path)
  (test-module-effect-tag)
  (test-effect-tag-config)
  (test-injectable-effect-tag-config)
  (test-derive-class-method-effects)
  (test-io-operation-decomposition)
  (test-collision-resolves-by-class)
  (test-typed-op-parts)
  (test-derive-class-supers)
  (test-handle-effect-domain)
  (displayln "All imports tests passed."))
