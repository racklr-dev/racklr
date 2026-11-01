#lang racket

;; racklr-test/effects-non-django-python-test.rkt
;;
;; The non-Django Python half of the effect contract, as a gate:
;;
;;   1. The import graph canonicalizes a module by its PACKAGE CHAIN, not by
;;      the scan root, so a repository checkout whose package lives under
;;      `src/` (`…/flask/src/flask/app.py`) resolves as `flask.app` and the
;;      corpus's own imports resolve. The pinned Django baseline is unaffected
;;      (the chain for a package-root scan is the same dotted name).
;;   2. The Python lexicon carries the non-Django web frameworks as DATA:
;;      Flask/Requests/Werkzeug/Click/Jinja2 module rules with a documented
;;      fail-closed split, so calls into them are classified instead of
;;      `unknown`.
;;   3. The Network operation vocabulary types their capabilities: a
;;      `Session.get(...)` reached through receiver-type dataflow is
;;      `network<Session>.read`, not the coarse `ops=()` entry.
;;   4. Real source using the frameworks flows through the whole import/effect
;;      pipeline to the same typed rows (parse → lower → import graph →
;;      fixpoint), not just a hand-built environment.

(require rackunit
         racket/file
         racket/path
         racket/string
         racklr/uir
         racklr/effects/contract
         racklr/effects/imports
         racklr/effects/infer
         racklr/effects/lexicon
         racklr/effects/library-tokens
         racklr/effects/lexicons/python
         racklr/effects/lexicons/javascript
         "../tools/sha256.rkt"
         effervesce/check/host-python)

;; ── helpers ─────────────────────────────────────────────────────────

(define (sym s) (uir-symbol s))
(define (v s) (uir-var (uir-symbol s)))
(define (dot b f) (uir-get b (uir-symbol f)))
(define (fn name body) (uir-fn (uir-symbol name) '() body #f #f))
(define (prog . stmts) (uir-block stmts))
(define (py-method name body) (uir-method (uir-symbol name) '() body 'public))
(define (py-class name methods) (uir-class (uir-symbol name) '() '() methods))

(define (row-lookup row name)
  (for/first ([e (in-list (uir-effect-row-entries row))]
              #:when (eq? (string->symbol (uir-symbol-name (uir-effect-entry-name e)))
                          name))
    e))
(define (row-names row)
  (sort (map (λ (e) (string->symbol (uir-symbol-name (uir-effect-entry-name e))))
             (uir-effect-row-entries row))
        symbol<?))
(define (entry-ops entry) (map (λ (o) (string->symbol (uir-symbol-name o)))
                               (uir-effect-entry-ops entry)))

;; ── 1. module path = package chain, not scan root ───────────────────

;; Build a throw-away repository checkout: a `src/`-layout package plus a
;; package-root tree, a docs tree and a loose script.
(define (make-corpus)
  (define root (make-temporary-file "corpus~a" 'directory))
  (define (make-file rel [text ""])
    (define p (build-path root rel))
    (make-directory* (path-only p))
    (call-with-output-file p (λ (o) (display text o)))
    p)
  (make-file "src/pkg/__init__.py")
  (make-file "src/pkg/mod.py")
  (make-file "src/pkg/sub/__init__.py")
  (make-file "src/pkg/sub/deep.py")
  (make-file "docs/conf.py")
  (make-file "scripts/tool.py")
  (make-file "pkgroot/__init__.py")
  (make-file "pkgroot/db/__init__.py")
  (make-file "pkgroot/db/models.py")
  (values root (λ (rel) (build-path root rel))))

(define (test-module-path-uses-package-chain)
  (define-values (root at) (make-corpus))
  ;; A repository root is not a package: the canonical name comes from the
  ;; package chain under it (`src/pkg` → pkg), so the corpus's own absolute
  ;; imports (`import pkg.mod`) resolve.
  (check-equal? (path->module-path "flask" root (at "src/pkg/__init__.py"))
                "pkg")
  (check-equal? (path->module-path "flask" root (at "src/pkg/mod.py"))
                "pkg.mod")
  (check-equal? (path->module-path "flask" root (at "src/pkg/sub/deep.py"))
                "pkg.sub.deep")
  ;; A package root that IS the scanned root keeps the historical dotted name,
  ;; which is what the pinned Django baseline depends on.
  (check-equal? (path->module-path "pkgroot" (at "pkgroot")
                                   (at "pkgroot/db/models.py"))
                "pkgroot.db.models")
  ;; A file outside every package keeps the root-relative fallback name.
  (check-equal? (path->module-path "flask" root (at "docs/conf.py"))
                "flask.docs.conf")
  (check-equal? (path->module-path "scripts" (at "scripts") (at "scripts/tool.py"))
                "scripts.tool")
  (delete-directory/files root))

;; ── 2. the framework lexicon data ───────────────────────────────────

(define (test-framework-import-rules)
  ;; Flask: network lifecycle, io helpers/streams, pure registries.
  (check-equal? (module-effect-tag "flask.app") 'network)
  (check-equal? (module-effect-tag "flask.testing") 'network)
  (check-equal? (module-effect-tag "flask.helpers") 'io)
  (check-equal? (module-effect-tag "flask.templating") 'io)
  (check-equal? (module-effect-tag "flask.sessions") 'pure)
  (check-equal? (module-effect-tag "flask.globals") 'pure)
  ;; Werkzeug: server/test client are network, stream plumbing io, data pure.
  (check-equal? (module-effect-tag "werkzeug.serving") 'network)
  (check-equal? (module-effect-tag "werkzeug.wsgi") 'io)
  (check-equal? (module-effect-tag "werkzeug.urls") 'pure)
  ;; Click: terminal output is io, decorators/parameter types pure.
  (check-equal? (module-effect-tag "click.echo") 'io)
  (check-equal? (module-effect-tag "click.secho") 'io)
  (check-equal? (module-effect-tag "click.decorators") 'pure)
  ;; Jinja2: template loading is io, the engine surface pure.
  (check-equal? (module-effect-tag "jinja2.loaders") 'io)
  (check-equal? (module-effect-tag "jinja2.Environment.get_template") 'io)
  (check-equal? (module-effect-tag "jinja2.runtime") 'pure)
  ;; Requests: transport modules network, in-memory structures pure, and the
  ;; mixed data-object module deliberately untagged (the receiver-TYPE table
  ;; types Response body parsing vs stream consumption precisely).
  (check-equal? (module-effect-tag "requests.sessions") 'network)
  (check-equal? (module-effect-tag "requests.adapters") 'network)
  (check-equal? (module-effect-tag "requests.cookies") 'pure)
  (check-equal? (module-effect-tag "requests.models") #f)
  ;; Requests' dependencies.
  (check-equal? (module-effect-tag "urllib3.connectionpool") 'network)
  (check-equal? (module-effect-tag "urllib3.util.retry") 'pure)
  (check-equal? (module-effect-tag "idna") 'pure)
  ;; The public re-export surface of a package that is not itself scanned:
  ;; `from flask import Flask` / `from requests import Session` resolve.
  (check-equal? (module-effect-tag "flask.Flask") 'network)
  (check-equal? (module-effect-tag "requests.Session") 'network))

;; ── 3. typed network operations ─────────────────────────────────────

(define (test-network-vocabulary-typed-ops)
  ;; The vocabulary is injected data, reachable through the lexicon.
  (check-equal? (method-ops 'network "requests.sessions" "Session" "get")
                '(network<Session>.read))
  (check-equal? (method-ops 'network "requests.sessions" "Session" "post")
                '(network<Session>.write))
  (check-equal? (method-ops 'network "requests.sessions" "Session" "request")
                '(network<Session>.request))
  ;; A method with no vocabulary entry keeps the coarse capability.
  (check-equal? (method-ops 'network "requests.sessions" "Session"
                            "prepare_request")
                '())
  ;; The JavaScript host supplies no network vocabulary, so the same call kind
  ;; stays coarse there (the injection seam is real, not Python-hardcoded).
  (check-equal? (method-ops 'network "requests.sessions" "Session" "get"
                            javascript-lexicon)
                '()))

;; ── 4. receiver-type dataflow reaches the typed op ──────────────────

(define (test-typed-op-through-receiver-dataflow)
  ;; requests.sessions.Session.get with a locally-constructed receiver: the
  ;; tracked class resolves to requests.sessions (a network-tagged module), and
  ;; the network vocabulary types the operation.
  (define sessions
    (prog (py-class "Session"
                    (list (py-method "get" (uir-null))
                          (py-method "post" (uir-null))
                          (py-method "prepare_request" (uir-null))))))
  (define app
    (prog (fn "fetch"
              (uir-block
               (list (uir-set! (v "s") (uir-call (v "Session") '()))
                     (uir-call (dot (v "s") "get") (list (uir-string "https://x"))))))))
  (define env
    (import-env (hash "sessions.py" "requests.sessions"
                      "app.py" "app")
                (hash) (hash)))
  (define rows (infer-effects-all (list (cons "sessions.py" sessions)
                                        (cons "app.py" app))
                                  #:import-env env))
  (define r (hash-ref rows (cons "app.py" 'fetch)))
  (check-equal? (row-names r) '(network))
  (check-equal? (entry-ops (row-lookup r 'network)) '(network<Session>.read)))

(define (test-flask-typed-op)
  ;; Same dataflow through a Flask application object: the lifecycle method is
  ;; typed `network<Flask>.wsgi_app` from the vocabulary, not a bare `network`.
  (define flask-app
    (prog (py-class "Flask"
                    (list (py-method "wsgi_app" (uir-null))
                          (py-method "run" (uir-null))))))
  (define app
    (prog (fn "serve"
              (uir-block
               (list (uir-set! (v "app") (uir-call (v "Flask") '()))
                     (uir-call (dot (v "app") "wsgi_app") '()))))))
  (define env
    (import-env (hash "app.py" "flask.app" "user.py" "user")
                (hash) (hash)))
  (define r (hash-ref (infer-effects-all (list (cons "app.py" flask-app)
                                               (cons "user.py" app))
                                         #:import-env env)
                      (cons "user.py" 'serve)))
  (check-equal? (row-names r) '(network))
  (check-equal? (entry-ops (row-lookup r 'network)) '(network<Flask>.wsgi_app)))

;; ── 5. real source through the whole pipeline ───────────────────────

(define (pipeline-rows source)
  (define uir (python-source->uir source))
  (check-true (and uir (uir-block? uir)) "snippet parses and lowers")
  (define tokens (py-tokenize source))
  (define imports (extract-imports (for/list ([t (in-list tokens)])
                                     (cons (py-tok-type t) (py-tok-value t)))))
  (define mods (build-module-table (list (module-spec "main" uir imports))))
  (define exports (resolve-exports mods (set)))
  (define env (import-env (hash "main.py" "main") exports
                          (derive-class-method-effects mods)))
  (infer-effects-all (list (cons "main.py" uir)) #:import-env env))

(define (test-real-source-frameworks)
  ;; A small program in the style of the reported corpus: an imported Flask
  ;; application object and an imported Requests session. Both are outside the
  ;; scanned corpus, so the import graph's synthesized handles plus the DATA
  ;; rules must carry the row.
  (define rows
    (pipeline-rows
     (string-append
      "from flask import Flask\n"
      "from requests.sessions import Session\n"
      "\n"
      "def serve():\n"
      "    app = Flask('demo')\n"
      "    return app.run()\n"
      "\n"
      "def fetch():\n"
      "    session = Session()\n"
      "    return session.get('https://example.com')\n")))
  (check-equal? (row-names (hash-ref rows (cons "main.py" 'serve)))
                '(network))
  (check-equal? (entry-ops (row-lookup (hash-ref rows (cons "main.py" 'serve))
                                       'network))
                '(network<Flask>.run))
  (check-equal? (row-names (hash-ref rows (cons "main.py" 'fetch)))
                '(network))
  (check-equal? (entry-ops (row-lookup (hash-ref rows (cons "main.py" 'fetch))
                                       'network))
                '(network<Session>.read)))

;; ── 6. the shared Django path stays pinned ──────────────────────────
;;
;; `path->module-path` is shared with the Django scan, so the package-chain walk
;; must resolve EVERY module of the pinned corpus to exactly the name the
;; historical root-relative derivation produced; otherwise
;; `baseline/type-graph.sha256` moves and a real regression could hide as a
;; re-pin. The full end-to-end hash check remains
;; `racket tools/baseline-check.rkt` (it runs type-graph over Django); this pins
;; the module-path half of that invariant cheaply, and carries the committed
;; content address so a silent snapshot re-pin is visible in review.

(define (historical-path->module-path root-name root-dir p)
  ;; The derivation before the package-chain change, kept verbatim as the
  ;; reference oracle.
  (define rel (path->string (find-relative-path root-dir p)))
  (define comps (map path->string (explode-path (string->path rel))))
  (define comps-clean (filter (λ (c) (not (string=? c "."))) comps))
  (define comps-no-init
    (cond
      [(null? comps-clean) '()]
      [(string=? (last comps-clean) "__init__.py") (drop-right comps-clean 1)]
      [else comps-clean]))
  (define base
    (if (null? comps-no-init)
        '()
        (let ([lastc (last comps-no-init)])
          (append (drop-right comps-no-init 1)
                  (list (regexp-replace #rx"[.]py$" lastc ""))))))
  (string-join (cons root-name base) "."))

(define (py-files-under dir)
  (define out '())
  (let loop ([d dir])
    (for ([e (in-list (directory-list d))])
      (define p (build-path d e))
      (cond
        [(directory-exists? p) (loop p)]
        [(regexp-match? #rx"[.]py$" (path->string e)) (set! out (cons p out))]
        [else (void)])))
  out)

(define (test-django-baseline-module-paths-unchanged)
  ;; Pin the exact property the shared resolve.rkt change could move: the
  ;; module-NAME projection of the pinned Django corpus. The full graph hash is
  ;; the baseline gate's job (`racket tools/baseline-check.rkt`, verified
  ;; bit-identical before/after this change); this digest is scoped to the
  ;; module paths, so an unrelated deliberate re-pin does not invalidate it,
  ;; while a single changed Django module name does.
  (define corpus-root
    (simplify-path (path->complete-path (build-path ".." "django" "django" "core")) #f))
  (cond
    [(directory-exists? corpus-root)
     (define files (sort (py-files-under corpus-root)
                         (λ (a b) (string<? (path->string a) (path->string b)))))
     (check-equal? (length files) 112 "the pinned django core corpus file count")
     (for ([f (in-list files)])
       (check-equal? (path->module-path "django.core" corpus-root f)
                     (historical-path->module-path "django.core" corpus-root f)
                     (path->string f)))
     (define projection
       (for/list ([f (in-list files)])
         (format "~a\t~a"
                 (path->string (find-relative-path corpus-root f))
                 (path->module-path "django.core" corpus-root f))))
     (check-equal? (sha256-hex (string->bytes/utf-8 (string-join projection "\n")))
                   "eb9812292ab8d45bf74c7e5257ead93fdce499d7af2791e020550041651df97b"
                   "django module-name projection digest")]
    [else
     ;; The django submodule is not initialized for this checkout; the baseline
     ;; gate itself always requires it, so this is an explicit environment skip.
     (printf "SKIP django module-path guard: ../django submodule not initialized\n")]))

;; ── run ─────────────────────────────────────────────────────────────

(module+ main
  (test-module-path-uses-package-chain)
  (test-framework-import-rules)
  (test-network-vocabulary-typed-ops)
  (test-typed-op-through-receiver-dataflow)
  (test-flask-typed-op)
  (test-real-source-frameworks)
  (test-django-baseline-module-paths-unchanged)
  (displayln "All non-Django Python effect tests passed."))
