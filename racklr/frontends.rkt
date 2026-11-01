#lang racket

;; racklr/frontends.rkt — the front-end registry.
;;
;; A front end is DATA, not a code edit. A spec value names the front end's
;; ANTLR4 grammar, its CST → UIR lowering hook, and its token lexicon; it
;; lives in the sibling `frontends/` directory. This module discovers specs by
;; listing that directory, so adding a language adds one spec file and never
;; edits this module or any engine module.
;;
;; `front-end-lower` is a procedure of the shape every existing lowerer has:
;;   (lower cst tk-type tk-value) → uir?
;; `front-end-lexicon` is opaque data owned by the spec: the language's
;; effect-token lexicon in whatever shape the language needs. Note that the
;; classifier currently exposes injection only at its leaves
;; (`direct-effect-of-callee`, `pure-stdlib-*`, ...); whole-pipeline
;; `infer-effects`/`classify-nonedge` injection is a separate engine change.
;;
;; Discovery status: NO production driver consumes `discover-front-ends` yet.
;; It is the discovery surface for future wiring and is exercised by the
;; front-end acceptance suites; today's parser/lowering/emit code is reached
;; through each front end's own module, not through this registry.
;;
;; Packaging assumption: discovery lists `racklr/frontends/` on disk and
;; `dynamic-require`s each spec by absolute source path, and it resolves the
;; grammar relative to the checkout. That is reliable in a source checkout (the
;; repo's supported mode: no CI, tests run with `PLTCOLLECTS` pointed at the
;; tree), but not under `raco distribute`/`raco exe`, which strip `.rkt`
;; sources; an installed collection also means any stray top-level `.rkt` in
;; `frontends/` would be loaded. A static spec list or a data-file manifest
;; would survive packaging and is the intended hardening if the registry gains
;; a production caller.
;;
;; The registry itself carries no language knowledge: no grammar path, no
;; lowering hook, no language name.

(require racket/file
         racket/list
         racket/path)

(provide
 front-end front-end?
 front-end-name
 front-end-grammar-path
 front-end-lower
 front-end-lexicon
 front-ends-directory
 repository-root
 front-end-grammar-file
 discover-front-ends
 load-front-end)

;; name:          symbol naming the front end (also the spec file basename)
;; grammar-path:  path string relative to the repository root
;; lower:         (lower cst tk-type tk-value) → uir?
;; lexicon:       any/c — the front end's effect-token lexicon data
(struct front-end (name grammar-path lower lexicon) #:transparent)

;; ── Locations ───────────────────────────────────────────────────────

;; Parent directory of a file-or-directory path.
(define (parent-dir p)
  (let-values ([(base name dir?) (split-path (path->directory-path p))])
    base))

;; `racklr/` directory, resolved through the collection path so a checkout
;; (not just an installed package) is enough to run the registry.
(define (racklr-directory)
  (parent-dir (collection-file-path "uir.rkt" "racklr")))

(define (front-ends-directory)
  (build-path (racklr-directory) "frontends"))

(define (repository-root)
  (parent-dir (parent-dir (collection-file-path "uir.rkt" "racklr"))))

;; ── Spec accessors with a real file path ────────────────────────────

(define (front-end-grammar-file spec)
  (build-path (repository-root) (front-end-grammar-path spec)))

;; ── Discovery ───────────────────────────────────────────────────────

(define (spec-file? path)
  (and (file-exists? path)
       (equal? (file-or-directory-type path)
               'file)
       (equal? (path-get-extension path) #".rkt")))

;; Load one spec module by absolute path. Requiring by path (instead of by
;; collection name) keeps discovery independent of whether the checkout is
;; installed as a package.
(define (load-spec-file path)
  (dynamic-require path 'front-end))

(define (load-front-end name)
  (load-spec-file (build-path (front-ends-directory)
                              (format "~a.rkt" name))))

;; All specs in `frontends/`, sorted by file name for deterministic tests.
;; A spec module provides `front-end`; nothing needs to be registered.
(define (discover-front-ends)
  (define dir (front-ends-directory))
  (define specs
    (for/list ([name (in-list (directory-list dir))]
               #:do [(define path (build-path dir name))]
               #:when (spec-file? path))
      path))
  (for/list ([path (in-list (sort specs
                                  (λ (a b) (string<? (path->string a)
                                                     (path->string b)))))])
    (load-spec-file path)))
