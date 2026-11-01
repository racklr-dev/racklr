#lang racket

;; tools/provenance-corpus.rkt — provenance completeness over a real Python
;; corpus (P4.2 definition of done, exercised on more than a toy program).
;;
;; Usage (from the repository root, with the submodules initialised and the
;; vendored grammars present, exactly like tools/baseline-check.rkt):
;;
;;   racket tools/provenance-corpus.rkt                 # django/django/core
;;   racket tools/provenance-corpus.rkt django/django   # explicit probe
;;   racket tools/provenance-corpus.rkt --record-only   # skip graph projection
;;
;; `--record-only` is for probes too large to project (the full django tree
;; builds ~100k atoms); it still runs the record-level reachability check, which
;; is the same property the graph projection enforces.
;;
;; It scans every .py file under the probe directories, resolves imports, runs
;; whole-program effect inference with `#:provenance-record`, and then checks:
;;
;;   1. `provenance-missing` over the record: every function/effect edge has a
;;      ground reachable along the inferred call graph;
;;   2. `provenance-graph` builds (it enforces the same property at the atom
;;      level and raises otherwise);
;;   3. a provenance query returns a call-chain path for EVERY effect edge,
;;      using one reusable graph index.
;;
;; This reads the corpus only; it never re-pins or touches baseline/. Exit 0
;; means all three checks passed; any failure exits 1 and names the edge.
;;
;; Prerequisites are the same as tools/baseline-check.rkt: the django and
;; effervesce submodules plus the vendored grammars-v4 Python3 grammar.

(require racket/file
         racket/list
         racket/string
         racklr/g4-parse
         racklr/gend-parser
         racklr/lower-python
         racklr/hypergraph
         racklr/uir
         racklr/effects/contract
         racklr/effects/imports
         racklr/effects/infer
         racklr/effects/provenance
         racklr/graph/provenance)

(define default-probe-dirs '("django/django/core"))

;; ── paths ───────────────────────────────────────────────────────────

(define (repo-root)
  (define here (simplify-path (path->complete-path (find-system-path 'run-file))))
  (simplify-path (build-path here 'up 'up)))

(define (python3-grammar root)
  (build-path root "grammars-v4" "python" "python3" "Python3Parser.g4"))

;; ── parser ──────────────────────────────────────────────────────────

;; Generate the Python3 parser once, into a temp module, and return the
;; tokenize / parse / token accessors. Parser-generation warnings are noise for
;; this report, so they are captured rather than printed.
(define (load-python-front-end root)
  (define grammar (python3-grammar root))
  (unless (file-exists? grammar)
    (error 'provenance-corpus
           (string-append
            "vendored grammar not found: ~a\n"
            "initialise the grammars-v4 clone the same way tools/baseline-check.rkt needs it")
           grammar))
  (define path (make-temporary-file "provenance-py-parser-~a.rkt"))
  (parameterize ([current-error-port (open-output-string)])
    (display-to-file
     (generate-parser-module (parse-g4-file grammar)
                             #:source-path (path->string grammar)
                             #:indent-tokens? #t)
     path #:exists 'replace))
  (values (dynamic-require path 'tokenize)
          (dynamic-require path 'parse-file_input)
          (dynamic-require path 'token-type)
          (dynamic-require path 'token-value)))

;; ── corpus scan (mirrors effervesce/tools/type-graph.rkt) ───────────

(define (find-py-files dir)
  (define out '())
  (let loop ([d dir])
    (for ([e (in-list (directory-list d))])
      (define p (build-path d e))
      (cond
        [(directory-exists? p) (loop p)]
        [(regexp-match? #rx"[.]py$" e) (set! out (cons p out))]
        [else (void)])))
  (reverse out))

;; Canonical dotted module name of a scanned root, walking up the __init__.py
;; chain so `django/django/core` probes as "django.core".
(define (package-root-name root-dir)
  (define comps '())
  (let loop ([d root-dir])
    (define-values (base name dir?) (split-path d))
    (cond
      [(and base (file-exists? (build-path d "__init__.py")))
       (set! comps (cons (path->string name) comps))
       (loop base)]
      [else
       (if (null? comps)
           (let-values ([(b n dir?) (split-path root-dir)]) (path->string n))
           (string-join comps "."))])))

;; Returns (values rows provenance-record).
(define (corpus-rows root dirs)
  (define-values (tokenize parse tok-type tok-value) (load-python-front-end root))
  (define roots
    (for/list ([dir (in-list dirs)])
      (define root-dir (simplify-path (path->complete-path dir) #f))
      (cons (package-root-name root-dir) root-dir)))
  (define files
    (sort (apply append (map (λ (r) (find-py-files (cdr r))) roots))
          (λ (a b) (string<? (path->string a) (path->string b)))))
  (when (null? files)
    (error 'provenance-corpus "no .py files under ~a" dirs))
  (define host-code (make-hash))
  (define specs '())
  (define packages (set))
  (define comp-modpath (make-hash))
  (define failed '())
  (for ([f (in-list files)])
    (define src (file->string f))
    (define tks
      (with-handlers ([exn:fail? (λ (e) #f)])
        (tokenize src)))
    (define uir
      (and (list? tks)
           (let ([res (with-handlers ([exn:fail? (λ (e) #f)])
                        (parse tks 0))])
             (and (list? res)
                  (= (length res) 2)
                  (lower-python (second res) tok-type tok-value)))))
    (if uir (hash-set! host-code (path->string f) uir) (set! failed (cons f failed)))
    (define modpath
      (for/or ([r (in-list roots)])
        (define rel (find-relative-path (cdr r) f))
        (and (not (string-prefix? (path->string rel) ".."))
             (path->module-path (car r) (cdr r) f))))
    (when modpath
      (define comp (path->string f))
      (hash-set! comp-modpath comp modpath)
      (when (regexp-match? #rx"__init__[.]py$" comp)
        (set! packages (set-add packages modpath)))
      (define import-tokens
        (for/list ([t (in-list (if (list? tks) tks '()))])
          (cons (tok-type t) (tok-value t))))
      (set! specs (cons (module-spec modpath (or uir (uir-block '()))
                                     (extract-imports import-tokens))
                        specs))))
  (define mods (build-module-table specs))
  (define env (import-env comp-modpath
                          (resolve-exports mods packages)
                          (derive-class-method-effects mods)))
  (define comp-programs (for/list ([(c p) (in-hash host-code)]) (cons c p)))
  (define prov-box (box #f))
  (define rows (infer-effects-all comp-programs #:import-env env
                                  #:provenance-record prov-box))
  (printf "files scanned:  ~a\n" (length files))
  (printf "files parsed:   ~a\n" (hash-count host-code))
  (printf "files failed:   ~a\n" (length failed))
  (when (and (pair? failed) (member "--verbose" (vector->list (current-command-line-arguments))))
    (for ([f (in-list failed)]) (printf "  FAIL ~a\n" f)))
  (values rows (unbox prov-box)))

;; ── checks ──────────────────────────────────────────────────────────

(define (edge-count rows)
  (for/sum ([(_ row) (in-hash rows)]) (length (uir-effect-row-entries row))))

(define (unknown-function-count rows)
  (for/sum ([(key row) (in-hash rows)])
    (if (for/or ([e (in-list (uir-effect-row-entries row))])
          (eq? (entry-effect-name e) 'unknown))
        1
        0)))

;; Ground sites by classification kind (inferred / unresolved / declared), so
;; the report shows where the evidence comes from — in particular that every
;; `unknown` entry has an `unresolved` ground and not merely an absent record.
(define (ground-kind-counts prov)
  (define out (make-hash))
  (for ([(fq by-effect) (in-hash (provenance-grounds prov))])
    (for ([(effect sites) (in-hash by-effect)])
      (for ([site (in-list sites)])
        (hash-update! out (prov-site-kind site) add1 0))))
  (sort (hash->list out) symbol<?
        #:key (λ (p) (car p))))

(define (link-count graph type)
  (for/sum ([a (in-list graph)] #:when (and (link? a) (eq? (link-type a) type))) 1))

(define (main)
  (define args (vector->list (current-command-line-arguments)))
  (define record-only? (pair? (member "--record-only" args)))
  (define dirs (filter (λ (a) (not (string-prefix? a "--"))) args))
  (define probe-dirs (if (null? dirs) default-probe-dirs dirs))
  (define root (repo-root))
  (parameterize ([current-directory root])
    (define-values (rows prov) (corpus-rows root probe-dirs))
    (define edges (edge-count rows))
    (printf "named functions: ~a\n" (hash-count rows))
    (printf "functions with an unknown entry: ~a\n" (unknown-function-count rows))
    (printf "function/effect edges: ~a\n" edges)
    (printf "ground sites by kind: ~a\n" (ground-kind-counts prov))

    ;; 1. record-level reachability.
    (define missing (provenance-missing rows prov))
    (printf "edges without provenance (record reachability): ~a\n" (length missing))
    (for ([m (in-list (take missing (min 20 (length missing))))])
      (printf "  MISSING ~a / ~a\n" (car m) (cadr m)))

    (if record-only?
        (begin
          (printf "record-only: skipped graph projection/query\n")
          (if (null? missing)
              (begin (printf "provenance OK (record level): every effect edge is grounded\n") (exit 0))
              (begin (printf "provenance FAILED\n") (exit 1))))
        (void))

    ;; 2. atom-level projection; provenance-graph raises if an edge is unprovable.
    (define graph
      (with-handlers ([exn:fail?
                       (λ (e)
                         (printf "projection FAILED: ~a\n" (exn-message e))
                         (exit 1))])
        (provenance-graph rows prov)))
    (printf "graph atoms: ~a\n" (length graph))
    (for ([ty (in-list (sort (remove-duplicates
                              (for/list ([a (in-list graph)] #:when (link? a)) (link-type a)))
                             (λ (a b) (string<? (symbol->string a) (symbol->string b)))))])
      (printf "  ~a: ~a\n" ty (link-count graph ty)))

    ;; 3. query every edge through one reusable index.
    (define idx (make-graph-index graph))
    (define paths 0)
    (define deepest 0)
    (define failures '())
    (define sample #f)
    (for* ([(key row) (in-hash rows)]
           [entry (in-list (uir-effect-row-entries row))])
      (define fq (function-fq key))
      (define eff (entry-effect-name entry))
      (define found (provenance-paths idx fq eff))
      (if (null? found)
          (set! failures (cons (list fq eff) failures))
          (begin
            (set! paths (+ paths (length found)))
            (for ([p (in-list found)])
              (define d (length (walk-path-hops p)))
              (when (> d deepest) (set! deepest d))
              (when (and (not sample) (eq? eff 'db) (>= d 1)) (set! sample p))))))
    (printf "queried every edge: ~a path(s), deepest chain ~a hop(s), ~a edge(s) with no path\n"
            paths deepest (length failures))
    (for ([f (in-list (take failures (min 20 (length failures))))])
      (printf "  NO PATH ~a / ~a\n" (car f) (cadr f)))
    (when sample
      (printf "sample db path:\n  ~a\n" (walk-path->string sample)))

    (define ok? (and (null? missing) (null? failures)))
    (if ok?
        (begin
          (printf "provenance OK: every effect edge is grounded\n")
          (exit 0))
        (begin
          (printf "provenance FAILED\n")
          (exit 1)))))

(module+ main
  (with-handlers ([exn:fail?
                   (λ (e)
                     (eprintf "provenance-corpus: ~a\n" (exn-message e))
                     (exit 2))])
    (main)))
