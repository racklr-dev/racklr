#lang racket

;; tools/unknown-census.rkt
;;
;; Real-context unknown census over a Python corpus.
;;
;; Feeds every .py file through the front end, builds the real import
;; environment (module table, export graph, derived Class.method capabilities),
;; runs the whole-program effect fixpoint, and reports every call site that
;; STILL classifies as `unknown` — bucketed by receiver shape, binding kind,
;; and the receiver-type-dataflow lever that would resolve it. A per-site TSV
;; (--tsv) is the machine-readable, diff-friendly artifact.
;;
;; This is not the empty-context census. `effervesce/tools/unknown-histogram.rkt`
;; classifies each call with no known names, so intra-program edges and imported
;; handles count as unknown (1463 non-edge sites on django/django/core). This
;; tool counts only what the real context leaves unresolved (249 sites on the
;; same corpus at the same revision) — the surface worth aiming levers at.
;;
;; Usage:
;;   racket tools/unknown-census.rkt <dir> [<dir> ...]
;;         [--top N] [--shape SHAPE] [--lever LEVER] [--sites] [--tsv PATH]
;;
;;   <dir>            corpus root(s); the dotted package root is derived by
;;                    walking the __init__.py chain (django/django -> "django")
;;   --top N          methods listed per lever (default 10)
;;   --shape SHAPE    only print per-site lines for that receiver shape
;;   --lever LEVER    only print per-site lines for that dataflow lever
;;   --sites          print per-site lines (all sites unless filtered)
;;   --tsv PATH       write the deterministic per-site TSV to PATH
;;
;; Requires PLTCOLLECTS to include the repo root, as
;; effervesce/tools/type-graph.rkt does:
;;
;;   PLTCOLLECTS="$(pwd):" racket tools/unknown-census.rkt django/django/core
;;
;; The effect inference and the census live in the racklr collection; the
;; Python front end (parse + lower) is the effervesce submodule's
;; check/host-python.rkt, the same module the effervesce tools use.

(require racket/file
         racklr/uir
         racklr/effects/contract
         racklr/effects/imports
         racklr/effects/census
         "../effervesce/check/host-python.rkt")

;; ── corpus walk ──────────────────────────────────────────────────────

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

;; Canonical dotted module name of a scanned root directory: walk up past every
;; __init__.py-bearing ancestor and join the names, so scanning django/django
;; yields "django" rather than the bare basename. Same rule as
;; effervesce/tools/type-graph.rkt; a root with no package ancestor falls back
;; to its basename.
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

;; ── report helpers ───────────────────────────────────────────────────

(define (pct n total)
  (if (zero? total) 0 (round (* 100 (/ n total)))))

(define (print-counts title counts total)
  (printf "\n~a\n" title)
  (for ([p (in-list counts)])
    (printf "  ~a  x~a  (~a%)\n" (car p) (cdr p) (pct (cdr p) total))))

;; (op . count) for the sites of one lever, highest first.
(define (lever-op-counts c lever)
  (define h (make-hash))
  (for ([s (in-list (census-sites c))]
        #:when (eq? (census-site-lever s) lever))
    (hash-update! h (or (census-site-op s) '?) add1 0))
  (sort (hash->list h) (λ (a b) (> (cdr a) (cdr b)))))

(define (site-line s)
  (format "~a :: ~a :: ~a :: ~a -> ~a [rx=~a kind=~a]"
          (census-site-component s)
          (census-site-function s)
          (census-site-shape s)
          (or (census-site-lever s) '?)
          (or (census-site-site s) "?")
          (or (census-site-receiver s) "?")
          (or (census-site-binding-kind s) "?")))

;; ── main ─────────────────────────────────────────────────────────────

(define (main)
  (define args (vector->list (current-command-line-arguments)))
  (define (flag-value flag)
    (for/or ([a (in-list args)] [i (in-naturals)]
             #:when (string=? a flag))
      (and (< (add1 i) (length args)) (list-ref args (add1 i)))))
  (define top-n (or (string->number (or (flag-value "--top") "10")) 10))
  (define shape-filter
    (let ([s (flag-value "--shape")]) (and s (string->symbol s))))
  (define lever-filter
    (let ([s (flag-value "--lever")]) (and s (string->symbol s))))
  (define print-sites? (and (member "--sites" args) #t))
  (define tsv-path (flag-value "--tsv"))
  (define dirs
    (filter (λ (a) (not (string-prefix? a "--")))
            (let loop ([as args] [acc '()])
              (cond
                [(null? as) (reverse acc)]
                [(member (car as) '("--top" "--shape" "--lever" "--tsv"))
                 (loop (if (pair? (cdr as)) (cddr as) (cdr as)) acc)]
                [else (loop (cdr as) (cons (car as) acc))]))))
  (unless (pair? dirs)
    (eprintf "usage: unknown-census.rkt <dir> [<dir> ...] [--top N] [--shape SHAPE] [--lever LEVER] [--sites] [--tsv PATH]\n")
    (exit 1))

  (define roots
    (for/list ([dir (in-list dirs)])
      (define root-dir (simplify-path (path->complete-path dir) #f))
      (cons (package-root-name root-dir) root-dir)))
  (define files
    (sort (apply append (map (λ (r) (find-py-files (cdr r))) roots))
          (λ (a b) (string<? (path->string a) (path->string b)))))
  (define host-code (make-hash))
  (define failed '())
  (define specs '())
  (define packages (set))
  (define comp-modpath (make-hash))
  (for ([f (in-list files)])
    (define src (file->string f))
    (define uir (python-source->uir src))
    (cond
      [uir (hash-set! host-code (path->string f) uir)]
      [else (set! failed (cons f failed))])
    (define modpath
      (for/or ([r (in-list roots)])
        (define rn (car r))
        (define rd (cdr r))
        (define rel (find-relative-path rd f))
        (and (not (string-prefix? (path->string rel) ".."))
             (path->module-path rn rd f))))
    (when modpath
      (define comp (path->string f))
      (hash-set! comp-modpath comp modpath)
      (when (regexp-match? #rx"__init__[.]py$" comp)
        (set! packages (set-add packages modpath)))
      (define tks (for/list ([t (in-list (py-tokenize src))])
                    (cons (py-tok-type t) (py-tok-value t))))
      (set! specs (cons (module-spec modpath (or uir (uir-block '()))
                                     (extract-imports tks))
                        specs))))

  (define mods (build-module-table specs))
  (define caps (derive-class-method-effects mods))
  (define env (import-env comp-modpath (resolve-exports mods packages) caps))
  ;; Report sites by dotted module path, not by absolute checkout path: the
  ;; TSV must be reproducible on another machine (module path is in
  ;; comp-modpath for every file under a scanned root). Renaming is part of
  ;; the census call, so the records stay sorted by their reported component.
  (define c
    (census-unknown-sites (for/list ([(comp p) (in-hash host-code)])
                            (cons comp p))
                          #:import-env env
                          #:component-name (λ (comp)
                                             (hash-ref comp-modpath comp comp))))

  ;; ── report ───────────────────────────────────────────────────────
  (printf "=== real-context unknown census ===\n")
  (printf "files scanned:  ~a   parsed: ~a   failed: ~a\n"
          (length files) (hash-count host-code) (length failed))
  (for ([f (in-list (reverse failed))])
    (printf "  FAIL ~a\n" f))
  (printf "functions:      ~a (in ~a files with inferred functions)\n"
          (census-function-count c) (census-file-count c))
  (printf "unknown call sites: ~a   unknown functions: ~a   unknown files: ~a\n"
          (census-site-count c)
          (census-unknown-functions c)
          (census-unknown-files c))

  (print-counts "Receiver shapes (unknown call sites):"
                (census-shape-counts c) (census-site-count c))
  (define local-pair (assq 'local (census-shape-counts c)))
  (define local-total (if local-pair (cdr local-pair) 0))
  (printf "\nLocal binding kinds (of ~a local receivers):\n" local-total)
  (for ([p (in-list (census-binding-kind-counts c))])
    (printf "  ~a  x~a  (~a% of local)\n" (car p) (cdr p)
            (pct (cdr p) local-total)))

  (printf "\nDataflow levers (aim receiver-type work here):\n")
  (for ([p (in-list (census-lever-counts c))])
    (printf "  ~a  x~a  (~a%)  — ~a\n"
            (car p) (cdr p) (pct (cdr p) (census-site-count c))
            (census-lever-description (car p))))

  (printf "\nTop methods per lever (top ~a):\n" top-n)
  (for ([p (in-list (census-lever-counts c))])
    (define lever (car p))
    (define ops (lever-op-counts c lever))
    (unless (null? ops)
      (printf "  ~a:\n" lever)
      (for ([op (in-list (take ops (min top-n (length ops))))])
        (printf "      ~a  x~a\n" (car op) (cdr op)))))

  (when print-sites?
    (define selected
      (filter (λ (s)
                (and (or (not shape-filter) (eq? (census-site-shape s) shape-filter))
                     (or (not lever-filter) (eq? (census-site-lever s) lever-filter))))
              (census-sites c)))
    (printf "\nPer-site unknowns (~a):\n" (length selected))
    (for ([s (in-list selected)]) (printf "  ~a\n" (site-line s))))

  (when tsv-path
    (call-with-output-file tsv-path
      (λ (out) (census->tsv c out))
      #:exists 'truncate)
    (printf "\nWrote ~a (~a site rows).\n" tsv-path (census-site-count c))))

(main)
