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
;; tool counts only what the real context leaves unresolved — the surface worth
;; aiming levers at.
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
;; The effect inference, the census, and the corpus/CLI helpers live in the
;; racklr collection; the Python front end (parse + lower) is the effervesce
;; submodule's check/host-python.rkt, the same module the effervesce tools use.

(require racket/file
         racklr/uir
         racklr/effects/contract
         racklr/effects/imports
         racklr/effects/census
         racklr/effects/census-cli
         "../effervesce/check/host-python.rkt")

;; ── report helpers ───────────────────────────────────────────────────

(define (pct n total)
  (if (zero? total) 0 (round (* 100 (/ n total)))))

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
  (define cli (parse-census-cli (vector->list (current-command-line-arguments))))
  (unless cli
    (eprintf "~a\n" census-cli-usage)
    (exit 1))
  (define dirs (census-cli-dirs cli))
  (define top-n (census-cli-top cli))
  (define shape-filter (census-cli-shape cli))
  (define lever-filter (census-cli-lever cli))
  (define print-sites? (census-cli-sites? cli))
  (define tsv-path (census-cli-tsv-path cli))

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

  (printf "\nReceiver shapes (unknown call sites):\n")
  (for ([p (in-list (census-shape-counts c))])
    (printf "  ~a  x~a  (~a%)\n" (car p) (cdr p)
            (pct (cdr p) (census-site-count c))))

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

(module+ main (main))
