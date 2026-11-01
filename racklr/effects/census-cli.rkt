#lang racket

;; racklr/effects/census-cli.rkt
;;
;; Corpus-scan and command-line helpers for tools/unknown-census.rkt.
;;
;; Kept out of both the analysis (census.rkt) and the driver (the tools script,
;; which needs the Python front end and is therefore expensive to load): this
;; module is pure filesystem and argument logic, so the dotted-package
;; derivation that labels every census component, and the flag parsing that
;; selects the report, are testable without a corpus or a grammar.

(require racket/file)

(provide
 ;; corpus scan
 find-py-files
 package-root-name
 ;; command line
 (struct-out census-cli)
 census-cli-usage
 parse-census-cli)

;; ── corpus scan ──────────────────────────────────────────────────────

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

;; ── command line ─────────────────────────────────────────────────────

;; dirs         corpus roots to scan
;; top          methods listed per lever (>= 0)
;; shape/lever  optional per-site report filters (symbols or #f)
;; sites?       print per-site lines
;; tsv-path     optional deterministic TSV destination
(struct census-cli (dirs top shape lever sites? tsv-path) #:transparent)

(define census-cli-usage
  (string-append
   "usage: unknown-census.rkt <dir> [<dir> ...] "
   "[--top N] [--shape SHAPE] [--lever LEVER] [--sites] [--tsv PATH]"))

;; (listof string) → census-cli or #f when the arguments are not a valid
;; invocation. Never raises: the driver prints the usage line on #f.
(define (parse-census-cli args)
  (let loop ([as args] [dirs '()] [top 10] [shape #f] [lever #f]
             [sites? #f] [tsv-path #f])
    (cond
      [(null? as)
       (and (pair? dirs)
            (census-cli (reverse dirs) top shape lever sites? tsv-path))]
      [(string=? (car as) "--top")
       (let ([n (and (pair? (cdr as)) (string->number (cadr as)))])
         (and n (exact-nonnegative-integer? n)
              (loop (cddr as) dirs n shape lever sites? tsv-path)))]
      [(string=? (car as) "--shape")
       (and (pair? (cdr as))
            (loop (cddr as) dirs top (string->symbol (cadr as)) lever
                  sites? tsv-path))]
      [(string=? (car as) "--lever")
       (and (pair? (cdr as))
            (loop (cddr as) dirs top shape (string->symbol (cadr as))
                  sites? tsv-path))]
      [(string=? (car as) "--sites")
       (loop (cdr as) dirs top shape lever #t tsv-path)]
      [(string=? (car as) "--tsv")
       (and (pair? (cdr as))
            (loop (cddr as) dirs top shape lever sites? (cadr as)))]
      [(string-prefix? (car as) "--") #f]   ;; unknown flag
      [else (loop (cdr as) (cons (car as) dirs) top shape lever
                  sites? tsv-path)])))
