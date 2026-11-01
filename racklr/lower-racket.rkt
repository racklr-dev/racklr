#lang racket

;; racklr/lower-racket.rkt
;;
;; Racket front end for the self-host loop (P6.1): a Racket module lowers to a
;; uir-module whose functions and call sites are the compiler's own program.
;;
;; Racket's reader is the grammar; lower-racket/ contains the lowering hook:
;;   reader.rkt  — datum stream of a module (the #lang line is stripped)
;;   scan.rkt    — first pass: module identity, requires, provides, defines
;;   resolve.rkt — second pass: cross-module name resolution (call-graph ids)
;;   expr.rkt    — expression / binding lowering, special forms
;;   module.rkt  — top-level module lowering
;;
;; The three public entry points mirror the other front ends: lower one source
;; string, lower one file, or lower a whole program of files together. The
;; program entry point is what the compiler-as-UIR driver uses: every module is
;; scanned before any is lowered, so a cross-file call resolves to the module
;; that defines it instead of an ambiguous bare name.

(require racket/list
         racket/path
         "lower-racket/reader.rkt"
         "lower-racket/scan.rkt"
         "lower-racket/resolve.rkt"
         "lower-racket/expr.rkt"
         "lower-racket/module.rkt")

(provide lower-racket
         lower-racket-file
         lower-racket-modules)

;; One source string → uir-module. No cross-module resolution: imports and
;; undefined names stay bare for the effect-token lexicon.
(define (lower-racket source)
  (define datums (read-source-datums source "<source>"))
  (define info (scan-module-datums datums 'standalone (current-directory) (current-directory)))
  (lower-module info (hash)))

;; One module file → uir-module, with its component identity derived from
;; `root` (default: the file's directory).
(define (lower-racket-file path [root (path-only (if (path? path) path (string->path path)))])
  (define results (lower-racket-modules (list path) #:root root))
  (if (pair? results) (cdr (car results)) (error 'lower-racket-file "no module: ~a" path)))

;; A whole program: scan every module, resolve imports across the scanned set,
;; then lower each module. Returns (listof (cons component-symbol uir-module))
;; in the input order (components are module ids: relative paths without .rkt).
(define (lower-racket-modules paths #:root [root (current-directory)])
  (define root* (simplify-path (if (path? root) root (string->path root))))
  (define path* (for/list ([p (in-list paths)])
                  (simplify-path (if (path? p) p (string->path p)))))
  (define infos (for/list ([p (in-list path*)]) (scan-module p root*)))
  (define info-table
    (for/hash ([info (in-list infos)])
      (values (module-info-id info) info)))
  (define resolution (build-resolution info-table))
  (for/list ([info (in-list infos)])
    (define id (module-info-id info))
    (cons id (lower-module info (resolved-imports resolution id)))))
