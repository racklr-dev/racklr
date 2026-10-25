#lang racket

;; racklr/effects/imports/resolve.rkt
;;
;; Module-path resolution and the monotone export-resolution fixpoint, plus the
;; resolved import surface (`import-env`) handed to the effect fixpoint.

(require racket/path
         racklr/effects/imports/import-extract
         racklr/effects/imports/handle)

(provide
  resolve-exports
  resolve-module-path
  parent-path
  path->module-path
  import-env? import-env import-env-comp-modpath import-env-exports import-env-caps)

;; PEP 8 class-name convention for a binding name from an unscanned module:
;; `MigrationLoader` is a class, `connections` / `get_migration` are not.
(define (capitalized-name? s)
  (and (positive? (string-length s))
       (char-upper-case? (string-ref s 0))))

;; Filesystem path → dotted module path, relative to a package root. `root-name`
;; is the top-level package name (e.g. "django"); `root-dir` is the package
;; directory on disk; `p` is the file. A package's `__init__.py` maps to its
;; directory name (NOT "...__init__"), so `db/__init__.py` → "django.db".
(define (path->module-path root-name root-dir p)
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

;; Resolve an import source against the importing module's path. Absolute
;; sources resolve directly against the module table; relative sources (leading
;; dots) resolve against the importing module's package.
(define (resolve-module-path src from-path mods packages)
  (define dots (relative-dot-count src))
  (cond
    [(= dots 0) (and (hash-has-key? mods src) src)]
    [else
     ;; package containing from-path: itself if it is a package, else its parent
     (define pkg (if (set-member? packages from-path) from-path (parent-path from-path)))
     ;; for k dots: base = pkg for k=1, parent(pkg) for k=2, ...
     (define base
       (let loop ([k 1] [b pkg])
         (cond [(>= k dots) b]
               [else (loop (add1 k) (parent-path b))])))
     (define rest (substring src dots))
     (cond
       [(string=? rest "") (and base (hash-has-key? mods base) base)]
       [else
        (define target (if (and base (not (string=? base "")))
                           (string-append base "." rest)
                           rest))
        (and (hash-has-key? mods target) target)])]))

(define (resolve-exports mods packages)
  ;; mods: (hash path → module-info). Returns (hash path → (hash name → handle)).
  (define exports (make-hash))
  (for ([(path mi) (in-hash mods)])
    (define e (make-hash))
    (for ([cname (in-hash-keys (module-info-classes mi))])
      (hash-set! e cname (handle 'class path cname)))
    (for ([fname (in-hash-keys (module-info-functions mi))])
      (hash-set! e fname (handle 'fn path fname)))
    (hash-set! exports path e))

  (let loop ([changed #t])
    (when changed
      (define next-changed #f)
      (for ([(path mi) (in-hash mods)])
        (define e (hash-ref exports path))
        (for ([rec (in-list (module-info-imports mi))])
          (define src (import-record-source rec))
          (define target-mod (resolve-module-path src path mods packages))
          (define te (and target-mod (hash-ref exports target-mod #f)))
          (cond
            [(eq? (import-record-form rec) 'from)
             (for ([b (in-list (import-record-bindings rec))])
               (define local (car b))
               (define full-target (cdr b))
               ;; `from pkg import submod`: the target `pkg.submod` is itself a
               ;; module, so bind the local name to a module handle (the export
               ;; table has no entry for it unless `pkg/__init__.py` re-exports
               ;; it). Otherwise fall back to the member-export lookup
               ;; (`from x import C`, including aliased class names). Star
               ;; imports bind no name.
               (unless (string=? local "*")
                 (define h
                   (or (and (hash-has-key? mods full-target)
                            (handle 'module full-target #f))
                       (let ([member (last-component full-target)])
                         (and te (hash-ref te member #f)))
                       ;; The target module is outside the scanned corpus, so
                       ;; there is no export table for it. Synthesize a handle
                       ;; for the binding so it stays resolvable: a class handle
                       ;; for a Capitalized binding (PEP 8 class convention),
                       ;; which keeps `from django.db.migrations.loader import
                       ;; MigrationLoader` reachable through imported-class-ref
                       ;; / imported-class-cap; and an fn handle for a lowercase
                       ;; binding, which lets a bare call to an external function
                       ;; (`from django.db.migrations.utils import
                       ;; get_migration_name_timestamp`) resolve to the module's
                       ;; effect domain rather than `unknown`. The fn handle is
                       ;; never a receiver TYPE (imported-class-ref requires kind
                       ;; 'class), so it only widens effect classification.
                       (let ([member (last-component full-target)])
                         (and (not te)
                              (handle (if (capitalized-name? member) 'class 'fn)
                                      src member)))))
                 (when (and h (not (hash-has-key? e local)))
                   (hash-set! e local h)
                   (set! next-changed #t))))]
            [else
             ;; 'import: binding target is already a module path
             (for ([b (in-list (import-record-bindings rec))])
               (define local (car b))
               (define modpath (cdr b))
               ;; A scanned module resolves to its own handle; an external one
               ;; (`import httpx`) still binds a module handle on its dotted
               ;; path, so imported-module-result-type and imported-effect can
               ;; consult the library config / result-type tables.
               (define h (handle 'module modpath #f))
               (when (not (hash-has-key? e local))
                 (hash-set! e local h)
                 (set! next-changed #t)))])))
      (loop next-changed)))
  exports)

;; The resolved import surface handed to the effect fixpoint: which dotted
;; module each component (source file) lives in, each module's export table
;; (name → handle), and the derived Class.method → effect-cap table. Built by
;; the caller (which owns the parse/tokenize step); infer-effects-all only
;; consumes it.
(struct import-env (comp-modpath exports caps) #:transparent)
