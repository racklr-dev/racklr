#lang racket

;; tools/emit-fizz.rkt <dir> [<dir> ...]
;;
;; Infer effect rows over a Python corpus, then synthesize an Effervesce
;; `.fizz` description: one `capability` per effect domain the corpus uses,
;; and one `component` per module with a `needs` row naming that module's
;; domains. This is the reverse of the bridge (which checks host code against
;; a hand-written .fizz); here the contract is DERIVED from the inferred
;; graph, so a library can be formalized without a human writing the spec.
;;
;; `dynamic` and `unknown` are deliberately not capabilities: dynamic is the
;; tracked deferral, unknown is eliminated before this runs. `pure` is the
;; absence of an effect, so it names no capability either.
;;
;; Usage (from the repo root, with PLTCOLLECTS set):
;;   racket tools/emit-fizz.rkt /tmp/libtry/httpx/httpx

(require racket/list
         racket/path
         racket/string
         racklr/uir
         racklr/effects/provenance
         "provenance-corpus.rkt")

(define default-probe-dirs '("django/django/core"))

(define (repo-root)
  (simplify-path (build-path (simplify-path (path->complete-path
                                             (find-system-path 'run-file)))
                             'up 'up)))

(define (package-root-name root-dir)
  (define comps '())
  (let loop ([d root-dir])
    (cond
      [(file-exists? (build-path d "__init__.py"))
       (set! comps (cons (path->string (file-name-from-path d)) comps))
       (define parent (path-only d))
       (and parent (loop parent))]
      [else
       (if (null? comps)
           (path->string (file-name-from-path root-dir))
           (string-join comps "."))])))

(define (clean-component comp-file dirs)
  ;; comp-file is a file path; derive "pkg.rel.dotted" (module path).
  (define cp (simplify-path (path->complete-path (string->path comp-file)) #f))
  (for/or ([d (in-list dirs)])
    (define dd (simplify-path (path->complete-path d) #f))
    (define rel (find-relative-path dd cp))
    (and (not (string-prefix? (path->string rel) ".."))
         (let* ([pkg (package-root-name dd)]
                [parts (string-split (path->string rel) "/")]
                [parts (map (λ (p) (string-replace p ".py" "")) parts)])
           (string-join (cons pkg parts) ".")))))

(define (domain-cap-name d)
  (string-titlecase (symbol->string d)))

(define (sanitize-id s)
  (define cleaned (regexp-replace* #rx"[^a-zA-Z0-9_]" s "_"))
  (if (regexp-match? #rx"^[0-9]" cleaned)
      (string-append "_" cleaned)
      cleaned))

(define (emit-fizz system-name domains comp-domains)
  (define caps
    (string-join
     (for/list ([d (in-list (sort (set->list domains) symbol<?))])
       (format "  capability ~a { op() -> Unit }\n" (domain-cap-name d)))
     ""))
  (define comps
    (string-join
     (for/list ([comp (in-list (sort (hash-keys comp-domains) string<?))])
       (define ds (sort (set->list (hash-ref comp-domains comp)) symbol<?))
       (define needs (string-join (map domain-cap-name ds) ", "))
       (if (string=? needs "")
           (format "  component ~a {}\n" (sanitize-id comp))
           (format "  component ~a needs ~a {}\n" (sanitize-id comp) needs)))
     ""))
  (format "system ~a {\n~a~a}\n" system-name caps comps))

(define (main)
  (define args (filter (λ (a) (not (string-prefix? a "--")))
                       (vector->list (current-command-line-arguments))))
  (define dirs (if (null? args) default-probe-dirs args))
  (define root (repo-root))
  (parameterize ([current-directory root])
    (define-values (rows prov) (corpus-rows root dirs))
    (define comp-domains (make-hash))
    (for ([(k row) (in-hash rows)])
      (define comp (clean-component (car k) dirs))
      (define doms (hash-ref! comp-domains comp set))
      (for ([e (in-list (uir-effect-row-entries row))])
        (define d (entry-effect-name e))
        (unless (memq d '(unknown dynamic pure))
          (hash-set! comp-domains comp (set-add doms d)))))
    (define all-domains
      (for/fold ([s (set)]) ([(comp ds) (in-hash comp-domains)])
        (set-union s ds)))
    (define system-name
      (string-titlecase
       (let ([b (path->string (file-name-from-path
                                (simplify-path (path->complete-path (first dirs)))))])
         (if (equal? b "") "System" b))))
    (display (emit-fizz system-name all-domains comp-domains))))

(module+ main
  (with-handlers ([exn:fail?
                   (λ (e)
                     (eprintf "emit-fizz: ~a\n" (exn-message e))
                     (exit 2))])
    (main)))
