#lang racket

;; tools/fx-rows.rkt <dir> [<dir> ...] — dump every function's inferred effect
;; row as JSON (component::function -> list of domains), for the reducer's
;; function-level correction stage.

(require racket/list
         json
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
  (define cp (simplify-path (path->complete-path (string->path comp-file)) #f))
  (for/or ([d (in-list dirs)])
    (define dd (simplify-path (path->complete-path d) #f))
    (define rel (find-relative-path dd cp))
    (and (not (string-prefix? (path->string rel) ".."))
         (let* ([pkg (package-root-name dd)]
                [parts (string-split (path->string rel) "/")]
                [parts (map (λ (p) (string-replace p ".py" "")) parts)])
           (string-join (cons pkg parts) ".")))))

(define (main)
  (define dirs (filter (λ (a) (not (string-prefix? a "--")))
                       (vector->list (current-command-line-arguments))))
  (define probe-dirs (if (null? dirs) default-probe-dirs dirs))
  (define root (repo-root))
  (parameterize ([current-directory root])
    ;; corpus-rows prints a scan summary; keep stdout as pure JSON.
    (define-values (rows prov)
      (parameterize ([current-output-port (open-output-string)])
        (corpus-rows root probe-dirs)))
    (define out
      (string-join
       (for/list ([(k row) (in-hash rows)])
         (define comp (clean-component (car k) probe-dirs))
         (define fq (format "~a::~a" comp (cdr k)))
         (define doms
           (remove-duplicates
            (for/list ([e (in-list (uir-effect-row-entries row))])
              (entry-effect-name e))))
         (format "{\"name\":\"~a\",\"domains\":[~a]}"
                 fq
                 (string-join
                  (for/list ([d (in-list doms)])
                    (format "\"~a\"" (symbol->string d)))
                  ",")))
       ","))
    (display (format "[~a]" out))))

(module+ main
  (with-handlers ([exn:fail?
                   (λ (e)
                     (eprintf "fx-rows: ~a\n" (exn-message e))
                     (exit 2))])
    (main)))
