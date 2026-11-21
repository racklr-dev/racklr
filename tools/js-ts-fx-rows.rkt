#lang racket

;; tools/js-ts-fx-rows.rkt <dir> [<dir> ...]
;;
;; The TS/JS analog of tools/fx-rows.rkt: scan .js/.mjs/.ts/.tsx files, lower
;; each through the JS/TS front ends, infer every function's effect row with
;; the javascript-lexicon, and dump {"name","domains"} as JSON for the formalize
;; loop's classifier stage. This is the nc-6717 host-dispatch entry point.

(require racket/list
         racket/path
         racket/string
         json
         racklr/uir
         racklr/effects/contract
         racklr/effects/whole-program
         racklr/effects/lexicons/javascript
         "js-ts-corpus-lib.rkt")

;; Stable dotted component name: the file path relative to its scan root,
;; slashes -> dots, the extension dropped.
(define (js-component-name root f)
  (define d (simplify-path (path->complete-path root) #f))
  (define rel (find-relative-path d (simplify-path (path->complete-path f) #f)))
  (string-join
   (map (λ (p) (string-replace p #rx"[.](m?js|tsx?)$" ""))
        (string-split (path->string rel) "/"))
   "."))

(define (main)
  (define dirs (filter (λ (a) (not (string-prefix? a "--")))
                       (vector->list (current-command-line-arguments))))
  (unless (pair? dirs)
    (eprintf "usage: js-ts-fx-rows.rkt <dir> ...\n")
    (exit 2))
  (define parsers (load-js-ts-parsers))
  (define comp-programs '())
  (define failed '())
  (for ([d (in-list dirs)])
    (unless (directory-exists? d)
      (eprintf "js-ts-fx-rows: no such dir ~a\n" d)
      (exit 2))
    (for ([f (in-list (js-ts-corpus-files d))])
      (define lang (lang-of f))
      (when (memq lang '(javascript typescript tsx))
        (define uir (lower-js-ts-source lang parsers (file->string f)))
        (if uir
            (set! comp-programs
                  (cons (cons (js-component-name d f) uir) comp-programs))
            (set! failed (cons (path->string f) failed))))))
  (define rows (infer-effects-all (reverse comp-programs)
                                  #:lexicon javascript-lexicon))
  (define out
    (string-join
     (for/list ([(k row) (in-hash rows)])
       (define doms (remove-duplicates
                     (for/list ([e (in-list (uir-effect-row-entries row))])
                       (effect-name-symbol e))))
       (format "{\"name\":\"~a::~a\",\"domains\":[~a]}"
               (car k) (cdr k)
               (string-join
                (for/list ([d (in-list doms)])
                  (format "\"~a\"" (symbol->string d)))
                ",")))
     ","))
  (display (format "[~a]" out)))

(module+ main (main))
