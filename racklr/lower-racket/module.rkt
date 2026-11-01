#lang racket

;; racklr/lower-racket/module.rkt
;;
;; Top-level module lowering: a Racket module's datum stream becomes one
;; uir-module whose body holds the lowered definitions and declarations.
;;
;; Top-level definitions are mangled to their defining module; submodule bodies
;; (module+ main / module+ test / module) are lowered with the same context so
;; their calls resolve the same way. require/provide become uir-import /
;; uir-export declarations (the resolution pass already consumed them).

(require racklr/uir
         "scan.rkt"
         "resolve.rkt"
         "syntax.rkt"
         "expr.rkt")

(provide lower-module)

;; module-info + resolved imports → uir-module.
(define (lower-module info imports)
  (define ctx (make-ctx (module-info-id info) info imports))
  (define body
    (let loop ([forms (module-info-datums info)] [acc '()])
      (cond
        [(null? forms) (reverse acc)]
        [else
         (define form (car forms))
         (define nodes (lower-top-form ctx form))
         (loop (cdr forms) (append (reverse nodes) acc))])))
  (uir-module (uir-symbol (symbol->string (module-info-id info)))
              (module-imports info)
              (module-exports info)
              (uir-block body)))

;; Top form → (listof uir?), in source order.
(define (lower-top-form ctx form)
  (cond
    [(not (and (pair? form) (symbol? (car form)))) (list (lower-expr ctx (hash) form))]
    [else
     (case (car form)
       [(require) (list (uir-import (uir-string (render-require (cdr form))) '()))]
       [(provide) (list (uir-export '()))]
       [(define define-values define/match define-syntax define-syntax-rule struct)
        (define-values (nodes _env) (lower-define-form ctx (hash) form #t))
        nodes]
       [(module+ begin)
        (lower-top-forms ctx (cddr form))]
       [(module)
        (lower-top-forms ctx (cdddr form))]
       [else (list (lower-expr ctx (hash) form))])]))

(define (lower-top-forms ctx forms)
  (append-map (λ (f) (lower-top-form ctx f)) forms))

;; ── declarations ───────────────────────────────────────────────────

(define (module-imports info)
  (for/list ([clause (in-list (module-info-requires info))])
    (define src (and (pair? (cdr clause)) (second clause)))
    (uir-import (uir-string (if (symbol? src) (symbol->string src) "external"))
                '())))

(define (module-exports info)
  (for/list ([spec (in-list (module-info-provides info))])
    (uir-export
     (for/list ([nm (in-list (provide-spec-names spec))])
       (uir-symbol (symbol->string nm))))))

(define (provide-spec-names spec)
  (case (car spec)
    [(name) (list (second spec))]
    [(rename-out) (map cdr (filter pair? (cdr spec)))]
    [(prefix-out)
     (define pfx (and (symbol? (second spec)) (symbol->string (second spec))))
     (if pfx
         (append-map
          (λ (inner)
            (for/list ([nm (in-list (provide-spec-names inner))])
              (string->symbol (string-append pfx (symbol->string nm)))))
          (third spec))
         '())]
    [else '()]))

(define (render-require specs)
  (string-join (for/list ([s (in-list specs)])
                 (format "~a" (cond [(symbol? s) s]
                                    [(string? s) s]
                                    [(path-string? s) s]
                                    [else (if (pair? s) (car s) s)])))
               ", "))
