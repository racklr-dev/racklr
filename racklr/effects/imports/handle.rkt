#lang racket

;; racklr/effects/imports/handle.rkt
;;
;; The abstract-domain values and the harvested module surface: `handle`
;; (module/class/fn/unknown), the `class-info` / `module-info` / `module-spec`
;; records, and the harvest that walks a module's UIR into these (no runtime,
;; deps, or DB).

(require racklr/uir)

(provide
  handle? handle handle-kind handle-path handle-name
  class-info? class-info-name class-info-super class-info-methods
  module-info? module-info-path module-info-classes module-info-functions module-info-imports module-info-values
  module-spec? module-spec-path module-spec-uir module-spec-imports
  module-spec
  build-module-table
  harvest-module)

(struct handle (kind path name) #:transparent)
;; kind: 'module | 'class | 'fn | 'unknown
;; path: module path string (for class/fn); the module path itself (for module)
;; name: class/function name (for class/fn); #f for module

(struct class-info (name super methods) #:transparent)
;; methods: (hash method-name-string → uir-method)

(struct module-info (path classes functions imports values) #:transparent)
;; classes: (hash class-name-string → class-info)
;; functions: (hash function-name-string → uir-fn)
;; imports: (listof import-record)

(struct module-spec (path uir imports) #:transparent)

(define (class-super-name super-expr)
  (cond
    [(uir-var? super-expr) (uir-symbol-name (uir-var-name super-expr))]
    [(uir-get? super-expr)
     (define f (uir-get-field super-expr))
     (cond [(uir-symbol? f) (uir-symbol-name f)]
           [(uir-string? f) (uir-string-value f)]
           [else #f])]
    [else #f]))

;; Super name strings for a `uir-class` node's base list, in left-to-right
;; source order; a non-symbol base (e.g. a subscripted generic) contributes
;; nothing for that position.
(define (class-super-names supers)
  (filter values (map class-super-name supers)))

(define (harvest-module path uir imports)
  (define classes (make-hash))
  (define functions (make-hash))
  (define values (make-hash))   ;; name-string → constructor-class-name (class-valued module globals)
  ;; A class method-list node → its uir-method, unwrapping (uir-decorated decos
  ;; inner) and (call "async-fn"/"gen-fn" (list method)) wrappers that the Python
  ;; lowerer now keeps on decorated / async def methods.
  (define (unwrap-method m)
    (cond
      [(uir-method? m) m]
      [(uir-decorated? m) (unwrap-method (uir-decorated-inner m))]
      [(and (uir-call? m)
            (uir-symbol? (uir-call-callee m))
            (member (uir-symbol-name (uir-call-callee m)) '("async-fn" "gen-fn"))
            (= (length (uir-call-args m)) 1))
       (unwrap-method (first (uir-call-args m)))]
      [else #f]))
  ;; Name a module-level assignment's class, when the RHS is a class
  ;; construction or bare class reference (Capitalized convention). This makes
  ;; `router = ConnectionRouter()` / `apps = Apps(...)` exportable so
  ;; `from django.db import router` resolves to a class-typed handle.
  (define (class-valued-name v)
    (define (cap? s) (and (string? s) (> (string-length s) 0)
                          (char-upper-case? (string-ref s 0))))
    (define nm
      (cond
        [(uir-call? v)
         (let ([c (uir-call-callee v)])
           (cond
             [(uir-var? c) (uir-symbol-name (uir-var-name c))]
             [(uir-symbol? c) (uir-symbol-name c)]
             [(uir-get? c)
              (define f (uir-get-field c))
              (cond [(uir-symbol? f) (uir-symbol-name f)]
                    [(uir-string? f) (uir-string-value f)]
                    [else #f])]
             [else #f]))]
        [(uir-var? v) (uir-symbol-name (uir-var-name v))]
        [(uir-symbol? v) (uir-symbol-name v)]
        [else #f]))
    (and nm (cap? nm) nm))
  (define (walk-stmt s)
    (cond
      [(uir-class? s)
       (define cname (and (uir-symbol? (uir-class-name s))
                          (uir-symbol-name (uir-class-name s))))
       (define methods (make-hash))
       (for ([m (in-list (uir-class-methods s))])
         (define mm (unwrap-method m))
         (define mn (and mm (uir-symbol? (uir-method-name mm))
                         (uir-symbol-name (uir-method-name mm))))
         (when mn (hash-set! methods mn mm)))
        (when cname
          (hash-set! classes cname
                     (class-info cname (class-super-names (uir-class-super s)) methods)))]
      [(uir-fn? s)
       (define fn (and (uir-symbol? (uir-fn-name s))
                       (uir-symbol-name (uir-fn-name s))))
       (when fn (hash-set! functions fn s))]
      [(uir-set!? s)
       (define lhs (and (uir-var? (uir-set!-name s))
                        (uir-symbol-name (uir-var-name (uir-set!-name s)))))
       (define cname (class-valued-name (uir-set!-value s)))
       (when (and lhs cname) (hash-set! values lhs cname))]
      [(uir-ann-set!? s)
       (define lhs (and (uir-var? (uir-ann-set!-lhs s))
                        (uir-symbol-name (uir-var-name (uir-ann-set!-lhs s)))))
       (define cname (class-valued-name (uir-ann-set!-value s)))
       (when (and lhs cname) (hash-set! values lhs cname))]
      [(uir-decorated? s) (walk-stmt (uir-decorated-inner s))]
      [else (void)]))
  (for ([s (in-list (uir-block-stmts uir))]) (walk-stmt s))
  (module-info path classes functions imports values))

(define (build-module-table specs)
  (for/hash ([spec (in-list specs)])
    (values (module-spec-path spec)
            (harvest-module (module-spec-path spec)
                            (module-spec-uir spec)
                            (module-spec-imports spec)))))
