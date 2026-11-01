#lang racket

;; racklr/lower-racket/resolve.rkt
;;
;; Second pass of the self-host front end: resolve names across the scanned
;; module graph.
;;
;; The effect fixpoint identifies call-graph nodes by the pair
;; (component . name). A Racket helper such as `find-kid` can be defined in
;; several front-end modules, so the lowerer cannot leave cross-file calls as
;; bare names: it resolves every call through the module's imports to the
;; module that defines the target, and both definition and call sites use the
;; mangled name "<defining module id>::<local name>". This makes the compiler's
;; own call graph explicit without a heuristic in engine code.
;;
;; Resolution is data: exports come from `provide`, imports from `require`
;; (plain, only-in, except-in, rename-in, prefix-in). External libraries stay
;; unresolved and their calls remain bare names for the effect-token lexicon.

(require racket/list
         racket/string
         "scan.rkt")

(provide build-resolution
         resolved-imports
         resolve-source-id
         mangle-name
         mangle-target)

;; infos: hash module-id -> module-info
;; → hash module-id -> (hash local-name -> (cons target-module-id local-name))
;; The table only contains imports that resolve to a scanned corpus module.
(define (build-resolution infos)
  (define exports-cache (make-hash))
  (define imports-cache (make-hash))
  (define visiting (make-hash))

  ;; hash export-name -> (cons defining-module-id local-name)
  ;;
  ;; A provided name that is not defined locally is a re-export of an imported
  ;; name (e.g. racklr/effects/imports.rkt re-exports handle-path from
  ;; imports/handle.rkt): resolve it through this module's own import table.
  (define (exports-of id)
    (cond
      [(hash-has-key? exports-cache id) (hash-ref exports-cache id)]
      [(hash-has-key? visiting (list 'exports id)) (hash)]
      [else
       (hash-set! visiting (list 'exports id) #t)
       (define info (hash-ref infos id))
       (define out (make-hash))
       (for ([spec (in-list (module-info-provides info))])
         (add-export! id info out spec))
       (hash-remove! visiting (list 'exports id))
       (hash-set! exports-cache id out)
       out]))

  (define (add-export! id info out spec)
    (define (add! export-name target)
      (if (hash-has-key? out export-name) (void) (hash-set! out export-name target)))
    (case (car spec)
      [(name)
       (define nm (second spec))
       (cond
         [(hash-has-key? (module-info-defines info) nm) (add! nm (cons id nm))]
         [else
          ;; re-export of an imported name
          (define tgt (hash-ref (imports-of id) nm #f))
          (if tgt (add! nm tgt) (void))])]
      [(all-from-out)
       (define src (resolve-source-id (second spec) infos))
       (if src
           (for ([(ext tgt) (in-hash (exports-of src))]) (add! ext tgt))
           (void))]
      [(all-defined-out)
       (for ([(nm kind) (in-hash (module-info-defines info))])
         (if (eq? kind 'syntax) (void) (add! nm (cons id nm))))]
      [(struct-out)
       (for ([nm (in-list (struct-names info (second spec)))]) (add! nm (cons id nm)))]
      [(rename-out)
       (for ([pr (in-list (cdr spec))])
         (if (pair? pr)
             (let ([local (car pr)])
               (cond
                 [(hash-has-key? (module-info-defines info) local)
                  (add! (cdr pr) (cons id local))]
                 [else
                  (define tgt (hash-ref (imports-of id) local #f))
                  (if tgt (add! (cdr pr) tgt) (void))]))
             (void)))]
      [(prefix-out)
       (define pfx (and (symbol? (second spec)) (symbol->string (second spec))))
       (if pfx
           (let ([inner (make-hash)])
             (for ([sub (in-list (third spec))]) (add-export! id info inner sub))
             (for ([(ext tgt) (in-hash inner)])
               (add! (string->symbol (string-append pfx (symbol->string ext))) tgt)))
           (void))]
      [else (void)]))

  ;; hash local-name -> (cons module-id local-name)
  (define (imports-of id)
    (cond
      [(hash-has-key? imports-cache id) (hash-ref imports-cache id)]
      [(hash-has-key? visiting (list 'imports id)) (hash)]
      [else
       (hash-set! visiting (list 'imports id) #t)
       (define info (hash-ref infos id))
       (define out (make-hash))
       (define (add-local! local target)
         (if (hash-has-key? out local) (void) (hash-set! out local target)))
       (define (import-all! exports excluded)
         (for ([(ext tgt) (in-hash exports)])
           (if (member ext excluded) (void) (add-local! ext tgt))))
       (for ([clause (in-list (module-info-requires info))])
         (define kind (car clause))
         (define src (resolve-source-id (and (pair? (cdr clause)) (second clause)) infos))
         (if src
             (let ([exports (exports-of src)])
               (case kind
                 [(plain) (import-all! exports '())]
                 [(only)
                  (for ([pr (in-list (cddr clause))])
                    (if (pair? pr)
                        (let ([tgt (hash-ref exports (cdr pr) #f)])
                          (if tgt (add-local! (car pr) tgt) (void)))
                        (void)))]
                 [(except) (import-all! exports (cddr clause))]
                 [(rename)
                  (for ([pr (in-list (cddr clause))])
                    (if (pair? pr)
                        (let ([tgt (hash-ref exports (car pr) #f)])
                          (if tgt (add-local! (cdr pr) tgt) (void)))
                        (void)))]
                 [(prefix)
                  (define pfx (caddr clause))
                  (if pfx
                      (for ([(ext tgt) (in-hash exports)])
                        (add-local! (string->symbol
                                     (string-append pfx (symbol->string ext)))
                                    tgt))
                      (void))]
                 [else (void)]))
             (void)))
       (hash-remove! visiting (list 'imports id))
       (hash-set! imports-cache id out)
       out]))

  (for/hash ([id (in-hash-keys infos)])
    (values id (imports-of id))))

;; The resolved import table for one module (empty when it imports no corpus
;; module).
(define (resolved-imports resolution id) (hash-ref resolution id (hash)))

;; A scanned module id symbol, or #f for an external library id.
(define (resolve-source-id source infos)
  (and (symbol? source) (hash-has-key? infos source) source))

;; Module ids are data; the mangled name is the call-graph node identity.
(define (mangle-name module-id local-name)
  (string->symbol (format "~a::~a" module-id local-name)))

;; A resolved (module-id . local-name) → mangled call-graph node symbol.
(define (mangle-target target)
  (mangle-name (car target) (cdr target)))

;; Generated names a struct provides / defines locally.
(define (struct-names info struct-name)
  (define fields (hash-ref (module-info-structs info) struct-name '()))
  (hash-keys (struct-generated-names struct-name fields '())))
