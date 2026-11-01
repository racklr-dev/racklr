#lang racket

;; racklr/uir/schema-macro.rkt — the schema-driven node-kind generator.
;;
;; Part of the hand-written, versioned kernel (see kernel.rkt). The macro reads
;; the UIR schema value and synthesises, for every node kind the schema
;; declares, the Racket struct, its constructor/predicate/accessors, and the
;; layout that lets the generic reader and writer handle it. Nothing in this
;; file names a node kind: adding a kind to schema-data.rkt (or passing an
;; extended schema value) is enough.
;;
;; `(define-uir-kinds)` generates from the canonical schema-data.rkt value.
;; `(define-uir-kinds <expr>)` generates from a schema value evaluated at
;; expansion time (phase 1), which is how a schema extension is proven: append
;; a node kind with schema-append-kind and rerun the generator, no core struct
;; edit.

(require (for-syntax racket/base
                     racket/list
                     racket/syntax
                     "kernel.rkt"
                     "schema-check.rkt"
                     "schema-data.rkt")
         "kernel.rkt")

(provide define-uir-kinds)

(define-for-syntax (generated-identifier ctx symbol)
  (datum->syntax ctx symbol))

(define-for-syntax (quoted ctx datum)
  (datum->syntax ctx (list 'quote datum)))

(define-for-syntax (generate-uir-kinds ctx schema)
  ;; The fixed point is validated at expansion time: the schema value must be
  ;; a well-formed schema, and its kernel-owned declarations must match the
  ;; hand-written kernel.
  (check-schema! schema)
  (define decls (schema-kind-decls schema))
  (define ir-decls
    (for/list ([d (in-list decls)] #:unless (uir-node-kind-kernel? d)) d))
  (define (field-ids decl)
    (for/list ([f (in-list (uir-node-kind-fields decl))])
      (generated-identifier ctx (uir-schema-field-name f))))
  (define struct-defs
    (for/list ([d (in-list ir-decls)])
      #`(struct #,(generated-identifier ctx (uir-node-kind-ctor d))
                (#,@(field-ids d))
                #:transparent)))
  (define layout-entries
    (for/list ([d (in-list ir-decls)])
      (define ctor (uir-node-kind-ctor d))
      (define tag (uir-node-kind-kind d))
      (define pred (generated-identifier ctx (string->symbol (format "~a?" ctor))))
      (define ctor-id (generated-identifier ctx ctor))
      (define fields (uir-node-kind-fields d))
      (define accessor-ids
        (for/list ([f (in-list fields)])
          (generated-identifier
           ctx (string->symbol (format "~a-~a" ctor (uir-schema-field-name f))))))
      (define codecs (for/list ([f (in-list fields)])
                       (quoted ctx (uir-schema-field-type f))))
      (define names (for/list ([f (in-list fields)])
                      (quoted ctx (uir-schema-field-name f))))
      (define defaults (for/list ([f (in-list fields)])
                         (quoted ctx (uir-schema-field-default f))))
      #`(uir-layout #,(quoted ctx tag) #,ctor-id #,pred
                    (list #,@accessor-ids)
                    (list #,@codecs)
                    (list #,@names)
                    (list #,@defaults))))
  (define provides
    (append
     (for/list ([d (in-list ir-decls)])
       #`(struct-out #,(generated-identifier ctx (uir-node-kind-ctor d))))
     (list (generated-identifier ctx 'UIR-LAYOUTS))))
  (define ir-layouts-id (generated-identifier ctx 'UIR-IR-LAYOUTS))
  (define layouts-id (generated-identifier ctx 'UIR-LAYOUTS))
  #`(begin
      #,@struct-defs
      (define #,ir-layouts-id
        (list #,@layout-entries))
      ;; The full registry: the hand-written kernel layouts plus every
      ;; schema-generated layout, in one layout list.
      (define #,layouts-id (append KERNEL-LAYOUTS #,ir-layouts-id))
      (provide #,@provides)))

(define-syntax (define-uir-kinds stx)
  (syntax-case stx ()
    [(_)
     (generate-uir-kinds (syntax-local-introduce stx) UIR-SCHEMA)]
    [(_ schema-expr)
     (generate-uir-kinds (syntax-local-introduce stx)
                         (syntax-local-eval #'schema-expr))]))
