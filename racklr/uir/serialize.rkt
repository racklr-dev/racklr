#lang racket

;; racklr/uir/serialize.rkt — UIR <-> s-expression serialization.
;;
;; The codec itself is schema-driven: schema-io.rkt reads and writes a UIR
;; value through its layout, so this module only supplies the default layout
;; registry. Every layout-aware entry point takes an optional layout list, so
;; a schema extension can serialize its new kinds with its own registry.

(require "types.rkt"
         "schema-io.rkt")

(provide uir->sexp sexp->uir)

(define (uir->sexp v [layouts UIR-LAYOUTS])
  (uir->sexp* v layouts))

(define (sexp->uir s [layouts UIR-LAYOUTS])
  (sexp->uir* s layouts))
