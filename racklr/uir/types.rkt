#lang racket

;; racklr/uir/types.rkt — the UIR node kinds, generated from the schema value.
;;
;; This module no longer defines any node kind by hand. It loads the schema
;; (schema-data.rkt), validates it against its own declarations
;; (schema-check.rkt), and lets the kernel's generator (schema-macro.rkt)
;; synthesise every struct, constructor, predicate, accessor, and serializer
;; layout. Adding a node kind is an addition to the schema value only.

(require "schema-data.rkt"
         "schema-check.rkt"
         "schema-macro.rkt")

;; `check-schema!` returns #t; a bare top-level expression in a loaded module
;; is echoed to stdout, which would prepend "#t" to any program that treats
;; this module's process stdout as data (e.g. tools/self-emit.rkt oracle).
;; Discard the value so loading the UIR does not write to stdout.
(void (check-schema! UIR-SCHEMA))

(define-uir-kinds)
