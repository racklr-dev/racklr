#lang racket

(require json
         "serialize.rkt"
         "types.rkt"
         "kernel.rkt"
         "schema-io.rkt"
         "schema-data.rkt")

(provide
 UIR-SCHEMA-VERSION
 uir->versioned-sexp versioned-sexp->uir
 register-uir-migration! migrate-uir-payload
 uir->json json->uir)

;; ── Schema versioning and migration ────────────────────────────────
;;
;; Serialized UIR is wrapped in an envelope carrying the schema version it
;; was written with. Reading a payload written by an older schema runs the
;; registered migration chain, so a struct field-count change is a
;; controlled, explicit event instead of a silent deserialization failure.
;; The bare `uir->sexp` / `sexp->uir` pair stays version-free: it is the
;; in-memory representation the hypergraph and the checkers consume.

;; The payload version is declared by the schema value itself (single
;; source of truth); the hand-written kernel checks that the schema value it
;; loads is a schema it supports. Bumping the version in schema-data.rkt is
;; therefore a schema-version event, not a constant edit in two places.
(define UIR-SCHEMA-VERSION (uir-schema-version UIR-SCHEMA))

;; from-version -> (payload-sexp -> payload-sexp)
(define uir-migrations (make-hasheqv))

(define (register-uir-migration! from-version migrate)
  (unless (and (exact-nonnegative-integer? from-version)
               (< from-version UIR-SCHEMA-VERSION))
    (error 'register-uir-migration!
           "migration source version must be an integer below ~a, got ~e"
           UIR-SCHEMA-VERSION from-version))
  (unless (procedure? migrate)
    (error 'register-uir-migration! "migration must be a procedure, got ~e" migrate))
  (hash-set! uir-migrations from-version migrate))

;; v2 adds the optional trailing `typed-param` parameter-kind slot. A version-1
;; payload is valid as-is (the reader fills the kind with #f), so the v1 -> v2
;; migration is the identity.
(register-uir-migration! 1 (lambda (payload) payload))

(define (migrate-uir-payload payload from-version [migrations uir-migrations])
  (let loop ([p payload] [v from-version])
    (if (>= v UIR-SCHEMA-VERSION)
        p
        (let ([step (hash-ref migrations v #f)])
          (unless step
            (error 'migrate-uir-payload
                   "no migration registered from UIR schema version ~a to ~a"
                   v (add1 v)))
          (loop (step p) (add1 v))))))

(define (uir->versioned-sexp v [layouts UIR-LAYOUTS])
  `(uir ,UIR-SCHEMA-VERSION ,(uir->sexp v layouts)))

(define (versioned-sexp->uir s [migrations uir-migrations] [layouts UIR-LAYOUTS])
  (match s
    [`(uir ,version ,payload)
     (unless (and (exact-nonnegative-integer? version)
                  (<= version UIR-SCHEMA-VERSION))
       (error 'versioned-sexp->uir
              "unsupported UIR schema version ~e (current: ~a)"
              version UIR-SCHEMA-VERSION))
     (sexp->uir (migrate-uir-payload payload version migrations) layouts)]
    [_ (error 'versioned-sexp->uir "not a versioned UIR s-expression: ~e" s)]))

;; ── JSON encoding (versioned) ──────────────────────────────────────
;;
;; The writer is generic: symbols become JSON strings. The reader is
;; schema-aware (schema-io.rkt): it restores symbols in `symbol` field
;; positions, so JSON round trips hold for every schema-declared node kind.

(define (uir-sexp->jsexpr s)
  (cond [(symbol? s) (symbol->string s)]
        [(list? s) (map uir-sexp->jsexpr s)]
        [(pair? s) (cons (uir-sexp->jsexpr (car s)) (uir-sexp->jsexpr (cdr s)))]
        [else s]))

(define (uir->json v [layouts UIR-LAYOUTS])
  (jsexpr->string (uir-sexp->jsexpr (uir->versioned-sexp v layouts))))

(define (json->uir jstr [layouts UIR-LAYOUTS])
  (versioned-sexp->uir (jsexpr->uir-sexp (string->jsexpr jstr) layouts)
                       uir-migrations layouts))
