#lang racket

(require json)
(require "serialize.rkt")

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

(define UIR-SCHEMA-VERSION 1)

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

(define (uir->versioned-sexp v)
  `(uir ,UIR-SCHEMA-VERSION ,(uir->sexp v)))

(define (versioned-sexp->uir s [migrations uir-migrations])
  (match s
    [`(uir ,version ,payload)
     (unless (and (exact-nonnegative-integer? version)
                  (<= version UIR-SCHEMA-VERSION))
       (error 'versioned-sexp->uir
              "unsupported UIR schema version ~e (current: ~a)"
              version UIR-SCHEMA-VERSION))
     (sexp->uir (migrate-uir-payload payload version migrations))]
    [_ (error 'versioned-sexp->uir "not a versioned UIR s-expression: ~e" s)]))

;; ── JSON encoding (versioned) ──────────────────────────────────────

(define (uir-sexp->jsexpr s)
  (cond [(symbol? s) (symbol->string s)]
        [(list? s) (map uir-sexp->jsexpr s)]
        [(pair? s) (cons (uir-sexp->jsexpr (car s)) (uir-sexp->jsexpr (cdr s)))]
        [else s]))

(define uir-tag-set (set 'uir 'null 'bool 'number 'string 'fstring 'list 'record 'symbol
                         'fn 'call 'let 'var 'set! 'ann-set! 'walrus 'type-alias 'if 'block 'return 'for-each 'while 'try 'with 'await 'yield 'decorated 'get 'paren
                         'class 'method 'field 'new 'interface 'module 'import 'export 'enum 'enum-variant
                         'component 'element 'attribute 'event 'slot 'text-node 'style 'effect 'state 'jsx-expr 'spread
                         'effect-row 'effect-entry 'subtype 'effect-decl 'effect-op 'effect-handler
                         'match 'case 'pat-literal 'pat-capture 'pat-wildcard 'pat-value 'pat-or 'pat-as
                         'pat-sequence 'pat-star 'pat-mapping 'pat-double-star 'pat-class 'pat-group
                         'eff-system 'eff-capability 'eff-capability-op 'eff-ref 'eff-given 'eff-handler
                         'eff-component 'eff-state 'eff-action 'eff-invariant 'eff-policy))

(define (uir-jsexpr->sexp j)
  (match j
    [(? list? l)
     (if (and (pair? l) (string? (car l))
              (set-member? uir-tag-set (string->symbol (car l))))
         (cons (string->symbol (car l))
               (map uir-jsexpr->sexp (cdr l)))
         (map uir-jsexpr->sexp l))]
    [(? pair? p) (cons (uir-jsexpr->sexp (car p)) (uir-jsexpr->sexp (cdr p)))]
    [_ j]))

(define (uir->json v)
  (jsexpr->string (uir-sexp->jsexpr (uir->versioned-sexp v))))

(define (json->uir jstr)
  (versioned-sexp->uir (uir-jsexpr->sexp (string->jsexpr jstr))))
