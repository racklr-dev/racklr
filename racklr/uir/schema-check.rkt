#lang racket

;; racklr/uir/schema-check.rkt — validation and extension of the UIR schema.
;;
;; Part of the hand-written, versioned kernel (see kernel.rkt). `check-schema!`
;; proves the fixed point: it validates the schema value as a UIR value using
;; only the declarations the schema itself contains, and it checks that the
;; kernel-owned node kinds declared by the schema match the hand-written
;; KERNEL-LAYOUTS exactly. `schema-append-kind` adds a node kind to the schema
;; value, which is how the code generator learns a new kind — no core struct
;; definition changes. Defaulted fields are validated as a trailing suffix: the
;; serializer omits them from the payload and the reader fills them back in.

(require racket/list
         racket/match
         "kernel.rkt")

(provide check-schema! validate-uir-against-schema schema-append-kind)

(define (proper-list? v) (and (list? v) #t))

;; ── Validating a UIR value against a schema ────────────────────────
;;
;; This validator reads a UIR value structurally (struct->vector) and checks
;; every field against the schema's declared codec. It needs no generated
;; layouts, so the kernel can validate the schema value against the schema.

(define (validate-uir-against-schema v schema)
  (define decls (schema-kind-decls schema))
  (define (struct-type-symbol ctor) (string->symbol (format "struct:~a" ctor)))
  (define (decl-for-struct st)
    (for/or ([d (in-list decls)])
      (and (eq? (struct-type-symbol (uir-node-kind-ctor d)) st) d)))
  (define (bad where fmt . args)
    (error 'schema-validation "~a at ~a" (apply format fmt args) (reverse where)))
  (define (check-value codec x where)
    (case codec
      [(uir) (check-node x where)]
      [(list-uir)
       (unless (proper-list? x) (bad where "expected a list"))
       (for ([e (in-list x)]) (check-node e where))]
      [(list-pair-uir)
       (unless (proper-list? x) (bad where "expected a list of pairs"))
       (for ([p (in-list x)])
         (unless (pair? p) (bad where "expected a pair"))
         (check-node (car p) where)
         (check-node (cdr p) where))]
      [(catch-list)
       (unless (proper-list? x) (bad where "expected a catch list"))
       (for ([t (in-list x)])
         (unless (and (proper-list? t) (= (length t) 3))
           (bad where "expected a 3-element catch clause"))
         (for ([e (in-list t)]) (check-node e where)))]
      [(with-item-list)
       (unless (proper-list? x) (bad where "expected a with-item list"))
       (for ([p (in-list x)])
         (unless (and (proper-list? p) (= (length p) 2))
           (bad where "expected a 2-element with item"))
         (check-node (car p) where)
         (if (cadr p) (check-node (cadr p) where) (void)))]
      [(handler-op-list)
       (unless (proper-list? x) (bad where "expected a handler op list"))
       (for ([t (in-list x)])
         (unless (and (proper-list? t) (= (length t) 3))
           (bad where "expected a 3-element handler op"))
         (check-node (car t) where)
         (unless (proper-list? (cadr t)) (bad where "expected a parameter list"))
         (for ([e (in-list (cadr t))]) (check-node e where))
         (check-node (caddr t) where))]
      [(eff-op-list)
       (unless (proper-list? x) (bad where "expected an effect op list"))
       (for ([q (in-list x)])
         (unless (and (proper-list? q) (= (length q) 4))
           (bad where "expected a 4-element effect op"))
         (check-node (car q) where)
         (unless (proper-list? (cadr q)) (bad where "expected a parameter list"))
         (for ([e (in-list (cadr q))]) (check-node e where))
         (if (caddr q) (check-node (caddr q) where) (void))
         (check-node (cadddr q) where))]
      [(optional-uir) (if x (check-node x where) (void))]
      [(optional-uir-marker) (if x (check-node x where) (void))]
      [(optional-int) (unless (or (eq? x #f) (exact-integer? x))
                        (bad where "expected #f or an integer"))]
      [(text)
       (check-node x where)
       (define string-decl (schema-kind-decl-by-tag schema 'string))
       (unless (and string-decl
                    (eq? (struct-type-symbol (uir-node-kind-ctor string-decl))
                         (vector-ref (struct->vector x) 0)))
         (bad where "expected the schema's string node"))]
      [(raw) (void)]
      [(bool) (unless (boolean? x) (bad where "expected a boolean"))]
      [(string) (unless (string? x) (bad where "expected a string"))]
      [(symbol) (unless (symbol? x) (bad where "expected a symbol"))]
      [(name) (unless (or (string? x) (symbol? x))
                (bad where "expected a name (string or symbol)"))]
      [(param-kind) (unless (uir-param-kind? x)
                      (bad where "expected #f or a parameter binding kind"))]
      [(int) (unless (exact-integer? x) (bad where "expected an integer"))]
      [else (bad where "unknown field codec ~e" codec)]))
  (define (check-node x where)
    (unless (struct? x) (bad where "expected a UIR node, got ~e" x))
    (define sv (struct->vector x))
    (define decl (decl-for-struct (vector-ref sv 0)))
    (unless decl (bad where "unknown UIR node kind ~a" (vector-ref sv 0)))
    (define fields (cdr (vector->list sv)))
    (define decl-fields (uir-node-kind-fields decl))
    (unless (= (length fields) (length decl-fields))
      (bad where "~a expects ~a field(s), got ~a"
           (uir-node-kind-ctor decl) (length decl-fields) (length fields)))
    (for ([f (in-list decl-fields)] [val (in-list fields)])
      (check-value (uir-schema-field-type f) val
                   (cons (uir-schema-field-name f) where))))
  (check-node v '())
  #t)

;; ── Validating a schema value as a schema ──────────────────────────

(define (check-schema! schema)
  (unless (uir-schema? schema)
    (error 'check-schema! "not a UIR schema value: ~e" schema))
  (define version (uir-schema-version schema))
  (define kernel-version (uir-schema-kernel-version schema))
  (unless (and (exact-integer? version) (positive? version))
    (error 'check-schema! "schema version must be a positive integer, got ~e" version))
  (unless (and (exact-integer? kernel-version) (positive? kernel-version))
    (error 'check-schema! "schema kernel version must be a positive integer, got ~e"
           kernel-version))
  (unless (= kernel-version UIR-KERNEL-VERSION)
    (error 'check-schema! "schema requires kernel version ~a, this kernel implements ~a"
           kernel-version UIR-KERNEL-VERSION))
  (define decls (schema-kind-decls schema))
  (unless (proper-list? decls)
    (error 'check-schema! "schema node-kinds must be a list"))
  (define seen-tags '())
  (define seen-ctors '())
  (for ([d (in-list decls)])
    (unless (uir-node-kind? d)
      (error 'check-schema! "node kind declaration is not a uir-node-kind: ~e" d))
    (define tag (uir-node-kind-kind d))
    (define ctor (uir-node-kind-ctor d))
    (unless (symbol? tag) (error 'check-schema! "node kind tag must be a symbol: ~e" tag))
    (unless (symbol? ctor) (error 'check-schema! "node kind ctor must be a symbol: ~e" ctor))
    (unless (boolean? (uir-node-kind-kernel? d))
      (error 'check-schema! "node kind kernel? must be a boolean: ~e" d))
    (when (memq tag seen-tags)
      (error 'check-schema! "duplicate node kind tag: ~e" tag))
    (when (memq ctor seen-ctors)
      (error 'check-schema! "duplicate node kind constructor: ~e" ctor))
    (set! seen-tags (cons tag seen-tags))
    (set! seen-ctors (cons ctor seen-ctors))
    (define fields (uir-node-kind-fields d))
    (unless (proper-list? fields)
      (error 'check-schema! "node kind ~a fields must be a list" tag))
    (define names '())
    (for ([f (in-list fields)])
      (unless (uir-schema-field? f)
        (error 'check-schema! "node kind ~a field is not a uir-schema-field: ~e" tag f))
      (define nm (uir-schema-field-name f))
      (define ty (uir-schema-field-type f))
      (define df (uir-schema-field-default f))
      (unless (symbol? nm)
        (error 'check-schema! "node kind ~a field name must be a symbol: ~e" tag nm))
      (unless (memq ty UIR-FIELD-CODECS)
        (error 'check-schema! "node kind ~a field ~a has unknown codec: ~e" tag nm ty))
      (unless (field-default-marker? df)
        (error 'check-schema! "node kind ~a field ~a has unknown default marker: ~e"
               tag nm df))
      (when (and (field-defaulted? df)
                 (not (memq ty '(optional-uir optional-uir-marker optional-int
                                 param-kind))))
        (error 'check-schema! "node kind ~a field ~a codec ~a cannot default to #f"
               tag nm ty))
      (when (memq nm names)
        (error 'check-schema! "node kind ~a has duplicate field name: ~e" tag nm))
      (set! names (cons nm names)))
    (define codecs (schema-kind-field-codecs d))
    (define defaults (schema-kind-field-defaults d))
    ;; Defaulted fields may only form a trailing suffix: the serialized payload
    ;; omits them from the end, so a required field cannot follow one.
    (let loop ([ds (reverse defaults)] [seen-required? #f])
      (cond [(null? ds) (void)]
            [(eq? (car ds) 'required) (loop (cdr ds) #t)]
            [(and seen-required? (eq? (car ds) 'false))
             (error 'check-schema! "node kind ~a: defaulted fields must be trailing" tag)]
            [else (loop (cdr ds) seen-required?)]))
    (when (or (memq 'spliced-uir codecs) (memq 'spliced-pair-uir codecs))
      (unless (and (= 1 (length codecs))
                   (or (eq? (first codecs) 'spliced-uir)
                       (eq? (first codecs) 'spliced-pair-uir)))
        (error 'check-schema! "node kind ~a: spliced codecs must be the only field" tag))))
  ;; The hand-written kernel layouts must be exactly what the schema declares
  ;; for the kernel node kinds, and every kernel-declared kind must be here.
  (for ([l (in-list KERNEL-LAYOUTS)])
    (define d (schema-kind-decl-by-tag schema (uir-layout-tag l)))
    (unless d
      (error 'check-schema! "kernel layout tag ~a is missing from the schema"
             (uir-layout-tag l)))
    (unless (eq? (uir-node-kind-kernel? d) #t)
      (error 'check-schema! "kernel layout tag ~a must be declared kernel? #t"
             (uir-layout-tag l)))
    (unless (and (eq? (uir-node-kind-ctor d) (object-name (uir-layout-ctor l)))
                 (eq? (uir-node-kind-kind d) (uir-layout-tag l))
                 (equal? (schema-kind-field-names d) (uir-layout-field-names l))
                 (equal? (schema-kind-field-codecs d) (uir-layout-codecs l))
                 (equal? (schema-kind-field-defaults d)
                         (uir-layout-defaults l)))
      (error 'check-schema! "schema declaration for ~a does not match the hand-written kernel"
             (uir-layout-tag l))))
  (for ([d (in-list decls)] #:when (uir-node-kind-kernel? d))
    (unless (for/or ([l (in-list KERNEL-LAYOUTS)])
              (eq? (uir-layout-tag l) (uir-node-kind-kind d)))
      (error 'check-schema! "schema declares kernel node kind ~a that the kernel does not implement"
             (uir-node-kind-kind d))))
  ;; The fixed point: the schema value is a UIR value that validates under the
  ;; declarations it itself contains.
  (validate-uir-against-schema schema schema)
  #t)

;; ── Extending the schema (data only) ───────────────────────────────
;;
;; fields is a list of (name codec) pairs. A new node kind is an addition to
;; the schema value; no core struct definition changes.
(define (schema-append-kind schema tag ctor kernel? fields)
  (unless (uir-schema? schema)
    (error 'schema-append-kind "not a UIR schema value: ~e" schema))
  (define decl
    (uir-node-kind tag ctor kernel?
                   (for/list ([f (in-list fields)])
                     (match f
                       [(list name codec) (uir-schema-field name codec 'required)]
                       [(list name codec default) (uir-schema-field name codec default)]
                       [_ (error 'schema-append-kind "invalid field spec: ~e" f)]))))
  (struct-copy uir-schema schema
               [node-kinds (append (uir-schema-node-kinds schema) (list decl))]))
