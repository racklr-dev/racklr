#lang racket

;; racklr/uir/kernel.rkt — the hand-written, versioned fixed-point kernel:
;; the vocabulary the self-describing UIR schema is written in.
;;
;; The node kinds of the IR are data (see schema-data.rkt and the generator
;; schema-macro.rkt), but the vocabulary the schema is written in is not. The
;; three bootstrap node kinds — `uir-schema`, `uir-node-kind`, and
;; `uir-schema-field` — and the field codecs are Racket definitions written by
;; hand here. That is the fixed point: the schema describes every node kind,
;; including the three bootstrap kinds it is itself made of, and
;; schema-check.rkt validates the schema value against its own declarations.
;;
;; The hand-written kernel is the trust root and is versioned. Bump
;; UIR-KERNEL-VERSION whenever the vocabulary or the codec set changes shape.
;; The schema value declares the kernel version it requires, and loading a
;; schema whose kernel version does not match is an error, so a kernel change
;; is an explicit, versioned event rather than a silent reinterpretation.

(require racket/list)

(provide
 ;; Version of the hand-written kernel (trust root).
 UIR-KERNEL-VERSION
 ;; Layouts: an ordered description of one node kind.
 uir-layout uir-layout? uir-layout-tag uir-layout-ctor uir-layout-pred
 uir-layout-accessors uir-layout-codecs uir-layout-field-names uir-layout-defaults
 KERNEL-LAYOUTS
 layout-for-value layout-by-tag
 ;; Hand-written bootstrap node kinds (the vocabulary the schema is made of).
 uir-schema uir-schema? uir-schema-version uir-schema-kernel-version
 uir-schema-node-kinds
 uir-node-kind uir-node-kind? uir-node-kind-kind uir-node-kind-ctor
 uir-node-kind-kernel? uir-node-kind-fields
 uir-schema-field uir-schema-field? uir-schema-field-name
 uir-schema-field-type uir-schema-field-default
 ;; Field codecs and how they map values to serialized data.
 UIR-FIELD-CODECS UIR-FIELD-DEFAULTS
 field-default-marker? field-defaulted? field-default-value
 field-value-is-default?
 ;; Parameter binding kinds (domain of the `param-kind` codec).
 UIR-PARAM-KINDS uir-param-kind?
 ;; Schema introspection.
 schema-kind-decls schema-kind-decl-by-tag schema-kind-field-names
 schema-kind-field-codecs schema-kind-field-defaults)

;; ── Version ────────────────────────────────────────────────────────

(define UIR-KERNEL-VERSION 2)

;; ── Layouts ────────────────────────────────────────────────────────

;; One node kind: its tag, its structure/constructor, the predicate and field
;; accessors used to read an instance, the codec for each field, and the field
;; names. Generated layouts (schema-macro.rkt) are derived from the schema
;; value; KERNEL-LAYOUTS below is the hand-written part.
(struct uir-layout (tag ctor pred accessors codecs field-names defaults) #:transparent)

;; ── Bootstrap vocabulary: the three node kinds the schema is made of ─

(struct uir-schema (version kernel-version node-kinds) #:transparent)

(struct uir-node-kind (kind ctor kernel? fields) #:transparent)

;; A field declares its name, its codec, and a default marker. Defaults are
;; a payload-compatibility mechanism, not optional constructor arguments: the
;; generated struct and constructor stay exact-arity (see schema-data.rkt).
(struct uir-schema-field (name type default) #:transparent)

(define KERNEL-LAYOUTS
  (list
   (uir-layout 'schema uir-schema uir-schema?
               (list uir-schema-version
                     uir-schema-kernel-version
                     uir-schema-node-kinds)
               '(int int list-uir)
               '(version kernel-version node-kinds)
               '(required required required))
   (uir-layout 'node-kind uir-node-kind uir-node-kind?
               (list uir-node-kind-kind
                     uir-node-kind-ctor
                     uir-node-kind-kernel?
                     uir-node-kind-fields)
               '(symbol symbol bool list-uir)
               '(kind ctor kernel? fields)
               '(required required required required))
   (uir-layout 'schema-field uir-schema-field uir-schema-field?
               (list uir-schema-field-name
                     uir-schema-field-type
                     uir-schema-field-default)
               '(symbol symbol symbol)
               '(name type default)
               '(required required required))))

;; ── Field codecs ───────────────────────────────────────────────────
;;
;; A field type in the schema is one of these codec names. Every codec is
;; hand-written here: adding a new *node kind* only needs the schema, but
;; adding a new *field type* is a kernel change and bumps UIR-KERNEL-VERSION.
;;
;;   uir                  a UIR node, recursively encoded
;;   list-uir             a list of UIR nodes, encoded as one nested list
;;   spliced-uir          a list of UIR nodes, spliced into the node's sexp
;;   list-pair-uir        a list of (cons uir uir), encoded as (k v) pairs
;;   spliced-pair-uir     same, spliced into the node's sexp
;;   catch-list           a list of (list uir uir uir) try/catch triples
;;   with-item-list       a list of (list uir optional-uir) with items
;;   handler-op-list      a list of (list uir list-uir uir) handler ops
;;   eff-op-list          a list of (list uir list-uir optional-uir uir) ops
;;   optional-uir         #f, or a UIR node encoded recursively
;;   optional-uir-marker  #f, or a UIR node; #f is written as 'unspecified
;;   optional-int         #f, or an exact integer
;;   text                 a uir-string node, written as its raw content
;;   raw                  passed through untouched
;;   bool string symbol   the Racket value with that type
;;   name                 an identifier: a string, or a legacy packaged symbol
;;   param-kind           #f or a Python parameter binding kind (`kw-only`,
;;                        `var-positional`, `var-keyword`); serialized as a
;;                        string so it survives JSON's symbol/string mapping
;;   int                  an exact integer
;;
;; A field additionally carries a default marker: `required` or `false` (the
;; Racket value #f). Only a trailing suffix of fields may be defaulted. The
;; serialized payload omits trailing fields whose value equals the default and
;; the reader fills them back in, which keeps older payloads readable. The
;; generated struct and constructor are still exact-arity; a default is a
;; payload-compatibility mechanism, not an optional constructor argument.

(define UIR-FIELD-CODECS
  '(uir list-uir spliced-uir list-pair-uir spliced-pair-uir
    catch-list with-item-list handler-op-list eff-op-list
    optional-uir optional-uir-marker optional-int
    text raw bool string symbol name param-kind int))

(define UIR-FIELD-DEFAULTS '(required false))

(define (field-default-marker? m)
  (and (symbol? m) (memq m UIR-FIELD-DEFAULTS) #t))

(define (field-defaulted? m)
  (not (eq? m 'required)))

;; The value a missing (trailing) defaulted field decodes to.
(define (field-default-value marker)
  (case marker
    [(required) (error 'uir-field-default "required field has no default value")]
    [(false) #f]
    [else (error 'uir-field-default "unknown field default marker: ~e" marker)]))

;; True when a present field value may be omitted from its serialized form.
(define (field-value-is-default? marker v)
  (case marker
    [(required) #f]
    [(false) (eq? v #f)]
    [else (error 'uir-field-default "unknown field default marker: ~e" marker)]))

;; The closed set of Python parameter binding kinds the `param-kind` codec
;; admits. `#f` is positional-or-keyword (the value every non-Python front end
;; produces). Shared with schema-check.rkt's validation.
(define UIR-PARAM-KINDS '(kw-only var-positional var-keyword))

(define (uir-param-kind? k)
  (or (eq? k #f) (and (memq k UIR-PARAM-KINDS) #t)))


;; ── Dispatch over a layout list ────────────────────────────────────

(define (layout-for-value v layouts)
  (and (struct? v)
       (for/or ([l (in-list layouts)])
         (and ((uir-layout-pred l) v) l))))

(define (layout-by-tag tag layouts)
  (findf (lambda (l) (eq? (uir-layout-tag l) tag)) layouts))

;; ── Schema introspection ───────────────────────────────────────────

(define (schema-kind-decls schema)
  (uir-schema-node-kinds schema))

(define (schema-kind-decl-by-tag schema tag)
  (findf (lambda (d) (eq? (uir-node-kind-kind d) tag))
         (schema-kind-decls schema)))

(define (schema-kind-field-names decl)
  (for/list ([f (in-list (uir-node-kind-fields decl))])
    (uir-schema-field-name f)))

(define (schema-kind-field-codecs decl)
  (for/list ([f (in-list (uir-node-kind-fields decl))])
    (uir-schema-field-type f)))

(define (schema-kind-field-defaults decl)
  (for/list ([f (in-list (uir-node-kind-fields decl))])
    (uir-schema-field-default f)))
