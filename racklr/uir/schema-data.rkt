#lang racket

;; racklr/uir/schema-data.rkt — the UIR schema, written as a UIR value.
;;
;; This is the single source of truth for the node kinds of the IR. Each
;; `uir-node-kind` records the node's tag, the Racket constructor the
;; generator defines for it, whether the hand-written kernel owns it, and its
;; fields. Each `uir-schema-field` records the field name and its codec (see
;; UIR-FIELD-CODECS in kernel.rkt); the codec is the field's type.
;;
;; The last three declarations make the schema self-describing: `schema`,
;; `node-kind`, and `schema-field` are the node kinds this value is itself
;; written in. The kernel defines those three by hand and schema-check.rkt
;; verifies that this value, read under its own declarations, is valid UIR
;; data. Adding a node kind is an addition to this value only.
;;
;; Every kind is a flat, transparent Racket struct and its generated constructor
;; is exact-arity: one positional argument per field. A field's default marker
;; is a payload-compatibility mechanism, not an optional constructor argument —
;; a trailing `false`-defaulted field is omitted from the serialized payload
;; when it holds #f and the reader fills #f back in (this is how the legacy
;; three-element `typed-param` payload stays readable). Call sites pass every
;; field explicitly.
;;
;; Codec shape notes: an `spliced-uir` / `spliced-pair-uir` field is the kind's
;; only field: the struct still has that one field (a list), but its serialized
;; form is the tag followed by the list's elements (`list`, `record`,
;; `effect-row`, `style`) instead of one nested list.

(require "kernel.rkt")

(provide UIR-SCHEMA)

(define UIR-SCHEMA
  (uir-schema 2 2
    (list
      (uir-node-kind 'null 'uir-null #f '())
      (uir-node-kind 'bool 'uir-bool #f (list (uir-schema-field 'value 'bool 'required)))
      (uir-node-kind 'number 'uir-number #f (list (uir-schema-field 'value 'string 'required)))
      (uir-node-kind 'string 'uir-string #f (list (uir-schema-field 'value 'string 'required)))
      (uir-node-kind 'fstring 'uir-fstring #f (list (uir-schema-field 'value 'string 'required)))
      (uir-node-kind 'list 'uir-list #f (list (uir-schema-field 'items 'spliced-uir 'required)))
      (uir-node-kind 'record 'uir-record #f (list (uir-schema-field 'entries 'spliced-pair-uir 'required)))
      (uir-node-kind 'symbol 'uir-symbol #f (list (uir-schema-field 'name 'name 'required)))
      (uir-node-kind 'fn 'uir-fn #f
        (list (uir-schema-field 'name 'optional-uir 'required) (uir-schema-field 'params 'list-uir 'required) (uir-schema-field 'body 'uir 'required) (uir-schema-field 'return-type 'optional-uir 'required) (uir-schema-field 'effects 'optional-uir 'required)))
      (uir-node-kind 'typed-param 'uir-typed-param #f
        (list (uir-schema-field 'name 'uir 'required) (uir-schema-field 'type 'optional-uir 'required) (uir-schema-field 'default 'optional-uir 'required) (uir-schema-field 'kind 'param-kind 'false)))
      (uir-node-kind 'call 'uir-call #f
        (list (uir-schema-field 'callee 'uir 'required) (uir-schema-field 'args 'list-uir 'required)))
      (uir-node-kind 'let 'uir-let #f
        (list (uir-schema-field 'name 'uir 'required) (uir-schema-field 'value 'uir 'required) (uir-schema-field 'body 'uir 'required)))
      (uir-node-kind 'var 'uir-var #f (list (uir-schema-field 'name 'uir 'required)))
      (uir-node-kind 'set! 'uir-set! #f (list (uir-schema-field 'name 'uir 'required) (uir-schema-field 'value 'uir 'required)))
      (uir-node-kind 'ann-set! 'uir-ann-set! #f
        (list (uir-schema-field 'lhs 'uir 'required) (uir-schema-field 'type 'uir 'required) (uir-schema-field 'value 'uir 'required)))
      (uir-node-kind 'walrus 'uir-walrus #f
        (list (uir-schema-field 'name 'uir 'required) (uir-schema-field 'value 'uir 'required)))
      (uir-node-kind 'if 'uir-if #f
        (list (uir-schema-field 'test 'uir 'required) (uir-schema-field 'then 'uir 'required) (uir-schema-field 'else 'uir 'required)))
      (uir-node-kind 'block 'uir-block #f (list (uir-schema-field 'stmts 'list-uir 'required)))
      (uir-node-kind 'return 'uir-return #f (list (uir-schema-field 'value 'uir 'required)))
      (uir-node-kind 'for-each 'uir-for-each #f
        (list (uir-schema-field 'var 'uir 'required) (uir-schema-field 'iterable 'uir 'required) (uir-schema-field 'body 'uir 'required) (uir-schema-field 'else-body 'uir 'required)))
      (uir-node-kind 'while 'uir-while #f
        (list (uir-schema-field 'test 'uir 'required) (uir-schema-field 'body 'uir 'required) (uir-schema-field 'else-body 'uir 'required)))
      (uir-node-kind 'try 'uir-try #f
        (list (uir-schema-field 'body 'uir 'required) (uir-schema-field 'catches 'catch-list 'required) (uir-schema-field 'else-body 'optional-uir-marker 'required) (uir-schema-field 'finally-body 'optional-uir-marker 'required)))
      (uir-node-kind 'with 'uir-with #f
        (list (uir-schema-field 'items 'with-item-list 'required) (uir-schema-field 'body 'uir 'required)))
      (uir-node-kind 'await 'uir-await #f (list (uir-schema-field 'expr 'uir 'required)))
      (uir-node-kind 'yield 'uir-yield #f
        (list (uir-schema-field 'value 'uir 'required) (uir-schema-field 'from? 'bool 'required)))
      (uir-node-kind 'decorated 'uir-decorated #f
        (list (uir-schema-field 'decorators 'list-uir 'required) (uir-schema-field 'inner 'uir 'required)))
      (uir-node-kind 'get 'uir-get #f (list (uir-schema-field 'base 'uir 'required) (uir-schema-field 'field 'uir 'required)))
      (uir-node-kind 'paren 'uir-paren #f (list (uir-schema-field 'inner 'uir 'required)))
      (uir-node-kind 'type-alias 'uir-type-alias #f
        (list (uir-schema-field 'name 'uir 'required) (uir-schema-field 'type-params 'list-uir 'required) (uir-schema-field 'value 'uir 'required)))
      (uir-node-kind 'match 'uir-match #f
        (list (uir-schema-field 'subject 'uir 'required) (uir-schema-field 'cases 'list-uir 'required)))
      (uir-node-kind 'case 'uir-case #f
        (list (uir-schema-field 'pattern 'uir 'required) (uir-schema-field 'guard 'uir 'required) (uir-schema-field 'body 'uir 'required)))
      (uir-node-kind 'pat-literal 'uir-pat-literal #f (list (uir-schema-field 'value 'uir 'required)))
      (uir-node-kind 'pat-capture 'uir-pat-capture #f (list (uir-schema-field 'name 'uir 'required)))
      (uir-node-kind 'pat-wildcard 'uir-pat-wildcard #f '())
      (uir-node-kind 'pat-value 'uir-pat-value #f (list (uir-schema-field 'path 'uir 'required)))
      (uir-node-kind 'pat-or 'uir-pat-or #f (list (uir-schema-field 'alts 'list-uir 'required)))
      (uir-node-kind 'pat-as 'uir-pat-as #f
        (list (uir-schema-field 'pattern 'uir 'required) (uir-schema-field 'name 'optional-uir 'required)))
      (uir-node-kind 'pat-sequence 'uir-pat-sequence #f (list (uir-schema-field 'elements 'list-uir 'required)))
      (uir-node-kind 'pat-star 'uir-pat-star #f (list (uir-schema-field 'name 'optional-uir 'required)))
      (uir-node-kind 'pat-mapping 'uir-pat-mapping #f
        (list (uir-schema-field 'entries 'list-pair-uir 'required) (uir-schema-field 'rest 'optional-uir 'required)))
      (uir-node-kind 'pat-double-star 'uir-pat-double-star #f (list (uir-schema-field 'name 'optional-uir 'required)))
      (uir-node-kind 'pat-class 'uir-pat-class #f
        (list (uir-schema-field 'cls 'uir 'required) (uir-schema-field 'positional 'list-uir 'required) (uir-schema-field 'keyword 'list-pair-uir 'required)))
      (uir-node-kind 'pat-group 'uir-pat-group #f (list (uir-schema-field 'pattern 'uir 'required)))
      (uir-node-kind 'class 'uir-class #f
        (list (uir-schema-field 'name 'uir 'required) (uir-schema-field 'super 'list-uir 'required) (uir-schema-field 'fields 'list-uir 'required) (uir-schema-field 'methods 'list-uir 'required)))
      (uir-node-kind 'method 'uir-method #f
        (list (uir-schema-field 'name 'uir 'required) (uir-schema-field 'params 'list-uir 'required) (uir-schema-field 'body 'uir 'required) (uir-schema-field 'visibility 'symbol 'required)))
      (uir-node-kind 'field 'uir-field #f
        (list (uir-schema-field 'name 'uir 'required) (uir-schema-field 'type 'uir 'required) (uir-schema-field 'init 'uir 'required)))
      (uir-node-kind 'new 'uir-new #f
        (list (uir-schema-field 'class 'uir 'required) (uir-schema-field 'args 'list-uir 'required)))
      (uir-node-kind 'interface 'uir-interface #f
        (list (uir-schema-field 'name 'uir 'required) (uir-schema-field 'methods 'list-uir 'required)))
      (uir-node-kind 'module 'uir-module #f
        (list (uir-schema-field 'name 'uir 'required) (uir-schema-field 'imports 'list-uir 'required) (uir-schema-field 'exports 'list-uir 'required) (uir-schema-field 'body 'uir 'required)))
      (uir-node-kind 'import 'uir-import #f
        (list (uir-schema-field 'source 'uir 'required) (uir-schema-field 'names 'list-uir 'required)))
      (uir-node-kind 'export 'uir-export #f (list (uir-schema-field 'names 'list-uir 'required)))
      (uir-node-kind 'enum 'uir-enum #f
        (list (uir-schema-field 'name 'uir 'required) (uir-schema-field 'variants 'list-uir 'required)))
      (uir-node-kind 'enum-variant 'uir-enum-variant #f
        (list (uir-schema-field 'name 'uir 'required) (uir-schema-field 'fields 'list-uir 'required) (uir-schema-field 'discriminant 'optional-uir 'required)))
      (uir-node-kind 'component 'uir-component #f
        (list (uir-schema-field 'name 'uir 'required) (uir-schema-field 'props 'list-uir 'required) (uir-schema-field 'states 'list-uir 'required) (uir-schema-field 'effects 'list-uir 'required) (uir-schema-field 'template 'uir 'required)))
      (uir-node-kind 'element 'uir-element #f
        (list (uir-schema-field 'tag 'uir 'required) (uir-schema-field 'attrs 'list-uir 'required) (uir-schema-field 'children 'list-uir 'required) (uir-schema-field 'events 'list-uir 'required)))
      (uir-node-kind 'attribute 'uir-attribute #f
        (list (uir-schema-field 'name 'uir 'required) (uir-schema-field 'value 'uir 'required)))
      (uir-node-kind 'event 'uir-event #f
        (list (uir-schema-field 'name 'uir 'required) (uir-schema-field 'handler 'uir 'required)))
      (uir-node-kind 'slot 'uir-slot #f
        (list (uir-schema-field 'name 'uir 'required) (uir-schema-field 'fallback 'uir 'required)))
      (uir-node-kind 'text-node 'uir-text-node #f (list (uir-schema-field 'content 'text 'required)))
      (uir-node-kind 'style 'uir-style #f (list (uir-schema-field 'styles 'spliced-pair-uir 'required)))
      (uir-node-kind 'effect 'uir-effect #f
        (list (uir-schema-field 'deps 'list-uir 'required) (uir-schema-field 'body 'uir 'required)))
      (uir-node-kind 'state 'uir-state #f (list (uir-schema-field 'name 'uir 'required) (uir-schema-field 'init 'uir 'required)))
      (uir-node-kind 'jsx-expr 'uir-jsx-expr #f (list (uir-schema-field 'content 'raw 'required)))
      (uir-node-kind 'spread 'uir-spread #f (list (uir-schema-field 'expr 'uir 'required)))
      (uir-node-kind 'effect-entry 'uir-effect-entry #f
        (list (uir-schema-field 'name 'uir 'required) (uir-schema-field 'modality 'symbol 'required) (uir-schema-field 'ops 'list-uir 'required) (uir-schema-field 'budget 'optional-int 'required) (uir-schema-field 'scope 'optional-uir 'required)))
      (uir-node-kind 'effect-row 'uir-effect-row #f (list (uir-schema-field 'entries 'spliced-uir 'required)))
      (uir-node-kind 'subtype 'uir-subtype #f
        (list (uir-schema-field 'left 'uir 'required) (uir-schema-field 'right 'uir 'required)))
      (uir-node-kind 'effect-decl 'uir-effect-decl #f
        (list (uir-schema-field 'name 'uir 'required) (uir-schema-field 'ops 'list-uir 'required)))
      (uir-node-kind 'effect-op 'uir-effect-op #f
        (list (uir-schema-field 'name 'uir 'required) (uir-schema-field 'params 'list-uir 'required) (uir-schema-field 'result-type 'optional-uir 'required)))
      (uir-node-kind 'effect-handler 'uir-effect-handler #f
        (list (uir-schema-field 'effect 'optional-uir 'required) (uir-schema-field 'ops 'handler-op-list 'required) (uir-schema-field 'return-clause 'optional-uir 'required) (uir-schema-field 'body 'uir 'required)))
      (uir-node-kind 'eff-system 'uir-eff-system #f
        (list (uir-schema-field 'name 'uir 'required) (uir-schema-field 'decls 'list-uir 'required)))
      (uir-node-kind 'eff-capability 'uir-eff-capability #f
        (list (uir-schema-field 'name 'uir 'required) (uir-schema-field 'type-params 'list-uir 'required) (uir-schema-field 'ops 'list-uir 'required)))
      (uir-node-kind 'eff-capability-op 'uir-eff-capability-op #f
        (list (uir-schema-field 'name 'uir 'required) (uir-schema-field 'params 'list-uir 'required) (uir-schema-field 'result-type 'optional-uir 'required)))
      (uir-node-kind 'eff-ref 'uir-eff-ref #f
        (list (uir-schema-field 'name 'uir 'required) (uir-schema-field 'type-args 'list-uir 'required)))
      (uir-node-kind 'eff-given 'uir-eff-given #f (list (uir-schema-field 'row 'list-uir 'required)))
      (uir-node-kind 'eff-handler 'uir-eff-handler #f
        (list (uir-schema-field 'effect 'uir 'required) (uir-schema-field 'row 'list-uir 'required) (uir-schema-field 'ops 'eff-op-list 'required)))
      (uir-node-kind 'eff-component 'uir-eff-component #f
        (list (uir-schema-field 'name 'uir 'required) (uir-schema-field 'needs 'list-uir 'required) (uir-schema-field 'provides 'list-uir 'required) (uir-schema-field 'ops 'eff-op-list 'required)))
      (uir-node-kind 'eff-state 'uir-eff-state #f
        (list (uir-schema-field 'name 'uir 'required) (uir-schema-field 'type 'uir 'required) (uir-schema-field 'init 'uir 'required)))
      (uir-node-kind 'eff-action 'uir-eff-action #f
        (list (uir-schema-field 'name 'uir 'required) (uir-schema-field 'steps 'list-uir 'required)))
      (uir-node-kind 'eff-invariant 'uir-eff-invariant #f (list (uir-schema-field 'expr 'uir 'required)))
      (uir-node-kind 'eff-policy 'uir-eff-policy #f
        (list (uir-schema-field 'kind 'symbol 'required) (uir-schema-field 'predicate 'uir 'required)))
      ;; ── the bootstrap vocabulary the schema is written in ──
      (uir-node-kind 'schema 'uir-schema #t
        (list (uir-schema-field 'version 'int 'required) (uir-schema-field 'kernel-version 'int 'required) (uir-schema-field 'node-kinds 'list-uir 'required)))
      (uir-node-kind 'node-kind 'uir-node-kind #t
        (list (uir-schema-field 'kind 'symbol 'required) (uir-schema-field 'ctor 'symbol 'required) (uir-schema-field 'kernel? 'bool 'required) (uir-schema-field 'fields 'list-uir 'required)))
      (uir-node-kind 'schema-field 'uir-schema-field #t
        (list (uir-schema-field 'name 'symbol 'required) (uir-schema-field 'type 'symbol 'required) (uir-schema-field 'default 'symbol 'required)))
      )))

