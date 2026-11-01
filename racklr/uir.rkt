#lang racket

(require "uir/types.rkt")
(require racklr/tree)
(require "uir/serialize.rkt")
(require "uir/version.rkt")

(provide
 (all-from-out "uir/types.rkt")
 ;; Predicate
 uir? uir-tag
 ;; Serialization
 (all-from-out "uir/serialize.rkt")
 ;; Schema versioning and migration
 (all-from-out "uir/version.rkt"))

;; ── Predicate ──────────────────────────────────────────────────────

(define (uir? v)
  (or (uir-null? v)
      (uir-bool? v)
      (uir-number? v)
      (uir-string? v)
      (uir-fstring? v)
      (uir-list? v)
      (uir-record? v)
      (uir-symbol? v)
      (uir-fn? v)
      (uir-typed-param? v)
      (uir-call? v)
      (uir-let? v)
      (uir-var? v)
      (uir-set!? v)
      (uir-ann-set!? v)
      (uir-walrus? v)
      (uir-if? v)
      (uir-block? v)
      (uir-return? v)
      (uir-for-each? v)
      (uir-while? v)
      (uir-try? v)
      (uir-with? v)
      (uir-await? v)
      (uir-yield? v)
      (uir-decorated? v)
      (uir-get? v)
      (uir-paren? v)
      (uir-type-alias? v)
      (uir-class? v)
      (uir-method? v)
      (uir-field? v)
      (uir-new? v)
      (uir-interface? v)
      (uir-module? v)
      (uir-import? v)
      (uir-export? v)
      (uir-enum? v)
      (uir-enum-variant? v)
      (uir-component? v)
      (uir-element? v)
      (uir-attribute? v)
      (uir-event? v)
      (uir-slot? v)
      (uir-text-node? v)
      (uir-style? v)
      (uir-effect? v)
      (uir-state? v)
      (uir-jsx-expr? v)
      (uir-spread? v)
      (uir-match? v)
      (uir-case? v)
      (uir-pat-literal? v)
      (uir-pat-capture? v)
      (uir-pat-wildcard? v)
      (uir-pat-value? v)
      (uir-pat-or? v)
      (uir-pat-as? v)
      (uir-pat-sequence? v)
      (uir-pat-star? v)
      (uir-pat-mapping? v)
      (uir-pat-double-star? v)
      (uir-pat-class? v)
      (uir-pat-group? v)
      (uir-effect-row? v)
      (uir-effect-entry? v)
      (uir-subtype? v)
      (uir-effect-decl? v)
      (uir-effect-op? v)
      (uir-effect-handler? v)
      (uir-eff-system? v)
      (uir-eff-capability? v)
      (uir-eff-capability-op? v)
      (uir-eff-ref? v)
      (uir-eff-given? v)
      (uir-eff-handler? v)
      (uir-eff-component? v)
      (uir-eff-state? v)
      (uir-eff-action? v)
      (uir-eff-invariant? v)
      (uir-eff-policy? v)))

(define (uir-tag v)
  (cond [(uir-null? v)   'null]
        [(uir-bool? v)   'bool]
        [(uir-number? v) 'number]
        [(uir-string? v) 'string]
        [(uir-fstring? v) 'fstring]
        [(uir-list? v)   'list]
        [(uir-record? v) 'record]
        [(uir-symbol? v) 'symbol]
        [(uir-fn? v)     'fn]
        [(uir-typed-param? v) 'typed-param]
        [(uir-call? v)   'call]
        [(uir-let? v)    'let]
        [(uir-var? v)    'var]
        [(uir-set!? v)  'set!]
        [(uir-ann-set!? v) 'ann-set!]
        [(uir-walrus? v) 'walrus]
        [(uir-if? v)     'if]
        [(uir-block? v)  'block]
        [(uir-return? v) 'return]
        [(uir-for-each? v) 'for-each]
        [(uir-while? v) 'while]
        [(uir-try? v)    'try]
        [(uir-with? v)   'with]
        [(uir-await? v)  'await]
        [(uir-yield? v)  'yield]
        [(uir-decorated? v) 'decorated]
        [(uir-get? v) 'get]
        [(uir-paren? v) 'paren]
        [(uir-type-alias? v) 'type-alias]
        [(uir-class? v)     'class]
        [(uir-method? v)    'method]
        [(uir-field? v)     'field]
        [(uir-new? v)       'new]
        [(uir-interface? v) 'interface]
        [(uir-module? v)    'module]
        [(uir-import? v)    'import]
        [(uir-export? v)    'export]
        [(uir-enum? v)         'enum]
        [(uir-enum-variant? v) 'enum-variant]
        [(uir-component? v)  'component]
        [(uir-element? v)    'element]
        [(uir-attribute? v)  'attribute]
        [(uir-event? v)      'event]
        [(uir-slot? v)       'slot]
        [(uir-text-node? v)  'text-node]
        [(uir-style? v)      'style]
        [(uir-effect? v)     'effect]
        [(uir-state? v)      'state]
        [(uir-jsx-expr? v)  'jsx-expr]
        [(uir-spread? v)    'spread]
        [(uir-match? v)          'match]
        [(uir-case? v)           'case]
        [(uir-pat-literal? v)    'pat-literal]
        [(uir-pat-capture? v)    'pat-capture]
        [(uir-pat-wildcard? v)   'pat-wildcard]
        [(uir-pat-value? v)      'pat-value]
        [(uir-pat-or? v)         'pat-or]
        [(uir-pat-as? v)         'pat-as]
        [(uir-pat-sequence? v)   'pat-sequence]
        [(uir-pat-star? v)       'pat-star]
        [(uir-pat-mapping? v)    'pat-mapping]
        [(uir-pat-double-star? v) 'pat-double-star]
        [(uir-pat-class? v)      'pat-class]
        [(uir-pat-group? v)      'pat-group]
        [(uir-effect-row? v)     'effect-row]
        [(uir-effect-entry? v)   'effect-entry]
        [(uir-subtype? v)        'subtype]
        [(uir-effect-decl? v)    'effect-decl]
        [(uir-effect-op? v)      'effect-op]
        [(uir-effect-handler? v) 'effect-handler]
        [(uir-eff-system? v)      'eff-system]
        [(uir-eff-capability? v)  'eff-capability]
        [(uir-eff-capability-op? v) 'eff-capability-op]
        [(uir-eff-ref? v)        'eff-ref]
        [(uir-eff-given? v)      'eff-given]
        [(uir-eff-handler? v)    'eff-handler]
        [(uir-eff-component? v)  'eff-component]
        [(uir-eff-state? v)      'eff-state]
        [(uir-eff-action? v)     'eff-action]
        [(uir-eff-invariant? v)  'eff-invariant]
        [(uir-eff-policy? v)     'eff-policy]
        [else (error 'uir-tag "not a UIR node: ~e" v)]))

(module+ main
  (displayln "racklr/uir — Universal Intermediate Representation loaded."))
