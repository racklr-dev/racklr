#lang racket

;; racklr/effects/receiver-module.rkt
;;
;; The MODULE half of receiver tracking: a receiver value whose MODULE (not
;; class) is known — `pylibmc` / `pymemcache` / `memcache` / `redis` module
;; handles, and module-qualified class references read from them
;; (`self._lib.Redis`, `self._lib.DriverInfo`). Module receivers resolve their
;; method calls to the module's effect DOMAIN; module-qualified class reads
;; resolve to the module's class TYPE table.
;;
;; Both mappings are DATA in the injected lexicon
;; (library-tokens.rkt / library-stdlib-types.rkt); this module holds the
;; lookup, never the names, so a different host supplies its own tables.
;; Split out of receiver-type.rkt to keep both modules under the 400-line
;; obligation.

(require racklr/effects/library-tokens)

(provide module-receiver? module-receiver-root
         module-class-ref-type module-receiver-attr-type
         tracked-receiver-type?)

;; A receiver value is a MODULE (not a class) when its name names an external
;; module in the receiver-modules token table (pylibmc / pymemcache / memcache /
;; redis). Module receivers resolve their method calls to the module's effect
;; domain in classify-all.
(define (module-receiver? nm [tokens default-effect-tokens])
  (and nm (hash-has-key? (effect-tokens-receiver-modules tokens)
                         (symbol->string nm))))

;; A receiver TYPE name that is not a corpus class definition but still a
;; tracked receiver: a stdlib type with a method table (`open`, `Path`), an
;; external module (pylibmc), or a module-path marker (`django.db`).
(define (tracked-receiver-type? nm [tokens default-effect-tokens])
  (define s (and nm (symbol->string nm)))
  (and s (or (hash-has-key? (effect-tokens-stdlib-type-methods tokens) s)
             (hash-has-key? (effect-tokens-receiver-modules tokens) s)
             (regexp-match? #rx"[.]" s))
       nm))

;; Root module name of a tracked MODULE receiver, or #f. A module receiver is
;; either the module name itself (`redis`, `pylibmc`) or a module-qualified
;; marker whose root names a module (`pymemcache.HashClient`).
(define (module-receiver-root bm [tokens default-effect-tokens])
  (and bm
       (let* ([s (symbol->string bm)]
              [m (regexp-match #rx"^([^.]+)" s)]
              [root (and m (cadr m))])
         (and root
              (or (and (hash-has-key? (effect-tokens-receiver-modules tokens) s)
                       s)
                  (and (hash-has-key? (effect-tokens-receiver-modules tokens) root)
                       root))))))

;; Receiver TYPE of a module-qualified CLASS reference whose MODULE ROOT is a
;; tracked module receiver (`self._lib.DriverInfo` where `self._lib = redis`):
;; the receiver-modules table names the module, and the module-class-ref table
;; names the class (`redis` → DriverInfo → DriverInfo). Both tables are DATA
;; from the injected lexicon. Returns a class-name symbol or #f.
(define (module-class-ref-type bm attr [tokens default-effect-tokens])
  (define root (module-receiver-root bm tokens))
  (and root attr
       (let* ([tbl (hash-ref (effect-tokens-module-class-refs tokens) root #f)]
              [t (and tbl (hash-ref tbl (symbol->string attr) #f))])
         (and t (string->symbol t)))))

;; Class-name symbol of a module-qualified VALUE read (`self._lib.Redis`): the
;; module-class-ref table when it names the attribute, else the dotted module
;; marker (`pymemcache.HashClient`) whose module root still carries the effect
;; domain to classify-all. Returns a class-name symbol or #f.
(define (module-receiver-attr-type bm attr [tokens default-effect-tokens])
  (define root (module-receiver-root bm tokens))
  (and root attr
       (or (module-class-ref-type bm attr tokens)
           (string->symbol (format "~a.~a" bm attr)))))
