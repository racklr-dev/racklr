#lang racket

;; racklr/effects/domain-lattice.rkt
;;
;; The effect-domain vocabulary and its subtyping order, as pure DATA.
;; This is the single source of truth for which effect NAMES exist and which
;; are finer than which: an umbrella category (io, network, db, time) is a
;; supertype of its leaves (fs/stdio, http/socket/websocket, sql/nosql/vector,
;; clock/sleep/timer), so a row carrying `http` satisfies a contract needing
;; `network`, while a contract needing `http` is precise.
;;
;; `pure`, `dynamic`, `unknown` and `callback` are NOT lattice members: pure is
;; the empty set, dynamic/unknown are tracked deferrals, callback is a type
;; variable. Only concrete effect domains live here.
;;
;; The order is a forest (every leaf has at most one immediate parent, the
;; umbrellas are roots), so it is reflexive, transitive and antisymmetric by
;; construction; the property test pins that. Keeping it DATA means widening
;; the vocabulary is one edit here plus token-table entries, never an engine
;; rewrite.

(require racket/string)

(provide
  effect-domain?
  effect-domains
  effect-domain-parent
  effect-name-subtype?     ; a <= b : a is b or a finer leaf under b
  effect-name-ancestors    ; name -> (listof umbrella) nearest-first
  effect-name-subtypes     ; name -> all domains <= name (name + its leaves)
  domain-capability-interpretation  ; capability-name -> covered host names
  effect-name-lub)         ; least upper bound, or #f when incomparable

;; leaf -> immediate umbrella (the one-level subtyping edge).
(define effect-domain-parents
  (hash
   'stdio 'io
   'fs 'io
   'http 'network
   'socket 'network
   'websocket 'network
   'sql 'db
   'nosql 'db
   'vector 'db
   'clock 'time
   'sleep 'time
   'timer 'time))

;; Every concrete domain name, leaves first then roots (deterministic order).
(define effect-domains
  '(fs stdio
    http socket websocket
    sql nosql vector
    clock sleep timer
    io network db time
    process cache llm email env rng crypto queue gpu))

(define (effect-domain? name)
  (and (memq name effect-domains) #t))

(define (effect-domain-parent name)
  (hash-ref effect-domain-parents name #f))

;; a <= b : a equals b, or b is an umbrella of a (reflexive-transitive).
(define (effect-name-subtype? a b)
  (or (eq? a b)
      (let loop ([cur a])
        (define p (effect-domain-parent cur))
        (and p (or (eq? p b) (loop p))))))

;; Ancestors of `name`, nearest umbrella first (excludes the name itself).
(define (effect-name-ancestors name)
  (let loop ([cur name] [acc '()])
    (define p (effect-domain-parent cur))
    (if p (loop p (cons p acc)) (reverse acc))))

;; The host effect names a capability covers: the name itself plus every leaf
;; beneath it, in the deterministic `effect-domains` order. This is the DATA
;; the bridge's capability interpretation consumes (a `Network` capability
;; interprets http/socket/websocket/network), so a fine host effect satisfies a
;; coarse declared `needs` without touching the kernel's name-equality subtyping.
(define (effect-name-subtypes name)
  (filter (λ (d) (effect-name-subtype? d name)) effect-domains))

;; The capability interpretation the bridge consumes: capability name
;; (titlecased domain, matching emit-fizz's `domain-cap-name`) -> the host
;; effect names that capability covers (itself plus every leaf beneath it, as
;; strings). A hand-written `component needs Network` therefore accepts a host
;; `http`/`socket`/`websocket` effect without the kernel subtyping changing.
(define (domain-capability-interpretation)
  (for/hash ([d (in-list effect-domains)])
    (values (string-titlecase (symbol->string d))
            (effect-name-subtypes d))))

;; Least upper bound in the forest, or #f when the two names are incomparable.
(define (effect-name-lub a b)
  (cond [(eq? a b) a]
        [(effect-name-subtype? a b) b]  ; b is a or an ancestor of a
        [(effect-name-subtype? b a) a]  ; a is a or an ancestor of b
        [else #f]))

(module+ main
  (displayln "racklr/effects/domain-lattice — effect-domain lattice loaded."))
