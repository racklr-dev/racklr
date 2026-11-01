#lang racket

;; racklr/effects/receiver-chain.rkt
;;
;; Class an arbitrary receiver expression resolves to. This is the walk that
;; lets a receiver chain cross object boundaries (`self.a.b.m(...)`), follow a
;; call result (`f(...).m()`), start from a fresh `new C(...)`, and read the
;; nullish guard both front ends emit for `x?.y` as its present value.
;;
;; Split out of receiver-resolve.rkt (which keeps the per-receiver effect
;; resolution) so both modules stay under the 400-line obligation.

(require racklr/uir
         racklr/effects/shape
         racklr/effects/class-resolve
         racklr/effects/receiver-type
         racklr/effects/return-type
         racklr/effects/library-tokens
         racklr/effects/lexicon)

(provide class-attr-lookup receiver-chain-class)

;; ── receiver-type chain ─────────────────────────────────────────────

;; Attribute `attr` of class `class-name`, resolved across defining components
;; and the superclass chain (most-derived first); #f when the attribute is not
;; tracked. The per-class attribute graph walk that lets a receiver chain cross
;; an object boundary (`self.a.b.m` → class of `a` → class of `a.b`).
(define (class-attr-lookup comp class-name attr class-defs class-attrs comp-supers)
  (define start (resolve-class-component class-name comp class-defs))
  (define seen (make-hash))
  (let loop ([c start] [cls class-name])
    (cond
      [(not (and c cls)) #f]
      [(hash-has-key? seen (cons c cls)) #f]
      [else
       (hash-set! seen (cons c cls) #t)
       (let ([val (hash-ref (hash-ref class-attrs (cons c cls) (hash)) attr #f)])
         (or val
             (for/or ([sr (in-list (hash-ref comp-supers (cons c cls) '()))])
               (let* ([c2 (car sr)] [s (cdr sr)])
                 (and c2 s (loop c2 s))))))])))

;; Class an arbitrary receiver expression resolves to, or #f when the dataflow
;; cannot ground it. `base` is any receiver expression:
;;
;;   `x`            — a tracked local (constructor / call-result / `new`)
;;   implicit recv. — the enclosing class (`self`/`cls`/`this`, names from the
;;                    injected host dialect)
;;   `a.b`          — an attribute of a resolved class, in either the Python
;;                    uir-get or the JS/TS dot-call encoding
;;   `x?.y`         — the present value of a nullish guard (lowered `?.`)
;;   `f(...)`       — the projected result class of a call
;;   `new C(...)`   — the constructed class C
(define (receiver-chain-class rt base comp class-defs class-attrs comp-supers
                              [owner #f] [return-types #f]
                              [methods #f] [fn-defs #f] [known #f]
                              [import-env #f] [tokens default-effect-tokens])
  (cond
    [(not base) #f]
    ;; Optional chain (`x?.y`, `x?.y.m`, `a?.b?.m`): follow the present branch.
    ;; The absent branch is `void 0`, which carries no receiver class, so it
    ;; must not stop the walk.
    [(and (uir-if? base) (optional-chain-present base))
     => (λ (present)
          (receiver-chain-class rt present comp class-defs class-attrs
                                comp-supers owner return-types methods fn-defs known
                                import-env tokens))]
    [(or (uir-var? base) (uir-symbol? base))
     (define nm (bare-name-symbol base))
     (cond
       [(and owner (tokens-self-name? tokens nm)) owner]
       [else (hash-ref (receiver-types-locals rt) nm #f)])]
    ;; A member access — Python `a.b` (uir-get) or JS/TS `a.b` (dot call):
    ;; both walk to the base's class and then through that class's attributes.
    [(member-access-callee? base)
     (define attr (member-access-field base))
     (define inner (member-access-base base))
     (cond
       [(not attr) #f]
       [(tokens-self-name? tokens (bare-name-symbol inner))
        ;; An assigned implicit-receiver attribute wins; when untracked, fall
        ;; back to a METHOD of the enclosing class whose return type was
        ;; projected (`@cached_property _cache` returning `self._class(...)`),
        ;; so `self._cache.set(...)` resolves to the cached property's result
        ;; class instead of `unknown`. The same fast path serves Python `self`
        ;; (a uir-var) and JS `this` (a uir-symbol).
        (or (hash-ref (receiver-types-self-attrs rt) attr #f)
            (and owner return-types attr
                 (mro-property-return-type comp owner attr return-types
                                           class-defs comp-supers)))]
       [else
        (define c (receiver-chain-class rt inner comp class-defs class-attrs comp-supers
                                        owner return-types methods fn-defs known import-env
                                        tokens))
        (and c
             (or (class-attr-lookup comp c attr class-defs class-attrs comp-supers)
                 ;; Bottom out at a module marker (`django.db` from
                 ;; `connection = connections[database]`, or `pylibmc.Client`):
                 ;; it has no corpus attribute graph to walk, but the marker
                 ;; still carries its module's domain to the caller. A class
                 ;; symbol never contains a dot, so the dotted test disambiguates.
                 (and (string-contains? (symbol->string c) ".") c)))])]
    ;; `new C(...)` constructs an instance of C: a fresh JS/TS object is a
    ;; receiver of class C when C is a tracked class (corpus, imported, or a
    ;; pure stdlib value type).
    [(uir-new? base)
     (bare-class-value (uir-new-class base) class-defs tokens import-env comp)]
    [(uir-call? base)
     ;; A `.attr` chain rooted at a call result (`Path(...).is_absolute`,
     ;; `md5(...).hexdigest`, `open(...).close`): the result class is the one
     ;; `call-result-return-class` projects — a corpus function/method call's
     ;; projected return, a method call through a receiver the relations typed
     ;; (`x.key_for(...)`), or the constructor/stdlib/imported result type.
     (call-result-return-class (uir-call-callee base) comp class-defs owner
                               rt methods fn-defs known comp-supers return-types
                               tokens import-env)]
    [else #f]))
