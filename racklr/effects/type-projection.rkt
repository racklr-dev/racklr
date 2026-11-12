#lang racket

;; racklr/effects/type-projection.rkt
;;
;; Call-result TYPE projection for type-preserving calls: a binary operator
;; whose result is the receiver's type (`Path(dirpath) / f` lowers to
;; `(call (symbol "/") (list (call Path ...) (str f)))`) and a value-returning
;; method (`mo_path = po_path.with_suffix(".mo")`). Without the projection the
;; result value has no receiver type, so a later `.stat()` stays `unknown`.
;;
;; The engine reads the (receiver-type, call) → result-type pairs from the
;; injected receiver-type projection table; the receiver-type / value-type
;; lookups stay in receiver-type.rkt and arrive as the small callbacks below,
;; so this module has no dependency cycle with the value→type projector.
;;
;; This is DATA-driven host neutrality: a host supplies its own table through
;; the lexicon instead of editing this module.

(require racklr/uir
         racklr/effects/shape
         racklr/effects/library-tokens
         racklr/effects/effect-roots
         racklr/effects/lexicon)

(provide operator-result-type method-result-type mapping-get-result-type
         literal-value-type)

;; Result type of `(receiver-type, call)` from the injected table, or #f.
(define (projection-result-type tokens rcls op)
  (and rcls op
       (let* ([table (hash-ref (effect-tokens-type-result-projections tokens)
                               (symbol->string (stdlib-class-symbol rcls tokens)) #f)]
              [t (and table (hash-ref table (symbol->string op) #f))])
         (and t (string->symbol t)))))

;; Result TYPE of a binary-operator call whose receiver type preserves itself
;; (`Path(dirpath) / f` → Path). `project` is the caller's value→type
;; projection for the receiver (the first argument).
(define (operator-result-type value project tokens)
  (and (uir-call? value)
       (uir-symbol? (uir-call-callee value))
       (pair? (uir-call-args value))
       (let* ([op (string->symbol (uir-symbol-name (uir-call-callee value)))]
              [rcls (project (first (uir-call-args value)))])
         (projection-result-type tokens rcls op))))

;; Result TYPE of a value-returning METHOD call whose receiver type is tracked
;; (`po_path.with_suffix(".mo")` → Path). `project-class` is the caller's
;; receiver-expression→class lookup, scoped to its current receiver env.
(define (method-result-type callee project-class tokens)
  (and (member-access-callee? callee)
       (let* ([base (member-access-base callee)]
              [op (effect-op-of-callee callee)]
              [bcls (and base (project-class base))])
         (projection-result-type tokens bcls op))))

;; Is `bcls` a mapping receiver (dict or a subclass): its stdlib method table
;; carries at least one of the host's mapping value-returning methods. Used to
;; project the value half of `mapping.get(k, default)` /
;; `mapping.setdefault(k, default)`.
(define (mapping-receiver-type? bcls tokens)
  (and bcls
       (let ([m (hash-ref (effect-tokens-stdlib-type-methods tokens)
                          (symbol->string (stdlib-class-symbol bcls tokens)) #f)])
         (and m (for/or ([mn (in-set (effect-tokens-mapping-value-methods tokens))])
                  (hash-ref m mn #f))))))

;; Result TYPE of a mapping value-returning method call
;; (`_caches.setdefault(name, OrderedDict())` → dict / the default's class).
;; The method is one of the host's mapping value-returning methods (DATA),
;; each of which takes an optional DEFAULT argument; the default argument's
;; projected class is a sound lower bound on the result (`setdefault` returns
;; the default only when the key is absent, `get` returns it whenever the key
;; is absent, `pop` returns it the same way). `project` is the caller's
;; value→type projection.
(define (mapping-get-result-type value project tokens)
  (and (uir-call? value)
       (member-access-callee? (uir-call-callee value))
       (let* ([callee (uir-call-callee value)]
              [base (member-access-base callee)]
              [op (member-access-field callee)]
              [bcls (and base (project base))]
              [args (uir-call-args value)])
         (and op
              (set-member? (effect-tokens-mapping-value-methods tokens)
                           (symbol->string op))
              (mapping-receiver-type? bcls tokens)
              (>= (length args) 2)
              (project (second args))))))

;; Receiver TYPE of a literal VALUE expression, from the host dialect's literal
;; receiver-type map: `xs = []; xs.append(v)` gets `list` from the dialect's
;; `list` entry, `d = {}; d.get(k)` gets `dict`, and a string literal gets the
;; host's string type. The host convention (which literal has which type) is
;; DATA; this only maps a UIR value to the dialect key.
(define (literal-value-type value tokens)
  (define kind (cond
                 [(uir-string? value) 'uir-string]
                 [(uir-fstring? value) 'uir-fstring]
                 [(uir-list? value) 'uir-list]
                 [(uir-record? value) 'uir-record]
                 [else #f]))
  (define t (and kind (tokens-literal-receiver-type tokens kind)))
  (and t (string->symbol t)))
