#lang racket

;; racklr/effects/labels.rkt
;;
;; Confidence and coverage labels for effect edges, and the weakest-link
;; combination a transitive effect profile uses.
;;
;; The analog is Rainbow's treatment of results: every result carries a maturity
;; level set by its WEAKEST link, and every theorem exposes a transitive axiom
;; profile. racklr's effect edges get two labels instead:
;;
;;   confidence — HOW the effect edge was established
;;     inferred  : a classifying call site / call-graph edge inference observed
;;                 (a ground recorded by the inference pass)
;;     declared  : a capability asserted by the DATA config at a method
;;                 definition (effects/imports/library-config.rkt)
;;     unknown   : the tracked elimination obligation (classification kind
;;                 'unresolved)
;;
;;   coverage — HOW FAR the claim generalizes
;;     universal     : for-all. Structural evidence that does not depend on a
;;                     runtime value: a call-graph edge, a literal/library call
;;                     site, a declared capability, self/MRO dispatch, or an
;;                     imported handle. Rainbow's "for all inputs".
;;     census-param  : worked instance. The classification needed parameter
;;                     dataflow (the unknown census's `param` receiver shape):
;;                     the claim holds for the actual call-site arguments, not
;;                     for every possible one.
;;     local-literal : worked instance. The classification needed a tracked
;;                     local / self-attribute / call-result value from the body
;;                     (the census's local/literal binding kinds).
;;
;; Confidence proves two things: the edge has a witnessed ground at all, and the
;; elimination obligation is explicit (`unknown` is a state, never a default).
;; Coverage keeps an inference from overclaiming: a conclusion drawn from one
;; worked instance is not the same artifact as a for-all structural rule.
;;
;; Both attributes are totally ordered weakest -> strongest and combined with the
;; WEAKEST link (`confidence-weakest` / `coverage-weakest`), mirroring Rainbow's
;; "the number means the weakest link" rule. A profile that reaches one unknown
;; or one worked-instance-only edge cannot claim a stronger label as a whole.

(require racklr/uir
         racklr/effects/shape)

(provide
 ;; vocabularies, ordered weakest -> strongest
 confidence-order coverage-order
 confidence? coverage?
 confidence<=? coverage<=?
 confidence-weakest coverage-weakest
 coverage-universal? coverage-worked-instance?
 ;; classification kind -> confidence
 confidence-of-kind
 ;; callee evidence -> coverage
 ground-coverage unresolved-coverage
 parameter-rooted?)

;; ── vocabularies ────────────────────────────────────────────────────

;; Weakest -> strongest. The indices are the evidence order: a smaller index is
;; at most as strong, so the weakest link is the smallest index.
(define confidence-order '(unknown declared inferred))
(define coverage-order '(local-literal census-param universal))

(define (confidence? v) (and (memq v confidence-order) #t))
(define (coverage? v) (and (memq v coverage-order) #t))

(define (rank-in order v)
  (for/first ([x (in-list order)] [i (in-naturals)] #:when (eq? x v)) i))

;; Is `sub` at most as strong as `super` in the evidence order?
(define (confidence<=? sub super)
  (<= (rank-in confidence-order sub) (rank-in confidence-order super)))

(define (coverage<=? sub super)
  (<= (rank-in coverage-order sub) (rank-in coverage-order super)))

;; The weakest link of zero or more labels. The empty fold is the strongest
;; label (there is no weak evidence); callers only combine non-empty sets.
(define (confidence-weakest . cs)
  (cond
    [(null? cs) 'inferred]
    [else
     (foldl (λ (a b) (if (confidence<=? a b) a b)) (first cs) (rest cs))]))

(define (coverage-weakest . cs)
  (cond
    [(null? cs) 'universal]
    [else
     (foldl (λ (a b) (if (coverage<=? a b) a b)) (first cs) (rest cs))]))

(define (coverage-universal? c) (eq? c 'universal))
(define (coverage-worked-instance? c)
  (or (eq? c 'census-param) (eq? c 'local-literal)))

;; The confidence a provenance ground's classification kind maps to. The
;; inference side names the unresolved obligation 'unresolved; the label side
;; names the resulting confidence 'unknown.
(define (confidence-of-kind kind)
  (case kind
    [(inferred) 'inferred]
    [(declared) 'declared]
    [(unresolved) 'unknown]
    [else (error 'confidence-of-kind "unknown classification kind: ~s" kind)]))

;; ── coverage of a callee ground ─────────────────────────────────────

;; The root name of a (possibly dotted) callee or receiver expression.
(define (expression-root-name e)
  (define root (dotted-root e))
  (and root (symbol? root) root))

;; Does the callee's receiver (or a bare callee) root in a parameter of the
;; enclosing function? `self`/`cls` are receivers, not parameters, even when the
;; front end keeps them in the parameter list.
(define (parameter-rooted? callee [param-names #f])
  (define root (expression-root-name callee))
  (and root param-names
       (not (memq root '(self cls)))
       (and (memq root param-names) #t)))

;; Coverage of a ground the classifier established from a call site. `evidence`
;; is what the classifier needed (classify-call-all's side channel): 'dataflow
;; means the receiver class came from receiver-type dataflow, 'structural means
;; a DATA rule, import handle, class attribute, or MRO rule classified it.
;;
;;   * dataflow through a parameter                                  -> census-param
;;   * dataflow through a local/attribute/chain/call-result value    -> local-literal
;;   * structural evidence (no worked-instance value was needed)     -> universal
;;
;; Per-file inference (infer-effects) classifies calls without a whole-program
;; receiver environment, so its grounds are structural and stay universal; the
;; whole-program fixpoint (infer-effects-all) is what distinguishes the
;; worked-instance cases.
(define (ground-coverage callee [evidence 'structural] [param-names #f])
  (if (eq? evidence 'dataflow)
      (if (parameter-rooted? callee param-names) 'census-param 'local-literal)
      'universal))

;; Coverage of an UNRESOLVED ground (the elimination obligation): the site's
;; evidence is a call expression, never a structural rule. A parameter-rooted
;; site is one parameter dataflow would resolve (census `param`); every other
;; site is a tracked local/literal call. The label is the elimination
;; obligation's evidence kind, kept as data instead of a boolean flag.
(define (unresolved-coverage callee [param-names #f])
  (if (parameter-rooted? callee param-names) 'census-param 'local-literal))
