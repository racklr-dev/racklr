#lang racket

;; racklr/effects/type-relations-widen.rkt
;;
;; Widening bookkeeping for the deterministic join in type-relations.rkt.
;;
;; `close-type-relations!` recomputes four relations from each other:
;; self-attr from (param, return), assign from self-attr, param and return from
;; (assign, self-attr). Every fold projects a value to a single class or to
;; "unresolved" (#f / absent), and that projection is NOT monotone: a value can
;; move single -> unresolved when a second candidate class appears, then back
;; when that candidate's own projection collapses. A feedback loop through the
;; four relations can therefore flip values forever, and the fixed iteration
;; cap that used to bound the loop silently returned a truncated state.
;;
;; This module turns the join into a terminating computation that leaves every
;; already-converging input bit-for-bit unchanged:
;;
;;   * the driver fingerprints the complete relation state every round. The
;;     round map is deterministic (sorted key iteration, single-class folds,
;;     no hash-order dependence), so a repeated state proves the iteration is
;;     periodic and can never reach a fixed point.
;;   * on a repeat, every flat relation entry whose value moved inside the
;;     detected cycle is WIDENED to unresolvable and FROZEN; later
;;     recomputations strip frozen entries, so they can never move again.
;;   * a converging input never repeats a state, so it never widens and its
;;     relation values are exactly the old ones. Each widening freezes at least
;;     one entry, so the loop terminates on any input. A widening prints one
;;     explicit diagnostic line; there is no silent truncation.
;;
;; A flat relation entry is a four-or-five element list; the last element is the
;; value and the rest is the entry key:
;;
;;   (assign    (comp . fname) local)  -> class
;;   (self-attr (comp . class) attr)   -> class
;;   (param     ((comp . fname) . idx)) -> class
;;   (return    (comp . fname))        -> class | #f
;;
;; Missing entries are unresolvable (#f/absent), which is exactly the widened
;; value: freezing an entry drops it from the relation.

(require file/sha1
         racket/list)

(provide (struct-out widening)
         make-widening
         widening-fingerprint
         widening-note-diff!
         widening-register!
         widening-cycle-start
         widening-widen!
         widening-strip-all
         state->flat
         run-join!)

(struct widening (frozen seen histories) #:transparent #:mutable)

;; Frozen entry keys so far: hash flat-key -> #t (only the key, never the value).
(define (make-widening)
  (widening (make-hash)   ;; frozen: flat entry key -> #t
            (make-hash)   ;; seen: state fingerprint -> round first seen
            (make-hash))) ;; histories: flat entry key -> (listof round changed), newest first

;; ── canonical flat state ────────────────────────────────────────────

;; The four relations reduced to one canonical, order-independent list of flat
;; entries. Every consumer (fingerprint, diff, widening) reads this shape, so
;; "same state" means "same flat list" and nothing about hash iteration order
;; leaks into cycle detection.
(define (state->flat assign self-attr param return)
  (define raw '())
  (for ([(k locals) (in-hash assign)])
    (for ([(local cls) (in-hash locals)])
      (set! raw (cons (list 'assign k local cls) raw))))
  (for ([(k attrs) (in-hash self-attr)])
    (for ([(attr cls) (in-hash attrs)])
      (set! raw (cons (list 'self-attr k attr cls) raw))))
  (for ([(kk cls) (in-hash param)])
    (set! raw (cons (list 'param kk cls) raw)))
  (for ([(k cls) (in-hash return)])
    (set! raw (cons (list 'return k cls) raw)))
  ;; Canonical order: sort on the entry key (the list minus its value), formatted
  ;; once per entry. `~s` of a string/symbol/pair is stable across rounds.
  (define keyed
    (for/list ([e (in-list raw)])
      (cons (format "~s" (drop-right e 1)) e)))
  (map cdr (sort keyed string<? #:key car)))

;; One flat list to an equal?-comparable table keyed by entry key.
(define (flat->table flat)
  (for/hash ([e (in-list flat)])
    (values (drop-right e 1) (last e))))

;; A full-state fingerprint: SHA-1 of the canonical flat state. The state is a
;; value (not a pointer), so hashing it is the identity test the fixed-point
;; loop needs; SHA-1 keeps the per-round memory constant (20 bytes) instead of
;; storing the whole state for every round since the last widening.
(define (widening-fingerprint flat)
  (sha1 (string->bytes/utf-8 (format "~s" flat))))

;; ── change tracking ─────────────────────────────────────────────────

;; Record the entry keys whose value moved between two consecutive states, and
;; the round the move happened at. A widening then freezes exactly the entries
;; that moved inside the detected cycle, not every entry that ever moved.
(define (widening-note-diff! w prev cur round)
  (define prev-table (flat->table prev))
  (define cur-table (flat->table cur))
  (define histories (widening-histories w))
  (define (note! kk)
    (hash-set! histories kk (cons round (hash-ref histories kk '()))))
  (for ([(kk v) (in-hash cur-table)])
    (unless (equal? (hash-ref prev-table kk absent-value) v)
      (note! kk)))
  (for ([(kk _) (in-hash prev-table)])
    (unless (hash-has-key? cur-table kk)
      (note! kk)))
  (void))

;; A sentinel distinct from every class symbol / #f value an entry can carry.
(define absent-value (string->symbol "\u0000absent"))

(define (widening-register! w fingerprint round)
  (hash-set! (widening-seen w) fingerprint round))

(define (widening-cycle-start w fingerprint)
  (hash-ref (widening-seen w) fingerprint #f))

;; ── widening ────────────────────────────────────────────────────────

;; Freeze every flat entry that moved after `cycle-start` (the round whose state
;; repeated at `round`) and print the one-line diagnostic. Returns the frozen
;; count and the per-relation breakdown. The seen/history tables are cleared:
;; the state after stripping is a new trajectory, so stale repeat records would
;; only cause spurious widening later.
(define (widening-widen! w cur-flat cycle-start round)
  (define histories (widening-histories w))
  (define moving
    (for/list ([(kk rounds) (in-hash histories)]
               #:when (for/or ([r (in-list rounds)]) (> r cycle-start)))
      kk))
  (when (null? moving)
    ;; Mathematically unreachable: a state can only repeat after some entry
    ;; moved, and every move is recorded until the next widening. Fail loudly
    ;; rather than loop forever if a future change breaks that invariant.
    (error 'close-type-relations!
           "cycle detected at round ~a but no moving entry was tracked"
           round))
  (for ([kk (in-list moving)])
    (hash-set! (widening-frozen w) kk #t))
  (hash-clear! (widening-seen w))
  (hash-clear! histories)
  (define by-rel
    (for/fold ([h (hash 'assign 0 'self-attr 0 'param 0 'return 0)])
              ([kk (in-list moving)])
      (hash-set h (first kk) (add1 (hash-ref h (first kk) 0)))))
  (eprintf
   (string-append
    "type-relations: widening ~a entry/entries to unresolved at cycle round"
    " ~a..~a (assign ~a, self-attr ~a, param ~a, return ~a)\n")
   (length moving) cycle-start round
   (hash-ref by-rel 'assign) (hash-ref by-rel 'self-attr)
   (hash-ref by-rel 'param) (hash-ref by-rel 'return))
  (values (length moving) by-rel))

;; ── applying the widened state ──────────────────────────────────────

;; Per-relation key sets of frozen entries, built once per round.
(define (frozen-sets w)
  (define out (make-hash))
  (for ([(kk _) (in-hash (widening-frozen w))])
    (hash-update! out (first kk) (λ (l) (cons kk l)) '()))
  out)

(define (frozen-keys sets rel) (hash-ref sets rel '()))

;; Strip frozen entries from all four freshly recomputed relations. A frozen
;; entry is unresolvable, so it is dropped: the local/attribute is removed, the
;; param key is absent, the return stays #f. Downstream folds then see the
;; unresolvable value and cannot resurrect it.
(define (widening-strip-all w assign self-attr param return)
  (define sets (frozen-sets w))
  (define a-keys (frozen-keys sets 'assign))
  (define sa-keys (frozen-keys sets 'self-attr))
  (define p-keys (frozen-keys sets 'param))
  (define r-keys (frozen-keys sets 'return))

  ;; The join's rebuild steps mutate `assign` and `return` in place, so every
  ;; stripped relation is a fresh MUTABLE hash (a `for/hash` result would be
  ;; immutable and break the next round's `hash-set!`).
  ;; assign: hash (comp . fname) -> { local -> class }
  (define assign*
    (if (null? a-keys)
        assign
        (let ([drop (make-hash)] [out (make-hash)])
          (for ([kk (in-list a-keys)])
            (hash-update! drop (second kk) (λ (s) (set-add s (third kk)))
                          (set)))
          (for ([(k locals) (in-hash assign)])
            (define d (hash-ref drop k #f))
            (hash-set! out k
                       (if d
                           (let ([inner (make-hash)])
                             (for ([(l c) (in-hash locals)]
                                   #:unless (set-member? d l))
                               (hash-set! inner l c))
                             inner)
                           locals)))
          out)))
  ;; self-attr: hash (comp . class) -> { attr -> class }
  (define self-attr*
    (if (null? sa-keys)
        self-attr
        (let ([drop (make-hash)] [out (make-hash)])
          (for ([kk (in-list sa-keys)])
            (hash-update! drop (second kk) (λ (s) (set-add s (third kk)))
                          (set)))
          (for ([(k attrs) (in-hash self-attr)])
            (define d (hash-ref drop k #f))
            (hash-set! out k
                       (if d
                           (let ([inner (make-hash)])
                             (for ([(attr c) (in-hash attrs)]
                                   #:unless (set-member? d attr))
                               (hash-set! inner attr c))
                             inner)
                           attrs)))
          out)))
  ;; param: hash ((comp . fname) . idx) -> class; a frozen key is absent.
  (define param*
    (if (null? p-keys)
        param
        (let ([drop (for/set ([kk (in-list p-keys)]) (second kk))]
              [out (make-hash)])
          (for ([(kk cls) (in-hash param)] #:unless (set-member? drop kk))
            (hash-set! out kk cls))
          out)))
  ;; return: hash (comp . fname) -> class | #f; a frozen key is #f.
  (define return*
    (if (null? r-keys)
        return
        (let ([drop (for/set ([kk (in-list r-keys)]) (second kk))]
              [out (make-hash)])
          (for ([(k cls) (in-hash return)])
            (hash-set! out k (if (set-member? drop k) #f cls)))
          out)))

  (values assign* self-attr* param* return*))

;; ── join driver ─────────────────────────────────────────────────────

;; Drive one join to a fixed point. `step!` returns the four relations for one
;; round (it closes over the caller's rebuild functions and the previous round's
;; values). The driver:
;;   * strips widened entries from every round before it is observed;
;;   * stops at the first round whose flat state equals the previous round's;
;;   * fingerprints every state and, on a repeat (a periodic orbit), widens the
;;     entries that moved inside the cycle, then continues;
;;   * returns the final four relations.
;; `#:max-rounds` is a diagnostic budget only: with `#:widen? #t` (the default)
;; termination is structural, because each widening removes at least one entry
;; from the live state and the remaining iteration is finite.
(define (run-join! step!
                   #:max-rounds [max-rounds #f]
                   #:widen? [widen? #t]
                   #:trace? [trace? #f]
                   #:widening-report [widening-report #f])
  (define w (make-widening))
  (let loop ([round 0] [prev-flat #f])
    (define t0 (current-inexact-milliseconds))
    (define-values (assign self-attr param return) (step!))
    (define-values (assign* self-attr* param* return*)
      (widening-strip-all w assign self-attr param return))
    (define flat (state->flat assign* self-attr* param* return*))
    (when trace?
      (eprintf "type-relations trace round=~a t=~ams entries=~a frozen=~a~n"
               round
               (inexact->exact (round (- (current-inexact-milliseconds) t0)))
               (length flat)
               (hash-count (widening-frozen w))))
    (define done?
      (and prev-flat (equal? prev-flat flat)))
    (cond
      [done? (values assign* self-attr* param* return*)]
      [(and max-rounds (>= round max-rounds))
       (eprintf (string-append
                 "type-relations: round budget ~a exhausted before convergence"
                 " (widening is not making progress)\n")
                round)
       (values assign* self-attr* param* return*)]
      [else
       (when prev-flat (widening-note-diff! w prev-flat flat round))
       (define fingerprint (widening-fingerprint flat))
       (define repeat (widening-cycle-start w fingerprint))
       (cond
         [(and widen? repeat)
          (define-values (count by-rel) (widening-widen! w flat repeat round))
          (when widening-report (set-box! widening-report (list count by-rel)))
          (loop (add1 round) flat)]
         [else
          (widening-register! w fingerprint round)
          (loop (add1 round) flat)])])))
