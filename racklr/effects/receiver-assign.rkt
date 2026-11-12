#lang racket

;; racklr/effects/receiver-assign.rkt
;;
;; Assignment-target typing for the receiver-type environment. An assignment
;; target may be a bare name (`x = make()`) or a member access on the host
;; implicit receiver (`self.attr = ...` in Python, `this.attr = ...` in JS/TS).
;; Two front-end encodings carry assignments: Python lowers them to `uir-set!`;
;; JS/TS lower them to the `=` construct call with the target and value as its
;; two arguments. Both are read here, so the two hosts feed one env rule.
;;
;; The module is host-neutral: the caller supplies the value→class projection,
;; the implicit-receiver name predicate (from the injected lexicon), and the
;; env setters. Split out of receiver-type.rkt to keep both under the 400-line
;; obligation.

(require racklr/uir
         racklr/effects/shape
         racklr/effects/walk)

(provide apply-assignments!)

;; Name symbol bound by an assignment target, or #f for a compound target (a
;; member access is handled by the caller). The Python lowerer emits a uir-var;
;; the JS/TS lowerer emits a uir-symbol.
(define (target-name-symbol lhs)
  (cond
    [(uir-var? lhs) (string->symbol (uir-symbol-name (uir-var-name lhs)))]
    [(uir-symbol? lhs) (string->symbol (uir-symbol-name lhs))]
    [else #f]))

;; Record the class of an assignment target. `value-class` projects a value
;; expression to its class; `self-name?` identifies the host's implicit
;; receiver name; `set-local!` / `set-self-attr!` update the caller's env.
;; `elem-spec-of` (optional) projects a value to its iterable ELEMENT spec, and
;; `set-local-elem!` records it for the target name (`entries = os.scandir(p)`
;; so a later `for entry in entries` types the loop var).
(define (apply-assignments! body value-class self-name? set-local! set-self-attr!
                            #:elem-spec-of [elem-spec-of #f]
                            #:set-local-elem! [set-local-elem! (λ (nm espec) (void))]
                            #:fn-alias-of [fn-alias-of #f]
                            #:set-local-fn!
                            [set-local-fn! (λ (nm desc) (void))]
                            #:set-self-attr-fn!
                            [set-self-attr-fn! (λ (attr desc) (void))])
  ;; `x = x.m(...)`: the call dispatches on the PRE-assignment receiver, so
  ;; re-typing `x` from the result would make pass 3 read the call through the
  ;; NEW type (`content = content.message(...)` re-typing content as the MIME
  ;; Message, so the `.message` call itself misses the corpus EmailMessage
  ;; edge). The single-final-type env cannot hold both, so keep the pre-call
  ;; type: the self-call classifies correctly, and the result type is still the
  ;; value of the call for any OTHER use via value-class. A fresh target
  ;; (`msg = message.message()`) has no pre-call type to preserve, so it is
  ;; still typed normally.
  ;; The single-final-type env cannot hold both a variable's pre- and
  ;; post-assignment types. When the RHS READS the target through a method
  ;; call — either directly (`x = x.m(...)`) or nested inside another call
  ;; (`x = dict(x._get_kwargs(), ...)`) — keep the pre-call type so that
  ;; inner call classifies through the OLD value; the result type remains
  ;; reachable via value-class for any other use.
  (define (self-reassign? lhs value)
    (define tnm (target-name-symbol lhs))
    (and tnm
         (let ([hit? #f])
           (walk-uir value
                     (λ (n)
                       (when (and (uir-call? n)
                                  (member-access-callee? (uir-call-callee n)))
                         (define bnm (bare-name-symbol
                                      (member-access-base (uir-call-callee n))))
                         (when (and bnm (eq? bnm tnm)) (set! hit? #t)))))
           hit?)))
  (define (record! lhs value)
    (unless (self-reassign? lhs value)
      (define cls (value-class value))
      (when cls
        (define attr (and (member-access-callee? lhs) (member-access-field lhs)))
        (cond
          [(and attr (self-name? (bare-name-symbol (member-access-base lhs))))
           (set-self-attr! attr cls)]
          [(target-name-symbol lhs) => (λ (nm) (set-local! nm cls))]
          [else (void)]))
      (when elem-spec-of
        (define lhsnm (target-name-symbol lhs))
        (when lhsnm
          (define espec (elem-spec-of value))
          (when espec (set-local-elem! lhsnm espec))))
      ;; A bound-method alias value (`x = obj.m` / `x = getattr(obj, "m", d)`,
      ;; or the self-attr form `self._read = stream.read`) is tracked by its
      ;; (receiver-class . method-name) descriptor so the later bare/self-attr
      ;; CALL resolves as `obj.m(...)`, not as a constructor of the alias.
      (when fn-alias-of
        (define desc (fn-alias-of value))
        (when desc
          (define attr (and (member-access-callee? lhs) (member-access-field lhs)))
          (cond
            [(and attr (self-name? (bare-name-symbol (member-access-base lhs))))
             (set-self-attr-fn! attr desc)]
            [(target-name-symbol lhs) => (λ (nm) (set-local-fn! nm desc))]
            [else (void)])))))
  ;; `(call "=" (list target value))`: the JS/TS assignment encoding.
  (define (assignment-call n)
    (and (uir-call? n)
         (uir-symbol? (uir-call-callee n))
         (eq? (string->symbol (uir-symbol-name (uir-call-callee n))) '=)
         (= (length (uir-call-args n)) 2)
         (record! (first (uir-call-args n)) (second (uir-call-args n)))))
  (walk-body body
             (λ (n)
               (cond
                 [(uir-set!? n) (record! (uir-set!-name n) (uir-set!-value n))]
                 [(assignment-call n) (void)]
                 [else (void)]))))
