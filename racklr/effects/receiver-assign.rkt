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
(define (apply-assignments! body value-class self-name? set-local! set-self-attr!)
  (define (record! lhs cls)
    (when cls
      (define attr (and (member-access-callee? lhs) (member-access-field lhs)))
      (cond
        [(and attr (self-name? (bare-name-symbol (member-access-base lhs))))
         (set-self-attr! attr cls)]
        [(target-name-symbol lhs) => (λ (nm) (set-local! nm cls))]
        [else (void)])))
  ;; `(call "=" (list target value))`: the JS/TS assignment encoding.
  (define (assignment-call n)
    (and (uir-call? n)
         (uir-symbol? (uir-call-callee n))
         (eq? (string->symbol (uir-symbol-name (uir-call-callee n))) '=)
         (= (length (uir-call-args n)) 2)
         (record! (first (uir-call-args n)) (value-class (second (uir-call-args n))))))
  (walk-body body
             (λ (n)
               (cond
                 [(uir-set!? n) (record! (uir-set!-name n) (value-class (uir-set!-value n)))]
                 [(assignment-call n) (void)]
                 [else (void)]))))
