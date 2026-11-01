#lang racket

;; ── The single emit dispatch ────────────────────────────────────────
;;
;; Every target emitter implements the contract in racklr/emit-contract:
;;
;;     render : uir? × (or/c #f uir-effect-row?) → string?
;;
;; This module is the target table and the one dispatch function. Any supported
;; target renders from the same input pair — the UIR value plus the effect row
;; that describes it — through the same call:
;;
;;     (emit 'python program row)
;;     (emit 'koka   program row)
;;     ...
;;
;; Passing `#f` as the row keeps the pre-contract call shape: the target uses
;; the rows the UIR carries (and Python/JavaScript inference, as before), so
;; existing one-argument callers keep working unchanged. Passing a
;; `uir-effect-row` makes it explicit input: the target reifies it in its
;; native vocabulary, or refuses it when it has none (never drops it).

(require racklr/uir
         "emit-contract.rkt"
         racklr/emit-python
         racklr/emit-javascript
         racklr/emit-jsx
         racklr/emit-koka
         racklr/emit-clojure
         racklr/emit-dom-javascript
         racklr/emit-html
         racklr/emit-json
         racklr/emit-effervesce)

(provide (all-from-out "emit-contract.rkt")
         emit
         emit-target-for
         emit-targets
         supported-targets)

;; ── The target table ────────────────────────────────────────────────
;;
;; Each entry is the same contract with the same call shape; only the native
;; row vocabulary differs. Building the table runs `make-emit-target`, which
;; rejects a renderer that does not accept the row argument.

(define contract-targets
  (list
   (make-emit-target 'python "Python 3 source"
                     'guard-decorator emit-python)
   (make-emit-target 'javascript "JavaScript (ES2020) source"
                     'guard-wrapper emit-javascript)
   (make-emit-target 'jsx "React.createElement JavaScript (JSX tree)"
                     'none emit-jsx)
   (make-emit-target 'koka "Koka source"
                     'effect-annotation emit-koka)
   (make-emit-target 'clojure "Clojure source"
                     'metadata emit-clojure)
   (make-emit-target 'dom-js "Vanilla DOM JavaScript (JSX tree)"
                     'none emit-dom-js)
   (make-emit-target 'html "HTML5 page with embedded JavaScript"
                     'guard-wrapper emit-html)
   (make-emit-target 'json "JSON text"
                     'none emit-json)
   (make-emit-target 'effervesce "Effervesce declaration source"
                     'none emit-effervesce)))

;; Every target that implements the contract, in table order.
(define (emit-targets)
  contract-targets)

;; The target symbols a caller may pass to `emit`.
(define (supported-targets)
  (map emit-target-target contract-targets))

;; Look up one target's contract, or raise a contract error naming the
;; supported targets.
(define (emit-target-for target)
  (cond
    [(findf (λ (t) (eq? (emit-target-target t) target)) contract-targets)
     => values]
    [else
     (error 'emit
            "unsupported emit target: ~a; supported targets: ~a"
            target (supported-targets))]))

;; ── The dispatch ────────────────────────────────────────────────────

;; Render one supported target from a UIR value plus an explicit effect row.
;; The row is `#f` or a `uir-effect-row`; each target either reifies it in its
;; native vocabulary or refuses it. See racklr/emit-contract for the contract.
(define (emit target uir [row #f])
  ((emit-target-render (emit-target-for target)) uir row))

(module+ main
  (displayln "racklr/emit — single dispatch over every emit-contract target loaded."))
