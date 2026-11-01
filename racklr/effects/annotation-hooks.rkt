#lang racket

;; racklr/effects/annotation-hooks.rkt
;;
;; The effect-row ANNOTATION HOOK a front end supplies: how to recognize a
;; source-language level effect-row annotation (and unwrap it) when writing
;; inferred rows back into a program. This is a front-end hook, not engine
;; logic: `infer.rkt` (the host-neutral fixpoint) takes an `annotation-hooks`
;; value and never names a language's annotation module.
;;
;; The default hook is the TypeScript/JavaScript `withEffects(row)(fn)`
;; surface, the annotation encoding the shared JS/TS/JSX front ends emit and
;; the emitter renders. A different host supplies its own hooks through
;; `annotate-effects #:annotation-hooks`.

(require racklr/effects/ts-annot)

(provide
 annotation-hooks? annotation-hooks
 annotation-hooks-with-effects-call?
 annotation-hooks-extract-row
 annotation-hooks-extract-fn
 default-annotation-hooks)

(struct annotation-hooks
  (with-effects-call?   ; (value -> boolean): is this a declaration annotation?
   extract-row          ; (value -> uir-effect-row or #f)
   extract-fn)          ; (value -> the annotated function value or #f)
  #:transparent)

(define default-annotation-hooks
  (annotation-hooks with-effects-call?
                    extract-with-effects-row
                    extract-with-effects-fn))
