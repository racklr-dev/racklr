#lang racket

;; racklr/lower-koka/hooks.rkt — late-bound lowering entry points.
;;
;; The Koka lowering passes are mutually recursive (expressions contain blocks
;; and lambdas; statements contain declarations; handlers contain expressions
;; and blocks). Each module installs its entry point here when it loads, and the
;; callers dispatch through the parameter at run time, so the module dependency
;; graph stays acyclic and every recursion is one Racket parameter call.
;;
;; The public entry point requires every module before lowering, so no hook is
;; #f when lowering runs.

(provide lower-expr-fn       ; expr node → uir (block in expr position = lambda)
         lower-blockexpr-fn  ; blockexpr node → uir (block = statement grouping)
         lower-block-fn      ; block node → uir-block
         lower-handlerexpr-fn ; handlerexpr node → uir-handler
         lower-withexpr-fn   ; withexpr node → uir-handler / uir-mask
         lower-funbody-fn    ; funbody node + name → uir-fn
         lower-fnexpr-fn     ; fnexpr node → uir-fn
         lower-withstat-fn   ; withstat node + body (or #f) → uir-handler/uir-mask
         lower-basicexpr-fn   ; basicexpr node → uir
         lower-ntlexpr-fn)    ; ntlexpr node → uir

(define lower-expr-fn (make-parameter #f))
(define lower-blockexpr-fn (make-parameter #f))
(define lower-block-fn (make-parameter #f))
(define lower-handlerexpr-fn (make-parameter #f))
(define lower-withexpr-fn (make-parameter #f))
(define lower-funbody-fn (make-parameter #f))
(define lower-fnexpr-fn (make-parameter #f))
(define lower-withstat-fn (make-parameter #f))
(define lower-basicexpr-fn (make-parameter #f))
(define lower-ntlexpr-fn (make-parameter #f))
