#lang racket

;; tools/check-spec.rkt <spec.fizz> <hostcode.json> — the kernel guardrail:
;; lower host code and check it against a .fizz spec.
;; hostcode.json: {"components": {"Store": "def fetch(k): ..."},
;;                  "interpretations": {"Db": ["db"], "Log": ["io"]}}
;; Prints JSON: [{"component": ..., "effect": ...|"unverified", "sites": [...]}]
;; (empty array = the kernel accepts every claim).

(require json
         racket/string
         racklr/uir
         effervesce/load
         effervesce/check/bridge
         effervesce/check/host-python)

(define (issue->jsexpr i)
  (cond
    [(bridge-violation? i)
     (hasheq 'component (bridge-violation-component i)
             'effect (symbol->string (bridge-violation-effect i))
             'sites (bridge-violation-sites i))]
    [(bridge-unverified? i)
     (hasheq 'component (bridge-unverified-component i)
             'effect "unverified"
             'sites '())]
    [else (hasheq)]))

(define (main)
  (define args (vector->list (current-command-line-arguments)))
  (define spec-file (car args))
  (define hostcode-file (cadr args))
  (define spec (load-effervesce (file->string spec-file)))
  (define in (call-with-input-file hostcode-file read-json))
  (define comps (hash-ref in 'components))
  ;; normalize JSON interp to check-bridge's shape: capability-name-string ->
  ;; (listof effect-symbol).
  (define interp
    (for/hash ([(k v) (in-hash (hash-ref in 'interpretations))])
      (values (if (symbol? k) (symbol->string k) k)
              (for/list ([e (in-list v)]) (string->symbol e)))))
  (define host-code
    (for/hash ([(k v) (in-hash comps)])
      (values (if (symbol? k) (symbol->string k) k)
              ;; the front end needs a trailing NEWLINE to terminate the last
              ;; statement.
              (python-source->uir (string-append v "\n")))))
  (define issues (check-bridge spec host-code interp))
  (display (jsexpr->string (map issue->jsexpr issues))))

(module+ main (main))
