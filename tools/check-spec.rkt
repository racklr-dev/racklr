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
         racklr/effects/domain-lattice
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
  ;; The domain lattice is the DEFAULT interpretation: a coarse capability
  ;; (Network/Db/Io/Time) covers its leaves, so a fine host effect satisfies a
  ;; coarse declared `needs`. hostcode.json interpretations overlay it and win
  ;; on overlap (a user may deliberately narrow a capability).
  (define interp
    (for/fold ([h (domain-capability-interpretation)])
              ([(k v) (in-hash (hash-ref in 'interpretations))])
      (hash-set h (if (symbol? k) (symbol->string k) k)
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
