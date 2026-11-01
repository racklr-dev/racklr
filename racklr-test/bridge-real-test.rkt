#lang racket

;; End-to-end bridge: real Python frameworks vs. Effervesce specs.
;;
;; Each case parses a small real source sample with the Python3 front end,
;; lowers it to UIR, loads the .fizz spec, and runs the bridge checker: the
;; code's inferred effect row must be covered by the component's `needs`
;; (interpreted through a capability-interpretation table).

(require rackunit
         racklr/tree
         racklr/uir
         racklr/gen-test
         racklr/lower-python
         effervesce/load
         effervesce/check/bridge)

(define-values (py-parse py-tokenize py-tok-type py-tok-value)
  (gen-and-load-py "../grammars/python3/Python3Parser.g4"))

(define py-parse-file-input
  (let ([mod-path (*last-module-path*)])
    (λ (src)
      (define tks ((dynamic-require mod-path 'tokenize) src))
      (match-define (list _fp res) ((dynamic-require mod-path 'parse-file_input) tks 0))
      res)))

(define (load-py path)
  (define cst (py-parse-file-input (file->string path)))
  (check-true (any-tree? cst) (format "parses ~a" path))
  (lower-python cst py-tok-type py-tok-value))

(define (load-spec name)
  (load-effervesce
   (file->string (format "../effervesce/examples/real/~a/main.fizz" name))))

(define (bridge-case name component interpretations)
  (define spec (load-spec name))
  (define program (load-py (format "../effervesce/examples/real/~a/main.py" name)))
  (check-bridge spec (hash component program) interpretations))

(define (violations issues)
  (map bridge-violation-effect (filter bridge-violation? issues)))

;; FastAPI: db, llm, network, cache are all declared and inferred.
(define fastapi-issues
  (bridge-case "fastapi" "Api"
               (hash "Db" '(db) "Llm" '(llm) "Http" '(network) "Cache" '(cache))))
(check-true (bridge-ok? fastapi-issues) (format "fastapi: ~a" (violations fastapi-issues)))

;; Django: the ORM entry point (Order.objects) infers db.
(define django-issues
  (bridge-case "django" "Store" (hash "Db" '(db))))
(check-true (bridge-ok? django-issues) (format "django: ~a" (violations django-issues)))

;; agno: the agent framework namespace infers llm.
(define agno-issues
  (bridge-case "agno" "Agent" (hash "Llm" '(llm))))
(check-true (bridge-ok? agno-issues) (format "agno: ~a" (violations agno-issues)))

;; langgraph: the workflow graph namespace infers llm.
(define langgraph-issues
  (bridge-case "langgraph" "Workflow" (hash "Llm" '(llm))))
(check-true (bridge-ok? langgraph-issues) (format "langgraph: ~a" (violations langgraph-issues)))

(displayln (format "bridge-real: fastapi=~a django=~a agno=~a langgraph=~a issues"
                   (length fastapi-issues) (length django-issues)
                   (length agno-issues) (length langgraph-issues)))
