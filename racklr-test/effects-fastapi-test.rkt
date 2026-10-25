#lang racket

;; Source-level FastAPI effect inference: parse the real example source
;; (effervesce/examples/real/fastapi/main.py), lower it to UIR, and check the
;; whole-program fixpoint's rows. This is the acceptance test for two receiver
;; levers at once:
;;   - call-result projection through an imported module function
;;     (`r = await httpx.get(...)` then `r.json()` is pure, not unknown)
;;   - typed parameter seeding (`item: Item`, `name: str`, `key: str`)
;; The five functions must infer exactly: get_db (), list_orders (db),
;; create_order (db llm), lookup (network), cached (cache network).

(require rackunit
         racklr/uir
         racklr/effects/infer
         racklr/effects/imports
         effervesce/check/host-python)

(define fastapi-src
  (file->string "../effervesce/examples/real/fastapi/main.py"))

(define program (python-source->uir fastapi-src))
(check-true (and program (uir-block? program))
            "fastapi main.py parses and lowers to a uir-block")

;; The example's imports: httpx and redis modules (network/cache domains via
;; the effect-token roots), FastAPI and pydantic BaseModel as class handles.
(define env
  (import-env
   (hash "main.py" "main")
   (hash "main" (hash "httpx" (handle 'module "httpx" #f)
                      "redis" (handle 'module "redis" #f)
                      "FastAPI" (handle 'class "fastapi" "FastAPI")
                      "BaseModel" (handle 'class "pydantic" "BaseModel")))
   (hash)))

(define rows (infer-effects-all (list (cons "main.py" program)) #:import-env env))

(define (row-names* nm)
  (sort (map (λ (e) (string->symbol (uir-symbol-name (uir-effect-entry-name e))))
             (uir-effect-row-entries (hash-ref rows (cons "main.py" nm))))
        symbol<?))

(check-equal? (row-names* 'get_db) '())
(check-equal? (row-names* 'list_orders) '(db))
(check-equal? (row-names* 'create_order) '(db llm))
(check-equal? (row-names* 'lookup) '(network))
(check-equal? (row-names* 'cached) '(cache network))

;; The same five rows, but through the REAL pipeline: tokenize → lower →
;; module-spec → build-module-table → resolve-exports → derive-class-method-
;; effects → infer-effects-all. This exercises the external-import class
;; synthesis (`from fastapi import FastAPI`, `from pydantic import BaseModel`)
;; instead of a hand-built import-env: the import graph alone must carry the
;; receiver types to the same answers.
(define tokens (py-tokenize fastapi-src))
(define fastapi-imports
  (extract-imports
   (for/list ([t (in-list tokens)]) (cons (py-tok-type t) (py-tok-value t)))))
(define fastapi-mods
  (build-module-table
   (list (module-spec "main" program fastapi-imports))))
(define fastapi-exports (resolve-exports fastapi-mods (set)))
(define fastapi-env
  (import-env (hash "main.py" "main")
              fastapi-exports
              (derive-class-method-effects fastapi-mods)))
(define pipeline-rows
  (infer-effects-all (list (cons "main.py" program)) #:import-env fastapi-env))
(define (pipeline-row-names nm)
  (sort (map (λ (e) (string->symbol (uir-symbol-name (uir-effect-entry-name e))))
             (uir-effect-row-entries (hash-ref pipeline-rows (cons "main.py" nm))))
        symbol<?))
(check-equal? (pipeline-row-names 'get_db) '())
(check-equal? (pipeline-row-names 'list_orders) '(db))
(check-equal? (pipeline-row-names 'create_order) '(db llm))
(check-equal? (pipeline-row-names 'lookup) '(network))
(check-equal? (pipeline-row-names 'cached) '(cache network))

(displayln "All FastAPI infer tests passed.")
