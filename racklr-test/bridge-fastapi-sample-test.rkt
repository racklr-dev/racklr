#lang racket

;; End-to-end bridge over a substantial real FastAPI sample.
;;
;; fastapi/app is the backend of the official FastAPI full-stack
;; template (release 0.10.0), vendored unmodified: 27 Python files / 1,611
;; lines covering JWT auth, CRUD routers, SQLModel models, alembic migrations,
;; startup probes, and the email utilities. fastapi/app.fizz
;; declares the capabilities the service needs (Db, Io, Time).
;;
;; This suite runs the Effervesce bridge over that tree with the tracked
;; Python3 grammar and pins the result:
;;   - every file parses and lowers (no PARSE-FAIL / LOWER-ERROR / FALLBACK),
;;   - the bridge finds no inferred effect the spec does not declare,
;;   - the run still reports the unresolved calls as `bridge-unverified`
;;     instead of silently counting them as covered,
;;   - rows are per file, so repeated definition names keep their own effects.

(require rackunit
         racket/list
         racklr/uir
         racklr/effects/contract
         effervesce/check/verify
         "../tools/bridge-python.rkt")

(define sample-dir (build-path ".." "fastapi" "app"))
(define spec-path (build-path ".." "fastapi" "app.fizz"))

(define run (run-bridge-over-dir sample-dir spec-path))

;; ── the front end handles the whole vendored tree ───────────────────

(check-equal? (length (bridge-run-files run)) 27 "vendored Python file count")
(check-equal?
 (for/list ([r (in-list (bridge-run-files run))]
            #:unless (eq? (source-run-status r) 'ok))
   (source-run-path r))
 '()
 "every vendored file parses and lowers cleanly")

;; ── the bridge is green ─────────────────────────────────────────────

(check-true (verify-ok? (bridge-run-findings run))
            "type/policy/model checkers accept the spec")
(check-equal? (bridge-run-violations run) '()
              "no inferred effect escapes the spec's needs")
(check-true (bridge-run-ok? run))

;; The four checkers are run over real host code under the spec's component.
(check-equal? (bridge-run-components run) '("Api"))

;; ── the run actually inferred the app's real effects ────────────────

(define hist (bridge-run-effect-histogram run))

(check-equal? (sort (hash-keys hist) symbol<?) '(db io time unknown)
              "the sample performs exactly the declared host effects")
(check-equal? (hash-ref hist 'db) 23)
(check-equal? (hash-ref hist 'io) 8)
(check-equal? (hash-ref hist 'time) 3)
(check-equal? (bridge-run-function-count run) 70)
(check-equal? (bridge-run-unknown-function-count run) 53)

;; Unresolved calls are reported, not silently passed: one bridge-unverified
;; note per file that still has an unknown row.
(check-equal? (length (bridge-run-unverified run)) 20)

;; ── spot-check rows, per file ───────────────────────────────────────

(define rows (bridge-run-rows-by-file run))

(define (row-effects file fn)
  (define table (hash-ref rows file (hash)))
  (define row (hash-ref table fn #f))
  (and row
       (sort (map effect-name-symbol (uir-effect-row-entries row)) symbol<?)))

(check-equal? (row-effects "crud.py" 'create_user) '(db unknown)
              "the SQLModel session work is grounded as db")
(check-equal? (row-effects "api/routes/items.py" 'read_item) '(db))
(check-equal? (row-effects "core/security.py" 'create_access_token) '(time)
              "datetime.now in the JWT helper is grounded as time")
(check-equal? (row-effects "utils.py" 'send_email) '(io unknown))
(check-equal? (row-effects "models.py" 'get_datetime_utc) '(time))

;; Five alembic migrations define `upgrade`/`downgrade`, and `create_user`
;; appears in a router and in crud. A merged program would key rows by name and
;; collapse those definitions; the per-file run keeps them apart. This pins
;; that it does: the same function name has different effects in two files.
(check-equal? (row-effects "api/routes/users.py" 'create_user) '(unknown))
(check-true (hash-has-key? rows "alembic/versions/e2412789c190_initialize_models.py"))

(displayln
 (format "bridge-fastapi-sample: ~a files, ~a functions, effects ~a, ~a bridge-unverified file findings"
         (length (bridge-run-files run))
         (bridge-run-function-count run)
         (for/list ([k (in-list (sort (hash-keys hist) symbol<?))])
           (format "~a=~a" k (hash-ref hist k)))
         (length (bridge-run-unverified run))))
