#lang racket

;; Real-world Koka 3.x front-end test over tracked source fixtures.
;;
;; Every fixture under koka-fixtures/ must tokenize without raising and parse
;; to a CST (non-#f). Fixtures whose constructs lower-koka and emit-koka cover
;; end to end are also round-tripped: parse -> lower -> emit -> re-parse.
;;
;; The two fixture lists below are exhaustive and disjoint. A fixture added
;; without being classified here fails the inventory case instead of being
;; silently skipped, and a classified fixture fails loudly at its real stage.
;;
;; Run from racklr-test/ (the grammar path and fixtures are resolved from this
;; file, so the generated parser module cannot leak a gen-tmp file into the
;; checkout):
;;
;;   cd racklr-test && PLTCOLLECTS="<checkout>:" racket koka-real-world-test.rkt

(require rackunit
         rackunit/text-ui
         racket/file
         racket/list
         racket/path
         racket/runtime-path
         racket/string
         racklr/tree
         racklr/uir
         racklr/gen-test
         racklr/lower-koka
         racklr/emit-koka)

(define-runtime-path grammar-path "../grammars/koka/Koka.g4")
(define-runtime-path fixtures-dir "koka-fixtures")

;; Generate the layout-aware parser in a private scratch directory so the test
;; leaves no generated module behind and parallel runs cannot collide on
;; gen-tmp-N.rkt in the working directory.
(define (load-koka-parser)
  (define scratch (make-temporary-file "racklr-koka-rwtest-~a" 'directory))
  (dynamic-wind
    void
    (λ ()
      (parameterize ([current-directory scratch])
        (define-values (parse tokenize tok-type tok-value)
          (gen-and-load-layout (path->string grammar-path)))
        (cleanup)
        (values parse tokenize tok-type tok-value)))
    (λ ()
      (if (directory-exists? scratch)
          (delete-directory/files scratch)
          (void)))))

(define-values (koka-parse koka-tokenize koka-tok-type koka-tok-value)
  (load-koka-parser))

;; ── Fixture classification ──────────────────────────────────────────

;; Round-tripped: parse -> lower-koka -> emit-koka -> re-parse, with no
;; `?`-prefixed fallback node left in the UIR. Each entry also names
;; emitted-text anchors. The anchors are the proof that lowering did not
;; silently discard the construct: an empty/degenerate UIR also emits text
;; that re-parses, so non-empty emitted text alone is not evidence.
(define round-trip-fixtures
  '((name "01-module-imports.kk"
     anchors ("fixtures/app/main" "import std/core/types" "answer"))
    (name "02-functions-signatures.kk"
     anchors ("fixtures/functions" "swap" "mapped" "risky"))
    (name "03-bindings.kk"
     anchors ("fixtures/bindings" "greeting" "counter" "local-bindings"))
    (name "04-patterns.kk"
     anchors ("fixtures/patterns" "type shape" "classify" "named-whole"))
    (name "05-types.kk"
     anchors ("fixtures/types" "tree<a>" "point2d" "alias pair<a>"))
    (name "06-effects.kk"
     anchors ("fixtures/effects" "effect counter" "named effect logger"
              "total-count"))
    (name "07-handlers.kk"
     anchors ("fixtures/handlers" "run-counter" "bump(n)"
              "resume(True)" "run-named"))
    (name "08-externs.kk"
     anchors ("fixtures/externs" "extern import" "ext-add" "@ext-null"))
    (name "09-control-flow.kk"
     anchors ("fixtures/control" "sign" "braced" "early"))
    (name "10-lambdas.kk"
     anchors ("fixtures/lambdas" "compose" "add-one" "map-twice"))
    (name "11-operators-fixity.kk"
     anchors ("fixtures/operators" "infixr 60" "(+++)" "combine"))
    (name "12-literals.kk"
     anchors ("fixtures/literals" "grouped" "0x2A" "'\\n'"))
    (name "13-continuations.kk"
     anchors ("fixtures/continuations" "matrix" "between" "chained"))
    (name "14-comments.kk"
     anchors ("fixtures/comments" "fun add" "fun sub"))
    (name "15-kinds.kk"
     anchors ("fixtures/kinds" "global-scope :: S" "alloc :: H -> X"
              "@default-value"))
    (name "16-program.kk"
     anchors ("fixtures/app/pipeline" "pub type step" "describe" "pipeline"))))

;; Parse-only: valid Koka 3.x the grammar must accept, but whose lowering /
;; emission is not yet part of the round-trip loss model. Listed explicitly on
;; purpose (an empty list is explicit); see docs/koka-real-world-battery.md.
(define parse-only-fixtures '())

(define (entry-name entry) (list-ref entry 1))
(define (entry-anchors entry) (list-ref entry 3))

(define (round-trip-names)
  (map entry-name round-trip-fixtures))

(define (fixture-path name)
  (build-path fixtures-dir name))

(define (fixture-names)
  (sort (for/list ([p (in-list (directory-list fixtures-dir))])
          (path->string p))
        string<?))

;; ── Helpers ─────────────────────────────────────────────────────────

(define (attempt thunk)
  (with-handlers ([(λ (e) #t) (λ (e) #f)])
    (thunk)))

(define (parses? src)
  (define cst (attempt (λ () (koka-parse src))))
  (and cst (any-tree? cst)))

(define (fallback-symbol? uir)
  (cond
    [(uir-symbol? uir)
     (define nm (uir-symbol-name uir))
     (and (> (string-length nm) 0) (char=? (string-ref nm 0) #\?))]
    [(struct? uir)
     (for/or ([field (in-list (cdr (vector->list (struct->vector uir))))])
       (fallback-symbol? field))]
    [(pair? uir) (or (fallback-symbol? (car uir)) (fallback-symbol? (cdr uir)))]
    [else #f]))

;; (label . passed?) rows for one fixture's round trip. Nothing here raises:
;; every stage is attempted and recorded, so one broken fixture cannot hide
;; the others in the suite report.
(define (round-trip-checks entry)
  (define name (entry-name entry))
  (define anchors (entry-anchors entry))
  (define src (file->string (fixture-path name)))
  (define cst (attempt (λ () (koka-parse src))))
  (define parsed? (and cst (any-tree? cst)))
  (define uir (and parsed?
                   (attempt (λ () (lower-koka cst koka-tok-type
                                             koka-tok-value)))))
  (define lowered? (and uir (uir? uir)))
  (define fallback? (and lowered? (fallback-symbol? uir)))
  (define emitted (and lowered? (attempt (λ () (emit-koka uir)))))
  (define emits? (and (string? emitted) (> (string-length emitted) 0)))
  (define reparses? (and emits? (parses? emitted)))
  (append
   (list (cons (format "~a: parses" name) (and parsed? #t))
         (cons (format "~a: lowers to UIR" name) (and lowered? #t))
         (cons (format "~a: no ?-fallback UIR nodes" name) (not fallback?))
         (cons (format "~a: emits non-empty Koka" name) emits?)
         (cons (format "~a: emitted Koka re-parses" name) reparses?))
   (for/list ([anchor (in-list anchors)])
     (cons (format "~a: emitted Koka contains ~s (lowering not vacuous)"
                   name anchor)
           (and (string? emitted) (string-contains? emitted anchor))))))

;; ── Suite ───────────────────────────────────────────────────────────
;;
;; Each stage aggregates its per-fixture results into one check so every
;; failure is reported by name. A rackunit test case stops at its first
;; failing check, which would hide all later fixtures otherwise.

(define (check-stage stage results)
  (define failures
    (for/list ([r (in-list results)] #:unless (cdr r)) (car r)))
  (check-equal? failures '() (format "~a: failing fixtures" stage)))

(define (inventory-case)
  (test-case "fixture inventory is explicit and exhaustive"
    (define names (fixture-names))
    (check-true (>= (length names) 16) "at least 16 fixtures")
    (check-true (pair? round-trip-fixtures) "round-trip list is not empty")
    (define classified (append (round-trip-names) parse-only-fixtures))
    (check-equal? (length classified)
                  (length (remove-duplicates classified))
                  "no fixture is classified twice")
    (check-equal? (sort classified string<?)
                  names
                  "every fixture is classified as round-trip or parse-only")))

(define (tokenize-case)
  (test-case "every fixture tokenizes without raising"
    (check-stage
     "tokenize"
     (for/list ([name (in-list (fixture-names))])
       (define tks (attempt (λ ()
                              (koka-tokenize (file->string (fixture-path name))))))
       (cons name (and (list? tks) (pair? tks) #t))))))

(define (parse-case)
  (test-case "every fixture parses to a CST"
    (check-stage
     "parse"
     (for/list ([name (in-list (fixture-names))])
       (cons name (and (parses? (file->string (fixture-path name))) #t))))))

(define (round-trip-case)
  (test-case "round-trip fixtures absorb, emit and re-parse"
    (check-stage
     "round-trip"
     (append* (for/list ([entry (in-list round-trip-fixtures)])
                (round-trip-checks entry))))))

(define suite
  (test-suite
   "Koka real-world fixtures"
   (inventory-case)
   (tokenize-case)
   (parse-case)
   (round-trip-case)))

(exit (run-tests suite))
