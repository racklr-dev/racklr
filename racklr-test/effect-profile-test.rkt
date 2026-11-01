#lang racket

;; Confidence and coverage labels on effect edges, and transitive effect
;; profiles (Rainbow's maturity-by-weakest-link + per-theorem axiom profile,
;; applied to racklr's effect graph).
;;
;; Proves:
;;   * the two label vocabularies are ordered weakest -> strongest and combine
;;     with the weakest link;
;;   * the classification kind maps to the confidence label;
;;   * a ground's coverage distinguishes structural for-all evidence
;;     ('universal) from worked instances (a 'census-param receiver or a
;;     'local-literal local value), recorded by both inference drivers;
;;   * a function's transitive profile exposes every reachable effect edge with
;;     its weakest-link labels and the exact witness chains;
;;   * the unresolved leaves are a data structure (obligations with chains and
;;     evidence kinds), not a boolean flag.

(require rackunit
         racklr/uir
         racklr/effects/contract
         racklr/effects/imports
         racklr/effects/infer
         racklr/effects/labels
         racklr/effects/provenance
         racklr/graph/provenance
         racklr/graph/profile)

;; ── helpers ─────────────────────────────────────────────────────────

(define (sym s) (uir-symbol s))
(define (v s) (uir-var (uir-symbol s)))
(define (dot b f) (uir-get b (uir-symbol f)))
(define (fn name body) (uir-fn (uir-symbol name) '() body #f #f))
(define (fn/params name params body) (uir-fn (uir-symbol name) params body #f #f))
(define (typed-param name) (uir-typed-param (uir-symbol name) #f #f #f))
(define (prog . stmts) (uir-block stmts))
(define (py-class name methods)
  (uir-class (uir-symbol name) '() '() methods))
(define (py-class/super name super methods)
  (uir-class (uir-symbol name) (list super) '() methods))
(define (py-method name body)
  (uir-method (uir-symbol name) '() body 'public))

;; Per-file inference + graph.
(define (infer-graph program)
  (define prov-box (box #f))
  (define rows (infer-effects program #:provenance-record prov-box))
  (values rows (provenance-graph rows (unbox prov-box))))

;; Whole-program inference + graph.
(define (infer-all-graph comp-programs #:import-env [env #f])
  (define prov-box (box #f))
  (define rows (infer-effects-all comp-programs
                                  #:import-env env
                                  #:provenance-record prov-box))
  (values rows (provenance-graph rows (unbox prov-box))))

(define (ground-of path) (walk-path-ground path))

;; ── label vocabularies ──────────────────────────────────────────────

(define (test-label-vocabularies)
  (for ([c (in-list '("inferred" "declared" "unknown"))])
    (check-true (confidence? (string->symbol c))))
  (check-false (confidence? 'probable))
  (check-true (coverage? 'universal))
  (check-true (coverage? 'census-param))
  (check-true (coverage? 'local-literal))
  (check-false (coverage? 'declared))
  ;; Weakest -> strongest.
  (check-equal? confidence-order '(unknown declared inferred))
  (check-equal? coverage-order '(local-literal census-param universal))
  ;; The evidence order, weakest at the top.
  (check-true (confidence<=? 'unknown 'declared))
  (check-true (confidence<=? 'declared 'inferred))
  (check-false (confidence<=? 'inferred 'declared))
  (check-true (coverage<=? 'local-literal 'census-param))
  (check-true (coverage<=? 'census-param 'universal))
  (check-false (coverage<=? 'universal 'census-param))
  ;; Weakest-link combination, in both dimensions.
  (check-equal? (confidence-weakest 'inferred 'declared) 'declared)
  (check-equal? (confidence-weakest 'declared 'unknown 'inferred) 'unknown)
  (check-equal? (coverage-weakest 'universal 'census-param) 'census-param)
  (check-equal? (coverage-weakest 'universal 'local-literal 'census-param)
                'local-literal)
  ;; The empty fold is the strongest label (no weak evidence to combine).
  (check-equal? (confidence-weakest) 'inferred)
  (check-equal? (coverage-weakest) 'universal)
  ;; Coverage helpers.
  (check-true (coverage-universal? 'universal))
  (check-false (coverage-universal? 'census-param))
  (check-true (coverage-worked-instance? 'census-param))
  (check-true (coverage-worked-instance? 'local-literal))
  (check-false (coverage-worked-instance? 'universal)))

;; ── kind -> confidence ──────────────────────────────────────────────

(define (test-confidence-of-kind)
  (check-equal? (confidence-of-kind 'inferred) 'inferred)
  (check-equal? (confidence-of-kind 'declared) 'declared)
  (check-equal? (confidence-of-kind 'unresolved) 'unknown)
  (check-exn exn:fail? (λ () (confidence-of-kind 'bogus))))

;; ── coverage evidence derivation ────────────────────────────────────

(define (test-ground-coverage-derivation)
  ;; No receiver dataflow needed: a structural rule or lexicon hit, for-all.
  (check-equal? (ground-coverage (dot (v "requests") "get"))
                'universal)
  (check-equal? (ground-coverage (dot (v "Model") "objects") 'structural '(Model))
                'universal)
  ;; A receiver resolved by the dataflow: a parameter is a census param (the
  ;; claim holds for the actual call-site arguments), a local/chain/call-result
  ;; value is a worked instance of a local/literal value.
  (check-equal? (ground-coverage (dot (v "qs") "filter") 'dataflow '(qs))
                'census-param)
  (check-equal? (ground-coverage (dot (v "qs") "filter") 'dataflow '(other))
                'local-literal)
  (check-equal? (ground-coverage (dot (dot (dot (v "self") "a") "b") "m")
                                 'dataflow '(other))
                'local-literal)
  ;; self/cls are receivers, never parameters even if the front end keeps them
  ;; in the parameter list.
  (check-equal? (unresolved-coverage (dot (v "self") "mystery") '(self))
                'local-literal)
  ;; A bare callee naming a parameter is a callback (higher-order) parameter.
  (check-equal? (unresolved-coverage (v "callback") '(callback))
                'census-param)
  (check-equal? (unresolved-coverage (v "callback") '(other))
                'local-literal)
  (check-equal? (unresolved-coverage (dot (v "arbitrary") "m")) 'local-literal))

;; ── ground coverage as recorded by the inference drivers ────────────

(define (test-recorded-ground-coverage)
  ;; Per-file inference: a structural effect ground is universal; an unresolved
  ;; site's evidence is the local call expression (worked instance).
  (define-values (_rows graph)
    (infer-graph (prog (fn "f" (uir-block
                                (list (uir-call (v "open") '())
                                      (uir-call (dot (v "arbitrary") "m") '())))))))
  (define io-ground (ground-of (first (provenance-paths graph "f" 'io))))
  (check-equal? (walk-ground-kind io-ground) 'inferred)
  (check-equal? (walk-ground-coverage io-ground) 'universal)
  (define unknown-ground (ground-of (first (provenance-paths graph "f" 'unknown))))
  (check-equal? (walk-ground-kind unknown-ground) 'unresolved)
  (check-equal? (walk-ground-coverage unknown-ground) 'local-literal)
  ;; The coverage label is in the reusable index too.
  (define idx (make-graph-index graph))
  (check-equal? (hash-ref (graph-index-site-coverages idx)
                          (walk-ground-origin io-ground))
                'universal))

(define (test-param-rooted-obligation-coverage)
  ;; Whole-program inference: an unresolved receiver that is a parameter is a
  ;; census-param obligation (parameter dataflow would resolve it); an
  ;; unresolved receiver in a function without parameters is a local/literal
  ;; one.
  (define param-program
    (prog (fn/params "go" (list (typed-param "l"))
                     (uir-call (dot (v "l") "mystery") '()))))
  (define local-program
    (prog (fn "local_call" (uir-call (dot (v "x") "mystery") '()))))
  (define-values (_rows graph)
    (infer-all-graph (list (cons "app.py" param-program)
                           (cons "b.py" local-program))))
  (define param-ground (ground-of (first (provenance-paths graph "app.py::go" 'unknown))))
  (check-equal? (walk-ground-kind param-ground) 'unresolved)
  (check-equal? (walk-ground-coverage param-ground) 'census-param)
  (define local-ground (ground-of (first (provenance-paths graph "b.py::local_call" 'unknown))))
  (check-equal? (walk-ground-coverage local-ground) 'local-literal))

(define (test-declared-ground-is-universal)
  ;; A blessed capability is asserted at the definition by the DATA config: it
  ;; is a for-all contract, so its coverage is universal.
  (define models (prog (py-class "Model" (list (py-method "save" (uir-null))))))
  (define app
    (prog (py-class/super "Sub" (v "Model")
                          (list (py-method "run"
                                           (uir-call (dot (v "self") "save") '()))))))
  (define env
    (import-env (hash "models.py" "django.db.models" "app.py" "app")
                (hash)
                (hash "Model.save"
                      (effect-cap 'db (list 'db<Model>.write)))))
  (define-values (_rows graph)
    (infer-all-graph (list (cons "models.py" models) (cons "app.py" app))
                     #:import-env env))
  (define ground (ground-of (first (provenance-paths graph "app.py::Sub.run" 'db))))
  (check-equal? (walk-ground-kind ground) 'declared)
  (check-equal? (walk-ground-coverage ground) 'universal))

;; ── transitive profiles ─────────────────────────────────────────────

(define (chain-program)
  (prog (fn "a" (uir-call (v "b") '()))
        (fn "b" (uir-call (v "c") '()))
        (fn "c" (uir-call (v "open") '()))))

(define (test-direct-profile)
  (define-values (_rows graph) (infer-graph (chain-program)))
  (define profile (transitive-profile graph "c"))
  (check-true (effect-profile? profile))
  (check-equal? (effect-profile-fn profile) "c")
  (check-true (effect-profile-complete? profile))
  (check-equal? (effect-profile-obligations profile) '())
  (define entry (profile-entry-for profile 'io))
  (check-true (profile-entry? entry))
  (check-equal? (profile-entry-effect entry) 'io)
  (check-equal? (profile-entry-confidence entry) 'inferred)
  (check-equal? (profile-entry-coverage entry) 'universal)
  ;; One witness, and it IS the ground: no hops for a direct effect.
  (check-equal? (length (profile-entry-paths entry)) 1)
  (check-equal? (walk-path-hops (first (profile-entry-paths entry))) '())
  (check-equal? (walk-ground-site (walk-path-ground
                                   (first (profile-entry-paths entry))))
                "open")
  ;; No entry for an effect the function does not have.
  (check-false (profile-entry-for profile 'db)))

(define (test-transitive-profile-witnesses)
  (define-values (_rows graph) (infer-graph (chain-program)))
  (define profile (transitive-profile graph "a"))
  (check-true (effect-profile-complete? profile))
  (define entry (profile-entry-for profile 'io))
  (check-equal? (profile-entry-confidence entry) 'inferred)
  (check-equal? (profile-entry-coverage entry) 'universal)
  (define path (first (profile-entry-paths entry)))
  (check-equal? (map walk-hop-from (walk-path-hops path)) '("a" "b"))
  (check-equal? (map walk-hop-to (walk-path-hops path)) '("b" "c"))
  (check-equal? (walk-ground-site (walk-path-ground path)) "open")
  ;; The profile is deterministic.
  (check-equal? (profile->string (transitive-profile graph "a"))
                (profile->string profile))
  ;; Rendering names the label and the open-obligation count.
  (check-true (string-contains? (profile->string profile)
                                "a: 1 effect(s), 0 open obligation(s)"))
  (check-true (string-contains? (profile->string profile) "io inferred/universal")))

(define (test-unknown-obligation-is-data)
  (define-values (_rows graph)
    (infer-graph (prog (fn "f" (uir-call (dot (v "arbitrary") "m") '())))))
  (define profile (transitive-profile graph "f"))
  (check-false (effect-profile-complete? profile))
  (check-equal? (length (effect-profile-obligations profile)) 1)
  (define obligation (first (effect-profile-obligations profile)))
  (check-equal? (walk-ground-kind (walk-path-ground obligation)) 'unresolved)
  (check-equal? (walk-ground-coverage (walk-path-ground obligation)) 'local-literal)
  (check-equal? (walk-ground-site (walk-path-ground obligation)) "arbitrary.m")
  ;; The unknown edge is an entry too, and its confidence is the obligation.
  (define entry (profile-entry-for profile 'unknown))
  (check-true (profile-entry? entry))
  (check-equal? (profile-entry-confidence entry) 'unknown)
  (check-equal? (profile-entry-coverage entry) 'local-literal)
  ;; The rendered profile lists the open obligation with its chain.
  (check-true (string-contains? (profile->string profile) "incomplete"))
  (check-true (string-contains? (profile->string profile) "! f => arbitrary.m")))

(define (test-weakest-link-in-profile)
  ;; `Sub.run` calls a blessed `Model.save` (declared db) and an imported
  ;; `qs.filter` (inferred db). Both support the same db edge; the label is the
  ;; WEAKEST link, so the profile reports declared, not inferred.
  (define models (prog (py-class "Model" (list (py-method "save" (uir-null))))))
  (define app
    (prog (py-class/super "Sub" (v "Model")
                          (list (py-method "run"
                                           (uir-block
                                            (list (uir-call (dot (v "self") "save") '())
                                                  (uir-call (dot (v "qs") "filter") '()))))))))
  (define env
    (import-env (hash "models.py" "django.db.models" "app.py" "app")
                (hash "app" (hash "qs" (handle 'class "django.db.models" "QuerySet")))
                (hash "Model.save" (effect-cap 'db (list 'db<Model>.write)))))
  (define-values (_rows graph)
    (infer-all-graph (list (cons "models.py" models) (cons "app.py" app))
                     #:import-env env))
  (define profile (transitive-profile graph "app.py::Sub.run"))
  (define entry (profile-entry-for profile 'db))
  (check-equal? (profile-entry-confidence entry) 'declared)
  (check-equal? (profile-entry-coverage entry) 'universal)
  ;; Two grounds, two witnesses: the declared definition and the imported call.
  (check-equal? (length (profile-entry-paths entry)) 2)
  (check-not-false
   (member "models.py::Model.save"
           (map walk-ground-fn
                (map walk-path-ground (profile-entry-paths entry)))))
  (check-true (string-contains? (profile->string profile) "db declared/universal")))

(define (test-pure-function-profile)
  ;; A pure leaf that is only a callee is still in the graph: its profile is
  ;; empty and complete, not absent.
  (define-values (_rows graph)
    (infer-graph (prog (fn "a" (uir-call (v "pure_leaf") '()))
                       (fn "pure_leaf" (uir-null)))))
  (define profile (transitive-profile graph "pure_leaf"))
  (check-true (effect-profile? profile))
  (check-equal? (effect-profile-entries profile) '())
  (check-true (effect-profile-complete? profile))
  (check-true (string-contains? (profile->string profile)
                                "pure_leaf: 0 effect(s), 0 open obligation(s)"))
  ;; The caller's profile reaches the pure leaf but has no effect entry for it.
  (check-equal? (effect-profile-entries (transitive-profile graph "a")) '()))

(define (test-all-profiles)
  (define-values (_rows graph) (infer-graph (chain-program)))
  (define profiles (transitive-profiles graph))
  (check-equal? (map effect-profile-fn profiles) '("a" "b" "c"))
  ;; An unknown function is not in the graph.
  (check-false (transitive-profile graph "missing")))

(module+ main
  (test-label-vocabularies)
  (test-confidence-of-kind)
  (test-ground-coverage-derivation)
  (test-recorded-ground-coverage)
  (test-param-rooted-obligation-coverage)
  (test-declared-ground-is-universal)
  (test-direct-profile)
  (test-transitive-profile-witnesses)
  (test-unknown-obligation-is-data)
  (test-weakest-link-in-profile)
  (test-pure-function-profile)
  (test-all-profiles)
  (displayln "All effect-profile tests passed."))
