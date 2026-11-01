#lang racket

;; Stable site origins for effect-edge provenance (nc-4A19).
;;
;; Proves that a provenance ground is keyed on a STABLE SITE IDENTITY — the
;; origin (file, owner, receiver chain, and a source-structural positional
;; discriminator) — and never on a value inference generated:
;;
;;   * a call site's identity is the same across runs over the same source;
;;   * two identical expressions at different sites are distinct origins (the
;;     evidence is not collapsed);
;;   * the effect/kind the site classified (inference results) do not enter the
;;     identity, so an edge whose classification changes keeps its site;
;;   * literal argument values do not enter the identity;
;;   * the receiver chain is structural, not a rendered expression, and the
;;     readable expression is a separate display atom;
;;   * whole-program origins carry the component file; a declared capability
;;     is a definition site (`@declared`) with the module/class/method chain;
;;   * a line number, when the UIR records one, is part of the identity and is
;;     projected as a `site-line` atom;
;;   * comparing `provenance-origins` across two runs localises drift to the
;;     sites whose identity moved.

(require rackunit
         racklr/hypergraph
         racklr/uir
         racklr/effects/contract
         racklr/effects/imports
         racklr/effects/infer
         racklr/effects/provenance
         racklr/effects/origin
         racklr/graph/provenance)

;; ── helpers ─────────────────────────────────────────────────────────

(define (sym s) (uir-symbol s))
(define (v s) (uir-var (uir-symbol s)))
(define (dot b f) (uir-get b (uir-symbol f)))
(define (fn name body #:params [params '()])
  (uir-fn (uir-symbol name) params body #f #f))
(define (prog . stmts) (uir-block stmts))
(define (py-class name methods)
  (uir-class (uir-symbol name) '() '() methods))
(define (py-class/super name super methods)
  (uir-class (uir-symbol name) (list super) '() methods))
(define (py-method name body)
  (uir-method (uir-symbol name) '() body 'public))

(define (infer-record program #:source-file [file #f])
  (define prov-box (box #f))
  (define rows (infer-effects program #:provenance-record prov-box
                              #:source-file file))
  (values rows (unbox prov-box)))

(define (infer-all-record comp-programs #:import-env [env #f])
  (define prov-box (box #f))
  (define rows (infer-effects-all comp-programs #:import-env env
                                  #:provenance-record prov-box))
  (values rows (unbox prov-box)))

(define (grounds-of prov owner effect)
  (hash-ref (hash-ref (provenance-grounds prov) owner (hash)) effect '()))

(define (origin-of-call program owner effect #:source-file [file #f])
  (define-values (_rows prov) (infer-record program #:source-file file))
  (prov-site-origin (first (grounds-of prov owner effect))))

;; ── the identity is the site, not the expression ────────────────────

(define (test-origin-fields-of-a-call-site)
  ;; `f` calls requests.get: the origin names file/owner/position/chain, and
  ;; the readable expression is a separate field that is NOT the identity.
  (define p (prog (fn "f" (uir-call (dot (v "requests") "get") (list)))))
  (define origin (origin-of-call p "f" 'network #:source-file "app.py"))
  (check-equal? (prov-origin-file origin) "app.py")
  (check-equal? (prov-origin-owner origin) "f")
  (check-equal? (prov-origin-position origin) 0)   ; outer call is the first call node
  (check-equal? (prov-origin-line origin) #f)      ; UIR carries no positions yet
  (check-equal? (prov-origin-chain origin) '("requests" "get"))
  (check-equal? (prov-origin->string origin) "app.py::f@0:requests.get")
  (check-not-equal? (prov-origin->string origin) "requests.get"))

(define (test-origin-without-file)
  ;; A per-file run without #:source-file still has a stable owner/position/
  ;; chain identity; it simply has no file component.
  (define p (prog (fn "f" (uir-call (v "open") (list)))))
  (define origin (origin-of-call p "f" 'io))
  (check-false (prov-origin-file origin))
  (check-equal? (prov-origin->string origin) "f@0:open"))

(define (test-identical-expressions-are-distinct-sites)
  ;; Two `open()` calls are two sites. The old identity (the rendered
  ;; expression) collapsed them; the positional discriminator keeps the
  ;; evidence distinct, and the graph carries both grounds.
  (define p (prog (fn "f" (uir-block (list (uir-call (v "open") (list))
                                           (uir-call (v "open") (list)))))))
  (define-values (rows prov) (infer-record p))
  (define sites (grounds-of prov "f" 'io))
  (check-equal? (length sites) 2)
  (check-equal? (sort (map (λ (s) (prov-origin->string (prov-site-origin s))) sites)
                      string<?)
                '("f@0:open" "f@1:open"))
  ;; Both are display "open" — the rendering is not the identity.
  (check-equal? (remove-duplicates (map prov-site-site sites)) '("open"))
  ;; The query returns one path per site.
  (define graph (provenance-graph rows prov))
  (check-equal? (length (provenance-paths graph "f" 'io)) 2)
  (check-equal? (provenance-origins graph) '("f@0:open" "f@1:open")))

(define (test-origin-is-stable-across-runs)
  ;; Two independent inference runs over the same source produce the same
  ;; origin ids and the same graph atoms: origins are not generated values.
  (define (build)
    (prog (fn "a" (uir-block (list (uir-call (v "b") (list))
                                   (uir-call (v "open") (list)))))
          (fn "b" (uir-call (dot (v "requests") "get") (list)))))
  (define-values (rows1 prov1) (infer-record (build) #:source-file "app.py"))
  (define-values (rows2 prov2) (infer-record (build) #:source-file "app.py"))
  (define graph1 (provenance-graph rows1 prov1))
  (define graph2 (provenance-graph rows2 prov2))
  (check-equal? graph1 graph2)
  (check-equal? (provenance-origins graph1) (provenance-origins graph2)))

;; ── inference results do not enter the identity ────────────────────

(define (test-origin-is-independent-of-effect-and-kind)
  ;; The same site classified as network vs io, inferred vs unresolved: the
  ;; site identity must not move with the classification value.
  (define callee (dot (v "requests") "get"))
  (define network (prov-site-origin (make-callee-site "f" 'network callee #:position 3)))
  (define io (prov-site-origin (make-callee-site "f" 'io callee #:position 3)))
  (define unresolved (prov-site-origin (make-callee-site "f" 'unknown callee
                                                         #:kind 'unresolved
                                                         #:position 3)))
  (check-equal? network io)
  (check-equal? network unresolved)
  (check-equal? (prov-origin->string network) "f@3:requests.get"))

(define (test-origin-is-independent-of-argument-values)
  ;; Literal argument values are source data, not site identity.
  (define (p n)
    (prog (fn "f" (uir-call (v "open") (list (uir-number n))))))
  (define o1 (origin-of-call (p "1") "f" 'io))
  (define o2 (origin-of-call (p "2") "f" 'io))
  (check-equal? o1 o2))

;; ── the receiver chain is structural ────────────────────────────────

(define (test-receiver-chain-is-structural)
  (check-equal? (callee-chain (v "open")) '("open"))
  (check-equal? (callee-chain (dot (v "self") "save")) '("self" "save"))
  (check-equal? (callee-chain (dot (dot (v "db") "objects") "filter"))
                '("db" "objects" "filter"))
  ;; A call used as a receiver (`super()...`) contributes the invoked name from
  ;; its structure, not from a rendered expression.
  (check-equal? (callee-chain (dot (uir-call (v "super") (list)) "__init__"))
                '("super" "__init__"))
  (check-equal? (callee-chain (uir-symbol "fetch")) '("fetch"))
  (check-equal? (callee-chain (uir-paren (dot (v "self") "save"))) '("self" "save")))

(define (test-graph-projects-origin-atoms)
  ;; The site atom is named by the origin id; the readable expression, the file
  ;; and the receiver chain ride as separate display/component atoms.
  (define p (prog (fn "f" (uir-call (dot (v "self") "save") (list)))))
  (define-values (rows prov) (infer-record p #:source-file "app.py"))
  (define graph (provenance-graph rows prov))
  (define path (first (provenance-paths graph "f" 'db)))
  (define ground (walk-path-ground path))
  (check-equal? (walk-ground-origin ground) "app.py::f@0:self.save")
  (check-equal? (walk-ground-site ground) "self.save")
  (check-equal? (walk-ground-file ground) "app.py")
  (check-equal? (walk-ground-chain ground) "self.save")
  (check-equal? (walk-ground-line ground) #f)
  (define (link-of-type type)
    (for/list ([a (in-list graph)]
               #:when (and (link? a) (eq? (link-type a) type)))
      a))
  (check-true (pair? (link-of-type 'site-file)))
  (check-true (pair? (link-of-type 'site-chain)))
  (check-false (pair? (link-of-type 'site-line)))
  (check-not-false (member (site-node "app.py::f@0:self.save") graph)))

;; ── whole-program origins carry the component file ─────────────────

(define (test-whole-program-origin-has-file)
  ;; Cross-file inference knows the component, so the origin has a file even
  ;; without #:source-file. A call hop and a ground are both identified by
  ;; their own site: Sub.run's `self.save` call, and Base.save's `open`.
  (define app
    (prog (py-class/super "Sub" (v "Base")
                          (list (py-method "run"
                                           (uir-call (dot (v "self") "save") (list)))))))
  (define base
    (prog (py-class "Base"
                    (list (py-method "save" (uir-call (v "open") (list)))))))
  (define-values (rows prov)
    (infer-all-record (list (cons "app.py" app) (cons "base.py" base))))
  ;; The call hop's origin names the caller, its file, and the receiver chain.
  (define call (first (hash-ref (provenance-calls prov) "app.py::Sub.run")))
  (define hop (prov-call-origin call))
  (check-equal? (prov-origin-file hop) "app.py")
  (check-equal? (prov-origin-owner hop) "Sub.run")
  (check-equal? (prov-origin-chain hop) '("self" "save"))
  (check-equal? (prov-origin->string hop) "app.py::Sub.run@0:self.save")
  ;; The ground is the callee's site, with the component file of its own file.
  (define sites (grounds-of prov "base.py::Base.save" 'io))
  (check-equal? (length sites) 1)
  (define ground (prov-site-origin (first sites)))
  (check-equal? (prov-origin-file ground) "base.py")
  (check-equal? (prov-origin->string ground) "base.py::Base.save@0:open")
  ;; The graph answers the cross-file query with both origins.
  (define graph (provenance-graph rows prov))
  (define path (first (provenance-paths graph "app.py::Sub.run" 'io)))
  (check-equal? (walk-hop-origin (first (walk-path-hops path)))
                "app.py::Sub.run@0:self.save")
  (check-equal? (walk-ground-origin (walk-path-ground path))
                "base.py::Base.save@0:open"))

(define (test-declared-origin-names-the-definition)
  ;; A blessed capability is grounded at the method definition: no position,
  ;; an explicit `@declared` identity, and the module/class/method chain.
  (define models (prog (py-class "Model" (list (py-method "save" (uir-null))))))
  (define app
    (prog (py-class/super "Sub" (v "Model")
                          (list (py-method "run"
                                           (uir-call (dot (v "self") "save") (list)))))))
  (define env
    (import-env (hash "models.py" "django.db.models" "app.py" "app")
                (hash)
                (hash "Model.save"
                      (effect-cap 'db (list 'db<Model>.write)))))
  (define-values (_rows prov)
    (infer-all-record (list (cons "models.py" models) (cons "app.py" app))
                      #:import-env env))
  (define sites (grounds-of prov "models.py::Model.save" 'db))
  (check-equal? (length sites) 1)
  (define origin (prov-site-origin (first sites)))
  (check-equal? (prov-origin-file origin) "models.py")
  (check-equal? (prov-origin-owner origin) "Model.save")
  (check-false (prov-origin-position origin))
  (check-equal? (prov-origin-chain origin)
                '("django.db.models" "Model" "save"))
  (check-equal? (prov-origin->string origin)
                "models.py::Model.save@declared:django.db.models.Model.save"))

;; ── a known line joins the identity and is projected ───────────────

(define (test-line-joins-the-identity-when-known)
  ;; The UIR carries no positions today, so `line` is #f in every inferred
  ;; origin. The record and projection already support it: when a caller knows
  ;; the line, it is part of the identity and is projected as a `site-line`
  ;; atom. This is the forward path for the front end recording positions.
  (define callee (v "open"))
  (define origin (make-call-origin "f" callee #:file "app.py" #:line 12 #:position 0))
  (check-equal? (prov-origin->string origin) "app.py::f@12:0:open")
  (define entry (uir-effect-entry (sym "io") 'may '() #f #f))
  (define prov (make-provenance))
  (provenance-add-ground!
   prov
   (prov-site "f" 'io "open" '("open") #f 'inferred 'universal origin))
  (define rows (hash 'f (uir-effect-row (list entry))))
  (define graph (provenance-graph rows prov))
  (check-equal? (provenance-origins graph) '("app.py::f@12:0:open"))
  (define path (first (provenance-paths graph "f" 'io)))
  (check-equal? (walk-ground-line (walk-path-ground path)) 12)
  (check-equal? (walk-ground-origin (walk-path-ground path)) "app.py::f@12:0:open")
  (define lines
    (for/list ([a (in-list graph)]
               #:when (and (link? a) (eq? (link-type a) 'site-line)))
      a))
  (check-equal? (length lines) 1))

;; ── drift is localised by origin ────────────────────────────────────

(define (test-origin-diff-localises-drift)
  ;; Same function `f` in both programs; `g` gains an earlier call, which
  ;; shifts the later site's position. The origin diff reports exactly the
  ;; moved site, and `f`'s origins are untouched — drift is diagnosable.
  (define (p extra?)
    (prog (fn "f" (uir-call (v "open") (list)))
          (fn "g" (uir-block
                   (append (if extra? (list (uir-call (v "open") (list))) '())
                           (list (uir-call (dot (v "requests") "get") (list))))))))
  (define (origins prog)
    (define-values (rows prov) (infer-record prog #:source-file "app.py"))
    (provenance-origins (provenance-graph rows prov)))
  (define before (origins (p #f)))
  (define after (origins (p #t)))
  (define added (filter-not (λ (o) (member o before)) after))
  (define removed (filter-not (λ (o) (member o after)) before))
  ;; f's site identity did not move.
  (check-not-false (member "app.py::f@0:open" before))
  (check-not-false (member "app.py::f@0:open" after))
  ;; The diff names the new site and the moved one, not the untouched sites.
  (check-equal? added '("app.py::g@0:open" "app.py::g@1:requests.get"))
  (check-equal? removed '("app.py::g@0:requests.get")))

(define (test-per-file-and-whole-program-origins-agree)
  ;; A site has one identity no matter which inference mode names it: per-file
  ;; with #:source-file "app.py" and whole-program with component "app.py" both
  ;; produce file::owner@position:chain.
  (define p (prog (fn "f" (uir-call (v "open") (list)))))
  (define per-file (origin-of-call p "f" 'io #:source-file "app.py"))
  (define-values (_rows prov) (infer-all-record (list (cons "app.py" p))))
  (define whole-program
    (prov-site-origin (first (grounds-of prov "app.py::f" 'io))))
  (check-equal? per-file whole-program)
  (check-equal? (prov-origin->string per-file) "app.py::f@0:open"))

;; ── the record's origins are exactly the graph's site names ────────

(define (record-origin-ids prov)
  (sort
   (remove-duplicates
    (append
     (for*/list ([(owner by-effect) (in-hash (provenance-grounds prov))]
                 [(effect sites) (in-hash by-effect)]
                 [site (in-list sites)])
       (prov-origin->string (prov-site-origin site)))
     (for*/list ([(caller calls) (in-hash (provenance-calls prov))]
                 [call (in-list calls)])
       (prov-origin->string (prov-call-origin call)))))
   string<?))

(define (test-record-and-graph-origins-agree)
  ;; The fingerprint the corpus tool prints is computed from the record before
  ;; projection; the projected site atoms must name the same origins, so record
  ;; and graph drift cannot disagree.
  (define p
    (prog (fn "f" (uir-call (v "open") (list)))
          (fn "g" (uir-call (v "f") (list)))))
  (define-values (rows prov) (infer-record p #:source-file "app.py"))
  (define record (record-origin-ids prov))
  (check-equal? record '("app.py::f@0:open" "app.py::g@0:f"))
  (check-equal? (provenance-origins (provenance-graph rows prov)) record))

;; ── identity helpers ────────────────────────────────────────────────

(define (test-function-identity-parts)
  (define-values (file name) (function-identity-parts "app.py::Sub.run"))
  (check-equal? file "app.py")
  (check-equal? name "Sub.run")
  (define-values (no-file same) (function-identity-parts "run"))
  (check-false no-file)
  (check-equal? same "run"))

(define (test-origin-string-is-injective-enough)
  ;; Distinct sites with the same chain still differ by position and owner.
  (check-not-equal? (prov-origin->string (prov-origin "app.py" #f "f" 0 '("open")))
                    (prov-origin->string (prov-origin "app.py" #f "f" 1 '("open"))))
  (check-not-equal? (prov-origin->string (prov-origin "app.py" #f "f" 0 '("open")))
                    (prov-origin->string (prov-origin "app.py" #f "g" 0 '("open"))))
  (check-not-equal? (prov-origin->string (prov-origin "a.py" #f "f" 0 '("open")))
                    (prov-origin->string (prov-origin "b.py" #f "f" 0 '("open")))))

(module+ main
  (test-origin-fields-of-a-call-site)
  (test-origin-without-file)
  (test-identical-expressions-are-distinct-sites)
  (test-origin-is-stable-across-runs)
  (test-origin-is-independent-of-effect-and-kind)
  (test-origin-is-independent-of-argument-values)
  (test-receiver-chain-is-structural)
  (test-graph-projects-origin-atoms)
  (test-whole-program-origin-has-file)
  (test-declared-origin-names-the-definition)
  (test-line-joins-the-identity-when-known)
  (test-origin-diff-localises-drift)
  (test-per-file-and-whole-program-origins-agree)
  (test-record-and-graph-origins-agree)
  (test-function-identity-parts)
  (test-origin-string-is-injective-enough)
  (displayln "All provenance origin tests passed."))
