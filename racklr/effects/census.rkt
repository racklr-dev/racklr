#lang racket

;; racklr/effects/census.rkt
;;
;; Real-context unknown census.
;;
;; Runs the whole-program effect fixpoint — the one that resolves cross-file
;; edges, imports, receiver-type dataflow, and MRO — and turns every call site
;; that still classifies as `unknown` into an aimed record: the component and
;; enclosing function, the receiver shape, the receiver name and binding kind,
;; the exact callee site string, and the dataflow lever that would resolve it.
;;
;; This is deliberately NOT the empty-context census. Classifying a callee with
;; no known names (`(classify-call callee (hash))`, the old diagnostic) reports
;; intra-program edges and imported handles as unknown, so its counts are an
;; overcount of the real surface and cannot aim work. Only sites the real
;; context cannot resolve are counted here.
;;
;; Usage from a corpus tool:
;;   (define c (census-unknown-sites comp-programs #:import-env env))
;;   (census-site-count c)          ;; unknown call sites (real context)
;;   (census-lever-counts c)        ;; (lever . count) sorted, aiming order
;;
;; The corpus driver over this module is tools/unknown-census.rkt.

(require racklr/uir
         racklr/effects/contract
         racklr/effects/infer)

(provide
 ;; one call site the real context left unknown
 (struct-out census-site)
 ;; the census result: sites + coverage counters
 (struct-out census)
 census-unknown-sites
 ;; lever attribution
 census-lever-of
 census-levers
 census-lever-description
 ;; aggregation (sorted, deterministic)
 census-shape-counts
 census-lever-counts
 census-shape-op-counts
 census-binding-kind-counts
 ;; deterministic serialization
 census-site<?
 census->tsv)

;; ── records ──────────────────────────────────────────────────────────

;; `shape` and `binding-kind` are receiver-shape's bucket and the enclosing
;; function's binding kind for the receiver/callee (classify-all.rkt /
;; whole-program.rkt). `lever` is the receiver-type-dataflow mechanism that
;; would resolve the site. `site` is the callee site string (e.g.
;; "self.connection.cursor"). `higher-order?` marks a bare callee that names
;; one of the enclosing function's parameters (a callback, resolved by row
;; holes); a function-valued local invoked bare is binding-kind 'fn and the
;; function-value-typing lever instead.
(struct census-site (component function shape op receiver binding-kind
                     lever site higher-order?)
  #:transparent)

;; `unknown-functions` / `unknown-files` are the coverage counters over the
;; inferred rows (a function counts once if any of its entries is `unknown`,
;; however many unknown call sites it contains).
(struct census (sites site-count unknown-functions unknown-files
                function-count file-count)
  #:transparent)

;; ── lever attribution ────────────────────────────────────────────────

;; The receiver-type-dataflow lever each unknown site needs. Shapes come from
;; classify-all.rkt's receiver-shape; `binding-kind` refines `local` receivers
;; and bare calls, both recorded by the whole-program walk:
;;   bare-call + 'param   a callback parameter (row-hole lever)
;;   bare-call + 'fn      a local function value (function-value-typing lever)
;;   bare-call + else     a free/global/imported name (lexicon lever)
(define (census-lever-of shape binding-kind)
  (case shape
    [(bare-call)
     (case binding-kind
       [(param) 'higher-order-row-hole]
       [(fn) 'function-value-typing]
       [(call-result) 'return-type-projection]
       [(loop-var comp-var) 'element-type-dataflow]
       [(attr alias) 'attribute-alias-typing]
       [(literal) 'literal-constructor-typing]
       [(exception) 'exception-type-dataflow]
       [else 'bare-call-lexicon])]
    [(param) 'parameter-dataflow]
    [(call-result) 'return-type-projection]
    [(self-attr) 'self-attr-typing]
    [(self-method) 'mro-edge-resolution]
    [(chained) 'multi-level-chain]
    [(symbol) 'import-handle-lexicon]
    [(local)
     (case binding-kind
       [(call-result) 'return-type-projection]
       [(loop-var comp-var) 'element-type-dataflow]
       [(attr alias) 'attribute-alias-typing]
       [(exception) 'exception-type-dataflow]
       [(literal) 'literal-constructor-typing]
       [(unbound) 'unbound-binding-resolution]
       [(fn) 'function-value-typing]
       [else 'local-constructor-tracking])]
    [else 'unclassified]))

;; Declared taxonomy order for documentation/reporting. Observed levers are
;; reported by count (census-lever-counts); this list keeps the full taxonomy
;; visible even when a lever currently has zero sites.
(define census-levers
  '(return-type-projection parameter-dataflow self-attr-typing
    element-type-dataflow attribute-alias-typing mro-edge-resolution
    multi-level-chain higher-order-row-hole bare-call-lexicon
    import-handle-lexicon literal-constructor-typing
    unbound-binding-resolution exception-type-dataflow
    function-value-typing local-constructor-tracking unclassified))

(define census-lever-descriptions
  (hash 'return-type-projection
        "project the return class of f(...) so f(...).m() resolves"
        'parameter-dataflow
        "propagate parameter types from the concrete call chain"
        'self-attr-typing
        "type self.attr from its constructor/annotation and MRO"
        'element-type-dataflow
        "project iterable element types into loop/comprehension variables"
        'attribute-alias-typing
        "carry a receiver type through an attribute copy or alias"
        'mro-edge-resolution
        "resolve self.m()/cls.m() through the class MRO to an edge"
        'multi-level-chain
        "walk a multi-level a.b.m() receiver chain to a class"
        'higher-order-row-hole
        "type a callback parameter as a row hole at its call sites"
        'bare-call-lexicon
        "add the missing bare stdlib/framework name to the DATA lexicon"
        'import-handle-lexicon
        "tag the imported module/object handle's effect domain"
        'literal-constructor-typing
        "give container/stdlib literals a receiver type"
        'unbound-binding-resolution
        "bind a module-global/import/closure name before typing the receiver"
        'exception-type-dataflow
        "type the caught exception binding"
        'function-value-typing
        "type a function value held in a local before calling it"
        'local-constructor-tracking
        "track the local's concrete constructor (KnownClass(...))"
        'unclassified
        "shape the census does not yet attribute to a lever"))

(define (census-lever-description lever)
  (hash-ref census-lever-descriptions lever "no description"))

;; ── the census ───────────────────────────────────────────────────────

;; `comp-programs` is the same (component . program) list infer-effects-all
;; takes; `#:import-env` is the real import environment, so imported handles
;; and cross-file edges resolve instead of counting as unknown.
;;
;; `#:component-name` renames a component before the records are sorted and
;; returned. A corpus tool passes a checkout-path -> dotted-module-path mapping
;; so the census is sorted by, and reports, module names instead of absolute
;; paths (the sort happens after renaming).
(define (census-unknown-sites comp-programs
                              #:import-env [import-env #f]
                              #:component-name [component-name values])
  (define tally (box '()))
  (define rows
    (infer-effects-all comp-programs
                       #:import-env import-env
                       #:unknown-tally tally))
  (define raw-sites (reverse (unbox tally)))
  (define sites
    (sort (for/list ([r (in-list raw-sites)])
            (match-define (list comp fn-name shape op rx kind site higher?)
              r)
            (census-site (component-name comp) fn-name shape op rx kind
                         (census-lever-of shape kind)
                         site
                         (and higher? #t)))
          census-site<?))
  (define unknown-names
    (for/hash ([(k row) (in-hash rows)]
               #:when (memq 'unknown
                            (map effect-name-symbol
                                 (uir-effect-row-entries row))))
      (values k #t)))
  (define unknown-files
    (remove-duplicates
     (for/list ([k (in-hash-keys unknown-names)]) (car k))))
  (define files (remove-duplicates (for/list ([k (in-hash-keys rows)]) (car k))))
  (census sites
          (length sites)
          (hash-count unknown-names)
          (length unknown-files)
          (hash-count rows)
          (length files)))

;; ── deterministic aggregation ────────────────────────────────────────

;; (listof (cons key count)), highest count first, ties by printed key, so two
;; runs over the same corpus produce byte-identical reports.
(define (counted-sorted h)
  (sort (hash->list h)
        (λ (a b)
          (cond
            [(not (= (cdr a) (cdr b))) (> (cdr a) (cdr b))]
            [else (string<? (format "~a" (car a)) (format "~a" (car b)))]))))

(define (count-by c key-fn)
  (for/fold ([h (hash)]) ([s (in-list (census-sites c))])
    (hash-update h (key-fn s) add1 0)))

(define (census-shape-counts c)
  (counted-sorted (count-by c census-site-shape)))

(define (census-lever-counts c)
  (counted-sorted (count-by c census-site-lever)))

(define (census-shape-op-counts c)
  (counted-sorted (count-by c (λ (s) (cons (census-site-shape s)
                                           (or (census-site-op s) '?))))))

;; Binding kinds of the `local` receivers only: the refinement of that shape.
(define (census-binding-kind-counts c)
  (counted-sorted
   (for/fold ([h (hash)]) ([s (in-list (census-sites c))]
                           #:when (eq? (census-site-shape s) 'local))
     (hash-update h (or (census-site-binding-kind s) '?unknown) add1 0))))

;; ── deterministic serialization ──────────────────────────────────────

;; (component, enclosing function, exact site, shape, op): stable across runs
;; and independent of hash iteration order.
(define (census-site<? a b)
  (define (key s)
    (format "~a\t~a\t~a\t~a\t~a"
            (census-site-component s)
            (census-site-function s)
            (or (census-site-site s) "")
            (census-site-shape s)
            (or (census-site-op s) "")))
  (string<? (key a) (key b)))

(define (tsv-field v)
  (define s (if v (format "~a" v) ""))
  (list->string
   (for/list ([ch (in-string s)])
     (if (or (char=? ch #\tab) (char=? ch #\newline) (char=? ch #\return))
         #\space
         ch))))

;; One header line, then one tab-separated line per site. Machine-readable and
;; diff-friendly: the header is fixed and rows are in census-site<? order.
(define (census->tsv c [out (current-output-port)])
  (displayln
   "component\tfunction\tshape\top\treceiver\tbinding-kind\tlever\tsite\thigher-order?"
   out)
  (for ([s (in-list (census-sites c))])
    (displayln
     (string-join
      (map tsv-field
           (list (census-site-component s)
                 (census-site-function s)
                 (census-site-shape s)
                 (census-site-op s)
                 (census-site-receiver s)
                 (census-site-binding-kind s)
                 (census-site-lever s)
                 (census-site-site s)
                 (and (census-site-higher-order? s) "yes")))
      "\t")
     out)))
