#lang racket

;; racklr/hypergraph/kernel.rkt
;;
;; The dependency-free typed-hypergraph kernel: the AtomSpace idea (typed
;; nodes, typed hyperlinks, a lattice over link slots, and graph pattern
;; matching) plus the generic value -> atombase materializer.
;;
;; This layer knows nothing about effects, types, capabilities, or UIR. It
;; defines only the atoms, the per-slot algebra, the pattern matcher, and the
;; generic projection of any Racket struct tree / S-expression into a flat
;; atom set. The six-graph vocabulary and the single graph instance live in
;; sibling modules (schema.rkt, instance.rkt, project.rkt); the facade is
;; racklr/hypergraph.rkt.

(provide
  ;; atoms
  node? node node-type node-name
  link? link link-type link-out
  var? var var-name
  ;; lattices
  lattice? lattice lattice-join lattice-meet lattice-leq
  ;; generic per-slot algebra over link out-sets
  slots-join slots-meet slots-leq
  ;; pattern matching / query
  hg-match? hg-query
  ;; generic AST / S-expression materialization
  materialize
  ;; flat atomspace closure
  flatten-atoms)

;; ── atoms ───────────────────────────────────────────────────────────

;; A typed, named vertex (AtomSpace Node).
(struct node (type name) #:transparent)

;; A typed hyperedge: `out` is the ordered list of connected atoms
;; (AtomSpace Link; arity is unconstrained, which is what makes it a
;; *hyper*edge rather than an edge).
(struct link (type out) #:transparent)

;; A pattern variable for hg-match?/hg-query.
(struct var (name) #:transparent)

;; ── lattices ────────────────────────────────────────────────────────

;; A bounded lattice over a value domain: join (least upper bound),
;; meet (greatest lower bound), and the partial order ≤. `leq` is
;; (≤ sub super): sub is at most super in the order.
(struct lattice (join meet leq) #:transparent)

;; ── generic per-slot algebra ────────────────────────────────────────

;; Join/meet the non-key slots of two equal-type links pointwise, using the
;; lattice at each slot position. Returns nodes (slots-join/meet) or a
;; boolean (slots-leq).
(define (slots-join lattices outs-a outs-b)
  (for/list ([la (in-list lattices)]
             [na (in-list outs-a)]
             [nb (in-list outs-b)])
    (node (node-type na) ((lattice-join la) (node-name na) (node-name nb)))))

(define (slots-meet lattices outs-a outs-b)
  (for/list ([la (in-list lattices)]
             [na (in-list outs-a)]
             [nb (in-list outs-b)])
    (node (node-type na) ((lattice-meet la) (node-name na) (node-name nb)))))

(define (slots-leq lattices outs-a outs-b)
  (andmap (λ (la na nb) ((lattice-leq la) (node-name na) (node-name nb)))
          lattices outs-a outs-b))

;; ── pattern matching / query ────────────────────────────────────────

;; Match a pattern (may contain `var` nodes) against an atom. Returns a
;; binding hash on success, #f on failure.
(define (hg-match? pat atom [env (hash)])
  (cond
    [(var? pat)
     (if (hash-has-key? env (var-name pat))
         (hg-match? (hash-ref env (var-name pat)) atom env)
         (hash-set env (var-name pat) atom))]
    [(node? pat)
     (and (node? atom)
          (eq? (node-type pat) (node-type atom))
          (let ([pn (node-name pat)]
                [an (node-name atom)])
            (if (var? pn)
                (if (hash-has-key? env (var-name pn))
                    (and (equal? (hash-ref env (var-name pn)) an) env)
                    (hash-set env (var-name pn) an))
                (and (equal? pn an) env))))]
    [(link? pat)
     (and (link? atom)
          (eq? (link-type pat) (link-type atom))
          (= (length (link-out pat)) (length (link-out atom)))
          (for/fold ([e env])
                    ([p (in-list (link-out pat))]
                     [a (in-list (link-out atom))])
            (define r (hg-match? p a e))
            #:break (not r)
            r))]
    [(and (list? pat) (list? atom))
     (and (= (length pat) (length atom))
          (for/fold ([e env])
                    ([p (in-list pat)]
                     [a (in-list atom)])
            (define r (hg-match? p a e))
            #:break (not r)
            r))]
    [else (and (equal? pat atom) env)]))

;; All atoms in `atoms` matching `pat`, as binding hashes.
(define (hg-query pat atoms)
  (filter values (map (λ (a) (hg-match? pat a)) atoms)))

;; ── generic AST / S-expression materialization ──────────────────────
;;
;; Turn any value — a transparent Racket struct tree (a UIR AST), a nested
;; list (an S-expression, e.g. `uir->sexp` output), or a scalar — into a
;; queryable hypergraph. Each struct becomes a vertex typed by its tag with a
;; fresh identity; each field becomes a `slot` link (vertex, slot index,
;; child). Lists become `list` vertices; scalars become typed leaf nodes
;; (`sym`, `str`, `num`, `bool`, `nil`). The result is a flat atom set that
;; hg-query / hg-match? / the Effervesce index can search directly.

;; Struct type name as a symbol, with the `struct:` prefix stripped.
(define (struct-tag v)
  (define t (vector-ref (struct->vector v) 0))
  (cond
    [(symbol? t)
     (define s (symbol->string t))
     (string->symbol
      (if (and (> (string-length s) 7)
               (string=? (substring s 0 7) "struct:"))
          (substring s 7)
          s))]
    [else 'struct]))

;; A scalar value as a typed leaf node.
(define (leaf-node v)
  (cond
    [(symbol? v) (node 'sym v)]
    [(string? v) (node 'str v)]
    [(number? v) (node 'num v)]
    [(boolean? v) (node 'bool v)]
    [(null? v) (node 'nil 'nil)]
    [else (node 'atom v)]))

(define (materialize v)
  (define counter (box 0))
  (define atoms '())
  (define (next!)
    (begin0 (unbox counter)
      (set-box! counter (add1 (unbox counter)))))
  (define (ref x)
    (cond
      [(struct? x)
       (define vertex (node (struct-tag x) (next!)))
       (set! atoms (cons vertex atoms))
       (for ([i (in-naturals)]
             [f (in-list (cdr (vector->list (struct->vector x))))])
         (set! atoms (cons (link 'slot (list vertex (node 'index i) (ref f))) atoms)))
       vertex]
      [(pair? x)
       (define vertex (node 'list (next!)))
       (set! atoms (cons vertex atoms))
       (for ([i (in-naturals)] [e (in-list x)])
         (set! atoms (cons (link 'slot (list vertex (node 'index i) (ref e))) atoms)))
       vertex]
      [else
       (define leaf (leaf-node x))
       (set! atoms (cons leaf atoms))
       leaf]))
  (ref v)
  (reverse atoms))

;; ── flat atomspace closure ──────────────────────────────────────────
;;
;; An AtomSpace is a FLAT set: every node referenced by a link is itself an
;; atom. Our link constructors embed their child nodes, so a set of top-level
;; links under-represents the space (a receiver type node nested inside an
;; effect-op hyperedge is not queryable on its own). Expand a set of atoms into
;; its full closure over link-out — every reachable node plus the links — so the
;; projection can be indexed, deduplicated and serialized exactly as an
;; AtomSpace holds it. Dedup is by `equal?` (transparent structs), so the same
;; ConceptNode appearing in many links collapses to one atom.
(define (flatten-atoms atoms)
  (define seen (make-hash))
  (define out '())
  (let loop ([as atoms])
    (for ([a (in-list as)])
      (unless (hash-ref seen a #f)
        (hash-set! seen a #t)
        (set! out (cons a out))
        (when (link? a) (loop (link-out a))))))
  (reverse out))

(module+ main
  (displayln "racklr/hypergraph/kernel — typed-hypergraph kernel loaded."))
