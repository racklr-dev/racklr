#lang racket

(require racklr/tree
         "lexer.rkt"
         "language-policy.rkt")

(provide detect-left-recursion
         gen-parser-helpers
         gen-parser-rules
         gen-parser-provides
         gen-entry
         gen-lexer-entry)

(define (gen-parser-helpers)
  "
(define (ctok tks pos)
  (if (< pos (length tks)) (list-ref tks pos)
      (token 'EOF \"\" (source-pos 0 0 0) (source-pos 0 0 0))))

(define (expect-tok tks pos type)
  (define t (ctok tks pos))
  (if (eq? (token-type t) type) (list (+ pos 1) t) #f))

(define (expect-lit tks pos val)
  (define t (ctok tks pos))
  (if (string=? (token-value t) val) (list (+ pos 1) t) #f))

(define (parse-star tks pos fn)
  (let loop ([p pos] [kids '()])
    (define r (fn tks p))
    (if r (loop (car r) (cons (cadr r) kids)) (list p (reverse kids)))))

(define (parse-plus tks pos fn)
  (define r (fn tks pos))
  (and r (let* ([rest (parse-star tks (car r) fn)])
           (list (car rest) (cons (cadr r) (cadr rest))))))

(define (parse-opt tks pos fn)
  (define r (fn tks pos))
  (if r r (list pos 'none)))

(define (parse-group tks pos fns)
  (let loop ([fs fns])
    (if (null? fs) #f
        (let ([r ((car fs) tks pos)])
          (if r r (loop (cdr fs)))))))

(define (child-range child)
  ;; Extract (start-pos . end-pos) from either a token, a tree node, or a list
  (cond [(null? child) (cons (pos 0 0 0) (pos 0 0 0))]
        [(pair? child)
         ;; List from parse-star/parse-plus: combine first/last
         (cons (child-start (car child)) (child-end (car (reverse child))))]
        [(any-tree? child) (any-tree-range child)]
        [(eq? child 'none) (cons (pos 0 0 0) (pos 0 0 0))]
        [else (cons (token-start child) (token-end child))]))

(define (child-start child)
  (car (child-range child)))

(define (child-end child)
  (cdr (child-range child)))

(define (longest rs)
  ;; Return the parse that consumes the most tokens, ties resolved to the
  ;; first (grammar order). Used for non-left-recursive ordered choice so a
  ;; prefix-overlapping alternative (e.g. namespaceName) wins over a shorter
  ;; prefix (identifier) that ANTLR4 would reject via lookahead.
  (define good (filter (lambda (r) r) rs))
  (and (pair? good)
       (for/fold ([best (car good)]) ([r (cdr good)])
         (if (> (car r) (car best)) r best))))")

(define (gen-parser-rules parser-rules)
  (string-join (map gen-one-parser-rule parser-rules) "\n\n"))

(define (gen-one-parser-rule rule)
  (define name (any-tree-text (first (any-tree-children rule))))
  (define alts-node (second (any-tree-children rule)))
  (define all-alts (any-tree-children alts-node))
  ;; Split alternatives into primary (non-left-recursive) and
  ;; binary (left-recursive, starting with self-ref) groups
  (define primaries '())
  (define binaries '())
  (for ([alt all-alts])
    (define elems (any-tree-children alt))
    (define real-elems (filter (lambda (e) (not (member (any-tree-tag e) '(action label)))) elems))
    (if (and (pair? real-elems)
             (eq? (any-tree-tag (first real-elems)) 'rule-ref)
             (string=? (any-tree-text (first real-elems)) name))
        (set! binaries (cons alt binaries))
        (set! primaries (cons alt primaries))))
  (set! primaries (reverse primaries))
  (set! binaries (reverse binaries))
  (if (null? binaries)
      ;; No left recursion — try every alternative and keep the longest parse
      ;; (ANTLR4 resolves prefix-overlapping alternatives via lookahead, which
      ;; the generated recursive-descent parser lacks; see namespaceName).
      (format "(define (parse-~a tks pos)\n  (longest (list ~a)))"
              name
              (string-join (map (lambda (a) (gen-parser-alt a name)) primaries) "\n              "))
      ;; Left recursion — generate iterative accumulation form
      (gen-leftrec-rule name primaries binaries)))

;; Generate a left-recursion-free iterative parser for a rule with
;; primary alternatives (base cases) and binary alternatives (op rhs).
;; The pattern: parse a primary, then loop trying binary suffixes,
;; accumulating left-associatively.
(define (gen-leftrec-rule name primaries binaries)
  (define primary-code
    (if (null? primaries)
        "#f"
        (string-join (map (lambda (a) (gen-parser-alt a name)) primaries) "\n               ")))
  (define bin-clauses
    (for/list ([alt binaries])
      (define elems (any-tree-children alt))
      (define real-elems (filter (lambda (e) (not (member (any-tree-tag e) '(action label)))) elems))
      ;; Drop the first element (the left-recursive self-ref to this rule)
      (define suffix (cdr real-elems))
      (gen-leftrec-clause suffix name)))
  (string-append
   (format "(define (parse-~a tks pos)\n" name)
   "  (define (prim tks pos)\n"
   (format "    (or ~a\n        #f))\n" primary-code)
   "  (let ([r (prim tks pos)])\n"
   "    (and r\n"
   "         (let bin-loop ([p (car r)] [acc (cadr r)])\n"
   (format "           (or ~a\n"
           (string-join bin-clauses "\n               "))
   "               (list p acc))))))\n"))

;; Generate one let* clause for a binary left-recursive suffix.
;; The suffix is the sequence of elements after the initial self-ref.
;; Any self-ref (rule-ref to name) in the suffix calls (prim tks pos).
(define (gen-leftrec-clause suffix name)
  (define N (length suffix))
  ;; A ternary alternative (`A '?' A ':' A`) has two self-refs in its suffix;
  ;; both operands are full expressions, not primaries, so `item ? item.name :
  ;; x` must recurse rather than call `prim` (which cannot parse member access).
  (define full? (>= (length (filter (lambda (e) (and (eq? (any-tree-tag e) 'rule-ref)
                                                     (string=? (any-tree-text e) name)))
                                    suffix))
                    2))
  (if (zero? N)
      "(list p acc)" ;; degenerate: no suffix (shouldn't happen for valid grammar)
      (let build ([i 0])
        (define e (list-ref suffix i))
        (define pos-ref (if (zero? i) "p" (format "(car r~a)" (- i 1))))
        (define expr (lrec-elem-expr e pos-ref name full?))
        (define body
          (if (= i (- N 1))
              ;; Final element: feed into bin-loop with accumulated node
              (format "(and r~a (bin-loop (car r~a) (node '~a (list acc ~a) #:start (child-start acc) #:end (child-end (cadr r~a)))))"
                      i i name
                      (string-join
                       (for/list ([j (in-range N)])
                         (format "(cadr r~a)" j))
                       " ")
                      (- N 1))
              ;; Intermediate: chain to next element
              (format "(and r~a ~a)" i (build (+ i 1)))))
        (format "(let ([r~a ~a]) ~a)" i expr body))))

;; Like elem-expr but replaces self-refs with (prim tks pos) calls — or, for
;; ternary alternatives, with a full recursive call so member-access operands
;; parse.
(define (lrec-elem-expr e pos-ref self-name full?)
  (define tag (any-tree-tag e))
  (cond
    [(and (eq? tag 'rule-ref) (string=? (any-tree-text e) self-name))
     (format "(~a tks ~a)" (if full? (format "parse-~a" self-name) "prim") pos-ref)]
    [else (elem-expr e pos-ref)]))

;; elem-expr without the surrounding let — just the expression part
(define (elem-expr e pos-ref)
  (define tag (any-tree-tag e))
  (cond
    [(eq? tag 'literal)
     (format "(expect-lit tks ~a ~s)" pos-ref (unescape-g4-literal (any-tree-text e)))]
    [(eq? tag 'token-ref)
     (format "(expect-tok tks ~a '~a)" pos-ref (any-tree-text e))]
    [(eq? tag 'rule-ref)
     (format "(parse-~a tks ~a)" (any-tree-text e) pos-ref)]
    [(eq? tag 'star)
     (define child (first (any-tree-children e)))
     (format "(parse-star tks ~a (lambda (t p) ~a))" pos-ref (gen-parser-single child))]
    [(eq? tag 'plus)
     (define child (first (any-tree-children e)))
     (format "(parse-plus tks ~a (lambda (t p) ~a))" pos-ref (gen-parser-single child))]
    [(eq? tag 'optional)
     (define child (first (any-tree-children e)))
     (format "(parse-opt tks ~a (lambda (t p) ~a))" pos-ref (gen-parser-single child))]
    [(eq? tag 'group)
     (define sub-alts (any-tree-children e))
     (format "(parse-group tks ~a (list ~a))" pos-ref
             (string-join
              (for/list ([sa sub-alts])
                (format "(lambda (t p) ~a)"
                        (gen-parser-seq (any-tree-children sa) "group" "p")))
              " "))]
    [(eq? tag 'action) (format "(list ~a (list))" pos-ref)]
    [(eq? tag 'negated) (format "(list ~a (list))" pos-ref)]
    [(eq? tag 'labeled)
     (define inner-elem (second (any-tree-children e)))
     (elem-expr inner-elem pos-ref)]
    [(eq? tag 'append-labeled)
     (define inner-elem (second (any-tree-children e)))
     (elem-expr inner-elem pos-ref)]
    [else (error "unknown parser elem tag:" tag)]))

(define (gen-parser-alt alt rule-name)
  (define elems (any-tree-children alt))
  ;; Filter out action elements and alternative labels
  (define real-elems (filter (lambda (e) (not (member (any-tree-tag e) '(action label)))) elems))
  (if (null? real-elems)
      (format "(list pos (node '~a (list)))" rule-name)
      (gen-parser-seq real-elems rule-name)))

(define (gen-parser-seq elems rule-name [pos-var "pos"])
  (define N (length elems))
  (define (elem-expr e pos-ref)
    (define tag (any-tree-tag e))
    (cond
      [(eq? tag 'literal)
       (format "(expect-lit tks ~a ~s)" pos-ref (unescape-g4-literal (any-tree-text e)))]
      [(eq? tag 'token-ref)
       (format "(expect-tok tks ~a '~a)" pos-ref (any-tree-text e))]
      [(eq? tag 'rule-ref)
       (format "(parse-~a tks ~a)" (any-tree-text e) pos-ref)]
      [(eq? tag 'star)
       (define child (first (any-tree-children e)))
       (format "(parse-star tks ~a (lambda (t p) ~a))" pos-ref (gen-parser-single child))]
      [(eq? tag 'plus)
       (define child (first (any-tree-children e)))
       (format "(parse-plus tks ~a (lambda (t p) ~a))" pos-ref (gen-parser-single child))]
      [(eq? tag 'optional)
       (define child (first (any-tree-children e)))
       (format "(parse-opt tks ~a (lambda (t p) ~a))" pos-ref (gen-parser-single child))]
      [(eq? tag 'group)
       (define sub-alts (any-tree-children e))
       (format "(parse-group tks ~a (list ~a))" pos-ref
               (string-join
                (for/list ([sa sub-alts])
                  (format "(lambda (t p) ~a)"
                          (gen-parser-seq (any-tree-children sa) "group" "p")))
                " "))]
      [(eq? tag 'action) (format "(list ~a (list))" pos-ref)] ;; skip actions
      [(eq? tag 'negated) (format "(list ~a (list))" pos-ref)] ;; skip negated token sets
      [(eq? tag 'labeled)
       (define inner-elem (second (any-tree-children e)))
       (elem-expr inner-elem pos-ref)]
      [(eq? tag 'append-labeled)
       (define inner-elem (second (any-tree-children e)))
       (elem-expr inner-elem pos-ref)]
      [else (error "unknown parser elem tag:" tag)]))
  (if (zero? N)
      (format "(list ~a (node '~a (list)))" pos-var rule-name)
      (let build ([i 0])
        (define e (list-ref elems i))
        (define tag (any-tree-tag e))
        (define pos-ref (if (zero? i) pos-var (format "(car r~a)" (- i 1))))
        (define body
          (if (= i (- N 1))
              ;; Final element: produce the result list, still checking for failure
              (format "(and r~a (list (car r~a) (node '~a (list ~a) #:start (child-start (cadr r0)) #:end (child-end (cadr r~a)))))"
                      i i rule-name
                      (string-join (for/list ([j (in-range N)])
                                     (format "(cadr r~a)" j)) " ")
                      (- N 1))
              ;; Intermediate: and ri (build (+ i 1))
              (format "(and r~a ~a)" i (build (+ i 1)))))
        (format "(let ([r~a ~a]) ~a)" i (elem-expr e pos-ref) body))))

(define (gen-parser-single elem)
  (define tag (any-tree-tag elem))
  (cond
    [(eq? tag 'literal)  (format "(expect-lit t p ~s)" (unescape-g4-literal (any-tree-text elem)))]
    [(eq? tag 'token-ref) (format "(expect-tok t p '~a)" (any-tree-text elem))]
    [(eq? tag 'rule-ref)  (format "(parse-~a t p)" (any-tree-text elem))]
    [(eq? tag 'group)
     (define sub-alts (any-tree-children elem))
     (format "(parse-group t p (list ~a))"
             (string-join
              (for/list ([sa sub-alts])
                (format "(lambda (t p) ~a)"
                        (gen-parser-seq (any-tree-children sa) "group" "p")))
              " "))]
    [(eq? tag 'optional)
     (define child (first (any-tree-children elem)))
     (format "(parse-opt t p (lambda (t p) ~a))" (gen-parser-single child))]
    [(eq? tag 'star)
     (define child (first (any-tree-children elem)))
     (format "(parse-star t p (lambda (t p) ~a))" (gen-parser-single child))]
    [(eq? tag 'plus)
      (define child (first (any-tree-children elem)))
      (format "(parse-plus t p (lambda (t p) ~a))" (gen-parser-single child))]
    [(eq? tag 'action) "(list p (list))"] ;; skip actions within suffixed elements
    [(eq? tag 'negated) "(list p (list))"] ;; skip negated within suffixed elements
    [(eq? tag 'labeled)
     (define inner-elem (second (any-tree-children elem)))
     (gen-parser-single inner-elem)]
    [(eq? tag 'append-labeled)
     (define inner-elem (second (any-tree-children elem)))
     (gen-parser-single inner-elem)]
    [else (error "unsupported parser single elem:" tag)]))

(define (gen-parser-provides parser-rules)
  (define names
    (for/list ([rule parser-rules])
      (any-tree-text (first (any-tree-children rule)))))
  (format "(provide ~a)"
          (string-join (map (λ (n) (format "parse-~a" n)) names) " ")))

(define (gen-entry parser-rules #:preserve-comments? [preserve-comments? #f]
                   #:policy [policy default-language-policy])
  ;; Prefer the host policy's entry-rule names (e.g. a grammar whose first
  ;; rule is a non-`program` start symbol); fall back to the first rule.
  (define preferred (language-policy-entry-rule-names policy))
  (define first-name
    (let ([preferred-rule
           (findf (lambda (r)
                    (member (any-tree-text (first (any-tree-children r))) preferred))
                  parser-rules)])
      (if preferred-rule
          (any-tree-text (first (any-tree-children preferred-rule)))
          (any-tree-text (first (any-tree-children (first parser-rules)))))))
  (define tks-expr
    (if preserve-comments?
        "(filter (lambda (t) (not (hidden-token? t))) (tokenize in))"
        "(tokenize in)"))
  (format "
(define (parse in)
  (define tks ~a)
  (match-define (list fp res) (parse-~a tks 0))
  res)" tks-expr first-name))

(define (detect-left-recursion parser-rules)
  ;; Check each parser rule for direct left-recursive alternatives
  ;; A left-recursive alternative starts with a rule-ref to the same rule
  ;; Left-recursive alternatives are filtered out during code generation.
  (for ([rule parser-rules])
    (define name (any-tree-text (first (any-tree-children rule))))
    (define alts-node (second (any-tree-children rule)))
    (define alts (any-tree-children alts-node))
    (for ([alt alts])
      (define elems (any-tree-children alt))
      ;; Skip alternative label leaves
      (define first-elem (findf (lambda (e) (not (eq? (any-tree-tag e) 'label))) elems))
      (when (and first-elem
                 (eq? (any-tree-tag first-elem) 'rule-ref)
                 (string=? (any-tree-text first-elem) name))
        (eprintf "~nWARNING: Left-recursive alternative in rule '~a' — eliminating via iteration.~n" name)
        (eprintf "  The generated parser handles this using an accumulator loop.~n~n")))))

(define (gen-lexer-entry)
  ;; For lexer-only grammars: provide a parse that tokenizes and returns tokens as-is
  "
(define (parse in)
  (tokenize in))")
