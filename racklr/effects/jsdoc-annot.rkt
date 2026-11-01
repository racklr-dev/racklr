#lang racket

;; JSDoc @effects annotation extraction (TS/JS/JSX front ends).
;;
;; With comment preservation enabled, the generated lexer keeps comment tokens
;; in the token stream. This module (a) pulls an `@effects { ... }` row out of
;; a comment's text, (b) turns the JS object-literal row into a uir-effect-row
;; reusing the same constraint convention as withEffects(...), and (c) attaches
;; the row to the following top-level declaration in the lowered UIR.
;;
;;   /** @effects { llm: { max: 5 } } */
;;   function f() { ... }
;;
;; attaches row `llm: may, budget 5` to f by writing it into the wrapped
;; uir-fn's effects slot (or wrapping arrows / function expressions in the
;; internal `(call "__withEffects" (list row fn))` shape the emitter renders).

(require racklr/tree
         racklr/uir
         racklr/effects/ts-annot)

(provide
  extract-jsdoc-row
  jsdoc-annotations
  take-annotation
  attach-jsdoc-row
  min-token-offset)

;; ── token offsets ───────────────────────────────────────────────────
;; The generated lexer's `token` struct is (struct token (type value start end)).
;; Reading offsets through struct->vector keeps lower-program from needing the
;; generated accessors threaded through.

(define (token-start-offset t) (source-pos-offset (vector-ref (struct->vector t) 3)))
(define (token-end-offset t) (source-pos-offset (vector-ref (struct->vector t) 4)))

;; Smallest start offset among all token leaves in a CST subtree. The generated
;; parser gives a node's range the start of its first *element*, which is the
;; dummy pos 0 when that element is an absent optional (Export?, etc.), so node
;; ranges are unreliable; real token offsets are the trustworthy signal.
(define (min-token-offset x)
  (let loop ([x x] [best +inf.0])
    (cond
      [(cst-node? x)
       (for/fold ([b best]) ([k (in-list (cst-node-children x))]) (loop k b))]
      [(pair? x)
       (for/fold ([b best]) ([k (in-list x)]) (loop k b))]
      [(or (null? x) (eq? x 'none) (cst-leaf? x)) best]
      [else (min best (token-start-offset x))])))

;; ── comment → annotation scan ───────────────────────────────────────

;; Parse the inside of a row object literal (the text between the outer braces)
;; into a uir-record, reusing the withEffects constraint convention: each key is
;; an opaque effect name; values are nested constraint objects, arrays, strings,
;; numbers, or the bare tokens true/false/null (all an unconstrained entry).
(define (parse-row-object s)
  (define n (string-length s))
  (define (skip i)
    (let loop ([i i])
      (if (and (< i n) (char-whitespace? (string-ref s i))) (loop (add1 i)) i)))
  (define (string-literal i q)
    (let loop ([j (add1 i)])
      (cond
        [(>= j n) (values (uir-string "") (add1 i))]
        [(char=? (string-ref s j) #\\) (loop (+ j 2))]
        [(char=? (string-ref s j) q) (values (uir-string (substring s (add1 i) j)) (add1 j))]
        [else (loop (add1 j))])))
  (define (number-literal i)
    (define start i)
    (let loop ([j i])
      (if (and (< j n) (or (char-numeric? (string-ref s j))
                           (member (string-ref s j) '(#\. #\- #\+ #\e #\E))))
          (loop (add1 j))
          (values (uir-number (substring s start j)) j))))
  (define (bare-literal i)
    (let loop ([j i])
      (if (and (< j n) (or (char-alphabetic? (string-ref s j))
                           (member (string-ref s j) '(#\_ #\$))))
          (loop (add1 j))
          (values (uir-null) j))))
  (define (parse-key i)
    (define i0 (skip i))
    (cond
      [(>= i0 n) (values (uir-string "?") i0)]
      [(or (char=? (string-ref s i0) #\") (char=? (string-ref s i0) #\'))
       (string-literal i0 (string-ref s i0))]
      [else
       (define start i0)
       (let loop ([j i0])
         (if (and (< j n) (or (char-alphabetic? (string-ref s j))
                              (member (string-ref s j) '(#\_ #\$ #\- #\?))))
             (loop (add1 j))
             (values (uir-string (substring s start j)) j)))]))
  (define (parse-object i)
    (define-values (entries i2) (parse-entries (add1 i) #t))
    (values (uir-record entries)
            (if (and (< i2 n) (char=? (string-ref s i2) #\})) (add1 i2) i2)))
  (define (parse-array i)
    (define items '())
    (let loop ([i (skip (add1 i))])
      (cond
        [(>= i n) (values (uir-list (reverse items)) i)]
        [(char=? (string-ref s i) #\]) (values (uir-list (reverse items)) (add1 i))]
        [else
         (define-values (v i2) (parse-value i))
         (set! items (cons v items))
         (define i3 (skip i2))
         (cond
           [(and (< i3 n) (char=? (string-ref s i3) #\,)) (loop (skip (add1 i3)))]
           [else (loop i3)])])))
  (define (parse-value i)
    (define i0 (skip i))
    (cond
      [(>= i0 n) (values (uir-null) i0)]
      [(char=? (string-ref s i0) #\{) (parse-object i0)]
      [(char=? (string-ref s i0) #\[) (parse-array i0)]
      [(or (char=? (string-ref s i0) #\") (char=? (string-ref s i0) #\'))
       (string-literal i0 (string-ref s i0))]
      [(or (char-numeric? (string-ref s i0))
           (and (member (string-ref s i0) '(#\- #\+))
                (< (add1 i0) n) (char-numeric? (string-ref s (add1 i0)))))
       (number-literal i0)]
      [else (bare-literal i0)]))
  (define (parse-entries i stop?)
    (define entries '())
    (let loop ([i (skip i)])
      (cond
        [(or (>= i n) (and stop? (char=? (string-ref s i) #\})))
         (values (reverse entries) i)]
        [else
         (define-values (k i1) (parse-key i))
         (define i2 (skip i1))
         (when (and (< i2 n) (char=? (string-ref s i2) #\:)) (set! i2 (add1 i2)))
         (define-values (v i3) (parse-value (skip i2)))
         (set! entries (cons (cons k v) entries))
         (define i4 (skip i3))
         (cond
           [(and (< i4 n) (char=? (string-ref s i4) #\,)) (loop (skip (add1 i4)))]
           [else (loop i4)])])))
  (uir-record (let-values ([(entries _i) (parse-entries 0 #f)]) entries)))

;; Extract a uir-effect-row from a comment token's text, or #f when the comment
;; carries no @effects tag. Finds "@effects", then the following balanced
;; { ... } object literal, then lowers the literal via the row parser.
(define (extract-jsdoc-row comment-text)
  (define m (regexp-match-positions #px"@effects\\s*\\{" comment-text))
  (and m
       (let* ([brace (sub1 (cdar m))]
              [close (matching-brace comment-text brace)])
         (and close
              (record->effect-row
               (parse-row-object (substring comment-text (add1 brace) close)))))))

;; Index of the '}' matching the '{' at `open`, or #f. Skips string literals so
;; a brace inside a quoted value does not confuse the depth count.
(define (matching-brace s open)
  (define n (string-length s))
  (let loop ([i (add1 open)] [depth 1])
    (cond
      [(>= i n) #f]
      [(char=? (string-ref s i) #\{) (loop (add1 i) (add1 depth))]
      [(char=? (string-ref s i) #\}) (if (= depth 1) i (loop (add1 i) (sub1 depth)))]
      [(or (char=? (string-ref s i) #\") (char=? (string-ref s i) #\'))
       (let ([q (string-ref s i)])
         (let jloop ([j (add1 i)])
           (cond
             [(>= j n) (loop (add1 i) depth)]
             [(char=? (string-ref s j) #\\) (jloop (+ j 2))]
             [(char=? (string-ref s j) q) (loop (add1 j) depth)]
             [else (jloop (add1 j))])))]
      [else (loop (add1 i) depth)])))

;; Walk the token stream (with comments preserved) and return an ordered list
;; of (cons comment-end-offset uir-effect-row) for every @effects comment.
(define (jsdoc-annotations tokens tk-type tk-value)
  (filter-map
   (λ (t)
     (and (struct? t)
          (member (tk-type t) '(SingleLineComment MultiLineComment))
          (let ([row (extract-jsdoc-row (tk-value t))])
            (and row (cons (token-end-offset t) row)))))
   tokens))

;; ── matching ────────────────────────────────────────────────────────

;; annotations: ordered (cons end-offset row). Given a declaration's start
;; offset, consume every annotation whose comment ends at or before it and
;; return (values closest-row remaining-annotations).
(define (take-annotation annotations start)
  (let loop ([as annotations] [best #f])
    (cond
      [(null? as) (values best '())]
      [(<= (car (car as)) start) (loop (cdr as) (cdr (car as)))]
      [else (values best as)])))

;; ── attachment ──────────────────────────────────────────────────────

(define (arrow-or-function-expr? v)
  (and (uir-call? v)
       (uir-symbol? (uir-call-callee v))
       (member (uir-symbol-name (uir-call-callee v)) '("=>" "function"))))

(define (async-or-gen-wrapper? v)
  (and (uir-call? v)
       (uir-symbol? (uir-call-callee v))
       (member (uir-symbol-name (uir-call-callee v)) '("async-fn" "gen-fn"))
       (= (length (uir-call-args v)) 1)
       (uir-fn? (first (uir-call-args v)))))

;; Attach a row to a function value: write it into a uir-fn's effects slot, or
;; wrap an arrow / function expression in the internal __withEffects call.
;; A value that is already the internal `__withEffects(row, fn)` guard (which
;; is how the JS/TS emitter renders a row) keeps its shape and takes the
;; documented @effects row, so an emitted annotation re-absorbs to the same
;; native form.
(define (attach-jsdoc-value v row)
  (cond
    [(uir-fn? v) (struct-copy uir-fn v [effects row])]
    [(and (uir-call? v)
          (let ([c (uir-call-callee v)])
            (and (or (uir-symbol? c) (uir-var? c))
                 (string=? (uir-symbol-name
                            (if (uir-var? c) (uir-var-name c) c))
                           "__withEffects")))
          (= (length (uir-call-args v)) 2))
     (struct-copy uir-call v
                  [args (list row (second (uir-call-args v)))])]
    [(async-or-gen-wrapper? v)
     (struct-copy uir-call v
                  [args (list (struct-copy uir-fn (first (uir-call-args v)) [effects row]))])]
    [(arrow-or-function-expr? v)
     (uir-call (uir-symbol "__withEffects") (list row v))]
    [else v]))

;; Attach a row to a top-level statement, descending through export/var/let/const
;; wrappers to reach the declaration.
(define (attach-jsdoc-row stmt row)
  (cond
    [(uir-set!? stmt)
     (struct-copy uir-set! stmt [value (attach-jsdoc-value (uir-set!-value stmt) row)])]
    [(and (uir-call? stmt)
          (uir-symbol? (uir-call-callee stmt))
          (member (uir-symbol-name (uir-call-callee stmt)) '("export" "var" "let" "const")))
     (struct-copy uir-call stmt
                  [args (map (λ (a) (attach-jsdoc-row a row)) (uir-call-args stmt))])]
    [(uir-fn? stmt) (struct-copy uir-fn stmt [effects row])]
    [(arrow-or-function-expr? stmt)
     (uir-call (uir-symbol "__withEffects") (list row stmt))]
    [else stmt]))
