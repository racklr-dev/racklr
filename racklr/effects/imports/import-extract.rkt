#lang racket

;; racklr/effects/imports/import-extract.rkt
;;
;; Import extraction: a token-level state machine that reads `import` /
;; `from ... import` statements from a flat token list WITHOUT executing any
;; module body. Fixes the known lossy lowering (the CST→UIR lowering flattens
;; every NAME into one dotted source and drops names/aliases) by reading the
;; raw token stream instead. Also the shared dotted-name / path helpers.

(provide
  import-record? import-record-form import-record-source import-record-bindings
  import-record
  extract-imports
  first-component last-component
  parent-path
  relative-dot-count)

;; `tokens` is a flat list of (cons type-symbol value-string), e.g.
;; (list (cons 'FROM "from") (cons 'NAME "django") (cons 'DOT ".") ...).
;; The scanner walks for IMPORT/FROM at statement start (preceded by
;; NEWLINE/INDENT/DEDENT/COLON/SEMI_COLON or the first token) and reads to the
;; statement end.

(struct import-record (form source bindings) #:transparent)
;; form: 'import | 'from
;; source: module path string ("a.b", or leading "." for relative)
;; bindings: (listof (cons local-name target))
;;   'import: target is the module path the local name binds to
;;   'from:   target is "<module>.<member>" (local "*" for star imports)

(define (first-component mod)
  (let loop ([i 0])
    (cond [(>= i (string-length mod)) mod]
          [(char=? (string-ref mod i) #\.) (substring mod 0 i)]
          [else (loop (add1 i))])))

(define (last-component s)
  (let loop ([i (sub1 (string-length s))])
    (cond [(< i 0) s]
          [(char=? (string-ref s i) #\.) (substring s (add1 i))]
          [else (loop (sub1 i))])))

(define (parent-path p)
  (let loop ([i (sub1 (string-length p))])
    (cond [(< i 0) #f]
          [(char=? (string-ref p i) #\.) (substring p 0 i)]
          [else (loop (sub1 i))])))

(define (relative-dot-count src)
  (let loop ([i 0])
    (cond [(>= i (string-length src)) i]
          [(char=? (string-ref src i) #\.) (loop (add1 i))]
          [else i])))

(define (extract-imports tokens)
  (define n (length tokens))
  (define tv (list->vector tokens))
  (define (typ i) (and (< i n) (car (vector-ref tv i))))
  (define (val i) (and (< i n) (cdr (vector-ref tv i))))
  (define (stmt-start? i)
    (or (= i 0)
        (memq (typ (sub1 i)) '(NEWLINE INDENT DEDENT COLON SEMI_COLON))))

  ;; dotted name starting at i → (values name-string next-index) or (values #f i)
  (define (read-dotted i)
    (cond
      [(and (< i n) (eq? (typ i) 'NAME))
       (define parts '())
       (let loop ([j i])
         (set! parts (append parts (list (val j))))
         (cond
           [(and (< (+ j 1) n) (eq? (typ (+ j 1)) 'DOT)
                 (< (+ j 2) n) (eq? (typ (+ j 2)) 'NAME))
            (loop (+ j 2))]
           [else (values (string-join parts ".") (add1 j))]))]
      [else (values #f i)]))

  ;; `import a.b as c, d.e`
  (define (parse-import-name i)
    (define bindings '())
    (let loop ([j (add1 i)])
      (define-values (mod k) (read-dotted j))
      (define alias #f)
      (define l k)
      (when (and (< l n) (eq? (typ l) 'AS))
        (set! alias (val (add1 l)))
        (set! l (+ l 2)))
      ;; `import a.b as c` binds the local `c` to the full module `a.b`;
      ;; `import a.b` (no alias) binds the top package `a` (existing behavior).
      (define local (or alias (first-component mod)))
      (define target (if alias mod (first-component mod)))
      (set! bindings (append bindings (list (cons local target))))
      (cond
        [(and (< l n) (eq? (typ l) 'COMMA)) (loop (add1 l))]
        [else (values (import-record 'import mod bindings) l)])))

  ;; `from [dots] [mod] import names`  (or `from . import x`)
  (define (parse-import-from i)
    (define dots 0)
    (let scan-dots ([j (add1 i)])
      (cond
        [(and (< j n) (eq? (typ j) 'DOT)) (set! dots (+ dots 1)) (scan-dots (add1 j))]
        [(and (< j n) (eq? (typ j) 'ELLIPSIS)) (set! dots (+ dots 3)) (scan-dots (add1 j))]
        [else
         (define-values (mod k) (read-dotted j))
         (define src (string-append (make-string dots #\.) (or mod "")))
         (cond
           [(and (< k n) (eq? (typ k) 'IMPORT))
            (define bindings '())
            (define m (add1 k))
            (cond
              [(and (< m n) (eq? (typ m) 'STAR))
               (values (import-record 'from src (list (cons "*" src))) (add1 m))]
              [else
               (let loop-names ([p m])
                 (cond
                   [(and (< p n) (memq (typ p) '(OPEN_PAREN CLOSE_PAREN)))
                    (loop-names (add1 p))]
                   [(and (< p n) (eq? (typ p) 'NAME))
                    (define nm (val p))
                    (define q (add1 p))
                    (define alias #f)
                    (when (and (< q n) (eq? (typ q) 'AS))
                      (set! alias (val (add1 q)))
                      (set! q (+ q 2)))
                    (define local (or alias nm))
                    (define target
                      (cond
                        [(string=? src "") nm]
                        [(char=? (string-ref src (sub1 (string-length src))) #\.)
                         (string-append src nm)]
                        [else (string-append src "." nm)]))
                    (set! bindings (append bindings (list (cons local target))))
                    (cond
                      [(and (< q n) (eq? (typ q) 'COMMA)) (loop-names (add1 q))]
                      [else (values (import-record 'from src bindings) q)])]
                   [else (values (import-record 'from src bindings) p)]))])]
           [else (values #f j)])])))

  (define out '())
  (let loop ([i 0])
    (when (< i n)
      (cond
        [(and (eq? (typ i) 'IMPORT) (stmt-start? i))
         (define-values (rec j) (parse-import-name i))
         (set! out (append out (list rec)))
         (loop j)]
        [(and (eq? (typ i) 'FROM) (stmt-start? i))
         (define-values (rec j) (parse-import-from i))
         (when rec (set! out (append out (list rec))))
         (loop j)]
        [else (loop (add1 i))])))
  out)
