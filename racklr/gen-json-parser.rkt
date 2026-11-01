#lang racket
(require racklr/tree)
(provide token token? token-type token-value token-start token-end tokenize parse)
(struct token (type value start end) #:transparent)

(define (mlit s p in)
  (define sl (string-length s))
  (if (and (<= (+ p sl) (string-length in))
           (string=? (substring in p (+ p sl)) s))
      (list (+ p sl) s)
      #f))

(define (mrange lo hi p in)
  (if (>= p (string-length in)) #f
      (let ([c (string-ref in p)])
        (if (char<=? lo c hi) (list (+ p 1) (string c)) #f))))

(define (mcclass pat p in)
  (if (>= p (string-length in)) #f
      (let ([c (string-ref in p)])
        (if (cc-match c pat) (list (+ p 1) (string c)) #f))))

(define (mstar f p in)
  (let loop ([pp p] [a ""])
    (define r (f pp in))
    (if r (loop (car r) (string-append a (cadr r))) (list pp a))))

(define (mplus f p in)
  (define r (f p in))
  (and r (let ([rest (mstar f (car r) in)])
           (list (car rest) (string-append (cadr r) (cadr rest))))))

(define (mopt f p in)
  (define r (f p in))
  (or r (list p "")))

(define (mnot f p in)
  (if (>= p (string-length in)) #f
      (let ([r (f p in)])
        (if r #f (list (+ p 1) (string (string-ref in p)))))))

(define (malt fs p in)
  (let loop ([xs fs])
    (and (pair? xs) (or ((car xs) p in) (loop (cdr xs))))))

(define (mseq fs p in)
  (let loop ([xs fs] [pp p] [a ""])
    (if (null? xs) (list pp a)
        (let ([r ((car xs) pp in)])
          (and r (loop (cdr xs) (car r) (string-append a (cadr r))))))))

(define (cc-match c pat)
  (define pl (string-length pat))
  (let loop ([i 1])
    (cond [(>= i (- pl 1)) #f]
          [(and (< (+ i 2) pl) (char=? (string-ref pat (+ i 1)) #\-)
                (char<=? (string-ref pat i) c (string-ref pat (+ i 2)))) #t]
          [(char=? (string-ref pat i) c) #t]
          [(and (< (+ i 2) pl) (char=? (string-ref pat (+ i 1)) #\-)) (loop (+ i 3))]
          [else (loop (+ i 1))])))
(define (STRING-match p in) (mseq (list (lambda (p i) (mlit "\"" p i)) (lambda (p i) (mstar (lambda (p i) (malt (list (lambda (p in) (ESC-match p in)) (lambda (p in) (SAFECODEPOINT-match p in))) p in)) p i)) (lambda (p i) (mlit "\"" p i))) p in))
(define (NUMBER-match p in) (mseq (list (lambda (p i) (mopt (lambda (p i) (mlit "-" p i)) p i)) INT-match (lambda (p i) (mopt (lambda (p i) (mseq (list (lambda (p i) (mlit "." p i)) (lambda (p i) (mplus (lambda (p i) (mcclass "[0-9]" p i)) p i))) p in)) p i)) (lambda (p i) (mopt EXP-match p i))) p in))
(define (WS-match p in) ((lambda (p i) (mplus (lambda (p i) (mcclass "[ \t\n\r]" p i)) p i)) p in))
(define (ESC-match p in) (mseq (list (lambda (p i) (mlit "\\\\" p i)) (lambda (p i) (malt (list (lambda (p in) ((lambda (p i) (mcclass "[\"\\\\/bfnrt]" p i)) p in)) (lambda (p in) (UNICODE-match p in))) p in))) p in))
(define (UNICODE-match p in) (mseq (list (lambda (p i) (mlit "u" p i)) HEX-match HEX-match HEX-match HEX-match) p in))
(define (HEX-match p in) ((lambda (p i) (mcclass "[0-9a-fA-F]" p i)) p in))
;; Hand-corrected after generation (stopgap; follow-up #96 regenerates this
;; file). This checked-in parser predates the current racklr/gend-parser
;; char-class unescaping: the grammar's SAFECODEPOINT class uses the escaped
;; range \u0000-\u001F, which the old generator emitted raw, so 'u', 'A'-'Z',
;; and '0'-'9' were rejected inside JSON strings. The generator at HEAD decodes
;; \uXXXX escapes in classes and literals; JSON's safe codepoints here are
;; anything except '"', '\\', and U+0000 through U+001F.
(define (SAFECODEPOINT-match p in)
  (if (>= p (string-length in))
      #f
      (let ([c (string-ref in p)])
        (if (and (not (char=? c #\"))
                 (not (char=? c #\\))
                 (> (char->integer c) #x1F))
            (list (+ p 1) (string c))
            #f))))
(define (INT-match p in) (malt (list (lambda (p in) ((lambda (p i) (mlit "0" p i)) p in)) (lambda (p in) (mseq (list (lambda (p i) (mcclass "[1-9]" p i)) (lambda (p i) (mstar (lambda (p i) (mcclass "[0-9]" p i)) p i))) p in))) p in))
(define (EXP-match p in) (mseq (list (lambda (p i) (mcclass "[Ee]" p i)) (lambda (p i) (mopt (lambda (p i) (mcclass "[+-]" p i)) p i)) (lambda (p i) (mplus (lambda (p i) (mcclass "[0-9]" p i)) p i))) p in))
(define (null-match p in) ((lambda (p i) (mlit "null" p i)) p in))
(define (false-match p in) ((lambda (p i) (mlit "false" p i)) p in))
(define (true-match p in) ((lambda (p i) (mlit "true" p i)) p in))
(define (_x5d_-match p in) ((lambda (p i) (mlit "]" p i)) p in))
(define (_x5b_-match p in) ((lambda (p i) (mlit "[" p i)) p in))
(define (_x2c_-match p in) ((lambda (p i) (mlit "," p i)) p in))
(define (_x3a_-match p in) ((lambda (p i) (mlit ":" p i)) p in))
(define (_x7d_-match p in) ((lambda (p i) (mlit "}" p i)) p in))
(define (_x7b_-match p in) ((lambda (p i) (mlit "{" p i)) p in))
(define (tokenize in)
  (define il (string-length in))
  (let loop ([p 0] [l 1] [c 1] [o 0] [tks '()])
    (if (>= p il)
        (reverse (cons (token 'EOF "" (pos l c o) (pos l c o)) tks))
        (let ([ch (string-ref in p)])
          (cond
            [(char-whitespace? ch)
             (if (char=? ch #\newline)
                 (loop (+ p 1) (+ l 1) 1 (+ o 1) tks)
                 (loop (+ p 1) l (+ c 1) (+ o 1) tks))]
            [(STRING-match p in) => (lambda (r) (match-define (list np v) r) (define sl (string-length v)) (define tk (token 'STRING v (pos l c o) (pos l (+ c sl) (+ o sl)))) (loop np l (+ c sl) (+ o sl) (cons tk tks)))]
            [(NUMBER-match p in) => (lambda (r) (match-define (list np v) r) (define sl (string-length v)) (define tk (token 'NUMBER v (pos l c o) (pos l (+ c sl) (+ o sl)))) (loop np l (+ c sl) (+ o sl) (cons tk tks)))]
            [(WS-match p in) => (lambda (r) (match-define (list np _) r) (loop np l c o tks))]
            [(null-match p in) => (lambda (r) (match-define (list np v) r) (define sl (string-length v)) (define tk (token 'null v (pos l c o) (pos l (+ c sl) (+ o sl)))) (loop np l (+ c sl) (+ o sl) (cons tk tks)))]
            [(false-match p in) => (lambda (r) (match-define (list np v) r) (define sl (string-length v)) (define tk (token 'false v (pos l c o) (pos l (+ c sl) (+ o sl)))) (loop np l (+ c sl) (+ o sl) (cons tk tks)))]
            [(true-match p in) => (lambda (r) (match-define (list np v) r) (define sl (string-length v)) (define tk (token 'true v (pos l c o) (pos l (+ c sl) (+ o sl)))) (loop np l (+ c sl) (+ o sl) (cons tk tks)))]
            [(_x5d_-match p in) => (lambda (r) (match-define (list np v) r) (define sl (string-length v)) (define tk (token '|]| v (pos l c o) (pos l (+ c sl) (+ o sl)))) (loop np l (+ c sl) (+ o sl) (cons tk tks)))]
            [(_x5b_-match p in) => (lambda (r) (match-define (list np v) r) (define sl (string-length v)) (define tk (token '|[| v (pos l c o) (pos l (+ c sl) (+ o sl)))) (loop np l (+ c sl) (+ o sl) (cons tk tks)))]
            [(_x2c_-match p in) => (lambda (r) (match-define (list np v) r) (define sl (string-length v)) (define tk (token '|,| v (pos l c o) (pos l (+ c sl) (+ o sl)))) (loop np l (+ c sl) (+ o sl) (cons tk tks)))]
            [(_x3a_-match p in) => (lambda (r) (match-define (list np v) r) (define sl (string-length v)) (define tk (token '|:| v (pos l c o) (pos l (+ c sl) (+ o sl)))) (loop np l (+ c sl) (+ o sl) (cons tk tks)))]
            [(_x7d_-match p in) => (lambda (r) (match-define (list np v) r) (define sl (string-length v)) (define tk (token '|}| v (pos l c o) (pos l (+ c sl) (+ o sl)))) (loop np l (+ c sl) (+ o sl) (cons tk tks)))]
            [(_x7b_-match p in) => (lambda (r) (match-define (list np v) r) (define sl (string-length v)) (define tk (token '|{| v (pos l c o) (pos l (+ c sl) (+ o sl)))) (loop np l (+ c sl) (+ o sl) (cons tk tks)))]
            [else (error 'tokenize "unexpected char ~a at ~a:~a" ch l c)])))))


(define (ctok tks pos)
  (if (< pos (length tks)) (list-ref tks pos)
      (token 'EOF "" (pos 0 0 0) (pos 0 0 0))))

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
(define (parse-json tks pos)
  (or (let ([r0 (parse-value tks pos)]) (and r0 (let ([r1 (expect-tok tks (car r0) 'EOF)]) (and r1 (list (car r1) (node 'json (list (cadr r0) (cadr r1))))))))
      #f))

(define (parse-obj tks pos)
  (or (let ([r0 (expect-lit tks pos "{")]) (and r0 (let ([r1 (parse-pair tks (car r0))]) (and r1 (let ([r2 (parse-star tks (car r1) (lambda (t p) (parse-group t p (list (lambda (t p) (let ([r0 (expect-lit tks p ",")]) (and r0 (let ([r1 (parse-pair tks (car r0))]) (and r1 (list (car r1) (node 'group (list (cadr r0) (cadr r1)))))))))))))]) (and r2 (let ([r3 (expect-lit tks (car r2) "}")]) (and r3 (list (car r3) (node 'obj (list (cadr r0) (cadr r1) (cadr r2) (cadr r3))))))))))))
      (let ([r0 (expect-lit tks pos "{")]) (and r0 (let ([r1 (expect-lit tks (car r0) "}")]) (and r1 (list (car r1) (node 'obj (list (cadr r0) (cadr r1))))))))
      #f))

(define (parse-pair tks pos)
  (or (let ([r0 (expect-tok tks pos 'STRING)]) (and r0 (let ([r1 (expect-lit tks (car r0) ":")]) (and r1 (let ([r2 (parse-value tks (car r1))]) (and r2 (list (car r2) (node 'pair (list (cadr r0) (cadr r1) (cadr r2))))))))))
      #f))

(define (parse-arr tks pos)
  (or (let ([r0 (expect-lit tks pos "[")]) (and r0 (let ([r1 (parse-value tks (car r0))]) (and r1 (let ([r2 (parse-star tks (car r1) (lambda (t p) (parse-group t p (list (lambda (t p) (let ([r0 (expect-lit tks p ",")]) (and r0 (let ([r1 (parse-value tks (car r0))]) (and r1 (list (car r1) (node 'group (list (cadr r0) (cadr r1)))))))))))))]) (and r2 (let ([r3 (expect-lit tks (car r2) "]")]) (and r3 (list (car r3) (node 'arr (list (cadr r0) (cadr r1) (cadr r2) (cadr r3))))))))))))
      (let ([r0 (expect-lit tks pos "[")]) (and r0 (let ([r1 (expect-lit tks (car r0) "]")]) (and r1 (list (car r1) (node 'arr (list (cadr r0) (cadr r1))))))))
      #f))

(define (parse-value tks pos)
  (or (let ([r0 (expect-tok tks pos 'STRING)]) (and r0 (list (car r0) (node 'value (list (cadr r0))))))
      (let ([r0 (expect-tok tks pos 'NUMBER)]) (and r0 (list (car r0) (node 'value (list (cadr r0))))))
      (let ([r0 (parse-obj tks pos)]) (and r0 (list (car r0) (node 'value (list (cadr r0))))))
      (let ([r0 (parse-arr tks pos)]) (and r0 (list (car r0) (node 'value (list (cadr r0))))))
      (let ([r0 (expect-lit tks pos "true")]) (and r0 (list (car r0) (node 'value (list (cadr r0))))))
      (let ([r0 (expect-lit tks pos "false")]) (and r0 (list (car r0) (node 'value (list (cadr r0))))))
      (let ([r0 (expect-lit tks pos "null")]) (and r0 (list (car r0) (node 'value (list (cadr r0))))))
      #f))

(define (parse in)
  (define tks (tokenize in))
  (match-define (list fp res) (parse-json tks 0))
  res)