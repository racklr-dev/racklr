#lang racket

(require racklr/uir
         racklr/emit-contract)

(provide emit-json)

(define (emit-string s)
  (define out (open-output-string))
  (write-json-string s out)
  (get-output-string out))

(define (write-json-string s out)
  (display "\"" out)
  (for ([c (in-string s)])
    (case c
      [(#\") (display "\\\"" out)]
      [(#\\) (display "\\\\" out)]
      [(#\/) (display "\\/" out)]
      [(#\backspace) (display "\\b" out)]
      [(#\page) (display "\\f" out)]
      [(#\newline) (display "\\n" out)]
      [(#\return) (display "\\r" out)]
      [(#\tab) (display "\\t" out)]
      [else
       (define n (char->integer c))
       (if (< n 32)
           (fprintf out "\\u~4,'0x" n)
           (display c out))]))
  (display "\"" out))

;; ── Pretty-printing ────────────────────────────────────────────────

;; Implements the emit contract (racklr/emit-contract): UIR plus an optional
;; explicit effect row in, JSON text out. JSON is a data target with no native
;; effect-row vocabulary, so an explicit row is refused rather than dropped.
(define (emit-json v [row #f] #:indent [indent #f])
  (if row
      (refuse-explicit-row 'emit-json "json" row)
      (let ([out (open-output-string)])
        (emit-node v out (if indent 0 #f) indent)
        (get-output-string out))))

;; indent-level: #f = compact, 0,1,2,... = pretty-printed at that level
(define (emit-node v out level width)
  (cond [(uir-null? v) (display "null" out)]
        [(uir-bool? v) (display (if (uir-bool-value v) "true" "false") out)]
        [(uir-number? v) (display (uir-number-value v) out)]
        [(uir-string? v) (write-json-string (uir-string-value v) out)]
        [(uir-list? v) (emit-list (uir-list-items v) out level width)]
        [(uir-record? v) (emit-record (uir-record-entries v) out level width)]
        [(uir-spread? v) (display "..." out) (emit-node (uir-spread-expr v) out level width)]
        [else (error 'emit-json "unexpected uir node: ~e" v)]))

(define (emit-list items out level width)
  (if (null? items)
      (display "[]" out)
      (let ([inner (and level (+ level width))])
        (display "[" out)
        (when inner (newline out))
        (for ([(item i) (in-indexed items)])
          (when inner (indent out inner))
          (emit-node item out inner width)
          (unless (= i (sub1 (length items)))
            (display "," out))
          (when inner (newline out)))
        (when inner (indent out level))
        (display "]" out))))

(define (emit-record entries out level width)
  (if (null? entries)
      (display "{}" out)
      (let ([inner (and level (+ level width))])
        (display "{" out)
        (when inner (newline out))
        (for ([(entry i) (in-indexed entries)])
          (define key (car entry))
          (define val (cdr entry))
          (when inner (indent out inner))
          (emit-node key out inner width)
          (display ": " out)
          (emit-node val out inner width)
          (unless (= i (sub1 (length entries)))
            (display "," out))
          (when inner (newline out)))
        (when inner (indent out level))
        (display "}" out))))

(define (indent out level)
  (display (make-string level #\space) out))

(module+ main
  (displayln "racklr/emit-json — UIR → JSON code generator loaded."))
