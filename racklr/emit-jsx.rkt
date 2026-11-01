#lang racket

(require racklr/uir
         racklr/emit-contract)

(provide emit-jsx)

;; ── JSX UIR → React.createElement JavaScript emitter ──────────────────────
;;
;; Implements the emit contract (racklr/emit-contract): UIR plus an optional
;; explicit effect row in, JavaScript out. This target renders a JSX tree (an
;; expression), which has no native effect-row vocabulary, so an explicit row
;; is refused rather than dropped.

(define (emit-jsx uir [row #f])
  (if (eq? row #f)
      (emit-jsx-node uir)
      (refuse-explicit-row 'emit-jsx "jsx" row)))

(define (emit-jsx-node uir)
  (match uir
    [(uir-string v)
     (format "~s" v)]
    
    [(uir-symbol v) v]
    
    [(uir-text-node text-uir)
     (emit-jsx-node text-uir)]
    
    [(uir-jsx-expr expr)
     expr]
    
    [(uir-attribute name-uir value-uir)
     (cons (emit-jsx-node name-uir) (emit-jsx-node value-uir))]
    
    [(uir-element tag-uir attrs children key-uir)
     (define tag-str (emit-jsx-node tag-uir))
     (define props-str (emit-props attrs key-uir))
     (define children-strs (map emit-jsx-node children))
     (format "React.createElement(~a, ~a~a)"
             tag-str
             props-str
             (if (null? children)
                 ""
                 (format ", ~a" (string-join children-strs ", "))))]
    
    [_ (error 'emit-jsx "unexpected UIR type: ~a" uir)]))

(define (emit-props attrs key-uir)
  (define attr-pairs
    (for/list ([attr (in-list attrs)])
      (match-define (cons name-str val-str) (emit-jsx-node attr))
      (format "~a: ~a" name-str val-str)))
  
  (define key-pair
    (match key-uir
      [(uir-string k)
       (list (format "key: ~s" k))]
      [_ '()]))
  
  (define all-pairs (append attr-pairs key-pair))
  
  (if (null? all-pairs)
      "null"
      (format "{ ~a }" (string-join all-pairs ", "))))
