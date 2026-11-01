#lang racket

(require racklr/uir
         racklr/emit-contract)

(provide emit-dom-js)

;; ── UIR → Vanilla DOM JavaScript emitter ───────────────────────────
;; Emits JavaScript that creates DOM elements with document.createElement
;; instead of React.createElement.
;;
;; Implements the emit contract (racklr/emit-contract): UIR plus an optional
;; explicit effect row in, JavaScript out. This target renders a JSX tree (an
;; expression), which has no native effect-row vocabulary, so an explicit row
;; is refused rather than dropped.

(define (emit-dom-js uir [row #f])
  (if (eq? row #f)
      (emit-dom-js-node uir)
      (refuse-explicit-row 'emit-dom-js "dom-js" row)))

(define (emit-dom-js-node uir)
  (match uir
    [(uir-string v)
     (format "~s" v)]
    
    [(uir-symbol v) v]
    
    [(uir-text-node text-uir)
     (format "document.createTextNode(~a)" (emit-dom-js-node text-uir))]
    
    [(uir-jsx-expr expr)
     expr]
    
    [(uir-attribute name-uir value-uir)
     ;; name-uir is always a uir-symbol from the tag/attribute name
     (cons (format "~s" (uir-symbol-name name-uir))
           (emit-dom-js-node value-uir))]
    
    [(uir-element tag-uir attrs children _events)
     ;; tag-uir is a uir-symbol from JSX tag name — emit as quoted string
     (define tag-str (format "~s" (uir-symbol-name tag-uir)))
     (define attr-code (emit-dom-attrs attrs))
     (define child-code (emit-dom-children children))
     
     (string-append
      "(function(){"
      "var _el=document.createElement(" tag-str ");"
      attr-code
      child-code
      "return _el;"
      "})()")]
    
    [_ (error 'emit-dom-js "unexpected UIR type: ~a" uir)]))

(define (emit-dom-attrs attrs)
  (string-join
   (for/list ([attr (in-list attrs)])
     (match-define (cons name-str val-str) (emit-dom-js-node attr))
     (format "_el.setAttribute(~a,~a);" name-str val-str))
   ""))

(define (emit-dom-children children)
  (string-join
   (for/list ([child (in-list children)])
     (format "_el.appendChild(~a);" (emit-dom-js-node child)))
   ""))
