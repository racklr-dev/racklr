#lang racket

;; racklr/uir/schema-io.rkt — the generic UIR reader and writer.
;;
;; This is the second half of the hand-written kernel: given a layout list it
;; encodes any UIR value to its serialized s-expression and decodes one back.
;; Nothing here knows a specific node kind; the layouts (KERNEL-LAYOUTS plus
;; the schema-generated layouts) and the field codecs carry that knowledge.
;; It is hand-written and versioned with UIR-KERNEL-VERSION in kernel.rkt.

(require racket/list
         racket/match
         "kernel.rkt")

(provide uir->sexp* sexp->uir* jsexpr->uir-sexp)

;; ── Generic writer: UIR value + layouts -> serialized sexp ──────────

(define (encode-field codec v layouts)
  (case codec
    [(uir) (uir->sexp* v layouts)]
    [(list-uir)
     (for/list ([x (in-list v)]) (uir->sexp* x layouts))]
    [(list-pair-uir)
     (for/list ([p (in-list v)])
       (list (uir->sexp* (car p) layouts) (uir->sexp* (cdr p) layouts)))]
    [(catch-list)
     (for/list ([t (in-list v)])
       (for/list ([x (in-list t)]) (uir->sexp* x layouts)))]
    [(with-item-list)
     (for/list ([p (in-list v)])
       (list (uir->sexp* (car p) layouts)
             (if (cadr p) (uir->sexp* (cadr p) layouts) 'unspecified)))]
    [(handler-op-list)
     (for/list ([t (in-list v)])
       (list (uir->sexp* (car t) layouts)
             (for/list ([x (in-list (cadr t))]) (uir->sexp* x layouts))
             (uir->sexp* (caddr t) layouts)))]
    [(eff-op-list)
     (for/list ([q (in-list v)])
       (list (uir->sexp* (car q) layouts)
             (for/list ([x (in-list (cadr q))]) (uir->sexp* x layouts))
             (if (caddr q) (uir->sexp* (caddr q) layouts) #f)
             (uir->sexp* (cadddr q) layouts)))]
    [(optional-uir) (if v (uir->sexp* v layouts) #f)]
    [(optional-uir-marker) (if v (uir->sexp* v layouts) 'unspecified)]
    [(optional-int) (if v v #f)]
    [(text)
     (define l (layout-for-value v layouts))
     (unless (and l (eq? (uir-layout-tag l) 'string))
       (error 'uir->sexp "text field is not a uir-string: ~e" v))
     ((first (uir-layout-accessors l)) v)]
    [(raw) v]
    [(bool)
     (if (boolean? v) v (error 'uir->sexp "boolean field: ~e" v))]
    [(string)
     (if (string? v) v (error 'uir->sexp "string field: ~e" v))]
    [(symbol)
     (if (symbol? v) v (error 'uir->sexp "symbol field: ~e" v))]
    [(name)
     (if (or (string? v) (symbol? v)) v (error 'uir->sexp "name field: ~e" v))]
    [(param-kind)
     (if (memq v UIR-PARAM-KINDS)
         (symbol->string v)
         (error 'uir->sexp "parameter kind field: ~e" v))]
    [(int)
     (if (exact-integer? v) v (error 'uir->sexp "integer field: ~e" v))]
    [else (error 'uir->sexp "unknown UIR field codec: ~e" codec)]))

(define (uir->sexp* v [layouts KERNEL-LAYOUTS])
  (define l (layout-for-value v layouts))
  (unless l (error 'uir->sexp "not a UIR node: ~e" v))
  (define codecs (uir-layout-codecs l))
  (define accessors (uir-layout-accessors l))
  (define defaults (uir-layout-defaults l))
  (cond
    [(equal? codecs '(spliced-uir))
     (cons (uir-layout-tag l)
           (for/list ([x (in-list ((first accessors) v))])
             (uir->sexp* x layouts)))]
    [(equal? codecs '(spliced-pair-uir))
     (cons (uir-layout-tag l)
           (for/list ([p (in-list ((first accessors) v))])
             (list (uir->sexp* (car p) layouts)
                   (uir->sexp* (cdr p) layouts))))]
    [else
     ;; A trailing field holding its default value is omitted from the payload;
     ;; compute that suffix first, then encode only the fields that remain.
     (define values (for/list ([acc (in-list accessors)]) (acc v)))
     (define keep
       (let loop ([n (length values)])
         (if (and (> n 0)
                  (let ([i (sub1 n)])
                    (and (field-defaulted? (list-ref defaults i))
                         (field-value-is-default? (list-ref defaults i)
                                                  (list-ref values i)))))
             (loop (sub1 n))
             n)))
     (cons (uir-layout-tag l)
           (for/list ([codec (in-list (take codecs keep))]
                      [val (in-list (take values keep))])
             (encode-field codec val layouts)))]))

;; ── Generic reader: serialized sexp + layouts -> UIR value ─────────

(define (decode-field codec s layouts)
  (case codec
    [(uir) (sexp->uir* s layouts)]
    [(list-uir) (map (lambda (x) (sexp->uir* x layouts)) s)]
    [(list-pair-uir)
     (for/list ([e (in-list s)])
       (match e
         [(list k v) (cons (sexp->uir* k layouts) (sexp->uir* v layouts))]
         [_ (error 'sexp->uir "invalid pair field: ~e" e)]))]
    [(catch-list)
     (for/list ([t (in-list s)])
       (match t
         [(list a b c)
          (list (sexp->uir* a layouts) (sexp->uir* b layouts)
                (sexp->uir* c layouts))]
         [_ (error 'sexp->uir "invalid catch clause: ~e" t)]))]
    [(with-item-list)
     (for/list ([p (in-list s)])
       (match p
         [(list ctx as-name)
          (list (sexp->uir* ctx layouts)
                (if (eq? as-name 'unspecified) #f (sexp->uir* as-name layouts)))]
         [_ (error 'sexp->uir "invalid with item: ~e" p)]))]
    [(handler-op-list)
     (for/list ([t (in-list s)])
       (match t
         [(list nm ps b)
          (list (sexp->uir* nm layouts)
                (map (lambda (x) (sexp->uir* x layouts)) ps)
                (sexp->uir* b layouts))]
         [_ (error 'sexp->uir "invalid handler op: ~e" t)]))]
    [(eff-op-list)
     (for/list ([q (in-list s)])
       (match q
         [(list nm ps rt b)
          (list (sexp->uir* nm layouts)
                (map (lambda (x) (sexp->uir* x layouts)) ps)
                (if (eq? rt #f) #f (sexp->uir* rt layouts))
                (sexp->uir* b layouts))]
         [_ (error 'sexp->uir "invalid effect op: ~e" q)]))]
    [(optional-uir) (if (eq? s #f) #f (sexp->uir* s layouts))]
    [(optional-uir-marker) (if (eq? s 'unspecified) #f (sexp->uir* s layouts))]
    [(optional-int)
     (if (eq? s #f) #f (if (exact-integer? s) s (error 'sexp->uir "integer field: ~e" s)))]
    [(text)
     (define l (layout-by-tag 'string layouts))
     (unless l (error 'sexp->uir "no 'string layout for a text field"))
     ((uir-layout-ctor l) s)]
    [(raw) s]
    [(bool) (if (boolean? s) s (error 'sexp->uir "boolean field: ~e" s))]
    [(string) (if (string? s) s (error 'sexp->uir "string field: ~e" s))]
    [(symbol) (if (symbol? s) s (error 'sexp->uir "symbol field: ~e" s))]
    [(name) (if (or (string? s) (symbol? s)) s (error 'sexp->uir "name field: ~e" s))]
    [(param-kind)
     (cond [(eq? s #f) #f]
           [(string? s)
            (define k (string->symbol s))
            (if (uir-param-kind? k)
                k
                (error 'sexp->uir "invalid parameter kind: ~e" s))]
           [else (error 'sexp->uir "parameter kind field: ~e" s)])]
    [(int) (if (exact-integer? s) s (error 'sexp->uir "integer field: ~e" s))]
    [else (error 'sexp->uir "unknown UIR field codec: ~e" codec)]))

;; The number of leading fields a serialized node must carry: every field up
;; to the last `required` one.
(define (required-field-count defaults)
  (let loop ([i (length defaults)])
    (if (and (> i 0) (field-defaulted? (list-ref defaults (sub1 i))))
        (loop (sub1 i))
        i)))

(define (sexp->uir* s [layouts KERNEL-LAYOUTS])
  (unless (and (pair? s) (list? s))
    (error 'sexp->uir "invalid uir sexp: ~e" s))
  (define l (layout-by-tag (car s) layouts))
  (unless l (error 'sexp->uir "invalid uir sexp: ~e" s))
  (define fields (cdr s))
  (define codecs (uir-layout-codecs l))
  (define defaults (uir-layout-defaults l))
  (cond
    [(equal? codecs '(spliced-uir))
     ((uir-layout-ctor l) (map (lambda (x) (sexp->uir* x layouts)) fields))]
    [(equal? codecs '(spliced-pair-uir))
     ((uir-layout-ctor l)
      (for/list ([e (in-list fields)])
        (match e
          [(list k v) (cons (sexp->uir* k layouts) (sexp->uir* v layouts))]
          [_ (error 'sexp->uir "invalid uir sexp: ~e" s)])))]
    [else
     ;; Trailing defaulted fields may be missing from the payload; fill them in.
     (unless (<= (required-field-count defaults)
                 (length fields)
                 (length codecs))
       (error 'sexp->uir "invalid uir sexp: ~e" s))
     (define (field-at i)
       (if (< i (length fields))
           (list-ref fields i)
           (field-default-value (list-ref defaults i))))
     (apply (uir-layout-ctor l)
            (for/list ([codec (in-list codecs)] [i (in-naturals)])
              (decode-field codec (field-at i) layouts)))]))

;; ── JSON decoding ───────────────────────────────────────────────────
;;
;; A JSON payload encodes symbols as strings, so the reader needs the layout
;; and field-codec knowledge to restore them: a field declared `symbol` is a
;; symbol, a field declared `string` or `text` stays a string. This is the
;; counterpart of the writer's generic symbol->string conversion in
;; version.rkt, and it is what keeps JSON round trips lossless for every
;; schema-declared node kind.

(define (proper-list? v) (and (list? v) #t))

(define (jsexpr->uir-sexp j layouts)
  (match j
    [(list (? string? envelope) version payload)
     #:when (equal? envelope "uir")
     (list 'uir version (jsexpr->node-sexp payload layouts))]
    [_ (error 'json->uir "not a versioned UIR JSON payload: ~e" j)]))

(define (jsexpr->node-sexp j layouts)
  (unless (and (pair? j) (list? j) (string? (first j)))
    (error 'json->uir "invalid UIR JSON node: ~e" j))
  (define tag (string->symbol (first j)))
  (define l (layout-by-tag tag layouts))
  (unless l (error 'json->uir "unknown UIR JSON node tag: ~e" (first j)))
  (define codecs (uir-layout-codecs l))
  (define defaults (uir-layout-defaults l))
  (define items (rest j))
  (cond
    [(equal? codecs '(spliced-uir))
     (cons tag (map (lambda (x) (jsexpr->node-sexp x layouts)) items))]
    [(equal? codecs '(spliced-pair-uir))
     (cons tag
           (for/list ([p (in-list items)])
             (unless (and (proper-list? p) (= (length p) 2))
               (error 'json->uir "invalid UIR JSON pair: ~e" p))
             (list (jsexpr->node-sexp (first p) layouts)
                   (jsexpr->node-sexp (second p) layouts))))]
    [else
     (unless (<= (required-field-count defaults)
                 (length items)
                 (length codecs))
       (error 'json->uir "invalid UIR JSON node: ~e" j))
     (cons tag
           (for/list ([codec (in-list codecs)] [i (in-naturals)])
             (if (< i (length items))
                 (jsexpr->codec codec (list-ref items i) layouts)
                 (field-default-value (list-ref defaults i)))))]))

(define (jsexpr->codec codec x layouts)
  (case codec
    [(uir) (jsexpr->node-sexp x layouts)]
    [(list-uir) (map (lambda (v) (jsexpr->node-sexp v layouts)) x)]
    [(list-pair-uir)
     (for/list ([p (in-list x)])
       (unless (and (proper-list? p) (= (length p) 2))
         (error 'json->uir "invalid UIR JSON pair: ~e" p))
       (cons (jsexpr->node-sexp (car p) layouts)
             (jsexpr->node-sexp (cadr p) layouts)))]
    [(catch-list)
     (for/list ([t (in-list x)])
       (unless (and (proper-list? t) (= (length t) 3))
         (error 'json->uir "invalid UIR JSON catch clause: ~e" t))
       (map (lambda (v) (jsexpr->node-sexp v layouts)) t))]
    [(with-item-list)
     (for/list ([p (in-list x)])
       (unless (and (proper-list? p) (= (length p) 2))
         (error 'json->uir "invalid UIR JSON with item: ~e" p))
       (list (jsexpr->node-sexp (car p) layouts)
             (cond [(eq? (cadr p) #f) #f]
                   [(equal? (cadr p) "unspecified") 'unspecified]
                   [else (jsexpr->node-sexp (cadr p) layouts)])))]
    [(handler-op-list)
     (for/list ([t (in-list x)])
       (unless (and (proper-list? t) (= (length t) 3))
         (error 'json->uir "invalid UIR JSON handler op: ~e" t))
       (list (jsexpr->node-sexp (car t) layouts)
             (map (lambda (v) (jsexpr->node-sexp v layouts)) (cadr t))
             (jsexpr->node-sexp (caddr t) layouts)))]
    [(eff-op-list)
     (for/list ([q (in-list x)])
       (unless (and (proper-list? q) (= (length q) 4))
         (error 'json->uir "invalid UIR JSON effect op: ~e" q))
       (list (jsexpr->node-sexp (car q) layouts)
             (map (lambda (v) (jsexpr->node-sexp v layouts)) (cadr q))
             (if (eq? (caddr q) #f) #f (jsexpr->node-sexp (caddr q) layouts))
             (jsexpr->node-sexp (cadddr q) layouts)))]
    [(optional-uir) (if (eq? x #f) #f (jsexpr->node-sexp x layouts))]
    [(optional-uir-marker)
     (cond [(eq? x #f) #f]
           [(equal? x "unspecified") 'unspecified]
           [else (jsexpr->node-sexp x layouts)])]
    [(optional-int) (if (eq? x #f) #f x)]
    [(text) x]
    [(raw) x]
    [(bool) x]
    [(string) x]
    [(int) x]
    [(symbol) (if (string? x) (string->symbol x) x)]
    [(name) x]
    [(param-kind)
     (cond [(eq? x #f) #f]
           [(string? x) x]
           [else (error 'json->uir "parameter kind field: ~e" x)])]
    [else (error 'json->uir "unknown UIR field codec: ~e" codec)]))
