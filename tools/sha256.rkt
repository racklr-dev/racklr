#lang racket

;; tools/sha256.rkt — dependency-free SHA-256 (FIPS 180-4).
;;
;; The baseline content-address needs a reproducible digest that does not
;; depend on the native OpenSSL build (or on a particular Racket package being
;; installed). This is a straight implementation of the standard; it is for
;; hashing short, in-memory serializations (the type-graph baseline snapshot),
;; not for bulk or adversarial input.
;;
;;   (sha256-hex #"abc") -> "ba7816bf...15ad"
;;
;; Reference vectors are exercised by racklr-test/baseline-check-test.rkt.

(provide sha256-bytes sha256-hex bytes->hex)

;; ── 32-bit word helpers ─────────────────────────────────────────────

(define (u32 n)
  (bitwise-and n #xFFFFFFFF))

(define (rotr32 x n)
  (u32 (bitwise-ior (arithmetic-shift x (- n))
                    (arithmetic-shift x (- 32 n)))))

(define (add32 . ns)
  (u32 (apply + ns)))

;; Round constants: first 32 bits of the fractional parts of the cube roots
;; of the first 64 primes.
(define sha256-k
  (vector
   #x428a2f98 #x71374491 #xb5c0fbcf #xe9b5dba5
   #x3956c25b #x59f111f1 #x923f82a4 #xab1c5ed5
   #xd807aa98 #x12835b01 #x243185be #x550c7dc3
   #x72be5d74 #x80deb1fe #x9bdc06a7 #xc19bf174
   #xe49b69c1 #xefbe4786 #x0fc19dc6 #x240ca1cc
   #x2de92c6f #x4a7484aa #x5cb0a9dc #x76f988da
   #x983e5152 #xa831c66d #xb00327c8 #xbf597fc7
   #xc6e00bf3 #xd5a79147 #x06ca6351 #x14292967
   #x27b70a85 #x2e1b2138 #x4d2c6dfc #x53380d13
   #x650a7354 #x766a0abb #x81c2c92e #x92722c85
   #xa2bfe8a1 #xa81a664b #xc24b8b70 #xc76c51a3
   #xd192e819 #xd6990624 #xf40e3585 #x106aa070
   #x19a4c116 #x1e376c08 #x2748774c #x34b0bcb5
   #x391c0cb3 #x4ed8aa4a #x5b9cca4f #x682e6ff3
   #x748f82ee #x78a5636f #x84c87814 #x8cc70208
   #x90befffa #xa4506ceb #xbef9a3f7 #xc67178f2))

;; Initial hash values: first 32 bits of the fractional parts of the square
;; roots of the first 8 primes.
(define sha256-h0
  (vector #x6a09e667 #xbb67ae85 #x3c6ef372 #xa54ff53a
          #x510e527f #x9b05688c #x1f83d9ab #x5be0cd19))

;; ── padding ─────────────────────────────────────────────────────────

;; Pad to a multiple of 64 bytes: 0x80, then zeroes, then the 64-bit
;; big-endian message length in bits. The zero count leaves 8 bytes of length
;; and any number of whole blocks.
(define (sha256-pad bs)
  (define len (bytes-length bs))
  (define zero-count (modulo (- 56 (add1 len)) 64))
  (define padded
    (make-bytes (+ len 1 zero-count 8) 0))
  (bytes-copy! padded 0 bs)
  (bytes-set! padded len #x80)
  (bytes-copy! padded (+ len 1 zero-count)
               (integer->integer-bytes (* 8 len) 8 #f #t))
  padded)

;; ── compression ─────────────────────────────────────────────────────

(define (read-word bs off)
  (integer-bytes->integer (subbytes bs off (+ off 4)) #f #t))

(define (small-sigma0 x)
  (bitwise-xor (rotr32 x 7) (rotr32 x 18) (u32 (arithmetic-shift x -3))))

(define (small-sigma1 x)
  (bitwise-xor (rotr32 x 17) (rotr32 x 19) (u32 (arithmetic-shift x -10))))

(define (sha256-compress! h bs off)
  (define w (make-vector 64 0))
  (for ([i (in-range 16)])
    (vector-set! w i (read-word bs (+ off (* i 4)))))
  (for ([i (in-range 16 64)])
    (vector-set! w i
                 (add32 (small-sigma1 (vector-ref w (- i 2)))
                        (vector-ref w (- i 7))
                        (small-sigma0 (vector-ref w (- i 15)))
                        (vector-ref w (- i 16)))))
  (define a (vector-ref h 0))
  (define b (vector-ref h 1))
  (define c (vector-ref h 2))
  (define d (vector-ref h 3))
  (define e (vector-ref h 4))
  (define f (vector-ref h 5))
  (define g (vector-ref h 6))
  (define hh (vector-ref h 7))
  (for ([i (in-range 64)])
    (define big-sigma1
      (bitwise-xor (rotr32 e 6) (rotr32 e 11) (rotr32 e 25)))
    (define choose
      (bitwise-xor (bitwise-and e f)
                   (bitwise-and (u32 (bitwise-not e)) g)))
    (define t1
      (add32 hh big-sigma1 choose (vector-ref sha256-k i) (vector-ref w i)))
    (define big-sigma0
      (bitwise-xor (rotr32 a 2) (rotr32 a 13) (rotr32 a 22)))
    (define majority
      (bitwise-xor (bitwise-and a b) (bitwise-and a c) (bitwise-and b c)))
    (define t2 (add32 big-sigma0 majority))
    (set! hh g)
    (set! g f)
    (set! f e)
    (set! e (add32 d t1))
    (set! d c)
    (set! c b)
    (set! b a)
    (set! a (add32 t1 t2)))
  (vector-set! h 0 (add32 (vector-ref h 0) a))
  (vector-set! h 1 (add32 (vector-ref h 1) b))
  (vector-set! h 2 (add32 (vector-ref h 2) c))
  (vector-set! h 3 (add32 (vector-ref h 3) d))
  (vector-set! h 4 (add32 (vector-ref h 4) e))
  (vector-set! h 5 (add32 (vector-ref h 5) f))
  (vector-set! h 6 (add32 (vector-ref h 6) g))
  (vector-set! h 7 (add32 (vector-ref h 7) hh)))

;; ── public surface ──────────────────────────────────────────────────

;; SHA-256 of a byte string, as 32 raw bytes.
(define (sha256-bytes bs)
  (define h (vector-copy sha256-h0))
  (define padded (sha256-pad bs))
  (for ([off (in-range 0 (bytes-length padded) 64)])
    (sha256-compress! h padded off))
  (apply bytes-append (for/list ([x (in-vector h)]) (integer->integer-bytes x 4 #f #t))))

(define (byte->hex b)
  (define s (number->string b 16))
  (if (= 1 (string-length s)) (string-append "0" s) s))

(define (bytes->hex bs)
  (let ([out (open-output-string)])
    (for ([b (in-bytes bs)])
      (write-string (byte->hex b) out))
    (get-output-string out)))

;; SHA-256 of a byte string, as lowercase hex.
(define (sha256-hex bs)
  (bytes->hex (sha256-bytes bs)))
