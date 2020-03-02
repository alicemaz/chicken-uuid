(module uuid (uuid uuid?)

(import scheme)
(import chicken.base)
(import chicken.string)
(import chicken.blob)
(import chicken.random)
(import chicken.bitwise)
(import chicken.irregex)
(import srfi-4)

(define (take n l)
  (cond ((= n 0) '())
        ((null? l) '())
        (else (cons (car l) (take (- n 1) (cdr l))))))

(define (drop n l)
  (cond ((= n 0) l)
        ((null? l) '())
        (else (drop (- n 1) (cdr l)))))

(define (number->hex n)
  (let ((s (number->string n 16)))
       (if (odd? (string-length s))
           (string-append "0" s)
           s)))

(define (uuid)
  (define b (make-blob 16))
  (random-bytes b)
  (define v (blob->u8vector/shared b))
  (u8vector-set! v 6 (bitwise-ior #b01000000 (bitwise-and #b00001111 (u8vector-ref v 6))))
  (u8vector-set! v 8 (bitwise-ior #b10000000 (bitwise-and #b00111111 (u8vector-ref v 8))))
  (define l (map number->hex (u8vector->list v)))
  (string-intersperse `(,@(take 4 l) "-"
                        ,@(take 2 (drop 4 l)) "-"
                        ,@(take 2 (drop 6 l)) "-"
                        ,@(take 2 (drop 8 l)) "-"
                        ,@(drop 10 l))
                      ""))

(define uuid-re (irregex "[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}" 'fast))
(define (uuid? s) (and (string? s) (irregex-match? uuid-re s)))

)
