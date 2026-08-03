#lang racket
(define (square n) (* n n))

(define (cont-frac n d k)
  (define (cont-frac-iter ans i)
    (if (= i 0) ans
        (cont-frac-iter (/ (n i) (- (d i) ans)) (- i 1))))
  (cont-frac-iter 0 k))

(define (tan-cf x k)
  (cont-frac (lambda (i) (if (= i 1) x (square x)))
           (lambda (i) (- (* 2 i) 1))
           k))


(tan-cf 23.0 133.0)