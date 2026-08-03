#lang racket
(define (cont-frac n d k)
  (define (cont-frac-iter ans i)
    (if (= i 0) ans
        (cont-frac-iter (/ (n i) (+ (d i) ans)) (- i 1))))
  (cont-frac-iter 0 k))

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           14)