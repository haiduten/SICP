#lang racket
(define (f n) (if (< n 3) n (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))))

(define (f-iter n count x y z)
  (define new (if (< count 3) count (+ x (* 2 y) (* 3 z))))
  (if (= count n) new (f-iter n (+ count 1) new x y))) 

(define (f_iter n) (f-iter n 0 0 0 0))

(f 12)
(f_iter 12)
