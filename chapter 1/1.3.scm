#lang racket
(define (square x) (* x x))
(define (sum_square x y) (+ (square x) (square y)))
(define (sum_sq_largest x y z)
  (if (> x y)
      (if (> y z) (sum_square x y) (sum_square x z))
      (if (> x z) (sum_square y x) (sum_square y z))))

(sum_sq_largest 9 1 0)
(sum_sq_largest 9 0 1)
(sum_sq_largest 1 9 0)
(sum_sq_largest 1 0 9)
(sum_sq_largest 0 9 1)
(sum_sq_largest 0 1 9)
   