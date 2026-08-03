#lang racket
(define (square x) (* x x))

(define (good-enough? guess oldguess)
  (< (/ (abs (- oldguess guess)) guess) 0.01))

(define (improve y x)
  (/ (+ (/ x (square y)) (* 2 y)) 3))

(define (cuberoot-iter guess x oldguess)
  (if (good-enough? guess oldguess) guess (cuberoot-iter (improve guess x) x guess)))

(define (cuberoot x) (cuberoot-iter 1 x 5))

(cuberoot 27.0)