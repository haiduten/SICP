#lang racket
(define (square x) (* x x))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y) 
  (/ (+ x y) 2))


;(square (sqrt 0.000000000000000000005)) returns a number way off 
; the below never stops running. 
;(square (sqrt 132412341234213412342341234123412341234122.1234123))

(define (good-enough? guess oldguess)
  (< (/ (abs (- oldguess guess)) guess) 0.01))


(define (sqrt-iter guess x oldguess)
  (if (good-enough? guess oldguess)
      guess
      (sqrt-iter (improve guess x) x guess)))


(define (sqrt x)
  (sqrt-iter 1.0 x 5))

(square (sqrt 0.000000000000000000005))
(square (sqrt 132412341234213412342341234123412341234122.1234123))