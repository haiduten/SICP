#lang racket
(define (iterative-improve goodEnough improve)
  (define (helper guess) (
     if (goodEnough guess (improve guess)) (improve guess) (helper (improve guess))))
  (lambda (guess) (helper guess)))

(define (average x y) 
  (/ (+ x y) 2))

(define (square n) (* n n ))

(define (sqrt x)
  (define (improve guess)
    (average guess (/ x guess)))

  (define (good-enough? old guess)
    (< (abs (- (square guess) x)) 0.001))
  ((iterative-improve good-enough? improve) 1.0))

(sqrt 9)

(define tolerance 0.00001)

;(define (fixed-point f first-guess)
;  (define (close-enough? v1 v2)
;    (< (abs (- v1 v2)) 
;       tolerance))
;  (define (try guess)
;    (let ((next (f guess)))
;      (if (close-enough? guess next)
;          next
;          (try next))))
;  (try first-guess))



(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) 
       tolerance))
  ((iterative-improve close-enough? f) 1.0))

(fixed-point cos 1.0)