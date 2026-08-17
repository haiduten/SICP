#lang racket
(define (cons x y) 
  (* (expt 2 x) (expt 3 y)))

(define (car p)
  (define (helper num count)
    (if (not (= (remainder num 2) 0)) count (helper (/ num 2) (+ count 1))))
  (helper p 0))


(define (cdr p)
  (define (helper num count)
    (if (not (= (remainder num 3) 0)) count (helper (/ num 3) (+ count 1))))
  (helper p 0))

(define x (cons 4 5))
(car x)
(cdr x)