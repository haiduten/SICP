#lang racket
(define (double x) (+ x x))
(define (halve x) (/ x 2))

(define (even? n)
  (= (remainder n 2) 0))

(define (*-iter a b c)
  (cond ((= b 0) c)
        ((even? b) (*-iter (double a) (halve b) c))
        (else (*-iter a (- b 1) (+ c a)))))

(define (* a b) (*-iter a b 0))

(* 5 5)