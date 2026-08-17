#lang racket
(define (make-interval a b) (cons a b))

(define (upper-bound int) (cdr int))

(define (lower-bound int) (car int))

(define (sub-interval a b)
  (let (
        (la (lower-bound a))
        (ua (upper-bound a))
        (lb (lower-bound b))
        (ub (upper-bound b))
       )
    (make-interval (- la ub) (- ua lb))))


(define first (make-interval 5 10))
(define second (make-interval 1 2))

(define (print x)
  (display (lower-bound x))
  (newline)
  (display (upper-bound x))
)

(print (sub-interval first second))