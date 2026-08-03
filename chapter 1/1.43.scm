#lang racket
(define (compose g f) (lambda (x) (g (f x))))
(define (square x) (* x x))

(define (repeated f n)
  (define (helper acc n)
    (if (= n 0) acc (helper (compose f acc) (- n 1))))
  (helper (lambda (x) x) n))



((repeated square 2) 5)