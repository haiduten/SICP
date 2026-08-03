#lang racket
(define (inc x) (+ x 1))
(define (square x) (* x x))
(define (compose g f) (lambda (x) (g (f x))))
((compose square inc) 6)