#lang racket
(define (make-interval a b) (cons a b))

(define (upper-bound int) (cdr int))

(define (lower-bound int) (car int))