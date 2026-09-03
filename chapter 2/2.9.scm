#lang racket
(define (make-interval a b) (cons a b))

(define (upper-bound int) (cdr int))

(define (lower-bound int) (car int))


(define (width a) (/ (- (upper-bound a) (lower-bound a)) 2.0))

(define (width-add a b)
  (+ (width a) (width b)))

;not true for multiplication. Take the intervals (0, 1) and (100, 100). Mutiply you get (0, 100) with a width of 50

; Take the intervals (0, 1) and (1, 1). Multiply and you get (0, 1) with a width of 0.5.

;Notice that (100, 100) and (1,1) have the same widths. So width alone is not enough to calculate width for mutiplication/division