#lang racket
(define (make-accumulator n)
  (let ((amount n))
    (lambda (x)
      (begin
        (set! amount (+ amount x))
        amount))))

(define A (make-accumulator 5))
(A 10)
(A 10)