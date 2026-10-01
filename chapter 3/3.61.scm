#lang sicp
(define (invert-unit-series S)
  (define inverse
    (cons-stream 1 (scale-stream (mul-series (stream-cdr S) inverse) -1)))
  inverse)