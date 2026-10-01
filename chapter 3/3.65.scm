#lang sicp
(define ones (cons-stream 1 ones))

(define ones-alt (cons-stream 1 (scale-stream ones-alt -1)))

(define integers
  (cons-stream 1 (add-streams integers ones)))

(define fractions
  (stream-map (lambda (x) (/ 1 x)) integers))

(define nat-ln-two
  (mul-streams ones-alt fractions))

