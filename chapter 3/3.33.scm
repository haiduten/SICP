#lang sicp
(define (average a b c)
  (let ((half (make-connector))
        (sum (make-connector)))
    (adder a b sum)
    (constant 0.5 half)
    (multiplier sum half c)
    'ok))