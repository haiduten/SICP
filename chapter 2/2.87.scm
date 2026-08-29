#lang racket
(define (zero-coeffs x)
  (if (empty-termlist? x) true
      (let ((first (first-term x))
            (rest (rest-term x))
            )
        (if (=zero (coeff first)) (zero-coeffs rest) false)))) 

(put '=zero '(polynomial) (lambda (x) (or (empty-termlist? (term-list x)) (zero-coeffs (term-list x))))) 