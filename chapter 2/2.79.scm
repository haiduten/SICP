#lang racket
(define (equ? x y)
  (apply-generic 'equ? x y))


(put 'equ '(scheme-number scheme-number) (lambda (x y) (equal? x y)))
(put 'equ '(rational rational) (lambda (x y) (and (equal? (numer x) (numer y)) (equal? (denom x) (denom y)))))
(put 'equ '(complex complex) (lambda (x y) (and (equal? (real-part x) (real-part y)) (equal? (imag-part x) (imag-part y)))))