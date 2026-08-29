#lang racket

(define (install-raise)
(define (raise-integer n)
  (make-rational  n 1))

(define (raise-rational n)
  (make-real (/ (numer n) (denom n)))

(define (raise-real n)
  (make-from-real-imag n 0))

(put 'raise '(integer) raise-integer)
(put 'raise '(rational) raise-rational)
(put 'raise '(real) raise-real)
)

(install-raise)

(define (raise x)
  (apply-generic 'raise x))