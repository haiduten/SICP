#lang racket
(define (=zero x)
  (apply-generic '=zero x))


(put '=zero ('scheme-number) (lambda (x ) (equal? x 0)))
(put '=zero ('rational) (lambda (x) (equal? (numer x) 0)))
(put '=zero ('complex) (lambda (x) (equal? (magnitude x) 0)))