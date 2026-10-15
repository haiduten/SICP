#lang racket
; if statements now are of this form (list predicate, alternate, consequent, 'if)

(define (if? exp) (eq? 'if (cadddr exp)))
(define (if-predicate exp) (car exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cadr exp)))
      (cadr exp)
      'false))