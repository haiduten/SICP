#lang racket
(define (lastpair lst)
  (if (null? (cdr lst)) (car lst) (lastpair (cdr lst))))

(last-pair (list 23 72 149 34))