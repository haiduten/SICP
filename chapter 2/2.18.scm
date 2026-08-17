#lang racket
(define (reverse lst)
  (define (helper iter cumm)
    (if (null? iter) cumm (helper (cdr iter) (cons (car iter) cumm))))
  (helper lst (list)))

(reverse (list 1 4 9 16 25))