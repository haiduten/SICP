#lang racket
(define (deep-reverse lst)
  (define (helper iter cumm)
    (if (null? iter) cumm (helper (cdr iter) (cons (deep-reverse (car iter)) cumm))))
  (if (pair? lst) (helper lst (list)) lst))


(define x 
  (list (list 1 2) (list 3 4)))

(deep-reverse x)