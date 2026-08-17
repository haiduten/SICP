#lang racket

(define (reverse lst)
  (define (helper iter cumm)
    (if (null? iter) cumm (helper (cdr iter) (cons (car iter) cumm))))
  (helper lst (list)))


(define (same-parity x . y)
  (define (helper acc lst)
    (let ((xParity (remainder x 2)))
      (cond ((null? lst) acc)
            ((= xParity (remainder (car lst) 2)) (helper (cons (car lst) acc) (cdr lst)))
            (else (helper acc (cdr lst))))))
  (reverse (helper (list) (cons x y))))


(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)