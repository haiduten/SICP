#lang racket
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (union-set set1 set2)
  (cond ((and (null? set1) (null? set2)) '())
        ((null? set1) set2)
        ((element-of-set? (car set1) set2) (union-set (cdr set1) set2))
        (else (cons (car set1) (union-set (cdr set1) set2)))))

(union-set (list 1 2 3) (list 4 5 6))
(union-set (list 1 4 3) (list 4 5 6))