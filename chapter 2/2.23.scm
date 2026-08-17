#lang racket
(define (for-each prod lst)
  (if (null? lst) null (prod (car lst)))
  (if (null? lst) 
      true
      (
       for-each prod (cdr lst))
      )
  )

(for-each 
 (lambda (x) (newline) (display x))
 (list 57 321 88))