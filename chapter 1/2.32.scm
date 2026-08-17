#lang racket
(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) 
            (append (cdr list1) 
                    list2))))

(define (subsets s)
  (if (null? s)
      (list null)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (append (list (car s)) x )) rest)))))

(subsets (list 1 2 3))