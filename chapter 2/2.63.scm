#lang racket
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append 
       (tree->list-1 
        (left-branch tree))
       (cons (entry tree)
             (tree->list-1 
              (right-branch tree))))))


; (1 3 5 7 9 11)
; (1 3 5 7 9 11)
; (1 3 5 7 9 11)


(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list 
         (left-branch tree)
         (cons (entry tree)
               (copy-to-list 
                (right-branch tree)
                result-list)))))
  (copy-to-list tree '()))

(copy-to-list tree '())
; (1 3 5 7 9 11)
; (1 3 5 7 9 11)
; (1 3 5 7 9 11)

; the first one you have to append two lists at each step so it becomes nlogn 

