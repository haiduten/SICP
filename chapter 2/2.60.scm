#lang racket
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
      (cons x set))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) 
         '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) 
                                 set2)))
        (else (intersection-set (cdr set1) 
                                set2))))

(define (union-set set1 set2)
  (append set1 set2))

; element-of-set and intersection-set is slower because you have more elements you must check
; adjoin-set is faster it is just constant time
; union-set is faster if the list is not much larger than the set version
; In applications, where adjoin-set and union-set is used frequently and the number of elements are not too large then use the duplicate one