#lang racket
(define (union-set-h set1 set2)
  (cond ((and (null? set1) (null? set2)) '())
        ((null? set1) set2)
        ((null? set2) set1)
        ((< (car set1) (car set2)) (cons (car set1) (union-set-h (cdr set1) set2)))
        ((= (car set1) (car set2)) (cons (car set1) (union-set-h (cdr set1) (cdr set2))))
        (else (cons (car set2) (union-set-h set1 (cdr set2))))))


(define (union-set t1 t2)
  (let ((l1 (tree->list-2 t1))
        (l2 (tree->list-2 t2)))
    (let ((l (union-set-h l1 l2)))
      (list->tree l))))


(define (intersection-set-h set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set-h
                         (cdr set1)
                         (cdr set2))))
              ((< x1 x2) (intersection-set-h
                          (cdr set1) 
                          set2))
              ((< x2 x1) (intersection-set-h
                          set1 
                          (cdr set2)))))))


(define (intersection-set t1 t2)
  (let ((l1 (tree->list-2 t1))
        (l2 (tree->list-2 t2)))
    (let ((l (intersection-set-h l1 l2)))
      (list->tree l))))


        