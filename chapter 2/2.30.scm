#lang racket
(define (square n) (* n n ))

(define (square-tree tree)
  (cond
    ((null? tree) null)
    ((not (pair? tree)) (square tree))
    (else (cons (square-tree (car tree)) (square-tree (cdr tree))))))

(square-tree (list 1 
                  (list 2 (list 3 4) 5) 
                  (list 6 7))
           )


(define (map proc items)
  (if (null? items)
      null
      (cons (proc (car items))
            (map proc (cdr items)))))


(define (square-tree-2 tree)
  (map (lambda (x) (if (not (pair? x)) (square x) (square-tree-2 x))) tree))

(square-tree-2 (list 1 
                  (list 2 (list 3 4) 5) 
                  (list 6 7))
           )