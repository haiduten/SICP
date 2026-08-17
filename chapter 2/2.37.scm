#lang racket
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op 
                      initial 
                      (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      null
      (cons (accumulate op init (map (lambda (seq) (car seq)) seqs))
            (accumulate-n op init (map (lambda (seq) (cdr seq)) seqs)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define v (list 1 1 1 1))
(define w (list 1 2 3 4))
(dot-product v w)

(define m (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))

(define (matrix-*-vector m v)
  (map (lambda (seq) (dot-product seq v)) m))

(matrix-*-vector m v)

(define (transpose mat)
  (accumulate-n cons null mat))

(transpose m)

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (map (lambda (col) (dot-product row col)) cols)) m)))

(define p (list (list 1 2 3) (list 4 5 6) (list 6 7 8)))
(matrix-*-matrix  p p)