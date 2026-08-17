#lang racket
(define (make-interval a b) (cons a b))

(define (upper-bound int) (cdr int))

(define (lower-bound int) (car int))


(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) 
               (lower-bound y)))
        (p2 (* (lower-bound x) 
               (upper-bound y)))
        (p3 (* (upper-bound x) 
               (lower-bound y)))
        (p4 (* (upper-bound x) 
               (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))


(define (div-interval x y)
  (if (or (= (upper-bound y) 0) (= (lower-bound y) 0)) (error "Cant divide by zero") 
  (mul-interval x 
                (make-interval 
                 (/ 1.0 (upper-bound y)) 
                 (/ 1.0 (lower-bound y))))))



(define first (make-interval 5 10))
(define second (make-interval 0 2))

(div-interval first second)