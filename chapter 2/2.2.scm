#lang racket
(define (make-point x y)
  (cons x y))

(define (average n m) (/ (+ n m) 2))

(define (x-point point) (car point))
(define (y-point point) (cdr point))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))


(define (make-segment p1 p2)
  (cons p1 p2))

(define (start-segment point) (car point))
(define (end-segment point) (cdr point))


(define (midpoint-segment line)
  (let ((x1 (x-point (start-segment line)))
       (x2 (x-point (end-segment line)))
       (y1 (y-point (start-segment line)))
       (y2 (y-point (end-segment line))))
    (make-point (average x1 x2) (average y1 y2))))


(define p1 (make-point 0 0))
(define p2 (make-point 10 10))
(define line (make-segment p1 p2))
(print-point (midpoint-segment line))