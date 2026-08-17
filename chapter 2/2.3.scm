#lang racket
(define (make-point x y)
  (cons x y))

(define (x-point point) (car point))
(define (y-point point) (cdr point))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-rect p1 p2 p3 p4) (cons (cons p1 p2) (cons p3 p4)))

(define (top-left rec) (car (car rec)))
(define (top-right rec) (cdr (car rec)))
(define (bottom-left rec) (car (cdr rec)))
(define (bottom-right rec) (cdr (cdr rec)))

(define (width rec) (- (x-point (top-right rec)) (x-point (top-left rec))))
(define (height rec) (- (y-point (top-right rec)) (y-point (bottom-right rec))))

(define (perimeter rec) (+ (* 2 (width rec)) (* 2 (height rec))))
(define (area rec) (* (width rec) (height rec)))


(define p1 (make-point 0 10))
(define p2 (make-point 10 10))
(define p3 (make-point 10 0))
(define p4 (make-point 0 0))

(define rec (make-rect p1 p2 p3 p4))
(perimeter rec)
(area rec)

(define (make-segment p1 p2)
  (cons p1 p2))

(define (start-segment point) (car point))
(define (end-segment point) (cdr point))

(define (make-rect-2 segment)
  (let ((x1 (x-point (start-segment line)))
       (x2 (x-point (end-segment line)))
       (y1 (y-point (start-segment line)))
       (y2 (y-point (end-segment line))))
    (cons (cons (make-point x1 y1) (make-point x2 y1)) (cons (make-point x2 y2) (make-point x1 y2)))))

(define q1 (make-point 0 10))
(define q2 (make-point 10 0))
(define line (make-segment q1 q2))

(define rec2 (make-rect-2 line))
(perimeter rec2)
(area rec2)