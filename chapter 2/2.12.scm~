#lang racket
(define (make-interval a b) (cons a b))

(define (upper-bound int) (cdr int))

(define (lower-bound int) (car int))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) 
        (upper-bound i)) 
     2))

(define (width i)
  (/ (- (upper-bound i) 
        (lower-bound i)) 
     2))

(define (percent i)
  (let (
        (width (width i))
        (center (center i))
       )
   (* (/ width center) 100)))

(define first (make-interval 0 10))
(percent first)

(define (make-center-percent center percent)
  (let (
        (lb (- center (* center (/ percent 100))))
        (ub (+ center (* center (/ percent 100))))
       )
    (make-interval lb ub)))

(define x (make-center-percent 5 100))
(define (print x)
  (display (lower-bound x))
  (newline)
  (display (upper-bound x))
)

(print x)