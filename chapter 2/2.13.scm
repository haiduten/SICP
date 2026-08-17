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


(define (make-center-percent center percent)
  (let (
        (lb (- center (* center (/ percent 100))))
        (ub (+ center (* center (/ percent 100))))
       )
    (make-interval lb ub)))


(define (print x)
  (display (lower-bound x))
  (newline)
  (display (upper-bound x))
)

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


(define x (make-center-percent 5 0.5))
(define y (make-center-percent 5 1.0))
(define z (mul-interval x y))
(width z)
(percent z)

(define (mul-percentage x y)
  (let ((perx (percent x)) (pery (percent y)))
    (+ perx pery)))

(mul-percentage x y)