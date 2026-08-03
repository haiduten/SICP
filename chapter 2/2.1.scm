#lang racket
(define (make-rat n d)
  (let ((denum (if (< d 0) (* -1 d) d)) (num (if (< n 0) (* -1 n) n)) (isPos (if (> (* d n) 0) true false)))
    
  (let ((g (gcd num denum))) 
    (cons (/ (if isPos num (* -1 num)) g) 
          (/ denum g)))))

(define x (make-rat 3 2))
(define y (make-rat -3 2))
(define z (make-rat 3 -2))
(define w (make-rat -3 -2))

(define (numer x) (car x))
(define (denom x) (cdr x))


(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(print-rat x)
(print-rat y)
(print-rat z)
(print-rat w)