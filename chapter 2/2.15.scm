#lang racket
; she is right. our method will treat repeated variables as different instances
; and not realize they are the same

(define (make-interval a b) (cons a b))

(define (upper-bound int) (cdr int))

(define (lower-bound int) (car int))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) 
        (upper-bound i)) 
     2))


(define (div-interval x y)
  (mul-interval x 
                (make-interval 
                 (/ 1.0 (upper-bound y)) 
                 (/ 1.0 (lower-bound y)))))


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

(define (add-interval x y)
  (make-interval (+ (lower-bound x) 
                    (lower-bound y))
                 (+ (upper-bound x) 
                    (upper-bound y))))

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


(define (par1 r1 r2)
  (div-interval 
   (mul-interval r1 r2)
   (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval 
     one
     (add-interval 
      (div-interval one r1) 
      (div-interval one r2)))))


(define A (make-center-percent 5 0.0005))
(center (par1 A A))
(center (par2 A A))
