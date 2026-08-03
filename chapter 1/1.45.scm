#lang racket
(define (average x y) (/ (+ x y) 2))
(define (compose g f) (lambda (x) (g (f x))))

(define (repeated f n)
  (define (helper acc n)
    (if (= n 0) acc (helper (compose f acc) (- n 1))))
  (helper (lambda (x) x) n))


(define (average-damp f)
  (lambda (x) 
    (average x (f x))))

(define (square x) (* x x))

(define (nth-power x n)
  ((repeated (lambda (y) (* y x)) n) x))

(define (average-damp-ith f i)
  (lambda (x) ((repeated (lambda (y) (/ (+ y x) 2)) i) (f x)))) 

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) 
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (fixed-point-of-transform 
         g transform guess)
  (fixed-point (transform g) guess))

(define dx 0.00001)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) 
            ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) 
               guess))


(define (sqrt x)
  (fixed-point 
   (lambda (y) (average y (/ x y)))
   1.0))


(define (nth-rooft x n)
  (fixed-point 
   (lambda (y) (average y (/ x (nth-power y (- n 1)))))
   12.0))

(define (nth-root x n)
  (fixed-point 
   (average-damp-ith (lambda (y) (/ x (nth-power y (- n 1)))) n)
   12.0))


(nth-root 32 4)
