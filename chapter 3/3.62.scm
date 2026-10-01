#lang sicp
(define (invert-series S)
  (define inverse
    (if (= (stream-car S) 0) (error "Cannot divide by zero")
    (scale-stream (cons-stream 1 (scale-stream (mul-series (stream-cdr S) inverse) -1)) (/ 1 (stream-car S)))))
  inverse)


(define (div-series s1 s2)
  (let ((s2-inverse (invert-series s2)))
    (mul-series s1 s2-inverse)))