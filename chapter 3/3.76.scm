#lang sicp
(define (smooth s)
  (let ((a (stream-car s)) (b (stream-car (stream-cdr s))))
    (cons-stream (/ (+ a b) 2) (smooth (stream-cdr s)))))

(define (make-zero-crossings
         input-stream last-value)
  (cons-stream
   (sign-change-detector 
    (stream-car input-stream) 
    last-value)
   (make-zero-crossings 
    (stream-cdr input-stream)
    (stream-car input-stream))))

(define (make-avg-zero-crossings input-stream last-value)
  (make-zero-crossings (smooth input-stream) last-value))

