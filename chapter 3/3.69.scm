#lang sicp
(define (triplets s t u)
  (let ((new-pairs (pairs t u)))
   (cons-stream
   (list (stream-car s) (stream-car t) (stream-car u))
   (interleave
    (stream-map (lambda (x) 
                  (cons (stream-car s) x))
                (stream-cdr new-pairs))
    (triplets (stream-cdr s) (stream-cdr t) (stream-cdr u))))))

(define pythagorus
  (stream-filter (lambda (triplet) (= (+ (squared (car triplet)) (squared (cadr triplet))) (squared (caddr triplet)))) (triplets integers integers integers)))
  