#lang racket
(define dx 0.00001)

(define (smooth f) (lambda (x) (/ (+ (f (+ x dx)) (f x) (f (- x dx))) 3.0)))

(define (repeated f n)
  (define (helper acc n)
    (if (= n 0) acc (helper (compose f acc) (- n 1))))
  (helper (lambda (x) x) n))

(define (compose g f) (lambda (x) (g (f x))))

(define (n-smooth f n) (repeated smooth n))