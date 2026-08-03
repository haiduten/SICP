#lang racket
(define (id x) x)
(define (inc x) (+ x 1))
(define (square x) (* x x))

(define (product term a next b)
  (define (product-iter a total)
    (
     if (> a b)
        total
        (product-iter (next a) (* (term a) total))))
  (product-iter a 1)

  )

(define (factorial n) (product id 1 inc n))

(factorial 5)

(define (even x) (* 2 x))
(define (odd x) (+ (* 2 x) 1))

(* 8.0 (even 1000) (/ (square (product even 2 inc 1000)) (square (product odd 1 inc 1000))))