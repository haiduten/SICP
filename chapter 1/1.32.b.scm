#lang racket

(define (accumulate 
 combiner null-value term a next b)
  (define (accumulate-iter a total)
    (
     if (> a b)
        total
        (accumulate-iter (next a) (combiner (term a) total))))
  (accumulate-iter a null-value))

(define (id x) x)
(define (inc x) (+ x 1))
(define (square x) (* x x))

(define (product term a next b)
  (accumulate * 1 term a next b))

(define (factorial n) (product id 1 inc n))

(factorial 5)

(define (even x) (* 2 x))
(define (odd x) (+ (* 2 x) 1))


(* 8.0 (even 1000) (/ (square (product even 2 inc 1000)) (square (product odd 1 inc 1000))))