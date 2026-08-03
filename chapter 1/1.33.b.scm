#lang racket
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (filtered-accumulate 
 filter combiner null-value term a next b)
  (if (> a b) null-value
      (if (filter a)
       (combiner (term a) (filtered-accumulate filter combiner null-value term (next a) next b))
       (filtered-accumulate filter combiner null-value term (next a) next b))))

(define (product-rel-prime n)
  (define (id x) x)
  (define (inc x) (+ x 1))
  (define (rel-prime x)
    (= (gcd x n) 1))
  (filtered-accumulate rel-prime * 1 id 1 inc n))

(product-rel-prime 10)