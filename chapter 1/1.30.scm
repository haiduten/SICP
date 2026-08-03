#lang racket
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(define (cube x) (* x x x))

(define (inc n) (+ n 1))

(define (integral f a b n)
  (define h (/ (- b a) n))
  (define (y k) (f (+ a (* k h))))
  (define (simpson k) (
    if (= (remainder k 2) 0)
       (* 2 (y k))
       (* 4 (y k))))
  (* (/ h 3) (+ (sum simpson 1 inc (- n 1)) (y 0) (y n))))

(integral cube 0 1 100.0)
(integral cube 0 1 1000.0)  