#lang racket
(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) 
         n)
        ((divides? test-divisor n) 
         test-divisor)
        (else (find-divisor 
               n 
               (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder 
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder 
          (* base (expmod base (- exp 1) m))
          m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) 
         (fast-prime? n (- times 1)))
        (else false)))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (square n) (* n n))


(define (filtered-accumulate 
 filter combiner null-value term a next b)
  (if (> a b) null-value
      (if (filter a)
       (combiner (term a) (filtered-accumulate filter combiner null-value term (next a) next b))
       (filtered-accumulate filter combiner null-value term (next a) next b))))

(define (inc n) (+ n 1))

(define (sum-prime-square n)
  (filtered-accumulate prime? + 0 square 2 inc n))

(sum-prime-square 10)




       