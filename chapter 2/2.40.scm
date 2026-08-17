#lang racket

(define (enumerate n)
  (if (= n 0) null (cons n (enumerate (- n 1)))))

(define (unique-pairs n)
  (append-map (lambda (i) (map (lambda (j) (list i j)) (enumerate (- i 1)))) (enumerate n)))


(define (make-pair-sum pair)
  (list (car pair) 
        (cadr pair) 
        (+ (car pair) (cadr pair))))

(define (square n) (* n n))

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

(define (prime? n)
  (= n (smallest-divisor n)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (flatmap proc seq)
  (foldr append null (map proc seq)))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter 
        prime-sum?
        (unique-pairs n))))

(prime-sum-pairs 6)
