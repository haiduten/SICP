#lang racket

(define (no-more? list)
  (if (null? list) true false))

(define (except-first-denomination coins) (cdr coins))

(define (first-denomination coins) (car coins))

(define (cc amount coin-values)
  (cond ((= amount 0) 
         1)
        ((or (< amount 0) 
             (no-more? coin-values)) 
         0)
        (else
         (+ (cc 
             amount
             (except-first-denomination 
              coin-values))
            (cc 
             (- amount
                (first-denomination 
                 coin-values))
             coin-values)))))


(define us-coins 
  (list 50 25 10 5 1))
(cc 100 us-coins)

;the order does not affect the answer