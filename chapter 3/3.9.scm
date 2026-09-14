#lang racket
(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

; (factorial 6)
;Create E1 with where n : 6. The body is (* 6 (factorial 5))
; Need to evaluate factorial 5

;Create E2 where n : 5 The body is (* 5 (factorial 4))
; Need to evaluate factorial 3=4

;Create E3 where n : 4 The body is (* 4 (factorial 3))
; Need to evaluate factorial 3

;Create E4 where n : 3 The body is (* 3 (factorial 2))
; Need to evaluate factorial 2

;Create E5 where n : 2 The body is (* 2 (factorial 1))
; Need to evaluate factorial 1

; Create E5 where n: 1. The body is 1

(define (factorial n)
  (fact-iter 1 1 n))

(define (fact-iter product 
                   counter 
                   max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))

; (factorial 6)
;Create E1 with where n : 6. The body is (fact-iter 1 1 6)
;Look in global env for fact-iter and  evaluate (fact-iter 1 1 6).

;Create E2 where product : 1, counter: 1, max-count: 6. The body is (fact-iter 1 2 6)
; evaluate (fact-iter 1 2 6)

;Create E3 where product : 1, counter : 2, max-count : 6. The body is (fact-iter 2 3 6)
;evaluate (fact-iter 2 3 6)

;Create E4 where product : 2, counter : 3, max-count : 6. the body is (fact-ter 6 4 6)
;evaluate (fact-ter 6 4 6)

;Create E5 where product : 6, counter : 4, max-count : 6. the body is (fact-ter 24 5 6)
;evaluate (fact-ter 24 5 6)

;Create E6 where product : 24, counter : 5, max-count : 6. the body is (fact-ter 120 6 6)
;evaluate (fact-ter 120 6 6)

;Create E7 where product : 120, counter : 6, max-count : 6. the body is (fact-ter 720 7 6)
;evaluate (fact-ter 720 7 6)

;Create E8 where product : 720, counter 7, max-count 6, the body is 720