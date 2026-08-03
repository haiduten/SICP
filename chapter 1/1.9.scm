#lang racket
; this one below is a recursive process
(define (+ a b)
  (if (= a 0) 
      b 
      (inc (+ (dec a) b))))

;this one below is an iterative process
(define (+ a b)
  (if (= a 0) 
      b 
      (+ (dec a) (inc b))))
