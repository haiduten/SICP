#lang sicp
(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) 
           den 
           radix)))
; this is the division of two numbers

;(1 4 2 8
; (expand 1 7 10) (1,  expand(3 7 10))
; expand(3 7 10) (4, (expand(2 7 10))
; (expand(2 7 10) (2, (expand(6, 7 10))
; (expand(6 7 10) (8, (epand(4, 7, 10))

;(3 7
; (expand 3 8 10) (3, (expand 6 8 10)
; (expand 6 8 10) (7, (expand 4 8 10)