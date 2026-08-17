#lang racket
(define zero (lambda (f) (lambda (x) x)))



(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))


;1
;(add-1 zero)
; (lambda (f) (lambda (x) (f ((zero f) x)))))
; (lambda (f) (lambda (x) (f x))))

;2
; (lambda (f) (lambda (x) (f (f x))))

(define (add n m)
  (lambda (f) (lambda (x) ((m f) ((n f) x)))))
