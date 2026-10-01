#lang sicp
(define (mul-series s1 s2)
  (cons-stream (* (car-stream s1) (car-stream s2)) (add-streams (scale-stream (cdr-stream s2) (car-stream s1)) (mul-series (cdr-stream s1) s2))))