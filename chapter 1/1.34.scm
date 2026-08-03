#lang racket
;error 2 is not a function

(define (f g) (g 2))

(f f)