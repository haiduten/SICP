#lang sicp
(define (factorial n)
  (unless (= n 1)
          (* n (factorial (- n 1)))
          1))

; if run (factorial 5) in a applicative order. We must always
; evaluate (factorial (- n 1)). So it would loop infinitely

; if we run it in a normal order, it would run as expected and return 5!