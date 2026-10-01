#lang sicp
(define (stream-limit s limit)
  (let ((s1 (stream-car s)) (s2 (stream-car (stream-cdr s))))
     (if (< (abs (- s1 s2)) limit) s2 (stream-limit (stream-cdr s) limit))))