#lang sicp
(define (RLC R L C dt)
  (lambda (v0 i0)
    (define vc (integral (delay dvc) v0 dt))
    (define il (integral (delay dil) i0 dt))
    (define dvc (scale-stream il (/ -1 C)))
    (define dil (add-streams (scale-stream vc (/ 1 L)) (scale-stream il (/ (* -1 R) L))))
    (cons vc il)))
    