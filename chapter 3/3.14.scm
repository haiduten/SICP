#lang sicp
(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

; the mystery is that it reverses the list

(define v (list 'a 'b 'c 'd))

(define w (mystery v))

v
;(a)

w
;(d c b a)