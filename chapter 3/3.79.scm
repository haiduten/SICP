#lang sicp
(define (solve-2nd f dt y0 dy0)
  (define y (integral (delay dy) y0 dt))
  (define dy
    (let ((ddy (delay (stream-map f dy y))))
      (integral ddy dy0 dt)))
  y)