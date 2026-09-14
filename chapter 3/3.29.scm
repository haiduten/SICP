#lang sicp
(define (or-gate s1 s2 output)
  (let ((c1 (make-wire))
        (c2 (make-wire))
        (final (make-wire))
    (inverter s1 c1)
    (inverter s2 c2)
    (and-gate c1 c2 final)
    (inverter final output)
    'ok))

; the delay time is 2x inverter-delay (the first two happen concurrently) and 1x and-gate-dealy