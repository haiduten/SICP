#lang sicp
(define (run-forever)
  (run-forever))

(define (try p)
  (if (halts? p p)
      (run-forever)
      'halted))

;evaluate  (try try)
; which would leads us to (halts? try try)

; if (try try) returns a value, then (halts? try try) would be true and we would return run-forever.
; So (try try) would run forever. This contradicts our hypothesis that (try try) returns a value

; if (try try) does not return a value, then (halts? try try) would be false and we would return 'halted.
; but then (try try) returns halted and that contradicts our hypothesis that (try try) does not return a valeu

; In either case, it leads to a contradiction. So halt? cannot exist
