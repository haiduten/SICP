#lang sicp
; unless is the equivalent of if pred exceptional-value usual-value 

(define (unless->if exp)
  (let ((pred (cadr exp))
        (usual (caddr exp))
        (exceptional (cadddr exp)))
    (make-if pred exceptional usual)))

; Say your unless generates a list. You may want it as a procedure to
; pass directly into map/filter/reduce

; Say you want to partially apply the arguments, then a procedure is useful. 