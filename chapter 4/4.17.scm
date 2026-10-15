#lang sicp
; when definitions are evaluated sequentally:
; global frame
;   function frame (bindings: vars, u v)


; when definitions are scanned
; global frame
;  function frame (bindings: vars)
;    inner lambda frame (bindings: u v)

;there is an extra frame because the let is a
; derived expression of a lambda statement

; in a correct program the internal defitions are all defined before any are called.
; so the two are equivalent

; when scanning add the define variables to the variables in make-procedure and
; add the set! statements to the body and add unassigned to the vals

