#lang sicp
; (define (squarer a b) (multiplier a a b))
; the flaw is what happens if call set-value b to 25
; it will not set a to 5.
; multiplier will check    ((and (has-value? product) (has-value? m1)) or ((and (has-value? product) (has-value? m1))
; and these conditions will be unmet so a will not be constrained even after b has a value

