#lang sicp
; what if the operator is the result of an argument? Then it would need to be forced
(define (call-fnc-with-two fn) (fn 2))