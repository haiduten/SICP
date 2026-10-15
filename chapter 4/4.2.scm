#lang sicp
;a
; (define x 3) will return application? true because it is a pair and it will try to evaluate it as a function application

;b
(define (application? exp) (tagged-list? exp 'call))