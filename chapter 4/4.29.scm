#lang sicp
(define (square x) (* x x))

;;; L-Eval input:
(square (id 10))

;;; L-Eval value:
;memoized: 100
;not memoized: 100
;;; L-Eval input:
count


;;; L-Eval value:
;memoized: 1
;not memoized: 2