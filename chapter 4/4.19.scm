#lang sicp
(let ((a 1))
  (define (f x)
    (define b (+ a x))
    (define a 5)
    (+ a b))
  (f 10))
; i support alyssa's view
; to do eva's view. we would use the scan method
; but we would change the order we call set!
; we would start with set! that have no dependencies
; and then go to ones that do have dependencies. like
; a topological sort 