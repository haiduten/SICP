#lang sicp
(define (c- x y)
  (let ((negy (make-connector))
        (z (make-connector)))
    (multiplier y (cv -1) negy)
    (adder x negy z)
    z))

(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))

(define (c/ x y)
  (let ((yinverse (make-connector))
        (z (make-connector)))
    (multiplier y yinverse (cv 1))
    (multiplier x yinverse z)
    z))


(define (cv value)
  (let ((z (make-connector)))
    (constant value z)
    z))
