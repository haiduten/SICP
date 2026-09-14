#lang sicp

(define (isLoop x)
  (define (getNthElt x n)
    (cond ((null? x) false)
          ((= n 0) x)
          (else (getNthElt (cdr x) (- n 1)))))
  (define (helper x n)
    (let ((nthElt (getNthElt x n)))
      (cond ((not nthElt) false)
            ((eq? nthElt x) true)
            (else (helper nthElt (+ n 1))))))
  (helper x 1))
    

(define nonLoop '(1 2 3))

(isLoop nonLoop)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c)))
(isLoop z)