#lang sicp
(define (isCycle x)
  (define (contains lst item)
    (if (null? lst) false
        (if (eq? (car lst) item) true (contains (cdr lst) item))))
  (let ((memory '()))
    (begin
      (define (helper y)
        (cond ((contains memory y) true)
              ((null? y) false)
              (else (begin (
                            set! memory (cons y memory))
                           (helper (cdr y))))))
      (helper x))))

(isCycle '(1 2 3))


(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c)))

(isCycle z)