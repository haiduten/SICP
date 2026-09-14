#lang sicp
(define (count-pairs lst)
  (define (contains lst item)
    (if (null? lst) false
        (if (eq? (car lst) item) true (contains (cdr lst) item))))
  (let ((visited '()))
    (begin
      (define (helper x)
      (cond ((contains visited x) 0)
            ((not (pair? x)) 0)
            (else (begin
                    (set! visited (cons x visited))
                    (+ (helper (car x))
                       (helper (cdr x))
                       1)))))
      (helper lst))))

(count-pairs (list 'a 'b 'c))

(define four (list 'a 'b 'c))
(set-car! (cdr four) (cdr (cdr four)))
(count-pairs four)

(define seven (list 'a 'b 'c))
(set-car! (cdr seven) (cdr (cdr seven)))
(set-car! seven (cdr seven))
(count-pairs seven)


(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c)))

(count-pairs z)