#lang racket
(define (equ? x y)
  (apply-generic 'equ? x y))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) 
         1)
        ((divides? test-divisor n) 
         test-divisor)
        (else (find-divisor 
               n 
               (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (make-rational-from-real n)
  (let ((divisor (round (smallest-divisor n))))
    (let ((a (round (/ n divisor))))
      (make-rational a divisor))))

(put 'equ '(scheme-number scheme-number) (lambda (x y) (equal? x y)))
(put 'equ '(rational rational) (lambda (x y) (and (equal? (numer x) (numer y)) (equal? (denom x) (denom y)))))
(put 'equ '(complex complex) (lambda (x y) (and (equal? (real-part x) (real-part y)) (equal? (imag-part x) (imag-part y)))))

(put 'project '(complex) (lambda (x) (make-real (real-part x))))
(put 'project '(real) (lambda (x) (make-rational-from-real x)))
(put 'project '(rational) (lambda (x) (round (/ (numer x) (denom x)))))

(define (project x) (apply-generic' 'project x))

(define (drop x)
  (if (get 'project (list (type-tag x)))
  (let ((projection (project x)))
        (if (equ? (raise projection) x) (drop projection) x)
       x))
  x))

(define (typelessthan a1 a2)
  (define (equalRaise a1 a2)
    (if (equal? (type-tag a1) (type-tag a2)) true
        (let ((proc (get 'raise (type-tag a1)))
          (if proc (equalRaise (proc (contents a1)) a2) false))))
  (if (equal? (type-tag a1) (type-tag a2)) false (equalRaise a1 a2)))

(define (apply-generic' op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                  (cond ((equal? type1 type2) (error "No method for these types"))
                        ((typelessthan a1 a2)
                         (apply-generic 
                          op (raise a1) a2))
                        ((typelessthan a2 a1)
                         (apply-generic' 
                          op a1 (raise a2)))
                        (else
                         (error 
                          "No method for 
                           these types"
                          (list 
                           op 
                           type-tags))))))
              (error 
               "No method for these types"
               (list op type-tags))))))
  
(define (apply-generic op . args)
 (let ((result apply-generic' op . args))
   (drop result)))