#lang racket

;a because they do not have a type tag

(define operation-table (make-hash))

(define (put op type item)
  (hash-set! operation-table (list op type) item))

(define (get op type)
  (hash-ref operation-table
            (list op type)
            #f))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))


(define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp) 
           (if (same-variable? exp var) 
               1 
               0))
         (else ((get (operator exp) 'deriv) 
                (operands exp) 
                var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))


(define (install-add-package)
  
  (define (addend s) (car s))

(define (augend s) (if (null? (cdr s)) 0 (cons '+ (cdr s))))

(define (make-sum a1 a2)

  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) 
         (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-sum-derive operands var)
  (make-sum (deriv (addend operands) var) (deriv (augend operands) var)))


(put '+ 'deriv make-sum-derive))

(install-add-package)

(define (install-multiply-package)
  (define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) 
         (+ a1 a2))
        (else (list '+ a1 a2))))
  
  (define (multiplier s) (car s))

(define (multiplicand s) (if (null? (cddr s)) (cadr s) (cons '* (cdr s))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) 
             (=number? m2 0)) 
         0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) 
         (* m1 m2))
        (else (list '* m1 m2))))

(define (make-product-derive operands var)
  (make-sum
          (make-product 
           (multiplier operands)
           (deriv (multiplicand operands) var))
          (make-product 
           (deriv (multiplier operands) var)
           (multiplicand operands))))


(put '* 'deriv make-product-derive))
(install-multiply-package)

(define (install-exp-package)
  (define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base p) (car p))

(define (exponent p) (cadr p))

  (define (make-product m1 m2)
  (cond ((or (=number? m1 0) 
             (=number? m2 0)) 
         0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) 
         (* m1 m2))
        (else (list '* m1 m2))))
  
  (define (make-exponentiation m1 m2)
    (cond 
        ((=number? m2 0) 1)
        ((=number? m2 1) m1)
        (else (list '** m1 m2))))
  (define (make-exp-derive operands var)
    (make-product (exponent operands)
                   (make-exponentiation (base operands) (- (exponent operands) 1))))

  


(put '** 'deriv make-exp-derive))
(install-exp-package)



(deriv '(* x 2) 'x)
(deriv '(* x y (+ x 3)) 'x)
(deriv '(+ x x x x) 'x)
(deriv '(** x 3) 'x)
