#lang racket

;; Operation table
(define operation-table (make-hash))

(define (put operation type-signature procedure)
  (hash-set! operation-table
             (list operation type-signature)
             procedure))

(define (get operation type-signature)
  (hash-ref operation-table
            (list operation type-signature)
            #f))


(define (type-tag datum)
  (cond ((number? datum)
         'scheme-number)
        ((pair? datum)
         (car datum))
        (else
         (error "Bad tagged datum -- TYPE-TAG" datum))))

(define (contents datum)
  (cond ((number? datum)
         datum)
        ((pair? datum)
         (cdr datum))
        (else
         (error "Bad tagged datum -- CONTENTS" datum))))

(define (typelessthan a1 a2)
  (define (equalRaise a1 a2)
    (if (equal? (type-tag a1) (type-tag a2)) true
        (let ((proc (get 'raise (list (type-tag a1)))))
          (if proc (equalRaise (proc (contents a1)) a2) false))))
  (if (equal? (type-tag a1) (type-tag a2)) false (equalRaise a1 a2)))

(put 'div '(scheme-number scheme-number) (lambda (x y) (/ x y)))

(define (apply-generic op . args)
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
                         (apply-generic 
                          op a1 (raise a2)))
                        (else
                         (error 
                          "No method for 
                           these types"
                          (list 
                           op 
                           type-tags)))))
              (error 
               "No method for these types"
               (list op type-tags)))))))
(define (attach-tag type-tag contents)
  (if (eq? type-tag 'scheme-number)
      contents
      (cons type-tag contents)))

(define (rational-number-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (greatest-common-divisor n d)))
      (cons (div n g) (div d g))))
  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
                 (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (sub (mul (numer x) (denom y))
                 (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
              (mul (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (mul (numer x) (denom y))
              (mul (denom x) (numer y))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d)))))

(rational-number-package)

(define (make-rational n d)
  ((get 'make 'rational) n d))


(define (=zero? x)
  (apply-generic '=zero? x))

(define (add x y)
  (apply-generic 'add x y))

(define (sub x y)
  (apply-generic 'sub x y))

(put 'add '(scheme-number scheme-number) (lambda (x y) (+ x y)))

(define (mul x y)
  (apply-generic 'mul x y))

(put 'mul '(scheme-number scheme-number) (lambda (x y) (* x y)))


(put '=zero? '(scheme-number) (lambda (x ) (equal? x 0)))
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

  ;; representation of terms and term lists
(define (adjoin-term term term-list)
  (if (=zero? (coeff term))
      term-list
      (cons term term-list)))
(define (the-empty-termlist) '())
(define (first-term term-list) (car term-list))
(define (rest-terms term-list) (cdr term-list))
(define (empty-termlist? term-list) 
  (null? term-list))
(define (make-term order coeff) 
  (list order coeff))
(define (order term) (car term))
(define (coeff term) (cadr term))

(define (zero-coeffs x)
  (if (empty-termlist? x) true
      (let ((first (first-term x))
            (rest (rest-terms x))
            )
        (if (=zero? (coeff first)) (zero-coeffs rest) false)))) 

(put '=zero? '(polynomial) (lambda (x) (or (empty-termlist? (term-list x)) (zero-coeffs (term-list x)))))


(define (raise-poly-y-to-x poly)
  (make-polynomial 'x (adjoin-term (make-term 0 (tag poly)) (the-empty-termlist))))

(put 'raise 'y raise-poly-y-to-x)

(define (raise-poly poly)
  (let ((proc (get 'raise (variable poly))))
    (if proc (proc poly) false)))

(define (polylessthan a1 a2)
  (define (equalRaise a1 a2)
    (if (equal? (variable a1) (variable a2)) true
        (let ((newa1 (raise-poly a1)))
          (if newa1 (equalRaise (contents newa1) a2) false))))
  (if (equal? (variable a1) (variable a2)) false (equalRaise a1 a2)))

(define (add-poly p1 p2)
  (if (same-variable? (variable p1) 
                      (variable p2))
      (make-poly 
       (variable p1)
       (add-terms (term-list p1)
                  (term-list p2)))
      (cond ((polylessthan p1 p2) (add-poly (contents (raise-poly p1)) p2))
            ((polylessthan p2 p1) (add-poly p1 (contents (raise-poly p2))))
            (else (error "something went wrong")))))
            

(define (mul-poly p1 p2)
  (if (same-variable? (variable p1) 
                      (variable p2))
      (make-poly 
       (variable p1)
       (mul-terms (term-list p1)
                  (term-list p2)))
      (cond ((polylessthan p1 p2) (mul-poly (contents (raise-poly p1)) p2))
            ((polylessthan p2 p1) (mul-poly p1 (contents (raise-poly p2))))
            (else (error "something went wrong")))))

(define (add-terms L1 L2)
  (cond ((empty-termlist? L1) L2)
        ((empty-termlist? L2) L1)
        (else
         (let ((t1 (first-term L1)) 
               (t2 (first-term L2)))
           (cond ((> (order t1) (order t2))
                  (adjoin-term
                   t1 
                   (add-terms (rest-terms L1) 
                              L2)))
                 ((< (order t1) (order t2))
                  (adjoin-term
                   t2 
                   (add-terms 
                    L1 
                    (rest-terms L2))))
                 (else
                  (adjoin-term
                   (make-term 
                    (order t1)
                    (add (coeff t1) 
                         (coeff t2)))
                   (add-terms 
                    (rest-terms L1)
                    (rest-terms L2)))))))))

(define (mul-terms L1 L2)
  (if (empty-termlist? L1)
      (the-empty-termlist)
      (add-terms 
       (mul-term-by-all-terms 
        (first-term L1) L2)
       (mul-terms (rest-terms L1) L2))))

(define (mul-term-by-all-terms t1 L)
  (if (empty-termlist? L)
      (the-empty-termlist)
      (let ((t2 (first-term L)))
        (adjoin-term
         (make-term 
          (+ (order t1) (order t2))
          (mul (coeff t1) (coeff t2)))
         (mul-term-by-all-terms 
          t1 
          (rest-terms L))))))

  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) 
         (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) 
         (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) 
         (tag (make-poly var terms))))

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

(define (raise-number n)
  (make-polynomial 'y (adjoin-term (make-term 0 n) (the-empty-termlist))))

(put 'raise '(scheme-number) raise-number)
(define (raise x)
  (apply-generic 'raise x))

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(put 'neg '(scheme-number) (lambda (x) (* -1 x)))
(put 'neg '(complex)
     (lambda (x)
       (make-complex-from-real-imag
        (neg (real-part x))
        (neg (imag-part x)))))

(define (neg-terms terms)
    (if (empty-termlist? terms) (the-empty-termlist)
        (let ((first (first-term terms)) (rest (rest-terms terms)))
          (let ((orderFirst (order first)) (coeffFirst (coeff first)))
            (let ((newTerm (make-term orderFirst (neg coeffFirst))))
              (adjoin-term newTerm (neg-terms rest)))))))


(define (neg-poly x) (make-poly (variable x) (neg-terms (term-list x))))

(define (div a b) (apply-generic 'div a b))

  
(put 'neg '(polynomial) (lambda (x) (tag (neg-poly x))))

(define (neg x) (apply-generic 'neg x))

(define (div-terms L1 L2)
  (if (empty-termlist? L1)
      (list (the-empty-termlist) 
            (the-empty-termlist))
      (let ((t1 (first-term L1))
            (t2 (first-term L2)))
        (if (> (order t2) (order t1))
            (list (the-empty-termlist) L1)
            (let ((new-c (div (coeff t1) 
                              (coeff t2)))
                  (new-o (- (order t1) 
                            (order t2))))
              (let ((rest-of-result
                     (let
                         ((newTerm (adjoin-term (make-term new-o new-c) (the-empty-termlist))))
                       (div-terms (add-terms L1 (neg-terms (mul-terms newTerm L2))) L2))))
                (list (adjoin-term (make-term new-o new-c) (car rest-of-result)) (cadr rest-of-result))))))))

(define (div-poly a b)
  (if (same-variable? (variable a) (variable b))
   (let ((result (div-terms (term-list a) (term-list b))))
     (list (make-poly (variable a) (car result)) (make-poly (variable a) (cadr result))))
  null))

(put 'div '(polynomial polynomial) (lambda (p1 p2) 
         (tag (div-poly p1 p2))))

(define (remainder-terms x y) (cadr (div-terms x y)))


(define (gcd-terms a b)
  (if (empty-termlist? b)
      a
      (gcd-terms b (remainder-terms a b))))

(put 'greatest-common-divisor '(scheme-number scheme-number) (lambda (x y) (gcd x y)))

(put 'greatest-common-divisor '(polynomial polynomial) (lambda (x y)
                                     (if (same-variable? (variable x) (variable y))
                                         (gcd-terms (term-list x) (term-list y))
                                         (error "Not same variable"))))

(define (greatest-common-divisor x y) (apply-generic 'greatest-common-divisor x y))

(define p1 
  (make-polynomial 
   'x '((2 1) (1 -2) (0 1))))

(define p2 
  (make-polynomial 
   'x '((2 11) (0 7))))


(define p3 
  (make-polynomial 
   'x '((1 13) (0 5))))

(define q1 (mul p1 p2))
(define q2 (mul p1 p3))

(greatest-common-divisor q1 q2)
