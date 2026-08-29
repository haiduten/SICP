#lang racket
(define (typelessthan a1 a2)
  (define (equalRaise a1 a2)
    (if (equal? (type-tag a1) (type-tag a2)) true
        (let ((proc (get 'raise (type-tag a1)))
          (if proc (equalRaise (proc (contents a1)) a2) false))))
  (if (equal? (type-tag a1) (type-tag a2)) false (equalRaise a1 a2)))

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
                           type-tags))))))
              (error 
               "No method for these types"
               (list op type-tags))))))