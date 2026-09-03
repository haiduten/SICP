#lang racket
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

(define (=zero? x)
  (apply-generic '=zero? x))

(put '=zero? '(scheme-number) (lambda (x ) (equal? x 0)))

(define (make-term order coeff) 
  (list order coeff))
(define (order term) (car term))
(define (coeff term) (cadr term))


(define (first-term term-list)
  (if (null? term-list)
      null
      (make-term (- (length term-list) 1) (car term-list))))

(define (rest-terms term-list) (cdr term-list))

(define (empty-termlist? term-list) 
  (null? term-list))

(define (the-empty-termlist) '())

(define (adjoin-term term term-list)
  (if (=zero? (coeff term)) term-list
  (if (= (order term) (length term-list)) (cons (coeff term) term-list)
      (adjoin-term term (cons 0 term-list)))))

  
 (adjoin-term (make-term 2 5) (adjoin-term (make-term 1 3) (the-empty-termlist)))