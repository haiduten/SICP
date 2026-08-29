#lang racket
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

  
  