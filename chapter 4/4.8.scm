#lang racket

(define (let->combination exp)
  (if (symbol? (cadr exp))
      (cons 
       (cons
        (make-lambda
         (list )
         (list
          (list 'define (cadr exp)
                (make-lambda
                 (map (lambda (pair) (car pair)) (caddr exp))
                 (cdddr exp)))
          (cadr exp)))
        (list))
       (map (lambda (pair) (cadr pair)) (caddr exp)))
      (cons
       (make-lambda
        (map (lambda (pair) (car pair)) (cadr exp))
        (cddr exp))
       (map (lambda (pair) (cadr pair)) (cadr exp)))))


