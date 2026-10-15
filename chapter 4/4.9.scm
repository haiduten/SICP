#lang racket
; while <truth condition> <body>

;('while <truth condition> body>

(define (while->if exp)
  (cons
   (cons
    (make-lambda
     (list)
     (list
      (list 'define
            'while-fn
            (make-lambda
             (list 'getPred 'getBody)
             (list (make-if (list 'getPred)
                      (list 'begin (list 'getBody) (list 'while-fn 'getPred 'getBody))
                      'false))))
      'while-fn))
    (list))
   (list
    (make-lambda (list) (list (cadr exp)))
    (make-lambda (list) (cddr exp)))))
    
    
          
   
  