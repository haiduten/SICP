#lang sicp
(define (analyze-let exp)
  (let ((vars (map (lambda (x) (car x)) (cadr exp)))
        (valsproc (map (lambda (x) (analyze (cadr x))) (cadr exp)))
        (bproc (analyze-sequence (cddr exp))))
    (lambda (env)
      (execute-application
       (make-procedure vars bproc env)
       (map (lambda (x) (x env)) valsproc)))))


(define (analyze exp)
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))
        ((quoted? exp) 
         (analyze-quoted exp))
        ((variable? exp) 
         (analyze-variable exp))
        ((assignment? exp) 
         (analyze-assignment exp))
        ((definition? exp) 
         (analyze-definition exp))
        ((if? exp) 
         (analyze-if exp))
        ((let? exp)
         (analyze-let exp))
        ((lambda? exp) 
         (analyze-lambda exp))
        ((begin? exp) 
         (analyze-sequence 
          (begin-actions exp)))
        ((cond? exp) 
         (analyze (cond->if exp)))
        ((application? exp) 
         (analyze-application exp))
        (else
         (error "Unknown expression 
                 type: ANALYZE" 
                exp))))