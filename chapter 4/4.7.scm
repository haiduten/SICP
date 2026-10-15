#lang racket
(define (let? exp) 
  (tagged-list? exp 'let))

(define (let*? exp) 
  (tagged-list? exp 'let*))


(define (let->combination exp)
  (cons
   (make-lambda
     (map (lambda (pair) (car pair)) (cadr exp))
     (cddr exp))
   (map (lambda (pair) (cadr pair)) (cadr exp))))

(define (let*->nested-lets exp)
  (let ((args (cadr exp))
        (body (cddr exp)))
    (if (or (null? args) (null? (cdr args)))
        (cons 'let (cons args body))
        (list 'let (list (car args)) (cons 'let* (cons (cdr args) body)))))) 

(define (eval exp env)
  (cond ((self-evaluating? exp) 
         exp)
        ((variable? exp) 
         (lookup-variable-value exp env))
        ((quoted? exp) 
         (text-of-quotation exp))
        ((assignment? exp) 
         (eval-assignment exp env))
        ((definition? exp) 
         (eval-definition exp env))
        ((if? exp) 
         (eval-if exp env))
        ((lambda? exp)
         (make-procedure 
          (lambda-parameters exp)
          (lambda-body exp)
          env))
        ((begin? exp)
         (eval-sequence 
          (begin-actions exp) 
          env))
        ((cond? exp) 
         (eval (cond->if exp) env))
        ((let? exp) 
         (eval (let->combination exp) env))
        ((let*? exp) 
         (eval (let*->nested-lets exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values 
                 (operands exp) 
                 env)))
        (else
         (error "Unknown expression 
                 type: EVAL" exp))))