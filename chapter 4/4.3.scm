#lang racket
(define operation-table (make-hash))

(define (put op type item)
  (hash-set! operation-table (list op type) item))

(define (get op type)
  (hash-ref operation-table
            (list op type)
            #f))

(put 'eval 'self-evaluating (lambda (x y) x))
(put 'eval 'variable lookup-variable-value)
(put 'eval 'quote (lambda (x y) (text-of-quotation x)))
(put 'eval 'set! eval-assignment)
(put 'eval 'define eval-definition)
(put 'eval 'if eval-if)
(put 'eval 'lambda (lambda (exp env) (make-procedure 
          (lambda-parameters exp)
          (lambda-body exp)
          env)))
(put 'eval 'begin (lambda (exp env) (eval-sequence 
          (begin-actions exp) 
          env)))
(put 'eval 'cond (lambda (exp env) (eval (cond->if exp) env)))
(put 'eval 'call (lambda (exp env) (apply (eval (operator exp) env)
                (list-of-values 
                 (operands exp) 
                 env))))

(define (getType exp)
  (cond ((pair? exp) (car exp))
        ((symbol? exp) 'variable)
        ((number? exp) 'self-evaluating)
        ((string? exp) 'self-evaluating)))

(define (eval exp env)
  (let ((handler (get 'eval (getType exp))))
    (if handler (handler exp env) (error "Unknown type"))))