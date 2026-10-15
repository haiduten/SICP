#lang sicp
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values 
             (rest-operands exps) 
             env))))

;left-to-right
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((first (eval (first-operand exps) env)))
        (cons first (list-of-values (rest-operands exps) env))))) 

;right-to-left
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((first (list-of-values (rest-operands exps) env)))
        (cons (eval (first-operand exps) env) first)))) 
