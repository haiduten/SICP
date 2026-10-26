#lang sicp
(define input-prompt  ";;; L-Eval input:")
(define output-prompt ";;; L-Eval value:")

(define (cons? exp)
  (tagged-list? exp 'cons))

(define (lazy-pair? exp)
  (tagged-list? exp 'lazy-pair))

(define (lazy-pair-body exp)
  (cadr exp))

(define (apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
          procedure
          (list-of-arg-values 
           arguments 
           env)))  ; changed
        ((lazy-pair? procedure)
         (apply (lazy-pair-body procedure) arguments env))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           (list-of-delayed-args 
            arguments 
            env)   ; changed
           (procedure-environment procedure))))
        (else (error "Unknown procedure 
                      type: APPLY" 
                     procedure))))

(define (car? exp)
  (tagged-list? exp 'car))

(define (cdr? exp)
  (tagged-list? exp 'cdr))


(define (cons->lambda exp)
   (list 
   (list
    'lambda
    (list 'x 'y)
    (list
     'lambda
     (list 'm)
     (list 'm 'x 'y)))
   (cadr exp)
   (caddr exp)))

(define (car->lambda exp)
  (list
   exp
   (list
    'lambda
    (list 'p 'q)
    'p)))

(define (cdr->lambda exp)
  (list
   exp
   (list
    'lambda
    (list 'p 'q)
    'q)))

(define (eval exp env)
  (cond ((self-evaluating? exp) 
         exp)
        ((variable? exp) 
         (lookup-variable-value exp env))
        ((quoted-list? exp)
         (eval (quoted-list->cons (cadr exp)) env))
        ((quoted? exp) 
         (text-of-quotation exp))
        ((assignment? exp) 
         (eval-assignment exp env))
        ((definition? exp) 
         (eval-definition exp env))
        ((if? exp) 
         (eval-if exp env))
        ((car? exp)
         (eval (car->lambda (cadr exp)) env))
        ((cdr? exp)
         (eval (cdr->lambda (cadr exp)) env))
        ((cons? exp)
         (list 'lazy-pair (eval (cons->lambda exp) env)))
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
        ((application? exp)
         (apply (actual-value (operator exp) env)
                (operands exp)
                env))
        (else
         (error "Unknown expression 
                 type: EVAL" exp))))
  
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (actual-value 
                   input 
                   the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
    (driver-loop))

(define (cons-print object)
  (define (helper count lst-exp)
    (cond
      ((null? lst-exp) 'done)
      ((= count 0)
        (display "..."))
      (else  (begin
          (user-print (force-it (apply (cadr lst-exp) (list (list 'lambda (list 'p 'q) 'p)) (procedure-environment (cadr lst-exp)))))
          (display " ")
          (helper (- count 1) (force-it (apply (cadr lst-exp) (list (list 'lambda (list 'p 'q) 'q)) (procedure-environment (cadr lst-exp)))))))))
  (helper 20 object))
    
(define (user-print object)
  (cond
    ((lazy-pair? object) (cons-print object))
    ((compound-procedure? object)
      (display 
       (list 'compound-procedure
             (procedure-parameters object)
             (procedure-body object)
             '<procedure-env>)))
      (else (display object))))