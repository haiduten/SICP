#lang sicp
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
        ((application? exp)
         (apply (actual-value (operator exp) env)
                (operands exp)
                env))
        (else
         (error "Unknown expression 
                 type: EVAL" exp))))


(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (lambda? exp) 
  (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))


(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda 
       (cdadr exp)   ; formal parameters
       (cddr exp))))


(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (map (lambda (x) (if (symbol? x) x (car x))) (cadr p)))
(define (procedure-strictness p) (map (lambda (x) (if (symbol? x) 'strict (cadr x))) (cadr p)))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))


(define (apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
          procedure
          (list-of-arg-values 
           arguments 
           env)))  ; changed
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           (list-of-delayed-args 
            arguments
            (procedure-strictness procedure)
            env)   ; changed
           (procedure-environment procedure))))
        (else (error "Unknown procedure 
                      type: APPLY" 
                     procedure))))


(define (delay-it exp strict env)
  (cond ((eq? strict 'strict) (actual-value exp env))
        ((eq? strict 'lazy) (list 'thunk exp env))
        ((eq? strict 'lazy-memo) (list 'thunk-mem exp env))))

(define (list-of-delayed-args exps strictness env)
  (if (no-operands? exps)
      '()
      (cons (delay-it 
             (first-operand exps)
             (car strictness)
             env)
            (list-of-delayed-args 
             (rest-operands exps)
             (cdr strictness)
             env))))


(define (force-it obj)
  (cond
    ((thunk? obj) (actual-value (thunk-exp obj) 
                    (thunk-env obj)))
    ((thunk-mem? obj)
         (let ((result
                (actual-value 
                 (thunk-exp obj)
                 (thunk-env obj))))
           (set-car! obj 'evaluated-thunk)
           ;; replace exp with its value:
           (set-car! (cdr obj) result) 
           ;; forget unneeded env:
           (set-cdr! (cdr obj) '()) 
           result))
        ((evaluated-thunk? obj)
         (thunk-value obj))
        (else obj)))


(define (thunk? obj) (tagged-list? obj 'thunk))
(define (thunk-mem? obj) (tagged-list? obj 'thunk-mem))
(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))

