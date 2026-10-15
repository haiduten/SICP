#lang sicp
;a
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop 
              (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) 
                        (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (let ((val (env-loop env)))
    (if (eq? val '*unassigned*)
        (error "value has yet to be assigned" var)
        val)))

;b
(define (scan-out-defines exp)
  (let ((define-statements (filter (lambda (x) (definition? x)) exp))
        (rest (filter (lambda (x) (not (definition? x))) exp)))
    (list (append
     (list
      'let
      (map (lambda (x) (list (definition-variable x) (list 'qoute '*unassigned*))) define-statements))
     (map (lambda (x) (list 'set! (definition-variable x) (definition-value x)) define-statements))
     rest))))

;c
;make-procedure so that it is run only once when creating the procedure
;procedure-body it would run every time you apply the procedure
     
    

