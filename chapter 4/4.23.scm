#lang sicp
(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc 
                            (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence: ANALYZE"))
    (loop (car procs) (cdr procs))))



(define (analyze-sequence exps)
  (define (execute-sequence procs env)
    (cond ((null? (cdr procs)) 
           ((car procs) env))
          (else ((car procs) env)
                (execute-sequence 
                 (cdr procs) env))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence: 
                ANALYZE"))
    (lambda (env) 
      (execute-sequence procs env))))

; when the sequence just has one expression:
; Text's will just return the first expression

; Alyssa's will create another frame and check if there is another expression and then execute the expression

; when the sequence just has two expression
; Text's will return a lambda with the analyed version of both expression as the body. The first analyzed expression is executed, then the second.

; Allyssa's will check if there is another expression, then execute the first,
; then get the rest of the expression list, check if there is another expression left, then execute the seceond