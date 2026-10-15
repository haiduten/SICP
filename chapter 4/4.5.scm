#lang racket

(define (expand-clauses clauses)
  (if (null? clauses)
      'false     ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp 
                 (cond-actions first))
                (error "ELSE clause isn't 
                        last: COND->IF"
                       clauses))
            (if (eq? '=> (cadr first))
                (list
                 (make-lambda
                   (list 'pred 'getAction 'getRest)
                   (list (make-if
                    'pred
                    (list (list 'getAction) 'pred)
                    (list 'getRest))))
                 (car first)
                 (make-lambda
                   '()
                   (list (sequence->exp (cddr first))))
                 (make-lambda
                   '()
                   (list (expand-clauses rest))))
                (make-if (cond-predicate first)
                     (sequence->exp 
                      (cond-actions first))
                     (expand-clauses 
                      rest)))))))

