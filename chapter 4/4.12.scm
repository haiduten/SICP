#lang sicp
(define (find-var-in-frame var frame)
  (define (helper vars vals)
    (cond ((null? vars) '())
          ((eq? var (car vars)) (cons vars vals))
          (else (helper (cdr vars) (cdr vals)))))
  (helper (frame-variables frame) (frame-values frame)))

(define (find-var-in-env var env)
  (if (eq? env the-empty-environment)
      '()
      (let ((result (find-var-in-frame var (first-frame env))))
        (if (null? result)
            (find-var-in-env var (enclosing-environment env))
            result))))

(define (lookup-variable-value var env)
  (let ((result (find-var-in-env var env)))
    (if (null? result)
        (error "Unbound variable" var)
        (car (cdr result)))))

(define (set-variable-value! var val env)
  (let ((result (find-var-in-env var env)))
    (if (null? result)
        (error "Unbound variable: SET!" var)
        (set-car! (cdr result) val))))


(define (define-variable! var val env)
  (let ((result (find-var-in-frame var (first-frame env))))
    (if (null? result)
        (add-binding-to-frame! var val (first-frame env))
        (set-car! (cdr result) val))))

