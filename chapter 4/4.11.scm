#lang sicp

(define (make-frame variables values)
  (list (map (lambda (x y) (cons x y)) variables values)))

(define (frame-variables frame) (map (lambda (x) (car x)) (car frame)))

(define (frame-values frame) (map (lambda (x) (cdr x)) (car frame)))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons (cons var val) (car frame))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan frame)
      (cond ((null? frame)
             (env-loop 
              (enclosing-environment env)))
            ((eq? var (car (car frame)))
             (cdr (car frame)))
            (else (scan (cdr frame)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (car frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan f)
      (cond ((null? f)
             (env-loop 
              (enclosing-environment env)))
            ((eq? var (car (car f)))
             (set-cdr! (car f) val))
            (else (scan (cdr f)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (car frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan f flst)
      (cond ((null? flst)
             (add-binding-to-frame! 
              var val f))
            ((eq? var (car (car flst)))
             (set-cdr! (car flst) val))
            (else (scan f (cdr flst)))))
    (scan frame (car frame))))