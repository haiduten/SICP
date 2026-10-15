#lang sicp
; My specification will be that we remove the binding on the first frame of the environment
; I justify this because the user could unintentionally rewrite binding in an enclosing frame
; and they would need to keep track of all the frames which is quite onerous.

(define (set-frame-variables frame vars)
  (set-car! frame vars))

(define (set-frame-values frame vals)
  (set-cdr! frame vals))

(define (make-unbound! var env)
  (define (delete vars vals prev-vars prev-vals frame)
    (cond ((null? vars)
             (error "var does not exist"))
            ((eq? var (car vars))
             (if (null? prev-vars)
                 (begin
                   (set-frame-variables frame (cdr vars))
                   (set-frame-values frame (cdr vals)))
                 (begin
                   (set-cdr! prev-vars (cdr vars))
                   (set-cdr! prev-vals (cdr vals)))))
            (else (delete (cdr vars) (cdr vals) vars vals frame))))
  (let ((f (first-frame env)))
    (delete (frame-variables f) (frame-values f) '() '() f)))

