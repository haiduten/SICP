#lang sicp
;1
(define (let-rec-derive exp)
  (let ((vars (map (lambda (x) (car x)) (cadr exp)))
        (vals (map (lambda (x) (cadr x)) (cadr exp)))
        (body (cddr exp)))
     (append
      (list
       'let
       (map (lambda (x) (list x (list 'quote '*unassigned*))) vars))
      (map (lambda (x y) (list 'set! x y)) vars vals)
      body)))

;2
; global environment
; f frame (binding x=5)
; (derived-let lambda frame (binding even? = lambda..., odd? = lambda ...

; when the even? is executed, it will look for odd?. the value of even? is passed in at the f frame, so it will check odd? at the f frame and not find and error

;vs

;global environment
; global environment
; f frame (binding x=5)
; (derived-let lambda frame (binding even? = undefined odd? = undefined
       