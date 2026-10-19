#lang sicp
; a
(define (eval-sequence exps env)
  (cond ((last-exp? exps) 
         (eval (first-exp exps) env))
        (else 
         (actual-value (first-exp exps) 
                       env)
         (eval-sequence (rest-exps exps) 
                        env))))

(define (for-each proc items)
  (if (null? items)
      'done
      (begin (proc (car items))
             (for-each proc 
                       (cdr items)))))



(for-each
 (lambda (x) (newline) (display x))
 (list 57 321 88))

;translates to

(begin
  (lambda (x) (newline) (display x)) (car items)
  (for-each proc 
                       (cdr items))

;evaluating (lambda (x) (newline) (display x)) (car items) returns (newline) and (display (thunk; car items))
; display is a primitive procedure so its argument gets actualized.


; b
(define (p1 x)
  (set! x (cons x '(2))) x)

(p1 1)
;original
; 1 is passed as a thunk. cons x '(2) forces the thunk and x is '(1 2)

; cy's version
; 1 is passed as a thunk. cons x '(2) forces the thunk and x is '(1 2)

(define (p2 x)
  (define (p e)
    e
    x)
  (p (set! x (cons x '(2)))))

(p2 1)
; original
; the argument is passed a thunk. It does nothing. We return 1

; cy's version
; the argument is passed as a thunk. We actualize it. Then we return '(1 2)

; c
; Because that part never returns thunk in the body. so it just passes through

;d
; I like cy's version because it is possible some side effeects are not run like in part b
