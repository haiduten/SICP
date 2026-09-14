#lang racket
(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance 
                     (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch passcode m)
    (cond ((not (eq? passcode password)) (error "Incorrect password"))
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request: 
                 MAKE-ACCOUNT" m))))
  dispatch)

(define peter-acc  
  (make-account 100 'open-sesame ))

((peter-acc 'open-sesame 'withdraw) 40)

(define (make-joint account oldpass newpass)
  (define (dispatch password m)
    (cond ((not (eq? password newpass)) (error "Incorrect password"))
          (else (account oldpass m))))
  dispatch)

(define paul-acc
  (make-joint peter-acc 
              'open-sesame 
              'rosebud))

((paul-acc 'rosebud 'withdraw) 10)
((peter-acc 'open-sesame 'withdraw) 10)