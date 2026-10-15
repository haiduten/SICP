#lang racket
(define (and? exp) (tagged-list? exp 'and))
(define (and-terms exp) (cdr exp))
(define (and-rest-terms exp) (cdr exp))
(define (and-first-term exp) (car exp))

(define (eval-and exp env)
  (define (helper exp before)
    (if (null? exp)
        before
        (let ((evaluated-term (eval (and-first-term exp) env)))
          (if (true? evaluated-term)
              (helper (and-rest-terms exp) evaluated-term)
              false))))
  (helper (and-terms exp) true))


(define (or? exp) (tagged-list? exp 'or))
(define (or-terms exp) (cdr exp))
(define (or-rest-terms exp) (cdr exp))
(define (or-first-term exp) (car exp))

(define (eval-or exp env)
  (define (helper exp)
    (if (null? exp)
        false
        (let ((evaluated-term (eval (or-first-term exp) env)))
          (if (true? evaluated-term)
              evaluated-term
              (helper (or-rest-terms exp))))))
  (helper (or-terms exp)))

