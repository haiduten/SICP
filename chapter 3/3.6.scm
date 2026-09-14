#lang racket
(define (rand-update x)
  (modulo (* 48271 x)
          2147483647))

(define rand
  (let ((base 23))
    (define (generate)
      (begin (set! base (rand-update base))
             base))
    (define (reset x)
      (set! base x))
    (lambda (m)
    (cond ((eq? m 'generate) (generate))
          ((eq? m 'reset) reset)))))

(rand 'generate)
(rand 'generate)
(rand 'generate)
((rand 'reset) 23)
(rand 'generate)
(rand 'generate)
(rand 'generate)
    
 