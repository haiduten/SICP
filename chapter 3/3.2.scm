#lang racket
(define (make-monitored fn)
  (let ((calls 0))
    (lambda (arg)
      (cond
        ((eq? arg 'how-many-calls?) calls)
        ((eq? arg 'reset-count) (set! calls 0))
        (else (begin
                (set! calls (+ calls 1))
                (fn arg)))))))


(define s (make-monitored sqrt))
(s 100)
(s 'how-many-calls?)
(s 'reset-count)
(s 'how-many-calls?)