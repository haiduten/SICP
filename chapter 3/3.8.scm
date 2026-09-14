#lang racket

(define f
  (let ((secret null))
    (lambda (n)
      (let ((old secret))
        (if (null? old)
            (begin (set! secret n)
                   0)
            (begin (set! secret n)
                   old))))))

(+ (f 0) (f 1))


(define g
  (let ((secret null))
    (lambda (n)
      (let ((old secret))
        (if (null? old)
            (begin (set! secret n)
                   0)
            (begin (set! secret n)
                   old))))))


(+ (g 1) (g 0))