#lang sicp
;a
(define ones (cons-stream 1 ones))

(define integers (cons-stream 1 (add-streams ones integers)))

(define fractions (stream-map (lambda (x) (/ 1 x)) integers))
(define (integrate-series s) (mul-streams s fractions))

;b

(define cosine-series 
  (cons-stream 1 (stream-map (lambda (x) (* -1 x)) (integrate-series sine-series))))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))
