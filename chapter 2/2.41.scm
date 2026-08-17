#lang racket
(define (enumerate n)
  (if (= n 0) null (cons n (enumerate (- n 1)))))

(define (pairs n)
  (append-map (lambda (x) (map (lambda (y) (list x y)) (enumerate (- x 1)))) (enumerate n)))

(define (triplets n) (append-map (lambda (x) (map (lambda (y) (cons x y)) (pairs (- x 1)))) (enumerate n)))

(define (sum-to triplets n)
  (= (foldr + 0 triplets) n))

(define (triplets-sum-to n s)
  (filter (lambda (x) (sum-to x s)) (triplets n)))

(triplets-sum-to 4 7)