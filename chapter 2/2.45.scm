#lang racket
(require sicp-pict)

(define (split original copy)
(define (helper painter n original copy)
  (if (= n 0)
      painter
      (let ((smaller (helper painter (- n 1) original copy)))
        (original painter 
                (copy smaller smaller)))))
  (lambda (painter n) (helper painter n original copy)))

(define right-split (split beside below))
(define up-split (split below beside))


(paint (up-split einstein 4))

(paint (right-split einstein 4))
