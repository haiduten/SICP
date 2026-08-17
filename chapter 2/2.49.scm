#lang racket

(require sicp-pict)

; The painter that draws the outline of the designated frame.
(define outline
  (segments->painter
   (list
    (make-segment (make-vect 0 0)
                  (make-vect 0 1))
    (make-segment (make-vect 0 1)
                  (make-vect 1 1))
    (make-segment (make-vect 1 1)
                  (make-vect 1 0))
    (make-segment (make-vect 1 0)
                  (make-vect 0 0)))))

(paint outline)

; The painter that draws an “X” by connecting opposite corners of the frame.
(define outline2
  (segments->painter
   (list
    (make-segment (make-vect 0 0)
                  (make-vect 1 1))
    (make-segment (make-vect 0 1)
                  (make-vect 1 0)))))


(paint outline2)
; The painter that draws a diamond shape by connecting the midpoints of the sides of the frame.
(define b1 (cons (cons 0 0.5) (cons 0.5 1)))
(define b2 (cons (cons 0.5 1) (cons 1 0.5)))
(define b3 (cons (cons 1 0.5) (cons 0.5 0)))
(define b4 (cons (cons 0.5 0) (cons 0 0.5)))
(define outline3
  (segments->painter
   (list
    (make-segment (make-vect 0 0.5)
                  (make-vect 0.5 1))
    (make-segment (make-vect 0.5 1)
                  (make-vect 1 0.5))
    (make-segment (make-vect 1 0.5)
                  (make-vect 0.5 0))
    (make-segment (make-vect 0.5 0)
                  (make-vect 0 0.5))
    )))
          
(paint outline3)

; The wave painter.
; Too long, don't care
               