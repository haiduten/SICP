#lang racket
(define (make-interval a b) (cons a b))

(define (upper-bound int) (cdr int))

(define (lower-bound int) (car int))

(define (isPos x) (> x 0))


(define (mul-interval x y)
  (let ((isxlPos (> (lower-bound x) 0))
        (isxuPos (> (upper-bound x) 0))
        (isylPos (> (lower-bound y) 0))
        (isyuPos (> (upper-bound y) 0))
        (xl (lower-bound x))
        (xu (upper-bound x))
        (yl (lower-bound y))
        (yu (upper-bound y))
        )
    (cond
      ((and isxlPos isxuPos isylPos isyuPos) (make-interval (* xl yl) (* xu yu)))
      ((and isxlPos isxuPos (not isylPos) isyuPos) (make-interval (* xu yl) (* xu yu)))
      ((and isxlPos isxuPos (not isylPos) (not isyuPos)) (make-interval (* xu yl) (* xl yu)))
      ((and (not isxlPos) isxuPos isylPos isyuPos) (make-interval (* xl yu) (* xu yu)))
      ((and (not isxlPos) isxuPos (not isylPos) isyuPos) (make-interval (min (* xl yu) (* xu yl)) (max (* xu yu) (* xl yl))))
      ((and (not isxlPos) isxuPos (not isylPos) (not isyuPos)) (make-interval (* xu yl) (* xl yl)))
      ((and (not isxlPos) (not isxuPos) isylPos isyuPos) (make-interval (* xl yu) (* xu yl)))
      ((and (not isxlPos) (not isxuPos) (not isylPos) isyuPos) (make-interval (* xl yu) (* xl yl)))
      ((and (not isxlPos) (not isxuPos) (not isylPos) (not isyuPos)) (make-interval (* xu yu) (* xl yl)))
    )
  )
)