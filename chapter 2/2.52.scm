#lang racket
(require sicp-pict)

;Add some segments to the primitive wave painter of Exercise 2.49 (to add a smile, for example).
; I will just make a smile

(define smile
  (segments->painter
   (list
    (make-segment (make-vect 0 1)
                  (make-vect 0.5 0.5))
    (make-segment (make-vect 0.5 0.5)
                  (make-vect 1 1)))))

(paint smile)

;Change the pattern constructed by corner-split (for example, by using only one copy of the up-split and right-split images instead of two).
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

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter 
                                (- n 1))))
        (let ((top-left up)
              (bottom-right right)
              (corner (corner-split painter 
                                    (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right 
                         corner))))))

(paint (corner-split einstein 10))

;Modify the version of square-limit that uses square-of-four so as to assemble the corners in a different pattern. (For example, you might make the big Mr. Rogers look outward from each corner of the square.)
(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) 
                       (tr painter)))
          (bottom (beside (bl painter) 
                          (br painter))))
      (below bottom top))))

(define (square-limit painter n)
  (let ((combine4 
         (square-of-four identity 
                         identity
                         identity 
                         identity)))
    (combine4 (corner-split painter n))))

(paint (square-limit einstein 10))