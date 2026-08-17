#lang racket
(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

(define (left-branch mobile) (car mobile))

(define (right-branch mobile) (cdr mobile))

(define (branch-length branch) (car branch))

(define (branch-structure branch) (cdr branch))

(define x (make-branch 4 23))
(define y (make-branch 1 100))
(define xy (make-mobile x y))
;(branch-length (left-branch xy))
;(branch-length (right-branch xy))
;(branch-structure (left-branch xy))
;(branch-structure (right-branch xy))

(define (total-weight mobile)
  (if (not (pair? mobile)) mobile
      (let
          ((left (branch-structure (left-branch mobile)))
            (right (branch-structure (right-branch mobile)))
           )
        (+ (total-weight left) (total-weight right)))))

;(total-weight xy)

(define (balanced? mobile)
  (if (not (pair? mobile)) true
  (let
          ((left (branch-structure (left-branch mobile)))
            (right (branch-structure (right-branch mobile)))
            (leftWeight (total-weight (branch-structure (left-branch mobile))))
            (rightWeight (total-weight (branch-structure (right-branch mobile))))
            (leftLength (branch-length (left-branch mobile)))
            (rightLength (branch-length (right-branch mobile)))
           )
        (and (balanced? left) (balanced? right) (= (* rightLength rightWeight) (* leftLength leftWeight))))))


(balanced? xy)
(define xx (make-mobile x x))
(balanced? xx)