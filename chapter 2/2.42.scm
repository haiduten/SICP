#lang racket
(define (enumerate-interval low high)
  (if (> low high)
      null
      (cons low 
            (enumerate-interval 
             (+ low 1) 
             high))))

(define empty-board (list))

(define (reverse lst) (foldl (lambda (x y) (cons x y)) null lst))


(define (safe? k board)
  (define (helper i x board)
    (foldr (lambda (a b) (if (or (= (car board) x) (= (+ (car board) i) x) (= (- (car board) i) x )) false (and b (helper (+ i 1) x (cdr board))))) true board))
  (helper 1 (car (reverse board)) (cdr (reverse board))))


(define (adjoin-position new-row k rest-of-queens)
  (append-map (lambda (x) x) (list rest-of-queens (list new-row))))

  

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) 
           (safe? k positions))
         (append-map
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position 
                    new-row 
                    k 
                    rest-of-queens))
                 (enumerate-interval 
                  1 
                  board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(queens 6)