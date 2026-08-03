#lang racket
(define (pascal row col)
  (cond 
        ((< row 1) 0)
        ((< col 1) 0)
        ((and (= row 1) (= col 1)) 1)
        (else (+ (pascal (- row 1) (- col 1)) (pascal (- row 1) col)))))

(pascal 1 1)
(pascal 2 1)
(pascal 3 1)
(pascal 3 2)
(pascal 4 2)
(pascal 5 3)