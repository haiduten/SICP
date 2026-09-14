#lang racket
(define (estimate-pi trials)
  (* 4 (monte-carlo trials test)))

(define (test)
   (let ((x (random-in-range -1 1))
         (y (random-in-range -1 1)))
     (in-unit-circle x y)))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) 
                 (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) 
                 trials-passed))))
  (iter trials 0))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (* range (/ (random 100) 100)))))

(define (square n) (* n n))

(define (in-unit-circle x y)
  (< (+ (square x) (square y)) 1))


(estimate-pi 100000.0)



