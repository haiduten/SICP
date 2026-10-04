#lang sicp
(define (stream-car stream) 
  (car stream))

(define (stream-cdr stream) 
  (force (cdr stream)))

(define (add-streams s1 s2) 
  (stream-map + s1 s2))

(define (scale-stream stream factor)
  (stream-map
   (lambda (x) (* x factor))
   stream))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map (lambda (s) (stream-car s)) argstreams))
       (apply stream-map
              (cons proc 
                    (map (lambda (s) (stream-cdr s))
                         argstreams))))))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams integers ones)))

(define (square x) (* x x))

(define (in-unit-circle x y)
  (< (+ (square x) (square y)) 1))


(define (estimate-integral P x1 x2 y1 y2)
  (define (run-exp)
    (let ((x (random-in-range x1 x2))
          (y (random-in-range y1 y2)))
      (cons-stream (if (P x y) 1.0 0.0) (run-exp))))
  (let ((exps (run-exp)))
        (define accum (cons-stream (stream-car exps) (add-streams accum (stream-cdr exps))))
          (stream-map (lambda (sum n) (/ sum n)) accum integers)))


(define (estimate-pi)
  (stream-map (lambda (x) (* x 4)) (estimate-integral in-unit-circle -1.0 1.0 -1.0 1.0)))


(define (num_exps n s)
  (if (= n 0) (stream-car s) (num_exps (- n 1) (stream-cdr s))))


(num_exps 1000000 (estimate-pi)) 