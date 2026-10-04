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

(define (integral integrand initial-value dt)
  (define int
    (cons-stream 
     initial-value
     (add-streams (scale-stream integrand dt)
                  int)))
  int)


(define (RC R C dt)
  (lambda (i v-0)
    (stream-map (lambda (x) (+ x v-0)) (add-streams (scale-stream (integral i 0 dt) (/ 1 C)) (scale-stream i R)))))



  