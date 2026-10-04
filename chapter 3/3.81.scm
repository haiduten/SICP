#lang sicp
(define (rand-update x)
  (modulo (* 48271 x)
          2147483647))

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

(define (rand req-s)
  (define s
    (cons-stream 23 (stream-map (lambda (req num)
                                  (cond ((eq? (car req) 'generate) (rand-update num))
                                        ((eq? (car req) 'reset) (cadr req)))) req-s s)))
  s)

(define y (cons-stream (list 'generate) y))

;(stream-car (rand y))
;(stream-car (stream-cdr (rand y)))
;(stream-car (stream-cdr (stream-cdr (rand y))))


(define x (cons-stream (list 'generate) (cons-stream (list 'generate) (cons-stream (list 'generate) (cons-stream (list 'reset 23) x)))))

(stream-car (rand x))
(stream-car (stream-cdr (rand x)))
(stream-car (stream-cdr (stream-cdr (rand x))))
(stream-car (stream-cdr (stream-cdr (stream-cdr (rand x)))))
(stream-car (stream-cdr (stream-cdr (stream-cdr (stream-cdr (stream-cdr (rand x)))))))