#lang sicp
(define (stream-car stream) 
  (car stream))

(define (stream-cdr stream) 
  (force (cdr stream)))

(define ones (cons-stream 1 ones))

(define (add-streams s1 s2) 
  (stream-map + s1 s2))

(define integers (cons-stream 1 (add-streams integers ones)))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream 
       (stream-car s1)
       (interleave s2 (stream-cdr s1)))))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map (lambda (s) (stream-car s)) argstreams))
       (apply stream-map
              (cons proc 
                    (map (lambda (s) (stream-cdr s))
                         argstreams))))))


(define (weighted-pairs s t w)
  (define (weighted-merge s1 s2)
    (if (< (w (stream-car s1)) (w (stream-car s2)))
      (cons-stream (stream-car s1) (weighted-merge (stream-cdr s1) s2))
      (cons-stream (stream-car s2) (weighted-merge s1 (stream-cdr s2)))))
  (cons-stream
   (list (stream-car s) (stream-car t))
   (let ((right (stream-map (lambda (x) 
                  (list (stream-car s) x))
                (stream-cdr t)))
         (down (weighted-pairs (stream-cdr s) (stream-cdr t) w)))
     (weighted-merge right down))))


(define (cubed x) (* x x x))

(define (rama x)
  (+ (cubed (car x)) (cubed (cadr x))))

(define rama-numbers (weighted-pairs integers integers rama))

(define rama-numbers-two (stream-map rama rama-numbers))

(define (same-in-a-row s)
  (if (= (stream-car s) (stream-car (stream-cdr s)))
      (cons-stream (stream-car s) (same-in-a-row (stream-cdr s)))
      (same-in-a-row (stream-cdr s))))

(define rama-final (same-in-a-row rama-numbers-two))
(stream-car rama-final)
(stream-car (stream-cdr rama-final))
(stream-car (stream-cdr (stream-cdr rama-final)))
(stream-car (stream-cdr (stream-cdr rama-final)))
(stream-car (stream-cdr (stream-cdr (stream-cdr rama-final))))
(stream-car (stream-cdr (stream-cdr (stream-cdr (stream-cdr rama-final))))))