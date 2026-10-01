#lang sicp
(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream 
       (stream-car s1)
       (interleave s2 (stream-cdr s1)))))


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
         (down (weighted-pairs (stream-cdr s) (stream-cdr t))))
     (weighted-merge right down w))))


; a
(define (w1 pair) (+ (car pair) (cadr pair)))
(weighted-pairs integers integers w1)

;b
(define (w2 pair) (let ((i (car pair)) (j (cadr pair)))
                    (+ (* 2 i) (* 3 j) (* 5 i j))))


(define (divis-by-two-three-five pair) 
  (let ((i (car pair))
        (j (cadr pair)))
    (not (or (= (remainder i 2) 0) (= (remainder i 3) 0) (= (remainder i 5) 0) (= (remainder j 2) 0) (= (remainder j 3) 0) (= (remainder j 5) 0)))))
                                        
(stream-filter divis-by-two-three-five (weighted-pairs integers integers w2))