#lang sicp
;a
(define (make-semaphore n)
  (let ((processes 0)
        (mutex (make-mutex)))
     (define (acquire)
       (mutex 'acquire)
       (if (< processes n)
           (begin 
           (set! processes (+ processes 1))
           (mutex 'release))
          (begin
            (mutex 'release)
            (acquire))))
     (define (the-semaphore m)
      (cond ((eq? m 'acquire) (acquire))
            ((eq? m 'release)
             (begin
              (mutex 'acquire)
             (set! processes (if (= processes 0) 0 (- processes 1)))
             (mutex 'release))))))
    the-semaphore)))

; b
(define (make-semaphore n)
  (let ((processes 0)
        (cell (list false)))
     (define (acquire)
       (if (test-and-set! cell)
           (acquire)
           (if (< processes n)
               (begin 
                 (set! processes (+ processes 1))
                 (clear! cell))
               (begin
                 (clear! cell)
                 (acquire)))))
    (define (release)
       (if (test-and-set! cell)
           (release)
           (begin
              (set! processes (if (= processes 0) 0 (- processes 1)))
              (clear! cell))))
     (define (the-semaphore m)
      (cond ((eq? m 'acquire) (acquire))
            ((eq? m 'release) (release))))
    the-semaphore))
                   
                   