#lang sicp

(define (make-queue)
  (let ((front-ptr '() )
        (rear-ptr '()))
      (define (empty-queue?)
        (null? front-ptr))
      (define (front-queue)
        (if (empty-queue?) (error "Empty queue") front-ptr))
      (define (insert-queue! item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue?)
           (begin (set! front-ptr new-pair)
           (set! rear-ptr new-pair)
           front-ptr))
          (else (set-cdr! rear-ptr
                          new-pair)
                (set! rear-ptr new-pair)
                front-ptr))))
      (define (delete-queue!)
  (cond ((empty-queue?)
         (error "DELETE! called with 
                 an empty queue"))
        (else (set! 
               front-ptr 
               (cdr front-ptr))
              )))
      
    (define (dispatch m)
      (cond ((eq? m 'empty-queue?) empty-queue?)
            ((eq? m 'front-queue) front-queue)
            ((eq? m 'insert-queue!) insert-queue!)
            ((eq? m 'delete-queue!) delete-queue!)
            (else (error "undefined operation"))))
    dispatch))


(define (empty-queue? q) ((q 'empty-queue?)))
(define (front-queue q) ((q 'front-queue)))
(define (insert-queue! q item) ((q 'insert-queue!) item))
(define (delete-queue! q) ((q 'delete-queue!)))
(define (print-queue q)
  ((q 'front-queue)))

(define q1 (make-queue))

(insert-queue! q1 'a)
(print-queue q1)


(insert-queue! q1 'b)
(print-queue q1)

(delete-queue! q1)
(print-queue q1)

(delete-queue! q1)
(print-queue q1)