#lang sicp
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) 
  (set-car! queue item))
(define (set-rear-ptr! queue item) 
  (set-cdr! queue item))

(define (empty-deque? queue) 
  (null? (front-ptr queue)))

(define (make-deque) (cons '() '()))

(define (front-deque queue)
  (if (empty-deque? queue)
      (error "FRONT called with an 
              empty deque" queue)
      (car (front-ptr queue))))

(define (rear-deque queue)
  (if (empty-deque? queue)
      (error "REAR called with an 
              empty deque" queue)
      (car (rear-ptr queue))))

(define (print queue)
  (define (helper q)
  (if (null? q) '() (cons (car q) (helper (caddr q)))))
  (helper (front-ptr queue)))

(define (insert-rear-deque! queue item)
  (let ((new-pair (list item '() '())))
    (cond ((empty-deque? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           (print queue))
          (else (set-cdr! (cdr (rear-ptr queue)) (list new-pair))
                (set-car! (cdr new-pair) (list (rear-ptr queue)))
                (set-rear-ptr! queue new-pair)
                (print queue)))))

(define (insert-front-deque! queue item)
  (let ((new-pair (list item '() '())))
    (cond ((empty-deque? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           (print queue))
          (else (set-cdr! (cdr new-pair)
                          (list (front-ptr queue)))
                (set-car! (cdr (front-ptr queue)) (list new-pair))
                (set-front-ptr! queue new-pair)
                (print queue)))))

(define (front-delete-deque! queue)
  (cond ((empty-deque? queue)
         (error "DELETE! called with 
                 an empty deque" queue))
        (else (set-front-ptr! 
               queue 
               (caddr (front-ptr queue)))
              (if (null? (front-ptr queue)) 0 (set-car! (cdr (front-ptr queue)) '()))
              (print queue))))

(define (rear-delete-deque! queue)
  (cond ((empty-deque? queue)
         (error "DELETE! called with 
                 an empty deque" queue))
        (else (set-rear-ptr! 
               queue 
               (if (null? (cadr (rear-ptr queue))) '()  (caadr (rear-ptr queue))))
              (if (and (null? (rear-ptr queue)) (not (empty-deque? queue))) (set-front-ptr! queue '()) 0)
              (if (null? (rear-ptr queue)) 0 (set-cdr! (cdr (rear-ptr queue)) (list '())))
              (print queue))))

(define q1 (make-deque))
(empty-deque? q1)
(insert-rear-deque! q1 'a)
(insert-rear-deque! q1 'b)
(insert-rear-deque! q1 'c)
(insert-rear-deque! q1 'z)
(insert-rear-deque! q1 'y)
(insert-rear-deque! q1 'w)

(front-delete-deque! q1)
(front-delete-deque! q1)
(front-delete-deque! q1)
(front-delete-deque! q1)
(front-delete-deque! q1)
(front-delete-deque! q1)


(insert-rear-deque! q1 'a)
(insert-rear-deque! q1 'b)
(insert-rear-deque! q1 'c)
(insert-rear-deque! q1 'z)
(insert-rear-deque! q1 'y)
(insert-rear-deque! q1 'w)


(rear-delete-deque! q1)
(rear-delete-deque! q1)
(rear-delete-deque! q1)
(rear-delete-deque! q1)
(rear-delete-deque! q1)
(rear-delete-deque! q1)

(insert-rear-deque! q1 'a)
(insert-rear-deque! q1 'b)
(insert-rear-deque! q1 'c)
(insert-rear-deque! q1 'z)
(insert-rear-deque! q1 'y)
(insert-rear-deque! q1 'w)

(front-delete-deque! q1)
(front-delete-deque! q1)
(rear-delete-deque! q1)
(rear-delete-deque! q1)
(rear-delete-deque! q1)
(rear-delete-deque! q1)


