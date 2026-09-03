#lang racket
;1
;it will just endlessly loop

;2
; apply-generic will return correctly as is

;3

(define operation-table (make-hash))

(define (put operation type-signature procedure)
  (hash-set! operation-table
             (list operation type-signature)
             procedure))

(define (get operation type-signature)
  (hash-ref operation-table
            (list operation type-signature)
            #f))

(define (attach-tag type-tag contents)
  (if (equal? type-tag 'scheme-number) contents
  (cons type-tag contents)))

(define (type-tag datum)
  (if (number? datum) 'scheme-number 
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum: 
              TYPE-TAG" datum))))

(define (contents datum)
  (if (number? datum) datum
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum: 
              CONTENTS" datum))))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (and (= (length args) 2) (not (equal? (car type-tags) (cadr type-tags))))
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 
                       (get-coercion type1
                                     type2))
                      (t2->t1 
                       (get-coercion type2 
                                     type1)))
                  (cond (t1->t2
                         (apply-generic 
                          op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic 
                          op a1 (t2->t1 a2)))
                        (else
                         (error 
                          "No method for 
                           these types"
                          (list 
                           op 
                           type-tags))))))
              (error 
               "No method for these types"
               (list op type-tags)))))))
