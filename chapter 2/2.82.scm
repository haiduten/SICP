#lang racket

(define (containsNull lst)
  (if (null? lst) false (if (null? (car lst)) true (containsNull (cdr lst)))))


(define (apply-generic op . args)
  (define (convertToType type)
    (map (lambda (x)
           (if (equal? (type-tag x) type)
               x
               (let
                   ((convers (get-coercion (type-tag x) type)))
                 (if convers (convers x) null))
               )
           )
         args)
    )
  
  (define (procByType type)
    (let
        ((convertedArgs (convertToType type)))
      (if (containsNull convertedArgs) false 
      (let
          ((proc (get op (map type-tag convertedArgs))))
        (if proc
            (cons proc convertedArgs)
            false)
        )
      )
    )
    )
  
  (define (newProc types)
    (if (null? types) false
    (let ((proc (procByType (car types))))
      (if proc proc (newProc (cdr types))))))
    
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (let ((new-proc (newProc type-tags)))
          (if new-proc (apply (car new-proc) (map contents (cdr new-proc))) (error "No Method for these types")))))))


 ; suppose we have a conversion of natural number to complex and integar to complex and that is it.
 ; this method would fail if the the two args were a natural number and an integar
 ; it would ignore the possibility of converting them to a complex number
          
          