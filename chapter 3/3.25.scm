#lang sicp
(define (lookup keys table)
  (if (= (length keys) 0) (error "Must provide keys")
  (let ((subtable (assoc (car keys) (cdr table))))
    (cond ((eq? subtable false) false)
          ((= (length keys) 1) (cdr subtable))
          (else (lookup (cdr keys) subtable))))))

(define (addKey keys value)
  (cond ((= (length keys) 0) (error "Must have keys"))
        ((= (length keys) 1) (cons (car keys) value))
        (else (cons (car keys) (list (addKey (cdr keys) value))))))

(define (insert! keys value table)
  (if (= (length keys) 0) (error "Must provide keys")
  (let ((subtable (assoc keys (cdr table))))
    (cond ((eq? subtable false) (set-cdr! table (cons (addKey keys value) (cdr table))))
          ((= (length keys) 1) (set-cdr! subtable value))
          (else (insert! (cdr keys) value subtable))))))

(define t1
  (cons 'table (list 
                 (list 'math (cons '+ 43) (cons '- 45) (cons '* 42))
                 (list 'letters (cons 'a 97) (cons 'b 98)))))

(lookup (list 'math '+) t1)

(insert! (list 'math '+) 99 t1)

(lookup (list 'math '+) t1)

(insert! (list 'math '+ '^ 'e) 42 t1)

(lookup (list 'math '+ '^ 'e) t1)
                                       