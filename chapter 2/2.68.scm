#lang racket
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))


(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) 
                (symbols right))
        (+ (weight left) (weight right))))


(define sample-tree
  (make-code-tree 
   (make-leaf 'A 4)
   (make-code-tree
    (make-leaf 'B 2)
    (make-code-tree 
     (make-leaf 'D 1)
     (make-leaf 'C 1)))))

(define (encode message tree)
  (if (null? message)
      '()
      (append 
       (encode-symbol (car message) 
                      tree)
       (encode (cdr message) tree))))

(define (contains lst symbol)
  (cond ((null? lst) false)
        ((equal? (car lst) symbol) true)
        (else (contains (cdr lst) symbol))))

(define (encode-symbol symbol tree)
  (cond
    ((not (contains (symbols tree) symbol)) (error "symbol not found"))
    ((leaf? tree) null)
    ((contains (symbols (left-branch tree)) symbol) (cons 0 (encode-symbol symbol (left-branch tree))))
    ((contains (symbols (right-branch tree)) symbol) (cons 1 (encode-symbol symbol (right-branch tree))))
    (else (error "symbol not found"))))

(encode '(A D A B B C A) sample-tree)
  