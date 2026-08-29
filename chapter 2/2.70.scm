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

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) 
         (cons x set))
        (else 
         (cons (car set)
               (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set 
         (make-leaf (car pair)    ; symbol
                    (cadr pair))  ; frequency
         (make-leaf-set (cdr pairs))))))


(define (generate-huffman-tree pairs)
  (successive-merge 
   (make-leaf-set pairs)))

(define (successive-merge set)
  (cond ((null? (cdr set)) (car set))
        (else
          (let ((first (car set)) (second (cadr set)) (rest (cddr set)))
            (let ((newTree (make-code-tree first second)))
              (successive-merge (adjoin-set newTree rest)))))))

(define tree (generate-huffman-tree '( (A 2) (BOOM 1) (GET 2) (JOB 2) (NA 16) (SHA 3) (YIP 9) (WAH 1))))

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


(define bits (encode '(GET A JOB SHA NA NA NA NA NA NA NA NA GET A JOB SHA NA NA NA NA NA NA NA NA WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP SHA BOOM) tree))
(foldl (lambda (new old) (+ 1 old)) 0 bits)
(* 3 (foldl (lambda (new old) (+ 1 old)) 0 '(GET A JOB SHA NA NA NA NA NA NA NA NA GET A JOB SHA NA NA NA NA NA NA NA NA WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP SHA BOOM)))