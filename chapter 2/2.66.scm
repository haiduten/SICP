#lang racket
(define (lookup given-key set-of-records)
  (cond 
         ((null? set-of-records) false)
         ((equal? (key (entry set-of-records)) given-key) (entry set-of-records))
         ((< (key (entry set-of-records)) given-key) (lookup given-key (right-branch set-of-records)))
         (else (lookup given-key (left-branch set-of-records)))))
        
        