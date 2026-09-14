#lang sicp
(define (full-adder a b c-in sum c-out)
  (let ((c1 (make-wire)) 
        (c2 (make-wire))
        (s  (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

(define (ripple-carry-adder alist blist slist c)
  (define (helper alist blist slist lastc)
    (cond ((null? alist) 'ok)
          ((= (length alist) 1)
           (let ((a (car alist))
                 (b (car blist))
                 (s (car slist)))
             (full-adder a b lastc s c)))
          (else
           (let ((a (car alist))
                 (b (car blist))
                 (s (car slist))
                 (new-c (make-wire)))
             (begin 
              (full-adder a b lastc s new-c)
              (helper (cdr alist) (cdr blist) (cdr slist) new-c))))))
  (helper alist blist slist (make-wire)))
           
; half-adder is 1 x and-gate + max (or-gate , and-gate + inverter)
; full-adder is 2 x half adder + or gate
; n-bit is 2n x and gate + 2n x max (or-gate, and-gate + inverter) + n x or gate
            