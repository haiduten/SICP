#lang sicp
(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

; return 3
;('a, .)==> ('b, .) ==> ('c, null)
(count-pairs (list 'a 'b 'c))

;---------------------------------------------------------

;returns 4
;('a, .)==> (., .) ==> ('c, null)
;            |           ^
;            -------------
(define four (list 'a 'b 'c))
(set-car! (cdr four) (cdr (cdr four)))
(count-pairs four)

;----------------------------------------------------------

;returns 7
; ------------
; |          |   
;(. , .)==> (., .) ==> ('c, null)
;            |           ^
;            -------------
(define seven (list 'a 'b 'c))
(set-car! (cdr seven) (cdr (cdr seven)))
(set-car! seven (cdr seven))

(count-pairs seven)

;----------------------------------------------------------

; never returns
; ;('a, .)==> ('b, .) ==> ('c, .)------
;     ^                                | 
;     ---------------------------------

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c)))

;(count-pairs z)