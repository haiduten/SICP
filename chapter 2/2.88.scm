#lang sicp

(put 'neg '(scheme-number) (lambda (x) (* -1 x)))
(put 'neg '(rational) (lambda (x) (make-rational (* -1 (numer x)) (denom x))))
(put 'neg '(complex)
     (lambda (x)
       (make-complex-from-real-imag
        (neg (real-part x))
        (neg (imag-part x)))))

(define (neg-terms terms)
    (if (empty_termlist? terms) (the-empty-termlist)
        (let ((first (first-term terms)) (rest (rest-term terms)))
          (let ((orderFirst (order first)) (coeffFirst (coeff first)))
            (let ((newTerm (make-term orderFirst (neg coeffFirst))))
              (adjoin-term newTerm (neg-terms rest)))))))


(define (neg-poly x) (make-poly (variable x) (neg-terms (term-list x))))
  
    

  
(put 'neg '(polynomial) (lambda (x) (tag (neg-poly x))))


(define (neg x) (apply-generic 'neg x))


(put 'sub '(polynomial polynomial)
  (lambda (p1 p2) 
         (tag (add-poly p1 (neg-poly p2)))))