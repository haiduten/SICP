#lang racket
(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size 
             (quotient (- n 1) 2)))
        (let ((left-result 
               (partial-tree 
                elts left-size)))
          (let ((left-tree 
                 (car left-result))
                (non-left-elts 
                 (cdr left-result))
                (right-size 
                 (- n (+ left-size 1))))
            (let ((this-entry 
                   (car non-left-elts))
                  (right-result 
                   (partial-tree 
                    (cdr non-left-elts)
                    right-size)))
              (let ((right-tree 
                     (car right-result))
                    (remaining-elts 
                     (cdr right-result)))
                (cons (make-tree this-entry 
                                 left-tree 
                                 right-tree)
                      remaining-elts))))))))

; It starts with the left tree. It determines the size by subtracting 1 from length (1 is resevered for the root) and dividing by two. Once this tree is formed,
; it does the same procedure for the right but it removes the first element from the left over nodes. that will be reserved for the node. then it creates a tree
; with the left subtree, the root, and the right subtree


;              5
;            /    \
;           1     9
;            \   / \
;             3  7  11


; O (n)