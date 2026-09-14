#lang racket
;global env (make-withdrawal)
;     ^            |       ^
;     |            v       |
;     |          (let..., <>)
;     E1  (initial-amount: 100)               
;              ^        ^                                                                                             
;              |        |                                                                                   
;              |        |                                                                                        
; (lambda (balance), <>)|                                                                                    
;                       |                                                                          
;                       |                                                                  
;E2 (balance: 100) ------                                                                   
;                    ^  ^                                                                        
;                    |  |                                                                     
; (lambda (amount), <>) |                                                                         
;                       |                                                                        
; E3 (amount: 50)--------                                                                    


; it evalutes to the same, but there is an extra layer