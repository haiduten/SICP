#lang racket
; GLOBAL environment
; make-acccount (body, points to global)


;E1: balance: 50 (points to global)

;withdraw, deposiit, dispatch point to E1


;E2: m = deposit (points to E1)  ; 53: amount: 40 (points to E1) ; E4: m = withdraw; E5: amount :60

; The local state is kept in E1

; Each one has its own environment frame

; the parts that are shared is the global environment and the text of the lambda experessions
