#lang sicp
; and gate has two inputs a1 and a2
; from beginning
; a1 = 0
; a2 = 1
; set-signal! a1 1
; this adds to the queue the and with a1 = 1 and a2 = 1
; set-signal a2 0
; this adds to the queue the and with a1 = 1 and a2 = 0
; if it is a queue, then we end with the output to and would be 0.
; if it is an ordinary list, then the intermediary step would run last and the output to and would finally be 1
