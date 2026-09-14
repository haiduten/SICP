#lang sicp
;global env

;cons:  procedural object (cons body and args, pointer to global env)

; frame1 (x = 1, y = 2) (points to global env)

; dispatch: procedural object (cons body and args, enclosing points to frame1)

; frame2 (x =x , y = x) (points to global env)

; dispatch2:procedural object (cons body and args, enclosing points to frame2)

; frame5+++ (m = cdr) paoints to frame2
