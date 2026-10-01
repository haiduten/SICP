#lang sicp
;process p1 gets to test-and-set!. It checks (car cell) returns false reaches begin, but then process p2 interleaves
; it checks (car cell) returns false and then completes the begin statement. then p1 completes the begin statement.
; so two processes have the mutex and they are open for race condition. 
; one of p1 or p2 releases the mutex but the other is still running so it is not really free