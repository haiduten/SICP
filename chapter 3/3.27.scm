#lang sicp
; env state fib 3

; env state fib 2   

; env state fib 1   ;env state fib 0


; It is proprotional to n because there n + 1 calls. Each time it is memoized so if
; an old call appears it just returns the value


; it would not work for memorize fib because it only memoizes the nth call. All calls
; less than n are computed the old exponential run time way