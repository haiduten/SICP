#lang racket
; Allysa P Hacker's way requires calling remainder on potentially a very large number (grows exponentially with n)
; The old way we only call remainder at something at most n^2