#lang sicp
;  (sqrt-stream x) spawns a brand new stream so the memoization of the old one is irrelevant
; if our implementation of delay did not use memoization it would be the same