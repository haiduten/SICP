#lang racket
; 1, 2, 4, 8, 16

;                      / \
;                     / \ 16
;                    / \ 8
;                   / \ 4
;                  1   2


; 1, 2, 4, 8, 16, 32, 64, 128, 256, 512



;                           / \
;                          / \ 512
;                         / \ 256
;                        / \ 128
;                       / \ 64
;                      / \ 32
;                     / \ 16
;                    / \ 8
;                   / \ 4
;                  1   2

;for the most frequent character is 1 bit
;for the least frequent symbol (n - 1)