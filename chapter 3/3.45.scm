#lang sicp
; this is a problem because the progam blocks
; when we call serialized-exchange with a1 and a2
; we must withdrawal from a1. but that a1 cannot run until
; the exchange is complete. but exhange cannot complete without running
; this operation