#lang sicp
; imagine a process p1 accesses resource1 to figure out it needs to access resource2
; image a process p2 accesses resource2 to figure out it needs access to resource1
; both these processes need the other and they deadlock