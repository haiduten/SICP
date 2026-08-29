#lang racket
; explicit dispatch
; to add new type:
; modify each operation to account for this new type

; to add new operation:
; add new operation. Make sure each existing type is included

; data-directed style
; to add new type:
; add a new entry in the operation-type table for all the operations

; to add new operation:
; in each module of every type add a new entry with that new operation

; message-passing style
; to add new type:
; create a new dispatch object that supports all the perations

; to add a new operation:
; update all the objects to handle that message

;For organization where new types are added often: data-directed

;For organization where new methods are added often: data-directed