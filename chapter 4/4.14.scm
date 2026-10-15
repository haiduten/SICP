#lang sicp
; it fails what if the function is a lambda expression. it evals a lambda expression as (list 'procedure parameters body env).
; map would pass the arguments to the list but it is expecting a function