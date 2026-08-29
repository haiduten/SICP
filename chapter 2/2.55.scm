#lang racket
 (car ''abracadabra)
; ''abracadabra evaluates to (qoute (qoute abracadabra)) so it becomes the list (qoute, abracardra)
; thus car should return qoute