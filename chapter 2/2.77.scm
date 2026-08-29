#lang racket
; this works because we are adding 'magnitude and 'complex to the operation-type table
; when calling magnitude z, apply generic looks up 'magnitude and 'complex to return magnitude
; magnitude itself is called with the contents of 'complex
; magnitude itself is implemented with apply generic and it is called with 'magnitude and 'rectangular
; this calls the magnitude for the rectangular package which is square root of 3^2 + 4^2 = 5