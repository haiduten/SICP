#lang racket

;1
; the divison's file should have a type-tag of the name of the devision
; and they need to add an entry 'get-record with that particular divison name
(define (get-record file person)
  (let ((devision (type-tag file)))
    (let ((proc (get 'get-record devision)))
      (if proc (proc (contents file) person) (error "No method get-record for this devision")))))


;2
; each emplyee record file should have a type-tag of the name of the devision
; and they need to add an entry 'get-salary with that particular divison name
(define (get-salary employee-record)
  (let ((devision (type-tag employee-record)))
    (let ((proc (get 'get-salary devision)))
      (if proc (proc (contents employee-record)) (error "No method get-salary for this devision")))))

;3
(define (find-employee-record employee devisions)
  (if (null? devisions) null
      (let ((record (get-record (car devisions) employee)))
        (if (null? record) (find-employee-record employee (cdr devisions)) record))))

;4
; they must add a type-tag and add the entry of get-record and get-salary