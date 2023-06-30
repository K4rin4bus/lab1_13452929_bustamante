#lang Racket
(provide make-drive)
(provide get-drive-letter)
(provide get-drive-name)
(provide get-drive-capacity)

#| Capa Constructora - TDA Drive  |#

; Dom: letter X name X capacity
; Rec: Drive
; Descripción: Función que crea unidad de disco
(define make-drive
  (lambda (letter name capacity)
    (list letter name capacity)))


#| Capa Selectora - TDA Drive  |#

; Dom: drive
; Rec: Atributo del drive
; Descripción: Funciones que seleccionan un atributo del drive
(define get-drive-letter car)
(define get-drive-name cadr)
(define get-drive-capacity caddr)
