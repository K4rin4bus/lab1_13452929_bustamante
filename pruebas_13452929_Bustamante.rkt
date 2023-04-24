#lang racket

;; ===== Script de pruebas =====
;creando un sistema
(define S0 (system "MiSystem"))
S0

;agregar unidad de disco
(define S1 ((run S0 add-drive) #\C "macOS" 5000))
S1
(define S2 ((run S1 add-drive) #\D "Datos" 4000))
S2
(define S3 ((run S2 add-drive) #\D "macOSsierra" 3000))
S3

;añadiendo usuarios. 
(define S4 ((run S3 register) "karina")) 
S4
(define S5 ((run S4 register) "carlos")) 
S5
(define S6 ((run S5 register) "juan")) 
S6
(define S7 ((run S6 register) "karina")) 
S7

;Inicio de sesión. 
(define S8 ((run S7 login) "karina")) ;inicia sesion karina
S8
(define S9 ((run S8 login) "juan")) ;inicia sesion juan, sin deslogaer karina
S9

;cierra sesion y cambia de usuario
(define S10 (run S9 logout)) ;cierra sesion usuario actual
S10
(define S11 ((run S10 login) "carlos"))
S11

;cambios de unidad, incuyendo unidad inexistente (current-drive)
(define S12 ((run S11 switch-drive) #\D))
S12
(define S13 ((run S12 switch-drive) #\F))
S13

;Añadiendo carpetas, incluye caso de carpetas duplicadas
