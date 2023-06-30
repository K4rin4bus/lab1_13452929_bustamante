#lang Racket
(require "tdaDrive.rkt")
(provide system)
(provide make-system)
(provide get-system-name)
(provide get-system-users)
(provide get-system-drives)
(provide get-system-current-user)
(provide get-system-current-drive)
(provide get-system-current-path)
(provide get-system-content)
(provide get-system-creation-date)
(provide exists-system-drive?)
(provide exists-system-user?)
(provide system-add-drive)
(provide system-register)
(provide system-login)
(provide system-logout)
(provide system-switch-drive)

#|  TDA system  |#

; Capa Constructora - TDA system
; Dom: name (str)
; Rec: system
; Descripción: Función que crea sistema operativo vacio
(define (system name)
  (list name '() '() "" #\0 "" '() (current-seconds)))

; Descripción: Función que crea sistema operativo con datos
(define make-system
  (lambda (name users drives current-user current-drive current-path content creation-date)
    (list name users drives current-user current-drive current-path content creation-date)))


;; Capa Selectora - TDA System
;; Dom: name (str)
;; Rec: Atributo del sistema
;; Descripción: Funciones que seleccionan un atributo del sistema operativo
(define get-system-name car)  ;Nombre sistema
(define get-system-users cadr) ;Lista usuarios sistema
(define get-system-drives caddr) ;Lista de unidades de disco sistema
(define get-system-current-user cadddr) ;Usuario actual del sistema
(define get-system-current-drive (lambda (system) (car (cdr (cdr (cdr (cdr system))))))) ;Drive actual sistema
(define get-system-current-path (lambda (system) (car (cdr (cdr (cdr (cdr (cdr system)))))))) ;Path actual sistema
(define get-system-content (lambda (system) (car (cdr (cdr (cdr (cdr (cdr (cdr system))))))))) ;Contenido sistema
(define get-system-creation-date (lambda (system) (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr system)))))))))) ;Fecha creacion sistema


;; Capa Pertenencia - TDA System
;; Dom: system
;; Rec: Boolean
;; Descripción: Funciones que verifican existencia de un atributo del sistema operativo
;; member verifica si existe un elemento de una lista
(define (exists-system-drive? letter system) ;existe letra unidad en sistema
  (member letter (map get-drive-letter (get-system-drives system))))
 
(define (exists-system-user? username system)  ;existe el usuario en el sistema?
  (member username (get-system-users system))) 


;; Capa Modificadora - TDA System

;; Dom: drive
;; Rec: system
;; Descripción: Función que modifica atributo drive del sistema operativo
(define system-add-drive
  (lambda (system new-drive)
    (make-system (get-system-name system)
                 (get-system-users system)
                 (cons new-drive (get-system-drives system))
                 (get-system-current-user system)
                 (get-system-current-drive system)
                 (get-system-current-path system)
                 (get-system-content system)
                 (get-system-creation-date system))))


;; Dom: user
;; Rec: system
;; Descripción: Función que modifica atributo user del sistema operativo
(define system-register
  (lambda (system new-user)
    (make-system (get-system-name system)
                 (cons new-user (get-system-users system))
                 (get-system-drives system)
                 (get-system-current-user system)
                 (get-system-current-drive system)
                 (get-system-current-path system)
                 (get-system-content system)
                 (get-system-creation-date system))))


;; Dom: current-user
;; Rec: system
;; Descripción: Función que modifica atributo current-user, inicio de sesion del sistema operativo
(define system-login
  (lambda (system current-user)
    (make-system (get-system-name system)
                 (get-system-users system)
                 (get-system-drives system)
                 current-user ;debo pasarle el usuario actual
                 (get-system-current-drive system)
                 (get-system-current-path system)
                 (get-system-content system)
                 (get-system-creation-date system))))


;; Dom: system
;; Rec: system
;; Descripción: Función que modifica atributo current-user, cierra sesion sistema operativo
(define system-logout
  (lambda (system)
    (make-system (get-system-name system)
                 (get-system-users system)
                 (get-system-drives system)
                 "" 
                 (get-system-current-drive system)
                 (get-system-current-path system)
                 (get-system-content system)
                 (get-system-creation-date system))))


;; Dom: 
;; Rec: system
;; Descripción: Función que modifica atributo current-drive del sistema operativo
(define system-switch-drive
  (lambda (system letter)
    (make-system (get-system-name system)
                 (get-system-users system)
                 (get-system-drives system)
                 (get-system-current-user system)
                 letter
                 (get-system-current-path system)
                 (get-system-content system)
                 (get-system-creation-date system))))


