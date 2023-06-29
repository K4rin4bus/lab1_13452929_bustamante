#lang Racket

;; Representación TDA System:
;; Type System = name (String) X users (String list) X drives (drive list) X
;;               current-user (String) X current-drive (char) X current-path (String) X
;;               date (num) X folder (string)

;; Representación TDA Drive:
;; TDA Drive: letter (Char) X name (String) X capacity (int)

;; Representación TDA Folder
;; TDA Folder rutaInicial (String) X newFolder (String)

;; Representación TDA Rutas:
;; TDA Rutas: ruta (String) X file (String list)

;; Representación TDA Ruta
;; TDA Ruta: pathname (String) X extention (String) X content (String)

;; Representación TDA File:
;; TDA File: filename (String) X  extention (String) X content (String)
;;           atrSecurity (Char) X atrReading (Char) X modyficationDate (int)






;; Capa Constructora - TDA system
;; Dom: name (str)
;; Rec: system
;; Descripción: Función que crea sistema operativo vacio
(define (system name)
  (list name '() '() "" #\0 "" (current-seconds)))

;; Descripción: Función que crea sistema operativo con datos
(define make-system
  (lambda (name users drives current-user current-drive current-path creation-date)
    (list name users drives current-user current-drive current-path creation-date)))


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
(define get-system-creation-date (lambda (system) (car (cdr (cdr (cdr (cdr (cdr (cdr system))))))))) ;Fecha creacion sistema


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
                 (get-system-creation-date system))))





;; Capa Constructora - TDA Drive

;; Dom: letter X name X capacity
;; Rec: Drive
;; Descripción: Función que crea unidad de disco
(define make-drive
  (lambda (letter name capacity)
    (list letter name capacity)))


;; Capa Selectora - TDA Drive

;; Dom: drive
;; Rec: Atributo del drive
;; Descripción: Funciones que seleccionan un atributo del drive
(define get-drive-letter car)
(define get-drive-name cadr)
(define get-drive-capacity caddr)


;; Capa Constructora - TDA Folder

;; Dom: rutaInical X newFolder
;; Rec: folder
;; Descripción: Función que crea una carpeta
(define (add-folder ruta newFolder)
  (string-append ruta "/" newFolder "/"))


(define (make-path ruta usuario)
  (list ruta usuario (current-seconds)))


;; Capa Constructora - TDA File

;; Dom: filename X extention X content X atrSecurity X atrReading
;; Rec: file
;; Descripción: Función que crea un archivo
(define (file filename extention content .atributos)
    (list filename extention content .atributos))






;; ===== Requerimientos Funcionales =====

;; RF3. TDA system - run
;; Dom: system X command
;; Rec: system
;; Descripción: Función que ejecuta un comando (o función) en el sistema operativo
(define (run system cmd)
  (cmd system))


;; RF4. TDA system - add-drive
;; Dom: system X drive
;; Rec: system
;; Descripción: Función que permite agregar una unidad de disco al sistema operativo, letra única
(define add-drive
  (lambda (system)
    (lambda (letter name capacity)
      (if (not (exists-system-drive? letter system)) ;; comprovar que letra de la unidad es única
          (system-add-drive system    ;;Si es verdadero, creo unidad en sistema
               (make-drive letter name capacity))
          system)))) ;;else return system


;; RF5. TDA system - register
;; Dom: System X
;;      username (str)
;; Rec: System
;; Descripción: Función que permite usuario al sistema operativo
(define register
  (lambda (system)
    (lambda (username)
      (if (not (exists-system-user? username system)) ;; si usuario no existe, entonces agregar
          (system-register system username) ;; retornar sistema
          system)))) ;; si usuario existe, retornar sistema sin cambios


;; RF6. TDA system - login
;; Dom: System X
;;      username (str)
;; Rec: System
;; Descripción: Función que permite iniciar sesion en el sistema operativo
(define login
  (lambda (system)
    (lambda (username)
      (if (string=? (get-system-current-user system) "") ;; si no hay usuario logeado
          (system-login system username) ;;se logea
          system)))) ;; si existe usuario activo, retorna sistema sin cambios


;; RF7. TDA system - logout
;; Dom: System 
;; Rec: System
;; Descripción: Función que permite cerrar sesion en el sistema operativo
(define (logout system)
      (if (not (string=? (get-system-current-user system) "")) ;; si hay usuario logeado
          (system-logout system) ;;cierra sesion
          system)) ;; si no, retorna sistema sin cambios


;; RF8. TDA system - swicth-drive
;; Dom: System X
;;      letter (char)
;; Rec: System
;; Descripción: Función que permite fijar unidad e disco, usuario logeado
(define switch-drive
  (lambda (system)
    (lambda (letter)
      (if (not (string=? (get-system-current-user system) "")) ;; si hay usuario logeado

          (if (exists-system-drive? letter system) ;; comprobar que la unidad existe
                (system-switch-drive system letter) ;;fija unidad
                system)
          system)))) ;;else return system



;; RF9. TDA system - md
;; Dom: System X
;;      name (string)
;; Rec: System
;; Descripción: Función que permite crear directorio en unidad de disco, usuario logeado



;; ===== Script de pruebas =====
;creando un sistema RF2.-
(define S0 (system "newSystem"))
S0
;ejecutando comando para agregar unidad de disco c: RF4.-
(define S1 ((run S0 add-drive) #\C "SO" 1000))
S1
;ejecutando comando para agregar unidad de disco existente (c:), 
(define S2 ((run S1 add-drive) #\C "SO1" 3000)) ;no agrega y retorna system actual sin cambios
S2
;ejecutando comando para agregar unidad de disco d:
(define S3 ((run S2 add-drive) #\D "Util" 2000))
S3

;añadiendo usuarios. 
(define S4 ((run S3 register) "user1")) ;agrega usuario
S4
(define S5 ((run S4 register) "user1")) ;intenta agregar usuario existente, retorna system actual sin cambios
S5
(define S6 ((run S5 register) "user2")) ;agrega usuario
S6

;Inicio de sesión. 
(define S7 ((run S6 login) "user1")) ;inicia sesion usuario 1
S7
(define S8 ((run S7 login) "user2")) ;inicia sesion usuario2, sin deslogaer user1
S8
(define S9 (run S8 logout)) ;inicia sesion usuario2, sin salir antes de user1
S9
(define S10 ((run S9 login) "user2"))
S10

;cambios de unidad, incuyendo unidad inexistente (current-drive)
(define S11 ((run S10 switch-drive) #\K))
S11
(define S12 ((run S11 switch-drive) #\C))
S12

;Añadiendo carpetas, incluye caso de carpetas duplicadas
;(define S13 ((run S12 md) "folder1"))
;S13
;(define S14 ((run S13 md) "folder2"))
;S14
;(define S15 ((run S14 md) "folder2"))
;S15
;(define S16 ((run S15 md) "folder3"))
;S16

