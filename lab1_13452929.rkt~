#lang Racket

;; Representación TDA System:
;; Type System = name (String) X users (String list) X drives (drive list) X
;;               current-user (String) X current-drive (char) X current-path (String)


;; Representación TDA Drive:
;; TDA Drive: letter (Char) x name (String) x capacity (int)


;; Capa Constructora - TDA system
;; Dom: name (str)
;; Rec: system
;; Descripción: Función que crea sistema operativo vacio
(define (system name)
  (list name '() '() "" #\0 ""))

;; Descripción: Función que crea sistema operativo con datos
(define make-system
  (lambda (name users drives current-user current-drive current-path)
    (list name users drives current-user current-drive current-path)))


;; Capa Selectora - TDA System
(define get-system-name car)  ;System -> String
(define get-system-users cadr) ;System -> String List
(define get-system-drives caddr) ;System -> Drive List
(define get-system-current-user cadddr) ;System -> String
(define get-system-current-drive (lambda (system) (car (cdr (cdr (cdr (cdr system))))))) ;System -> char
(define get-system-current-path (lambda (system) (car (cdr (cdr (cdr (cdr (cdr system)))))))) ;System -> String


;; Capa Pertenencia - TDA System
;; member verifica si existe un elemento de una lista
(define (exists-system-drive? letter system) ;;existe letra unidad en sistema
  (member letter (map get-drive-letter (get-system-drives system))))


;; Capa Modificadora - TDA System
;; agregar nuevo drive (se usa en RF4 add-drive)
(define system-add-drive
  (lambda (system new-drive)
    (make-system (get-system-name system)
                 (get-system-users system)
                 (cons new-drive (get-system-drives system))
                 (get-system-current-user system)
                 (get-system-current-drive system)
                 (get-system-current-path system))))




;; Capa Constructora - TDA Drive
;; Dom: 
;; Rec:
;; Descripción: Función que crea unidad de disco
(define make-drive
  (lambda (letter name capacity)
    (list letter name capacity)))

;; Capa Selectora - TDA Drive
(define get-drive-letter car)



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
(define add-drive
  (lambda (system)
    (lambda (letter name capacity)
      (if (not (exists-system-drive? letter system)) ;; comprovar que letra de la unidad es única
          (system-add-drive system    ;;Si es verdadero, creo unidad en sistema
               (make-drive letter name capacity))
          system)))) ;;else return system



;; ===== Script de pruebas =====
;creando un sistema RF2.-
(define S0 (system "newSystem"))
S0
;ejecutando comando para agregar unidad de disco RF4.-
(define S1 ((run S0 add-drive) #\C "OS" 10000000000))
S1