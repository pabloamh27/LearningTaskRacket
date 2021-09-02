#lang racket
; ------------------------------------------------------
; Instituto Tecnologico de Costa Rica
; ------------------------------------------------------
;                                    Pablo Muñoz Hidalgo
;                                 Jonathan Quesada Salas
; ------------------------------------------------------

; ---------------------- PROYECTO ----------------------

; ------------------------------------------------------
;                                                  ARBOL
; ------------------------------------------------------
;                                ESTRUCTURAS PRINCIPALES
; ------------------------------------------------------
;                    Estructura que va a tener cada nodo
; ------------------------------------------------------
; Aparte de sus valores se va a tener otro valor que 
; será hijoeste mismo nos va a ayudar para tener un 
; mejor manejo entre los arboles y subarboles
(struct nodo(ID nombre valor hijo)#:transparent #:mutable)

; ------------------------------------------------------
;                   Estructura que va a tener cada arbol
; ------------------------------------------------------
; La estructura del los respectivos antecesores 
(struct tree (antecesor)#:transparent #:mutable)

; ------------------------------------------------------
;                                  ESTRUCTURA SECUNDARIA
; ------------------------------------------------------
; Se define una estructura para cada nodo correspodiente
(define (creacion-nodo ID nombre valor hijo)(define nuevo-nodo(nodo ID nombre valor hijo))nuevo-nodo)

; ------------------------------------------------------
;                                     CREACION DEL ARBOL
; ------------------------------------------------------
(define (create-tree ID nombre valor)(define arbol-nuevo(tree(creacion-nodo ID nombre valor (list))))arbol-nuevo)

; ------------------------------------------------------
;                                 INSERTAR UN NUEVO NODO
; ------------------------------------------------------
; Entradas: Arbol
;           Nodo padre
;           ID
;           Nombre
;           Valor
;
; Salida:   El arbol con el nuevo nodo
; ------------------------------------------------------

(define(insert-node arbol padre ID nombre valor)(define recorrido (tree-antecesor arbol))
                                                (define insertar-padre(find-node padre arbol))
                                                (define insertar-nodo(creacion-nodo ID nombre valor '()))
  ; Si existe el nodo
  (cond[(not (equal? #f (find-node ID arbol)))(displayln "El nodo digitado ya existe")]  ; Se imprime que el nodo ya existe
  ; En caso que no exista se procede a insertar el nuevo nodo a la izquierda, cambiando la lista de hijos
  [else(set-nodo-hijo! insertar-padre(list* insertar-nodo (nodo-hijo insertar-padre)))]))


; ------------------------------------------------------
;                                         ENCONTRAR NODO
; ------------------------------------------------------
; Entradas: Arbol
;           Nodo a buscar
;
; Salida:   ID
;           Nombre
;           Valor
; ------------------------------------------------------

(define (find-node arbol ID)
  ; Se recorre todo el arbol
    (define comparativa (list (tree-antecesor arbol)))
  ; Si no esta en el arbol se retorna el mensaje
  (define estado "Nodo no encontrado")
(define (recorrer-nodo ID lista-nodos)
  ; Si el ID del primer nodo es el que estamos buscando
  (define nodo-actual (first lista-nodos))
     ; Se retorna el nodo si es del ID que buscamos
     (cond [(= (nodo-ID nodo-actual) ID) (set! estado nodo-actual)]
           ; Si no es el primer nodo analizado se recorre el arbol
           [else (cond[(not(empty? (nodo-hijo nodo-actual)))(repeticion ID (nodo-hijo nodo-actual))])]))

; Recorre todos los hijos simple y cuando no este vacio el arbol
(define (repeticion ID hijo)(unless(empty? hijo)
                               (recorrer-nodo ID hijo)(cond[(equal? estado "Nodo no encontrado")
                                                            (repeticion ID (rest hijo))])))
                                                                  ; Se llama la funcion repeticion y se retorna si nunca encontro el nodo
                                                                  (repeticion ID comparativa) estado)

; ------------------------------------------------------
;                                          ELIMINAR NODO
; ------------------------------------------------------
; Entradas: Arbol
;           Nodo a eliminar
;
; Salida:   Nuevo arbol 
; ------------------------------------------------------

(define(delete-note arbol ID)(define eliminado (find-node arbol ID)) ; funcion interna
                             (define (eliminar-nodo ID nodo hijo)    ; funcion interna
                                  ; Se cambian los datos con el primer hijo del nodo a elimiar
                                  (set-nodo-ID! nodo(nodo-ID(first hijo)))
                                  (set-nodo-nombre! nodo(nodo-nombre(first hijo)))
                                  (set-nodo-valor! nodo(nodo-valor(first hijo)))
                              ; Se retiene los posibles hijos que pueda tener el nodo a eliminar
                              (define hijo-almacen (nodo-hijo (first hijo)))
                                  ; Se procede a eliminar al hijo en la primera posicion de la lista
                                  (set-nodo-hijo! nodo (remove*(list (first hijo))(nodo-hijo nodo)))
                                  ;
                                  (set-nodo-hijo! nodo(append hijo-almacen(nodo-hijo nodo))))
                                  ; Elimina si es el caso que el nodo contenga hijos
                                  (cond[(not(empty?(nodo-hijo eliminado)))(eliminar-nodo ID eliminado(nodo-hijo eliminado))]
                                       ; En caso que no tenga hijos, simplemente se borra el nodo
                                       [else(define padre(ancestor id arbol))(set-nodo-hijo! padre (remove* (list eliminado)(nodo-hijo padre)))]))


; ------------------------------------------------------
;                                     BUSCAR EL ANCESTRO
; ------------------------------------------------------
; Entradas: Arbol
;           Nodo para buscar el ancestro
;
; Salida:   El nodo ancestro, que se compone de:
;               - ID
;               - Nombre
;               - Valor
; ------------------------------------------------------
(define(ancestor arbol ID))
; ------------------------------------------------------------------------------------ Domino ----------------------------------------------------------------------------------------------------------------------------------------------------------------------

(define (crear-fichas)(define lista-fichas(list (list 0 0) (list 0 1) (list 1 1) (list 0 2)
                                                (list 1 2) (list 2 2) (list 0 3) (list 1 3)
                                                (list 2 3) (list 3 3) (list 0 4) (list 1 4)
                                                (list 2 4) (list 3 4) (list 4 4) (list 0 5)
                                                (list 1 5) (list 2 5) (list 3 5) (list 4 5)
                                                (list 5 5) (list 0 6) (list 1 6) (list 2 6)
                                                (list 3 6) (list 4 6) (list 5 6) (list 6 6))) lista-fichas)

(crear-fichas)


(define (deleteItem lst item)
  (cond ((null? lst)
         '())
        ((equal? item (car lst))
         (cdr lst))
        (else
         (cons (car lst) 
               (deleteItem (cdr lst) item)))))

(deleteItem (crear-fichas) '(0 0))
