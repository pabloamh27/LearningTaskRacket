#lang slideshow
#|
FUNCIONA EL NODO
|#
(struct nodo (padre id# nombre valor hijos)#:transparent #:mutable)

#|Definición de la lista arbol y el nodo neutro|#
(define neutro(nodo "x" "x" "x" "x" "x"))
(define arbol(list neutro))

#|
FUNSIONES

CREATE-TREE (toma los datos ingresados y los inserta en el nodo y luego reemplaza
             el nuevo nodo en lugar del nodo neutro)
|#

(define (create-tree id nombre valor arbol)
;Se van a setear los capos ocupados por el nodo neutro con los valores a introducir en este nodo
  (set-nodo-id#! (first arbol) id)
  (set-nodo-nombre! (first arbol) nombre)
  (set-nodo-valor! (first arbol) valor)
  (print arbol)
)

#|
FIND-NODE(esta funcion busca en la lista arbol in nodo mediante su identificador u otro atributo reconocible)
|#


(define (find-node id arbol)
  (cond[(= (nodo-id# (first arbol)) id)
        (print (nodo-id# (first arbol)))
        (print (nodo-nombre (first arbol)))
        (print (nodo-valor (first arbol)))
        ][else (find-node id (rest arbol)) ])
)

#|
INSERT-NODE(recibe como parametros de entrada las parmamtros: id_padre y los datos del nuevo nodo)
En teoria busca el nodo del padre, agraga en la lista de hijos el nuevo nodo y agrega ese nodo
al arbol.
|#

#|
errores:
FALTA VALIDAR QUE EL ID DEL NUEVO NODO NO ESTÉ OCUPADO
El append al arbol no esta sirviendo
|#
(define (insert-node arbol id-padre id nombre valor)
  (define n-nodo(nodo id-padre id nombre valor "x"))
  ;se crea el nuevo nodo
  (cond[(= (nodo-id# (first arbol)) id-padre)
         (print "entró")
        ;una vez que ya encontró al padre del nodo
        (cond [(eqv? (nodo-hijos (first arbol)) "x")
               (print "primerizo")
        ;una vez determinado que es el primer hijo
               (set-nodo-hijos! (first arbol) (list id));agrega el id del hijo al espacio de hijos del padre
               (set! arbol (append arbol n-nodo));agrega el nuevo nodo al arbol
               ]
               [else (
                       (print "Padre experimetado")
                       (set-nodo-hijos! (first arbol) (append (list id) (nodo-hijos (first arbol))))
                       ;Agrega el id del hijo al espacio de hijos del padre con el apend del nuevo id
                       ;a la lista de hijos preexistente.
                       (set! arbol (append arbol n-nodo));agrega el nuevo nodo al arbol
                       )])
        ][else
          (print "else")
          (insert-node (rest arbol) id-padre id nombre valor)
          ;recusivamente sigue buscando el padre
        ])
  
  )





#|

(create-tree 1 "padre" "padre" arbol)

(insert-node arbol 1 100 "hijo1" "hijo1")

|#





