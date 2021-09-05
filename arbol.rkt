#lang racket
(require pict
         pict/tree-layout)
#| DEFINICION DE STRUCTS|#

#|
define la estructura del arbol, nada mas tiene el node root,
el cual es un node comun y corriente por el cual se empieza todo.
|#
(define-struct struct_tree(root))


#|define la estructura del node, el cual tiene
un id, un name, value y una lista de childs.|#
(define-struct struct_node (id name value childs))


#| FUNCIONES |#

#| Funcion que crea un node, recibe el id, name y el value|#
(define (create-node id name value )
  (make-struct_node id name value (list)))

(define (create-node-with-childs id name value childs)
  (make-struct_node id name value (list)))

#| Funcion que crea el arbol, recibe un node el cual pasara a ser la raiz del arbol.|#
(define (create-tree node)
  (make-struct_tree node))

#| agregar un hijo al node padre y retorna el nodo padre con el nuevo nodo ingresado en sus hijos.|#
(define (add-child father new-child)
 (define father-aux (struct-copy struct_node father [childs (append (struct_node-childs father) (list new-child))]))
 (set! father father-aux)
  father)


#| Recibe el arbol en el cual haremos la busqueda, y recibe el id del nodo que queremos buscar
devuelve falso sino encontro el nodo, o sino devuelve los datos del nodo|#
(define (find-node tree id)
  (cond
    [(empty? (struct_tree-root tree)) false]
    [else (check-node (struct_tree-root tree) id)]))


#| Recibe el nodo y el id para verificar si el id coincide con el id del nodo
primero pregunta si el id coincide,
sino pregunta si no tiene hijos (si tiene hijos hay que seguir buscando) sino tiene hijos devuelve falso
si tiene hijos entonces llama a una funcion que recorre los hijos del nodo para seguir buscando por el id

si los ids coinciden devuelve una lista con los datos del nodo
si los ids no coinciden devuelve falso o busca el id en los hijos del nodo |#
(define (check-node node id)
  (cond
    [(equal? (struct_node-id node) id) (list id (struct_node-name node) (struct_node-value node))]
    [(empty? (struct_node-childs node)) false]
    [else (check-node-childs (struct_node-childs node) id)]))

#| Es una funcion recursiva que recorre una lista de nodos para encontrar un nodo con el id que estamos buscando
termina si la lista de nodo esta vacia
sino esta vacia llama a la funcion check-node para verificar si el id del nodo coincide con el id de busqueda
si check-node devuelve una lista, significa que encontro el nodo y por ende devuelve esta lista
sino sigue recorriendo la lista, si al final se acaba la lista sin encontrar el nodo entonces devuelve falso.|#
(define (check-node-childs node-list id)
  (cond
    [(empty? node-list) #f]
    [else
        (define result (check-node (first node-list) id))
         (cond
               [(list? result) result]
               [else (check-node-childs (list-tail node-list 1) id)])]))


#|Recibe el arbol, el id del padre del nodo que vamos a insertar, y el id, nombre y valor del nuevo nodo
esta funcion crea un nuevo arbol con el nodo insertado
primero llama a una funcion auxiliar, esta va a ayudar a insertar el nodo y devuelve la nueva raiz del arbol|#
(define (insert-node tree father id name value)
  (define new-root (insert-node_aux (struct_tree-root tree) father id name value))
  (define new-tree (struct-copy struct_tree tree [root new-root])) #|se copia el mismo arbol con la nueva raiz |#
   new-tree) #| devuelve un arbol nuevo con el hijo ingreasdo|#


#| recibe la raiz, el id del padre del nodo a insertar, y el id,nombre y valor del nuevo nodo|#
(define (insert-node_aux root father id name value)
  (define new-node (create-node id name value))
  (cond
       [(equal? (struct_node-id root) father)
        (set! root (add-child root new-node))
        root]
     
       [else
        (define new-childs (through-node-list (struct_node-childs root) father new-node))
        (define aux (struct-copy struct_node root [childs new-childs]))
        (set! root aux)
        root]))


#| devuelve la raiz del arbol actualizada con su nuevo hijo en alguna parte|#  
(define (through-node-list node-list father new-node)
  (cond
    [(empty? node-list)
    (list)]
    [else
      (append (list (check-insert (first node-list) father new-node)) (through-node-list (list-tail node-list 1) father new-node))
     ]))

#| se fija si el id del nodo coincide con el id del padre, si coinciden ingresa un hijo|#
(define (check-insert act-node father new-node)
  (cond
    [(equal? (struct_node-id act-node) father) 
             (set! act-node (add-child act-node new-node))
             act-node]
   
    [else
          (define new-childs (through-node-list (struct_node-childs act-node) father new-node))
          (define node-aux (struct-copy struct_node act-node [childs new-childs]))
          (set! act-node node-aux)
           act-node]))

#| DELETE |#

(define (delete-node tree id-node)
  (cond
    [(equal? (delete-root? (struct_tree-root tree) id-node) 1)
     (define new-tree (create-tree (list)))
     new-tree]
    [else
  (define new-root (delete-node-aux (struct_tree-root tree) id-node))
  (define new-tree (struct-copy struct_tree tree [root new-root]))
   new-tree])) #| devuelve un arbol nuevo con el hijo ingreasdo|#

#|funcion para ver si se esta eliminando la raiz, es para fines de evitar bugs |#
(define (delete-root? root id-node)
  (cond
    [(equal? (struct_node-id root) id-node) 1]
    [else 0]
  ))

#| |#
(define (delete-node-aux root id-node)
     (define new-childs (through-node-list2 (struct_node-childs root) id-node))
     (define aux (struct-copy struct_node root [childs new-childs]))
     (set! root aux)
      root) #| devuelve la raiz del arbol actualizada con su nuevo hijo en alguna parte|#  

#| |#
(define (through-node-list2 node-list id)
  (cond
    [(empty? node-list)
    (list)]
    [else
       (define sub-tree (check-delete (first node-list) id))
       (cond
         [(empty? sub-tree) (through-node-list2 (list-tail node-list 1) id)]
         [else (append (list sub-tree)  (through-node-list2 (list-tail node-list 1) id))])]))
#| |#
(define (check-delete act-node id)
  (cond
    [(equal? (struct_node-id act-node) id) 
             (set! act-node (list)) #| convierto el nodo en una lista vacia|#
             (list)]
    [else
          (define new-childs (through-node-list2 (struct_node-childs act-node) id ))
          (define node-aux (struct-copy struct_node act-node [childs new-childs]))
          (set! act-node node-aux)
           act-node]))

#| ANCESTOR |#
#| |#
(define (ancestor tree id)
  (cond
    [(empty? (struct_tree-root tree)) false]
    [else (check-ancestor (struct_tree-root tree) id (struct_tree-root tree) )]))

#|
Recibe el nodo 
|#
(define (check-ancestor node id father)
  (cond
    [(equal? (struct_node-id node) id) (struct_node-id father)]
    [(empty? (struct_node-childs node)) false]
    [else (check-ancestor-childs (struct_node-childs node) id node)]))
#|
Recorre la lista de hijos de un nodo
|#
(define (check-ancestor-childs node-list id father)
  (cond
    [(empty? node-list) #f]
    [else
        (define result (check-ancestor (first node-list) id father))
         (cond
           [(boolean? result) (check-ancestor-childs (list-tail node-list 1) id father)]
           [else result])]))

#|
PRINT PINGA
|#
#| |#
(define (draw tree)
  (define (viz tree)
    (cond
      ((null? tree) #f)
      ((not (pair? tree))
       (tree-layout #:pict (cc-superimpose
                            (disk 30 #:color "white")
                            (text (number->string tree)))))
      ((not (pair? (car tree)))
       (apply tree-layout (map viz (cdr tree))
              #:pict (cc-superimpose
                      (disk 30 #:color "white")
                      (text (number->string (car tree))))))))
  (if (null? tree)
      #f
      (naive-layered (viz tree))))

#| |#
(define (draw-tree tree)
  (define root (struct_tree-root tree))
  (cond
    [(empty? root) false]
    [else (draw (get-tree root))]))
  

#| |#
(define (get-tree node)
  (cond
    [(empty? (struct_node-childs node)) (struct_node-id node) ] #|no tiene hijos entonces devuelvalo solamente |#
    [else (append (list(struct_node-id node)) (get-childs-tree (struct_node-childs node))) ])) #| a cada hijo hay que hacerle get-tree |#

 #| |# 
(define (get-childs-tree list-childs)
  (cond
    [(empty? list-childs) (list)]
    [else (append (list(get-tree (first list-childs))) (get-childs-tree (list-tail list-childs 1)))]))

#|

------------------------------------
TEST #2 , #3 definicions de arbol ingreso de nodos y busqueda de nodos
(define n1 (create-node 1 "1" 1))

(define tree (create-tree n1))
(set! tree (insert-node tree 1 2 "2" 2))
(set! tree (insert-node tree 1 3 "3" 3))

(set! tree (insert-node tree 2 4 "4" 4))
(set! tree (insert-node tree 2 5 "5" 5))

(find-node tree 1)
(find-node tree 2)
(find-node tree 3)
(find-node tree 4)
(find-node tree 5)
#
TEST #3 eliminar un nodo sin hijos
(set! tree (delete-node tree 3))
TEST #4 eliminar un nodo con hijos
(set! tree (delete-node tree 2))

TEST #5 eliminar la raiz
(set! tree (delete-node tree 1))


TEST #6 ancestro de hijo de raiz
(ancestor tree 2)

TEST #7 ancestro de hijo de hoja
(ancestor tree 4)

TEST #7 ancestro de raiz
(ancestor tree 1)
|#

;TEST #8 PRINT TREE
;(draw-tree tree)

