#lang racket
(require racket/include)
(require pict3d
         pict3d/universe)
(include "interfaz.rkt")
#|
------------------------------------- Logica ----------------------------
|#
#|                                                       ___________________
________________________________________________________/EJECUCION DEL JUEGO\________________________________________________________
|#

;Funcion auxiliar encargada de convertir que movimiento individual
(define (traducir_aux movimiento)
  (
   list
   ;Primer elemento, eje
   (
    cond
    ((equal? (substring movimiento 0 1) "C") "x")
    (else "y")
    )
   ;Segundo elemento, direccion
   (
    cond ;Si es igual a D o A, entonces es positiva, si no negativa
    ((or (equal? (substring movimiento 2) "D") (equal? (substring movimiento 2) "A") ) 90)
    (else (- 0 90))
    )
   ;Tercer elemento, linea
   (
    - (string->number (substring movimiento 1 2) ) 2
    )
   )
  )


; Funcion encargada de traducir los Movs del profe a movimientos de la interfaz
(define (traducirMovs Movs)
  (
   cond
   ((null? Movs) Movs)
   (else (
	  cons (traducir_aux (car Movs)) (traducirMovs (cdr Movs))
	  ))
   )
  )







#|
*Funcion: aplic_movs
*Argumentos: tamano cubo, cubo identificado por eje, lista con el eje, la direccion y la cara
*Devuelve: cubo con los cambios pertinentes aplicados
|#
(define (aplic_movs X Cubo_identificado EjeDireccionCara)(writeln (list X Cubo_identificado EjeDireccionCara))
        ;ROTACION DEL CUBO ENTERO
  (cond ((> (caddr EjeDireccionCara) X)   ;En caso de que sea una rotacion, el numero de la cara es X+2
       (cond ((equal? (car EjeDireccionCara) 'x)   ;Si es una rotacion en el eje X
              (identificar 'x (reord_rotacion X (rotar X (cadr (cambiar_agrupacion X Cubo_identificado 'x)) (caddr EjeDireccionCara) (cadr EjeDireccionCara)) (cadr EjeDireccionCara) (car EjeDireccionCara))))
             ((equal? (car EjeDireccionCara) 'y)
              (cambiar_agrupacion X (identificar 'y (reord_rotacion X (rotar X (cadr (cambiar_agrupacion X Cubo_identificado 'y)) (caddr EjeDireccionCara) (cadr EjeDireccionCara)) (cadr EjeDireccionCara) (car EjeDireccionCara))) 'x))))
        ;ROTACION DE UNA CARA EN X
      ((equal? (car EjeDireccionCara) 'x)   ;Para el cubo agrupado horizontalmente
       (cond ((> (cadr EjeDireccionCara) 0)   ;Para direccion positiva de rotacion
              ;Aplica rotacion + en X
              (identificar 'x (reordenar_x X (rotar X (cadr (cambiar_agrupacion X Cubo_identificado 'x)) (caddr EjeDireccionCara) 90) (caddr EjeDireccionCara) (caddr EjeDireccionCara) 90)))   ;(llamada recursiva (identifica agrupacion en 'x (reordena colores +x (aplica rotacion))))
             ((< (cadr EjeDireccionCara) 0)   ;Para direccion negativa de rotacion
              ;Aplica rotacion - en X
              (identificar 'x (reordenar_x X (rotar X (cadr (cambiar_agrupacion X Cubo_identificado 'x)) (caddr EjeDireccionCara) -90) (caddr EjeDireccionCara) (caddr EjeDireccionCara) -90)))))   ;(llamada recursiva (identifica agrupacion en 'x (reordena colores -x (aplica rotacion))))
        ;ROTACION DE UNA CARA EN Y
      ((equal? (car EjeDireccionCara) 'y)   ;Para el cubo agrupado verticalmente
       (cond ((< (cadr EjeDireccionCara) 0)   ;Para direccion positiva de rotacion
              ;Aplica rotacion + en Y
              (cambiar_agrupacion X (identificar 'y (reordenar_y X (rotar X (cadr (cambiar_agrupacion X Cubo_identificado 'y)) (caddr EjeDireccionCara) 90) (caddr EjeDireccionCara) (caddr EjeDireccionCara) 90)) 'x))   ;(llamada recursiva (cambia agrupacion de caras moviles a eje x(identifica la agrupacion de caras moviles y(reordena colores +y (aplica rotacion)))))
             ((> (cadr EjeDireccionCara) 0)   ;Para direccion negativa de rotacion
              ;Aplica rotacion - en Y
              (cambiar_agrupacion X (identificar 'y (reordenar_y X (rotar X (cadr (cambiar_agrupacion X Cubo_identificado 'y)) (caddr EjeDireccionCara) -90) (caddr EjeDireccionCara) (caddr EjeDireccionCara) -90)) 'x))))))   ;(llamada recursiva (cambia agrupacion de caras moviles a eje x(identifica la agrupacion de caras moviles y(reordena colores -y (aplica rotacion)))))



(define (cubo_standard_aux i j k x)
(
cond
; Condicion de parada
((> i x) '())

;Aumento de j
((> k x) (cubo_standard_aux i (+ j 1) 1 x))

;Aumento de i
((> j x) (cubo_standard_aux (+ i 1) 1 1 x))

;Vacios
((and (and (and (> i 1) (< i x)) (and (> j 1) (< j x))) (and (> k 1) (< k x)))
(append '(()) (cubo_standard_aux i j (+ k 1) x)))

;Esquinas
((and (equal? i 1) (equal? j 1) (equal? k 1))
(append '((rgba "white") (rgba "green") (rgba "orange")) (cubo_standard_aux i j (+ k 1) x) ))
((and (equal? i 1) (equal? j 1) (equal? k x))
(append '((rgba "white") (rgba "blue") (rgba "orange")) (cubo_standard_aux i j (+ k 1) x) ))
((and (equal? i 1) (equal? j x) (equal? k 1))
(append '((rgba "yellow") (rgba "green") (rgba "orange")) (cubo_standard_aux i j (+ k 1) x) ))
((and (equal? i 1) (equal? j x) (equal? k x))
(append '((rgba "blue") (rgba "yellow") (rgba "orange")) (cubo_standard_aux i j (+ k 1) x) ))
((and (equal? i x) (equal? j 1) (equal? k 1))
(append '((rgba "white") (rgba "green") (rgba "red")) (cubo_standard_aux i j (+ k 1) x) ))
((and (equal? i x) (equal? j 1) (equal? k x))
(append '((rgba "white") (rgba "blue") (rgba "red")) (cubo_standard_aux i j (+ k 1) x) ))
((and (equal? i x) (equal? j x) (equal? k 1))
(append '((rgba "yellow") (rgba "green") (rgba "red")) (cubo_standard_aux i j (+ k 1) x) ))
((and (equal? i x) (equal? j x) (equal? k x))
(append '((rgba "blue") (rgba "yellow") (rgba "red")) (cubo_standard_aux i j (+ k 1) x) ))

;Colores
((equal? i 1)
(
cond
((equal? j 1)
(append '((rgba "white") (rgba "orange")) (cubo_standard_aux i j (+ k 1) x) ))
((equal? j x)
(append '((rgba "yellow") (rgba "orange")) (cubo_standard_aux i j (+ k 1) x) ))
(else (
    cond
    ((equal? k 1) (append '((rgba "green") (rgba "orange")) (cubo_standard_aux i j (+ k 1) x) ))
    ((equal? k x) (append '((rgba "blue") (rgba "orange")) (cubo_standard_aux i j (+ k 1) x) ))
    (else (append '((rgba "orange")) (cubo_standard_aux i j (+ k 1) x) ))
    ))
))
((equal? i x)
(
cond
((equal? j 1) (append '((rgba "white") (rgba "red")) (cubo_standard_aux i j (+ k 1) x) ))
((equal? j x) (append '((rgba "yellow") (rgba "red")) (cubo_standard_aux i j (+ k 1) x) ))
(else (
    cond
    ((equal? k 1) (append '((rgba "green") (rgba "red")) (cubo_standard_aux i j (+ k 1) x) ))
    ((equal? k x) (append '((rgba "blue") (rgba "red")) (cubo_standard_aux i j (+ k 1) x) ))
    (else (append '((rgba "red") ) (cubo_standard_aux i j (+ k 1) x) ))
    ))
))
(else (
  cond
  ((equal? j 1)
   (
    cond
    ((equal? k 1) (append '((rgba "white") (rgba "green")) (cubo_standard_aux i j (+ k 1) x) ))
    ((equal? k x) (append '((rgba "white") (rgba "blue")) (cubo_standard_aux i j (+ k 1) x) ))
    (else (append '( (rgba "white")) (cubo_standard_aux i j (+ k 1) x) ))
    ))
  ((equal? j x)
   (
    cond
    ((equal? k 1) (append '((rgba "yellow") (rgba "green")) (cubo_standard_aux i j (+ k 1) x) ))
    ((equal? k x) (append '((rgba "blue") (rgba "yellow")) (cubo_standard_aux i j (+ k 1) x) ))
    (else (append '((rgba "yellow") ) (cubo_standard_aux i j (+ k 1) x) ))
    ))
  (else
    (
     cond
     ((equal? k 1) (append '((rgba "green") ) (cubo_standard_aux i j (+ k 1) x) ))
     ((equal? k x) (append '((rgba "blue") ) (cubo_standard_aux i j (+ k 1) x) ))
     )
    )
  ))
)
)





#|                                                       _____________________________________
________________________________________________________/FUNCIONES PARA LA CREACION DE UN CUBO\________________________________________________________
|#

#|
*Funcion: arista
*Argumentos: lista de colores, estado de primer vertice
*Devuelve: Una lista de cubos que componen un vertice del Cubo Rubik (cubo de 3 colores, N cubos de 2 colores, cubo de 3 colores)
|#
(define (arista Colores Vert)
(cond ((null? (cdddr Colores))
 (list (list (car Colores) (cadr Colores) (caddr Colores))))
((false? Vert)   ;En caso
 (cons (list (car Colores) (cadr Colores) (caddr Colores)) (arista (cdddr Colores) #t)))
(else (cons (list (car Colores) (cadr Colores)) (arista (cddr Colores) Vert)))))


;Function that creates a cube of nxn if there's no cube input
(define (cubo_standard X)
(cond
((> X 0) ( ; Cubo valido
       cubo_standard_aux 1 1 1 X
      ))
(else #f)
)
)



#|
*Funcion: centro_cara
*Argumentos: lista de colores, estado de primer borde
*Devuelve: Una lista de cubos que componen un centro de una cara (cubo de 2 colores, N cubos de un color, cubo de 2 colores)
|#
(define (centro_cara Colores Borde)
(cond ((null? (cddr Colores))
 (list (list (car Colores) (cadr Colores))))
((false? Borde)
 (cons (list (car Colores) (cadr Colores)) (centro_cara (cddr Colores) #t)))
(else (cons (list (car Colores)) (centro_cara (cdr Colores) Borde)))))

#|
*Funcion: centro_interior
*Argumentos: tamano cubo, lista de colores
*Devuelve: Una lista de cubos que componen un centro interior (un color por cubo)
|#
(define (centro_interior X Colores)
(cond ((equal? X 0)
 '())
((null? (car Colores))
 (cons (car Colores) (centro_interior (- X 1) (cdr Colores))))
(else (cons (list (car Colores)) (centro_interior (- X 1) (cdr Colores))))))

#|
*Funcion: get_elementos
*Argumentos: cantidad de elementos, lista de elementos
*Devuelve: Una sublista que contiene los elementos deseados
|#
(define (get_elementos Cantidad Lista)
(cond ((equal? Cantidad 0)
 '())
(else (cons (car Lista) (get_elementos (- Cantidad 1) (cdr Lista))))))

#|
*Funcion: borrar_elementos
*Argumentos: cantidad de elementos, lista de elementos
*Devuelve: la misma lista - (los primeros elementos )
|#
(define (borrar_elementos Cantidad Lista)
(cond ((equal? Cantidad 0)
 Lista)
(else (borrar_elementos (- Cantidad 1) (cdr Lista)))))



#|                                                       _____________________________________
________________________________________________________/FUNCIONES PAR DARLE FORMATO A UN CUBO\________________________________________________________
|#

;PASO 1 -> CUBO POR FILAS
#|
*Funcion: format_x
*Argumentos: tamano cubo, lista de colores, contador fila, contador de columna
*Devuelve: Cubo de Rubik agrupado por filas en el eje Z
|#
(define (format_x X Colores Fila Columna)
(cond ((> Columna X)   ;Columna se excede => termina
 '())
((> Fila X)   ;Fila se excede => Columna+1
 (format_x X Colores 1 (+ Columna 1)))
((or (equal? Fila 1) (equal? Fila X))   ;Fila 1 o N y Columna 1 o N => arista
 (cond ((or (equal? Columna 1) (equal? Columna X))
	(cons (arista (get_elementos (+ 6 (* 2 (- X 2))) Colores) #f) (format_x X (borrar_elementos (+ 6 (* 2 (- X 2))) Colores) (+ Fila 1) Columna)))
       ((and (> Columna 1) (< Columna X))   ;Fila 1 o N y 1<Columna<N => anade centro_cara
	(cons (centro_cara (get_elementos (+ 4 (- X 2)) Colores) #f) (format_x X (borrar_elementos (+ 4 (- X 2)) Colores) (+ Fila 1) Columna)))))
((and (or (equal? Columna 1) (equal? Columna X)) (and (> Fila 1) (< Fila X)))   ;Columna 1 o N y 1<Fila<X => anade centro_cara
 (cons (centro_cara (get_elementos (+ 4 (- X 2)) Colores) #f) (format_x X (borrar_elementos (+ 4 (- X 2)) Colores) (+ Fila 1) Columna)))
((and (and (> Fila 1) (< Fila X)) (and (> Columna 1) (< Columna X)))   ;1<Columna<X y 1<Fila<X => Centro interior
 (cons (centro_interior X (get_elementos X Colores)) (format_x X (borrar_elementos X Colores) (+ Fila 1) Columna)))))

;PASO 2 -> CUBO POR CARAS MOVILES
#|
*Funcion: caras_moviles
*Argumentos: tamano cubo, cubo agrupado por filas
*Devuelve: Cubo de Rubik agrupado por filas caras moviles
|#
(define (caras_moviles X Cubo)
(cond ((null? Cubo)
 '())
(else (cons (get_elementos X Cubo) (caras_moviles X (borrar_elementos X Cubo))))))

;PASO 3 -> CUBO IDENTIFICADO POR SU EJE DE ORIENTACION
#|
*Funcion: identificar
*Argumentos: tamano cubo, eje de orientacion, lista de colores del cubo
*Devuelve: pareja de eje y cubo agrupado por caras
|#
(define (identificar Eje Cubo)
(list Eje Cubo))



#|                                                       ______________________________________
________________________________________________________/Funciones para aplicar cambios al cubo\________________________________________________________
|#


#|
*Funcion: cambiar_agrupacion
*Argumentos: tamano cubo, cubo identificado por eje, nuevo eje de orientacion
*Devuelve: Cubo de Rubik agrupado por caras moviles horizontales o verticales
|#
(define (cambiar_agrupacion X Cubo_identificado Eje_nuevo)
(cond ((equal? (car Cubo_identificado) Eje_nuevo)   ;Si ya esta agrupado correctamente, no hacer cambio
 Cubo_identificado)
(else
 (cond ((equal? Eje_nuevo 'x)
	(identificar 'x (caras_moviles X (caras_moviles X (cambiar_agrupacion_x X (cadr Cubo_identificado) 1 1 1)))))
       ((equal? Eje_nuevo 'y)
	(identificar 'y (caras_moviles X (caras_moviles X (cambiar_agrupacion_y X (cadr Cubo_identificado) 1 1 1)))))))))

#|
*Auxiliar: cambiar_agrupacion_x
*Argumentos: tamano cubo, cubo agrupado por caras moviles verticales, contador cara, contador fila, contador elemento
*Devuelve: Cubo de Rubik agrupado por caras moviles en X (horizontales)
|#
(define (cambiar_agrupacion_x X Cubo Cara Fila Elemento)
(cond ((> Fila X)
 '())
((> Elemento X)
 (cambiar_agrupacion_x X Cubo 1 (+ Fila 1) 1))
((> Cara X)
 (cambiar_agrupacion_x X Cubo 1 Fila (+ Elemento 1)))
(else (cons (buscar_elemento Cubo Cara Fila Elemento) (cambiar_agrupacion_x X Cubo (+ Cara 1) Fila Elemento)))))

#|
*Auxiliar: cambiar_agrupacion_y
*Argumentos: tamano cubo, cubo agrupado por caras moviles horizontales, contador cara, contador fila, contador elemento
*Devuelve: Cubo de Rubik agrupado por caras moviles en Y (verticales)
|#
(define (cambiar_agrupacion_y X Cubo Cara Fila Elemento)
(cond ((> Elemento X)
 '())
((> Cara X)
 (cambiar_agrupacion_y X Cubo 1 Fila (+ Elemento 1)))
((> Fila X)
 (cambiar_agrupacion_y X Cubo (+ Cara 1) 1 Elemento))
(else (cons (buscar_elemento Cubo Cara Fila Elemento) (cambiar_agrupacion_y X Cubo Cara (+ Fila 1) Elemento)))))

#|
*Funcion: buscar_elemento
*Argumentos: lista original, contador cara, contador fila, contador elemento
*Devuelve: elemento buscado por cara, fila y numero de elemento
|#
(define (buscar_elemento Lista Cara Fila Elemento)
(cond ((null? Lista)
 '())
((zero? Elemento)
 (car Lista))
((zero? (- Cara 1))   ;Si encuentro la cara del cubo, la devuelvo
 (buscar_elemento (car Lista) (- Cara 1) Fila Elemento))
((> Cara 0)   ;Si no he encontrado la cara, sigo buscandola
 (buscar_elemento (cdr Lista) (- Cara 1) Fila Elemento))
((zero? (- Fila 1))   ;Si encuentro la fila/columna, la devuelvo
 (buscar_elemento (car Lista) Cara (- Fila 1) Elemento))
((> Fila 0)
 (buscar_elemento (cdr Lista) Cara (- Fila 1) Elemento))
((zero? (- Elemento 1))
 (buscar_elemento Lista Cara Fila (- Elemento 1)))
((> Elemento 0)
 (buscar_elemento (cdr Lista) Cara Fila (- Elemento 1)))))

#|
*Funcion: rotar
*Argumentos: tamano cubo, cubo, contador cara, direccion (+90 o -90) '+
*Devuelve: cubo resultante de la rotacion a una de sus caras
|#
(define (rotar X Cubo Num_cara Direccion)
(cond ((>= X Num_cara)  ;Si cara menor o igual al tamano, es rotacion de una sola cara
 (cond ((equal? Num_cara 1)   ;Si ya estoy en la cara indicada
	(cond ((> Direccion 0)
	 (cons (caras_moviles X (rotar+ X (car Cubo) X 1)) (cdr Cubo)))
	((< Direccion 0)
	 (cons (caras_moviles X (rotar- X (car Cubo) 1 X)) (cdr Cubo)))))
 (else (cons (car Cubo) (rotar X (cdr Cubo) (- Num_cara 1) Direccion)))))
(else (rotacion X Cubo Direccion X))))   ;Rota el cubo completo

#|
*Auxiliar: rotar+
*Argumentos: tamano cubo, cara por rotar, contador fila (decreciente), contador elemento (creciente)
*Devuelve: cara del cubo rotada en direccion positiva
|#
(define (rotar+ X Cara Fila Elem)
(cond ((> Elem X)
 '())
((zero? Fila)
 (rotar+ X Cara X (+ Elem 1)))
(else (cons (buscar_elemento Cara 0 Fila Elem) (rotar+ X Cara (- Fila 1) Elem)))))

#|
*Auxiliar: rotar-
*Argumentos: tamano cubo, cara por rotar, contador fila (creciente), contador elemento (decreciente)
*Devuelve: cara del cubo rotada en direccion negativa
|#
(define (rotar- X Cara Fila Elem)
(cond ((zero? Elem)
 '())
((> Fila X)
 (rotar- X Cara 1 (- Elem 1)))
(else (cons (buscar_elemento Cara 0 Fila Elem) (rotar- X Cara (+ Fila 1) Elem)))))

#|
*Auxiliar: rotacion
*Argumentos: tamano cubo, cubo, direccion, contador de caras (decreciente)
*Devuelve: cubo rotado
|#
(define (rotacion X Cubo Direccion Contador_caras)
(cond ((zero? Contador_caras)
 '())
((> Direccion 0)
 (cons (caras_moviles X (rotar+ X (car Cubo) X 1)) (rotacion X (cdr Cubo) Direccion (- Contador_caras 1))))
((< Direccion 0)
 (cons (caras_moviles X (rotar- X (car Cubo) 1 X)) (rotacion X (cdr Cubo) Direccion (- Contador_caras 1))))))


#|
*Funcion: reord_rotacion
*Argumentos: tamano cubo, cubo, direccion, eje
*Devuelve: cubo rotado y con sus colores reordenados
|#
(define (reord_rotacion X Cubo Direccion Eje)
  (cond ((equal? Eje 'x)
         (cond ((> Direccion 0)
                (reord_rotacion_+x X Cubo))
               ((< Direccion 0)
                (reord_rotacion_-x X Cubo))))
        ((equal? Eje 'y)
         (cond ((> Direccion 0)
                (reord_rotacion_+y X Cubo 1))
               ((< Direccion 0)
                (reord_rotacion_-y X Cubo 1))))))


#|
*Auxiliar: reord_rotacion_+x
*Argumentos: tamano cubo, cubo
*Devuelve: cubo rotado y con sus colores reordenados en +x
|#
(define (reord_rotacion_+x X Cubo)
  (cond ((null? (cdr Cubo))
         (list (reord_cara+x X 1 '() 1 (car Cubo))))
        (else (cons (reord_cara+x X 1 '() 1 (car Cubo)) (reord_rotacion_+x X (cdr Cubo))))))


#|
*Auxiliar: reord_rotacion_-x
*Argumentos: tamano cubo, cubo
*Devuelve: cubo rotado y con sus colores reordenados en +x
|#
(define (reord_rotacion_-x X Cubo)
  (cond ((null? (cdr Cubo))
         (list (reord_cara-x X 1 '() 1 (car Cubo))))
        (else (cons (reord_cara-x X 1 '() 1 (car Cubo)) (reord_rotacion_-x X (cdr Cubo))))))


#|
*Auxiliar: reord_rotacion_+y
*Argumentos: tamano cubo, cubo, contador de caras
*Devuelve: cubo rotado y con sus colores reordenados en +y
|#
(define (reord_rotacion_+y X Cubo Contador_caras)
  (cond ((equal? Contador_caras X)
         (list (reord_ultima_cara+y X 1 '() 1 (car Cubo))))
        ((equal? Contador_caras 1)
         (cons (reord_primera_cara+y X 1 '() 1 (car Cubo)) (reord_rotacion_+y X (cdr Cubo) (+ Contador_caras 1))))
        (else (cons (reord_cara_centro_y X 1 '() 1 (car Cubo)) (reord_rotacion_+y X (cdr Cubo) (+ Contador_caras 1))))))


#|
*Auxiliar: reord_rotacion_-y
*Argumentos: tamano cubo, cubo, contador de caras
*Devuelve: cubo rotado y con sus colores reordenados en +y
|#
(define (reord_rotacion_-y X Cubo Contador_caras)
  (cond ((equal? Contador_caras X)
         (list (reord_ultima_cara-y X 1 '() 1 (car Cubo))))
        ((equal? Contador_caras 1)
         (cons (reord_primera_cara-y X 1 '() 1 (car Cubo)) (reord_rotacion_-y X (cdr Cubo) (+ Contador_caras 1))))
        (else (cons (reord_cara_centro_y X 1 '() 1 (car Cubo)) (reord_rotacion_-y X (cdr Cubo) (+ Contador_caras 1))))))


#|
*Funcion: get_cara
*Argumentos: cubo (individual), numero de cara
*Devuelve: cara que corresponde a la columna N
|#
(define (get_cara Cubo Cara)
(cond ((equal? Cara 1)
 (car Cubo))
(else (get_cara (cdr Cubo) (- Cara 1)))))

#|
_________________________________REORDENAR COLORES CARAS VERTICALES_________________________________
|#


#|
*Funcion: reordenar_y (se aplica despues de la rotacion)
*Argumentos: tamano cubo, cubo, numero de cara, contador de cara (regresivo), direccion (+90 o -90)
*Devuelve: cubo resultante de la reorganizacion de colores en la cara rotada
|#
(define (reordenar_y X Cubo Cara Cont_cara Direccion)
(cond ((equal? Cont_cara 1)
 (cond ((> Direccion 0)   ;Si la rotacion fue en el eje +y
	(cond ((equal? Cara 1)   ;Si la cara rotada es la primera (de izq a der)
	       (cons (reord_primera_cara+y X 1 '() 1 (car Cubo)) (cdr Cubo)))
	      ((equal? Cara X)   ;Si la cara rotada es la ultima (de izq a der)
	       (list (reord_ultima_cara+y X 1 '() 1 (car Cubo))))   ;se debe ingresar en la lista por la ausencia de "cons"
	      (else (cons (reord_cara_centro_y X 1 '() 1 (car Cubo)) (cdr Cubo)))))   ;Si la cara rotada corresponde a las del medio
       ((< Direccion 0)   ;Si la rotacion fue en el eje -y
	(cond ((equal? Cara 1)   ;Si la cara rotada es la primera (de izq a der)
	       (cons (reord_primera_cara-y X 1 '() 1 (car Cubo)) (cdr Cubo)))
	      ((equal? Cara X)   ;Si la cara rotada es la ultima (de izq a der)
	       (list (reord_ultima_cara-y X 1 '() 1 (car Cubo))))   ;se debe ingresar en la lista por la ausencia de "cons"
	      (else (cons (reord_cara_centro_y X 1 '() 1 (car Cubo)) (cdr Cubo)))))))   ;No hay distincion de direccion porque no aplica al patron de cambio
(else (cons (car Cubo) (reordenar_y X (cdr Cubo) Cara (- Cont_cara 1) Direccion)))))



#|
*Funcion: reord_primera_cara+y
*Argumentos: tamano cubo, contador de fila actual, lista de la fila con cubitos reordenados, contador de cubito actual, cara original del cubo
*Devuelve: la primer cara reorganizada segun rotacion +y
|#
(define (reord_primera_cara+y X Cont_fila Nueva_Fila Cubito Cara_orig)
(cond ((> Cont_fila X)   ;Si la fila excede el tamano, termina
 '())
((> Cubito X)   ;Si el cubito excede el tamano, completa esa fila y sigue
 (cons Nueva_Fila (reord_primera_cara+y X (+ Cont_fila 1) '() 1 (cdr Cara_orig))))
((and (equal? Cubito 1) (equal? Cont_fila 1))   ;Si esta en la primer esquina
 (reord_primera_cara+y X Cont_fila (append Nueva_Fila (list (reacomodar_3 '(3 2 1) (caar Cara_orig)))) (+ Cubito 1) (cons (cdar Cara_orig) (cdr Cara_orig))))
((and (equal? Cubito X) (equal? Cont_fila 1))   ;Si esta en la segunda esquina
 (reord_primera_cara+y X Cont_fila (append Nueva_Fila (list (reacomodar_3 '(3 2 1) (caar Cara_orig)))) (+ Cubito 1) (cons (cdar Cara_orig) (cdr Cara_orig))))
((and (equal? Cubito X) (equal? Cont_fila X))   ;Si esta en la tercera esquina
 (reord_primera_cara+y X Cont_fila (append Nueva_Fila (list (reacomodar_3 '(3 2 1) (caar Cara_orig)))) (+ Cubito 1) (cons (cdar Cara_orig) (cdr Cara_orig))))
((and (equal? Cubito 1) (equal? Cont_fila X))   ;Si esta en la cuarta esquina
 (reord_primera_cara+y X Cont_fila (append Nueva_Fila (list (reacomodar_3 '(3 2 1) (caar Cara_orig)))) (+ Cubito 1) (cons (cdar Cara_orig) (cdr Cara_orig))))
((equal? (length_logic (caar Cara_orig) 0) 2)   ;Si esta en una arista (cubo de 2 colores)
 (reord_primera_cara+y X Cont_fila (append Nueva_Fila (list (reacomodar_2 (caar Cara_orig)))) (+ Cubito 1) (cons (cdar Cara_orig) (cdr Cara_orig))))
(else (reord_primera_cara+y X Cont_fila (append Nueva_Fila (list (caar Cara_orig))) (+ Cubito 1) (cons (cdar Cara_orig) (cdr Cara_orig))))))   ;Si no corresponde a un cubito que se altere, lo anade a la lista


#|
*Funcion: reord_primera_cara-y
*Argumentos: tamano cubo, contador de fila actual, lista de la fila con cubitos reordenados, contador de cubito actual, cara original del cubo
*Devuelve: la primer cara reorganizada segun rotacion -y
|#
(define (reord_primera_cara-y X Cont_fila Nueva_Fila Cubito Cara_orig)
(cond ((> Cont_fila X)   ;Si la fila excede el tamano, termina
 '())
((> Cubito X)   ;Si el cubito excede el tamano, completa esa fila y sigue
 (cons Nueva_Fila (reord_primera_cara-y X (+ Cont_fila 1) '() 1 (cdr Cara_orig))))
((and (equal? Cubito 1) (equal? Cont_fila 1))   ;Si esta en la primer esquina
 (reord_primera_cara-y X Cont_fila (append Nueva_Fila (list (reacomodar_3 '(3 2 1) (caar Cara_orig)))) (+ Cubito 1) (cons (cdar Cara_orig) (cdr Cara_orig))))
((and (equal? Cubito X) (equal? Cont_fila 1))   ;Si esta en la segunda esquina
 (reord_primera_cara-y X Cont_fila (append Nueva_Fila (list (reacomodar_3 '(3 2 1) (caar Cara_orig)))) (+ Cubito 1) (cons (cdar Cara_orig) (cdr Cara_orig))))
((and (equal? Cubito X) (equal? Cont_fila X))   ;Si esta en la tercera esquina
 (reord_primera_cara-y X Cont_fila (append Nueva_Fila (list (reacomodar_3 '(3 2 1) (caar Cara_orig)))) (+ Cubito 1) (cons (cdar Cara_orig) (cdr Cara_orig))))
((and (equal? Cubito 1) (equal? Cont_fila X))   ;Si esta en la cuarta esquina
 (reord_primera_cara-y X Cont_fila (append Nueva_Fila (list (reacomodar_3 '(3 2 1) (caar Cara_orig)))) (+ Cubito 1) (cons (cdar Cara_orig) (cdr Cara_orig))))
((equal? (length_logic (caar Cara_orig) 0) 2)   ;Si esta en una arista (cubo de 2 colores)
 (reord_primera_cara-y X Cont_fila (append Nueva_Fila (list (reacomodar_2 (caar Cara_orig)))) (+ Cubito 1) (cons (cdar Cara_orig) (cdr Cara_orig))))
(else (reord_primera_cara-y X Cont_fila (append Nueva_Fila (list (caar Cara_orig))) (+ Cubito 1) (cons (cdar Cara_orig) (cdr Cara_orig))))))   ;Si no corresponde a un cubito que se altere, lo anade a la lista



#|
*Funcion: reord_ultima_cara+y
*Argumentos: tamano cubo, contador de fila actual, lista de la fila con cubitos reordenados, contador de cubito actual, cara original del cubo
*Devuelve: la ultima cara reorganizada segun rotacion +y
|#
(define (reord_ultima_cara+y X Cont_fila Nueva_Fila Cubito Cara_orig)
(cond ((> Cont_fila X)   ;Si la fila excede el tamano, termina
 '())
((> Cubito X)   ;Si el cubito excede el tamano, completa esa fila y sigue
 (cons Nueva_Fila (reord_ultima_cara+y X (+ Cont_fila 1) '() 1 (cdr Cara_orig))))
((and (equal? Cubito 1) (equal? Cont_fila 1))   ;Si esta en la primer esquina
 (reord_ultima_cara+y X Cont_fila (append Nueva_Fila (list (reacomodar_3 '(3 2 1) (caar Cara_orig)))) (+ Cubito 1) (cons (cdar Cara_orig) (cdr Cara_orig))))
((and (equal? Cubito X) (equal? Cont_fila 1))   ;Si esta en la segunda esquina
 (reord_ultima_cara+y X Cont_fila (append Nueva_Fila (list (reacomodar_3 '(2 3 1) (caar Cara_orig)))) (+ Cubito 1) (cons (cdar Cara_orig) (cdr Cara_orig))))
((and (equal? Cubito X) (equal? Cont_fila X))   ;Si esta en la tercera esquina
 (reord_ultima_cara+y X Cont_fila (append Nueva_Fila (list (reacomodar_3 '(1 3 2) (caar Cara_orig)))) (+ Cubito 1) (cons (cdar Cara_orig) (cdr Cara_orig))))
((and (equal? Cubito 1) (equal? Cont_fila X))   ;Si esta en la cuarta esquina
 (reord_ultima_cara+y X Cont_fila (append Nueva_Fila (list (reacomodar_3 '(3 1 2) (caar Cara_orig)))) (+ Cubito 1) (cons (cdar Cara_orig) (cdr Cara_orig))))
((and (equal? Cubito 1) (and (not (equal? Cont_fila 1)) (not (equal? Cont_fila X))))   ;Si esta en la arista de la cara delantera (frente)
 (reord_ultima_cara+y X Cont_fila (append Nueva_Fila (list (reacomodar_2 (caar Cara_orig)))) (+ Cubito 1) (cons (cdar Cara_orig) (cdr Cara_orig))))
((and (equal? Cont_fila 1) (and (not (equal? Cubito 1)) (not (equal? Cubito X))))   ;Si esta en la arista de la cara superior (arriba)
 (reord_ultima_cara+y X Cont_fila (append Nueva_Fila (list (reacomodar_2 (caar Cara_orig)))) (+ Cubito 1) (cons (cdar Cara_orig) (cdr Cara_orig))))
(else (reord_ultima_cara+y X Cont_fila (append Nueva_Fila (list (caar Cara_orig))) (+ Cubito 1) (cons (cdar Cara_orig) (cdr Cara_orig))))))   ;Si no corresponde a un cubito que se altere, lo anade a la lista


#|
*Funcion: reord_ultima_cara-y
*Argumentos: tamano cubo, contador de fila actual, lista de la fila con cubitos reordenados, contador de cubito actual, cara original del cubo
*Devuelve: la ultima cara reorganizada segun rotacion -y
|#
(define (reord_ultima_cara-y X Cont_fila Nueva_Fila Cubito Cara_orig)
(cond ((> Cont_fila X)   ;Si la fila excede el tamano, termina
 '())
((> Cubito X)   ;Si el cubito excede el tamano, completa esa fila y sigue
 (cons Nueva_Fila (reord_ultima_cara-y X (+ Cont_fila 1) '() 1 (cdr Cara_orig))))
((and (equal? Cubito 1) (equal? Cont_fila 1))   ;Si esta en la primer esquina
 (reord_ultima_cara-y X Cont_fila (append Nueva_Fila (list (reacomodar_3 '(3 1 2) (caar Cara_orig)))) (+ Cubito 1) (cons (cdar Cara_orig) (cdr Cara_orig))))
((and (equal? Cubito X) (equal? Cont_fila 1))   ;Si esta en la segunda esquina
 (reord_ultima_cara-y X Cont_fila (append Nueva_Fila (list (reacomodar_3 '(1 3 2) (caar Cara_orig)))) (+ Cubito 1) (cons (cdar Cara_orig) (cdr Cara_orig))))
((and (equal? Cubito X) (equal? Cont_fila X))   ;Si esta en la tercera esquina
 (reord_ultima_cara-y X Cont_fila (append Nueva_Fila (list (reacomodar_3 '(2 3 1) (caar Cara_orig)))) (+ Cubito 1) (cons (cdar Cara_orig) (cdr Cara_orig))))
((and (equal? Cubito 1) (equal? Cont_fila X))   ;Si esta en la cuarta esquina
 (reord_ultima_cara-y X Cont_fila (append Nueva_Fila (list (reacomodar_3 '(3 2 1) (caar Cara_orig)))) (+ Cubito 1) (cons (cdar Cara_orig) (cdr Cara_orig))))
((and (equal? Cubito 1) (and (not (equal? Cont_fila 1)) (not (equal? Cont_fila X))))   ;Si esta en la arista de la cara delantera (frente)
 (reord_ultima_cara-y X Cont_fila (append Nueva_Fila (list (reacomodar_2 (caar Cara_orig)))) (+ Cubito 1) (cons (cdar Cara_orig) (cdr Cara_orig))))
((and (equal? Cont_fila X) (and (not (equal? Cubito 1)) (not (equal? Cubito X))))   ;Si esta en la arista de la cara inferior (abajo)
 (reord_ultima_cara-y X Cont_fila (append Nueva_Fila (list (reacomodar_2 (caar Cara_orig)))) (+ Cubito 1) (cons (cdar Cara_orig) (cdr Cara_orig))))
(else (reord_ultima_cara-y X Cont_fila (append Nueva_Fila (list (caar Cara_orig))) (+ Cubito 1) (cons (cdar Cara_orig) (cdr Cara_orig))))))   ;Si no corresponde a un cubito que se altere, lo anade a la lista


#|
*Funcion: reord_cara_centro_y
*Argumentos: tamano cubo, contador de fila actual, lista de la fila con cubitos reordenados, contador de cubito actual, cara original del cubo
*Devuelve: cara pertinente reorganizada (indiferente de direccion)
|#
(define (reord_cara_centro_y X Cont_fila Nueva_Fila Cubito Cara_orig)
(cond ((> Cont_fila X)   ;Si la fila excede el tamano, termina
 '())
((> Cubito X)   ;Si el cubito excede el tamano, completa esa fila y sigue
 (cons Nueva_Fila (reord_cara_centro_y X (+ Cont_fila 1) '() 1 (cdr Cara_orig))))
((equal? (length_logic (caar Cara_orig) 0) 2)  ;Si no es un cubo sin color () o un cubo de un solo elemento (2), respectivamente; es decir, uno de dos colores
       (reord_cara_centro_y X Cont_fila (append Nueva_Fila (list (reacomodar_2 (caar Cara_orig)))) (+ Cubito 1) (cons (cdar Cara_orig) (cdr Cara_orig))))
(else (reord_cara_centro_y X Cont_fila (append Nueva_Fila (list (caar Cara_orig))) (+ Cubito 1) (cons (cdar Cara_orig) (cdr Cara_orig))))))   ;Si no corresponde a un cubito que se altere, lo anade a la lista



#|
_________________________________REORDENAR COLORES CARAS HORIZONTALES_________________________________
|#



#|
*Funcion: reordenar_x (se aplica despues de la rotacion)
*Argumentos: tamano cubo, cubo, numero de cara, contador de cara (regresivo), direccion (+90 o -90)
*Devuelve: cubo resultante de la reorganizacion de colores en la cara rotada
|#
(define (reordenar_x X Cubo Cara Cont_cara Direccion)
(cond ((equal? Cont_cara 1)
 (cond ((> Direccion 0)   ;Si la rotacion fue en el eje +x
	(cons (reord_cara+x X 1 '() 1 (car Cubo)) (cdr Cubo)))   ;Aplcia para cualquier cara
       ((< Direccion 0)   ;Si la rotacion fue en el eje -x
	(cons (reord_cara-x X 1 '() 1 (car Cubo)) (cdr Cubo)))))   ;Aplica para cualquier cara
(else (cons (car Cubo) (reordenar_x X (cdr Cubo) Cara (- Cont_cara 1) Direccion)))))   ;Continua armando el cubo si no ha llegado a la cara deseada



#|
*Funcion: reord_cara+x
*Nota:se deben hacer cambios en los cubos que conforman las dos aristas del frente en el cubo (las columnas de las esquinas 1 y 2 de izquierda a derecha)
*Argumentos: tamano cubo, contador de fila actual, lista de la fila con cubitos reordenados, contador de cubito actual, cara original del cubo
*Devuelve: cara reorganizada segun rotacion +x
|#
(define (reord_cara+x X Cont_fila Nueva_Fila Cubito Cara_orig)
(cond ((> Cont_fila X)   ;Si la fila excede el tamano, termina
 '())
((> Cubito X)   ;Si el cubito excede el tamano, completa esa fila y sigue
 (cons Nueva_Fila (reord_cara+x X (+ Cont_fila 1) '() 1 (cdr Cara_orig))))
((or (and (equal? Cubito 1) (equal? Cont_fila 1)) (and (equal? Cubito X) (equal? Cont_fila 1)))   ;Si esta en la columna izquierda o derecha, respectivamente
 (cond ((equal? 3 (length_logic (caar Cara_orig) 0))   ;Si el cubo se trata de una esquina
	(reord_cara+x X Cont_fila (append Nueva_Fila (list (reacomodar_3 '(2 1 3) (caar Cara_orig)))) (+ Cubito 1) (cons (cdar Cara_orig) (cdr Cara_orig))))
       ((equal? 2 (length_logic (caar Cara_orig) 0))   ;Si el cubo se trata de una arista
	(reord_cara+x X Cont_fila (append Nueva_Fila (list (reacomodar_2 (caar Cara_orig)))) (+ Cubito 1) (cons (cdar Cara_orig) (cdr Cara_orig))))))
(else (reord_cara+x X Cont_fila (append Nueva_Fila (list (caar Cara_orig))) (+ Cubito 1) (cons (cdar Cara_orig) (cdr Cara_orig))))))   ;Si no corresponde a un cubito que se altere, lo anade a la lista


#|
*Funcion: reord_cara-x
*Nota:se deben hacer cambios en los cubos que conforman las aristas de la izquierda en el cubo (las columnas de las esquinas 1 y 3 de izquierda a derecha y del frente hacia atras)
*Argumentos: tamano cubo, contador de fila actual, lista de la fila con cubitos reordenados, contador de cubito actual, cara original del cubo
*Devuelve: cara reorganizada segun rotacion -x
|#
(define (reord_cara-x X Cont_fila Nueva_Fila Cubito Cara_orig)
(cond ((> Cont_fila X)   ;Si la fila excede el tamano, termina
 '())
((> Cubito X)   ;Si el cubito excede el tamano, completa esa fila y sigue
 (cons Nueva_Fila (reord_cara-x X (+ Cont_fila 1) '() 1 (cdr Cara_orig))))
((or (and (equal? Cubito 1) (equal? Cont_fila 1)) (and (equal? Cubito 1) (equal? Cont_fila X)))   ;Si esta en la columna izquierda delantera o trasera, respectivamente
 (cond ((equal? 3 (length_logic (caar Cara_orig) 0))   ;Si el cubo se trata de una esquina
	(reord_cara-x X Cont_fila (append Nueva_Fila (list (reacomodar_3 '(2 1 3) (caar Cara_orig)))) (+ Cubito 1) (cons (cdar Cara_orig) (cdr Cara_orig))))
       ((equal? 2 (length_logic (caar Cara_orig) 0))   ;Si el cubo se trata de una arista
	(reord_cara-x X Cont_fila (append Nueva_Fila (list (reacomodar_2 (caar Cara_orig)))) (+ Cubito 1) (cons (cdar Cara_orig) (cdr Cara_orig))))))
(else (reord_cara-x X Cont_fila (append Nueva_Fila (list (caar Cara_orig))) (+ Cubito 1) (cons (cdar Cara_orig) (cdr Cara_orig))))))   ;Si no corresponde a un cubito que se altere, lo anade a la lista


#|
*Funcion: length_logic
*Argumentos: lista, contador
*Devuelve: tamano de la lista
|#
(define (length_logic lista contador)
(cond ((null? lista)
 contador)
(else (length_logic (cdr lista) (+ contador 1)))))


#|
*Funcion: reacomodar_3
*Argumentos: lista del nuevo orden de los colores, cubito de 3 colores
*Devuelve: nuevo cubito de 3 colores reacomodado
|#
(define (reacomodar_3 Orden Cubito)
(cond ((null? Orden)   ;Si la lista de orden se vacio, retorna el nuevo cubito
 '())
((equal? (car Orden) 1)    ;Si sigue el primer elemento del cubo original
 (cons (car Cubito) (reacomodar_3 (cdr Orden) Cubito)))
((equal? (car Orden) 2)    ;Si sigue el segundo elemento del cubo original
 (cons (cadr Cubito) (reacomodar_3 (cdr Orden) Cubito )))
((equal? (car Orden) 3)    ;Si sigue el tercer elemento del cubo original
 (cons (caddr Cubito) (reacomodar_3 (cdr Orden) Cubito)))))


#|
*Funcion: reacomodar_2
*Argumentos: cubito de 2 colores
*Devuelve: nuevo cubito de 2 colores reacomodado
|#
(define (reacomodar_2 Cubito)
(list (cadr Cubito) (car Cubito)))   ;Solo basta con reacomodar sus dos elementos

(define (RS X Cubo Movs)
(cond ((> (round X) 1)
  (cond ((null? Cubo)
	  (big-bang3d (list (caras_moviles X (format_x X (cubo_standard X) 1 1)) X '() (traducirMovs Movs X)  "x" '()  ) #:on-draw on-draw #:on-frame on-frame #:frame-delay (/ 1000 140) #:on-mouse on-mouse))   ;Si no ingresa un cubo, genera un cubo NxN estandar
	(else ( big-bang3d (list (caras_moviles X (format_x X Cubo 1 1)) X '() (traducirMovs Movs X )  "x" '()  ) #:on-draw on-draw #:on-frame on-frame #:frame-delay (/ 1000 140) #:on-mouse on-mouse))))
(else #f)))


(RS 3 '() '("C4A"))
