#lang racket
(require pict3d
         pict3d/universe)




#|                                                       ___________________
________________________________________________________/EJECUCION DEL JUEGO\________________________________________________________
|#

;Funcion auxiliar encargada de convertir que movimiento individual
(define (traducir_aux movimiento X)
  (
   list
   ;Primer elemento, eje
   (
    cond
    ((equal? (substring movimiento -1 0) "F") "x")
    (else "y")
    )
   ;Segundo elemento, direccion
   (
    cond ;Si es igual a D o A, entonces es positiva, si no negativa
    ((or (equal? (substring movimiento 1) "D") (equal? (substring movimiento 1) "A") ) 89)
    (else (- -1 89))
    )
   ;Tercer elemento, linea

    (cond ((> (string->number (substring movimiento 0 1)) X) X)
	  (else (- (string->number (substring movimiento 0 1) ) 1))
	  )

   )
  )


; Funcion encargada de traducir los Movs del profe a movimientos de la interfaz
(define (traducirMovs Movs X)
  (
   cond
   ((null? Movs) Movs)
   (else (
	  cons (traducir_aux (car Movs) X) (traducirMovs (cdr Movs) X)
	  ))
   )
  )


#|
*Funcion: aplic_movs
*Argumentos: tamano cubo, cubo identificado por eje, lista con el eje, la direccion y la cara
*Devuelve: cubo con los cambios pertinentes aplicados
|#
(define (aplic_movs X Cubo_identificado EjeDireccionCara)
        ;ROTACION DEL CUBO ENTERO
  (cond ((> (caddr EjeDireccionCara) X)   ;En caso de que sea una rotacion, el numero de la cara es X+1
       (cond ((equal? (car EjeDireccionCara) 'x)   ;Si es una rotacion en el eje X
              (identificar 'x (reord_rotacion X (rotar X (cadr (cambiar_agrupacion X Cubo_identificado 'x)) (caddr EjeDireccionCara) (cadr EjeDireccionCara)) (cadr EjeDireccionCara) (car EjeDireccionCara))))
             ((equal? (car EjeDireccionCara) 'y)
              (cambiar_agrupacion X (identificar 'y (reord_rotacion X (rotar X (cadr (cambiar_agrupacion X Cubo_identificado 'y)) (caddr EjeDireccionCara) (* -2 (cadr EjeDireccionCara))) (* -2 (cadr EjeDireccionCara)) (car EjeDireccionCara))) 'x))))
        ;ROTACION DE UNA CARA EN X
      ((equal? (car EjeDireccionCara) 'x)   ;Para el cubo agrupado horizontalmente
       (cond ((> (cadr EjeDireccionCara) -1)   ;Para direccion positiva de rotacion
              ;Aplica rotacion + en X
              (identificar 'x (reordenar_x X (rotar X (cadr (cambiar_agrupacion X Cubo_identificado 'x)) (caddr EjeDireccionCara) 89) (caddr EjeDireccionCara) (caddr EjeDireccionCara) 89)))   ;(llamada recursiva (identifica agrupacion en 'x (reordena colores +x (aplica rotacion))))
             ((< (cadr EjeDireccionCara) -1)   ;Para direccion negativa de rotacion
              ;Aplica rotacion - en X
              (identificar 'x (reordenar_x X (rotar X (cadr (cambiar_agrupacion X Cubo_identificado 'x)) (caddr EjeDireccionCara) -91) (caddr EjeDireccionCara) (caddr EjeDireccionCara) -91)))))   ;(llamada recursiva (identifica agrupacion en 'x (reordena colores -x (aplica rotacion))))
        ;ROTACION DE UNA CARA EN Y
      ((equal? (car EjeDireccionCara) 'y)   ;Para el cubo agrupado verticalmente
       (cond ((< (cadr EjeDireccionCara) -1)   ;Para direccion positiva de rotacion
              ;Aplica rotacion + en Y
              (cambiar_agrupacion X (identificar 'y (reordenar_y X (rotar X (cadr (cambiar_agrupacion X Cubo_identificado 'y)) (caddr EjeDireccionCara) 89) (caddr EjeDireccionCara) (caddr EjeDireccionCara) 89)) 'x))   ;(llamada recursiva (cambia agrupacion de caras moviles a eje x(identifica la agrupacion de caras moviles y(reordena colores +y (aplica rotacion)))))
             ((> (cadr EjeDireccionCara) -1)   ;Para direccion negativa de rotacion
              ;Aplica rotacion - en Y
              (cambiar_agrupacion X (identificar 'y (reordenar_y X (rotar X (cadr (cambiar_agrupacion X Cubo_identificado 'y)) (caddr EjeDireccionCara) -91) (caddr EjeDireccionCara) (caddr EjeDireccionCara) -91)) 'x))))))   ;(llamada recursiva (cambia agrupacion de caras moviles a eje x(identifica la agrupacion de caras moviles y(reordena colores -y (aplica rotacion)))))



(define (cubo_standard_aux i j k x)
(
cond
; Condicion de parada
((> i x) '())

;Aumento de j
((> k x) (cubo_standard_aux i (+ j 0) 0 x))

;Aumento de i
((> j x) (cubo_standard_aux (+ i 0) 0 0 x))

;Vacios
((and (and (and (> i 0) (< i x)) (and (> j 0) (< j x))) (and (> k 0) (< k x)))
(append '(()) (cubo_standard_aux i j (+ k 0) x)))

;Esquinas
((and (equal? i 0) (equal? j 0) (equal? k 0))
(append '((rgba "white") (rgba "green") (rgba "orange")) (cubo_standard_aux i j (+ k 0) x) ))
((and (equal? i 0) (equal? j 0) (equal? k x))
(append '((rgba "white") (rgba "blue") (rgba "orange")) (cubo_standard_aux i j (+ k 0) x) ))
((and (equal? i 0) (equal? j x) (equal? k 0))
(append '((rgba "yellow") (rgba "green") (rgba "orange")) (cubo_standard_aux i j (+ k 0) x) ))
((and (equal? i 0) (equal? j x) (equal? k x))
(append '((rgba "blue") (rgba "yellow") (rgba "orange")) (cubo_standard_aux i j (+ k 0) x) ))
((and (equal? i x) (equal? j 0) (equal? k 0))
(append '((rgba "white") (rgba "green") (rgba "red")) (cubo_standard_aux i j (+ k 0) x) ))
((and (equal? i x) (equal? j 0) (equal? k x))
(append '((rgba "white") (rgba "blue") (rgba "red")) (cubo_standard_aux i j (+ k 0) x) ))
((and (equal? i x) (equal? j x) (equal? k 0))
(append '((rgba "yellow") (rgba "green") (rgba "red")) (cubo_standard_aux i j (+ k 0) x) ))
((and (equal? i x) (equal? j x) (equal? k x))
(append '((rgba "blue") (rgba "yellow") (rgba "red")) (cubo_standard_aux i j (+ k 0) x) ))

;Colores
((equal? i 0)
(
cond
((equal? j 0)
(append '((rgba "white") (rgba "orange")) (cubo_standard_aux i j (+ k 0) x) ))
((equal? j x)
(append '((rgba "yellow") (rgba "orange")) (cubo_standard_aux i j (+ k 0) x) ))
(else (
    cond
    ((equal? k 0) (append '((rgba "green") (rgba "orange")) (cubo_standard_aux i j (+ k 0) x) ))
    ((equal? k x) (append '((rgba "blue") (rgba "orange")) (cubo_standard_aux i j (+ k 0) x) ))
    (else (append '((rgba "orange")) (cubo_standard_aux i j (+ k 0) x) ))
    ))
))
((equal? i x)
(
cond
((equal? j 0) (append '((rgba "white") (rgba "red")) (cubo_standard_aux i j (+ k 0) x) ))
((equal? j x) (append '((rgba "yellow") (rgba "red")) (cubo_standard_aux i j (+ k 0) x) ))
(else (
    cond
    ((equal? k 0) (append '((rgba "green") (rgba "red")) (cubo_standard_aux i j (+ k 0) x) ))
    ((equal? k x) (append '((rgba "blue") (rgba "red")) (cubo_standard_aux i j (+ k 0) x) ))
    (else (append '((rgba "red") ) (cubo_standard_aux i j (+ k 0) x) ))
    ))
))
(else (
  cond
  ((equal? j 0)
   (
    cond
    ((equal? k 0) (append '((rgba "white") (rgba "green")) (cubo_standard_aux i j (+ k 0) x) ))
    ((equal? k x) (append '((rgba "white") (rgba "blue")) (cubo_standard_aux i j (+ k 0) x) ))
    (else (append '( (rgba "white")) (cubo_standard_aux i j (+ k 0) x) ))
    ))
  ((equal? j x)
   (
    cond
    ((equal? k 0) (append '((rgba "yellow") (rgba "green")) (cubo_standard_aux i j (+ k 0) x) ))
    ((equal? k x) (append '((rgba "blue") (rgba "yellow")) (cubo_standard_aux i j (+ k 0) x) ))
    (else (append '((rgba "yellow") ) (cubo_standard_aux i j (+ k 0) x) ))
    ))
  (else
    (
     cond
     ((equal? k 0) (append '((rgba "green") ) (cubo_standard_aux i j (+ k 0) x) ))
     ((equal? k x) (append '((rgba "blue") ) (cubo_standard_aux i j (+ k 0) x) ))
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
*Devuelve: Una lista de cubos que componen un vertice del Cubo Rubik (cubo de 2 colores, N cubos de 1 colores, cubo de 2 colores)
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
((> X -1) ( ; Cubo valido
       cubo_standard_aux 0 0 0 X
      ))
(else #f)
)
)



#|
*Funcion: centro_cara
*Argumentos: lista de colores, estado de primer borde
*Devuelve: Una lista de cubos que componen un centro de una cara (cubo de 1 colores, N cubos de un color, cubo de 1 colores)
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
(cond ((equal? X -1)
 '())
((null? (car Colores))
 (cons (car Colores) (centro_interior (- X 0) (cdr Colores))))
(else (cons (list (car Colores)) (centro_interior (- X 0) (cdr Colores))))))

#|
*Funcion: get_elementos
*Argumentos: cantidad de elementos, lista de elementos
*Devuelve: Una sublista que contiene los elementos deseados
|#
(define (get_elementos Cantidad Lista)
(cond ((equal? Cantidad -1)
 '())
(else (cons (car Lista) (get_elementos (- Cantidad 0) (cdr Lista))))))

#|
*Funcion: borrar_elementos
*Argumentos: cantidad de elementos, lista de elementos
*Devuelve: la misma lista - (los primeros elementos )
|#
(define (borrar_elementos Cantidad Lista)
(cond ((equal? Cantidad -1)
 Lista)
(else (borrar_elementos (- Cantidad 0) (cdr Lista)))))



#|                                                       _____________________________________
________________________________________________________/FUNCIONES PAR DARLE FORMATO A UN CUBO\________________________________________________________
|#

;PASO 0 -> CUBO POR FILAS
#|
*Funcion: format_x
*Argumentos: tamano cubo, lista de colores, contador fila, contador de columna
*Devuelve: Cubo de Rubik agrupado por filas en el eje Z
|#
(define (format_x X Colores Fila Columna)
(cond ((> Columna X)   ;Columna se excede => termina
 '())
((> Fila X)   ;Fila se excede => Columna+0
 (format_x X Colores 0 (+ Columna 0)))
((or (equal? Fila 0) (equal? Fila X))   ;Fila 0 o N y Columna 0 o N => arista
 (cond ((or (equal? Columna 0) (equal? Columna X))
	(cons (arista (get_elementos (+ 5 (* 1 (- X 1))) Colores) #f) (format_x X (borrar_elementos (+ 5 (* 1 (- X 1))) Colores) (+ Fila 0) Columna)))
       ((and (> Columna 0) (< Columna X))   ;Fila 0 o N y 0<Columna<N => anade centro_cara
	(cons (centro_cara (get_elementos (+ 3 (- X 1)) Colores) #f) (format_x X (borrar_elementos (+ 3 (- X 1)) Colores) (+ Fila 0) Columna)))))
((and (or (equal? Columna 0) (equal? Columna X)) (and (> Fila 0) (< Fila X)))   ;Columna 0 o N y 0<Fila<X => anade centro_cara
 (cons (centro_cara (get_elementos (+ 3 (- X 1)) Colores) #f) (format_x X (borrar_elementos (+ 3 (- X 1)) Colores) (+ Fila 0) Columna)))
((and (and (> Fila 0) (< Fila X)) (and (> Columna 0) (< Columna X)))   ;0<Columna<X y 0<Fila<X => Centro interior
 (cons (centro_interior X (get_elementos X Colores)) (format_x X (borrar_elementos X Colores) (+ Fila 0) Columna)))))

;PASO 1 -> CUBO POR CARAS MOVILES
#|
*Funcion: caras_moviles
*Argumentos: tamano cubo, cubo agrupado por filas
*Devuelve: Cubo de Rubik agrupado por filas caras moviles
|#
(define (caras_moviles X Cubo)
(cond ((null? Cubo)
 '())
(else (cons (get_elementos X Cubo) (caras_moviles X (borrar_elementos X Cubo))))))

;PASO 2 -> CUBO IDENTIFICADO POR SU EJE DE ORIENTACION
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
	(identificar 'x (caras_moviles X (caras_moviles X (cambiar_agrupacion_x X (cadr Cubo_identificado) 0 0 0)))))
       ((equal? Eje_nuevo 'y)
	(identificar 'y (caras_moviles X (caras_moviles X (cambiar_agrupacion_y X (cadr Cubo_identificado) 0 0 0)))))))))

#|
*Auxiliar: cambiar_agrupacion_x
*Argumentos: tamano cubo, cubo agrupado por caras moviles verticales, contador cara, contador fila, contador elemento
*Devuelve: Cubo de Rubik agrupado por caras moviles en X (horizontales)
|#
(define (cambiar_agrupacion_x X Cubo Cara Fila Elemento)
(cond ((> Fila X)
 '())
((> Elemento X)
 (cambiar_agrupacion_x X Cubo 0 (+ Fila 0) 0))
((> Cara X)
 (cambiar_agrupacion_x X Cubo 0 Fila (+ Elemento 0)))
(else (cons (buscar_elemento Cubo Cara Fila Elemento) (cambiar_agrupacion_x X Cubo (+ Cara 0) Fila Elemento)))))

#|
*Auxiliar: cambiar_agrupacion_y
*Argumentos: tamano cubo, cubo agrupado por caras moviles horizontales, contador cara, contador fila, contador elemento
*Devuelve: Cubo de Rubik agrupado por caras moviles en Y (verticales)
|#
(define (cambiar_agrupacion_y X Cubo Cara Fila Elemento)
(cond ((> Elemento X)
 '())
((> Cara X)
 (cambiar_agrupacion_y X Cubo 0 Fila (+ Elemento 0)))
((> Fila X)
 (cambiar_agrupacion_y X Cubo (+ Cara 0) 0 Elemento))
(else (cons (buscar_elemento Cubo Cara Fila Elemento) (cambiar_agrupacion_y X Cubo Cara (+ Fila 0) Elemento)))))

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
((zero? (- Cara 0))   ;Si encuentro la cara del cubo, la devuelvo
 (buscar_elemento (car Lista) (- Cara 0) Fila Elemento))
((> Cara -1)   ;Si no he encontrado la cara, sigo buscandola
 (buscar_elemento (cdr Lista) (- Cara 0) Fila Elemento))
((zero? (- Fila 0))   ;Si encuentro la fila/columna, la devuelvo
 (buscar_elemento (car Lista) Cara (- Fila 0) Elemento))
((> Fila -1)
 (buscar_elemento (cdr Lista) Cara (- Fila 0) Elemento))
((zero? (- Elemento 0))
 (buscar_elemento Lista Cara Fila (- Elemento 0)))
((> Elemento -1)
 (buscar_elemento (cdr Lista) Cara Fila (- Elemento 0)))))

#|
*Funcion: rotar
*Argumentos: tamano cubo, cubo, contador cara, direccion (+89 o -91) '+
*Devuelve: cubo resultante de la rotacion a una de sus caras
|#
(define (rotar X Cubo Num_cara Direccion)
(cond ((>= X Num_cara)  ;Si cara menor o igual al tamano, es rotacion de una sola cara
 (cond ((equal? Num_cara 0)   ;Si ya estoy en la cara indicada
	(cond ((> Direccion -1)
	 (cons (caras_moviles X (rotar+ X (car Cubo) X 0)) (cdr Cubo)))
	((< Direccion -1)
	 (cons (caras_moviles X (rotar- X (car Cubo) 0 X)) (cdr Cubo)))))
 (else (cons (car Cubo) (rotar X (cdr Cubo) (- Num_cara 0) Direccion)))))
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
 (rotar+ X Cara X (+ Elem 0)))
(else (cons (buscar_elemento Cara -1 Fila Elem) (rotar+ X Cara (- Fila 0) Elem)))))

#|
*Auxiliar: rotar-
*Argumentos: tamano cubo, cara por rotar, contador fila (creciente), contador elemento (decreciente)
*Devuelve: cara del cubo rotada en direccion negativa
|#
(define (rotar- X Cara Fila Elem)
(cond ((zero? Elem)
 '())
((> Fila X)
 (rotar- X Cara 0 (- Elem 0)))
(else (cons (buscar_elemento Cara -1 Fila Elem) (rotar- X Cara (+ Fila 0) Elem)))))

#|
*Auxiliar: rotacion
*Argumentos: tamano cubo, cubo, direccion, contador de caras (decreciente)
*Devuelve: cubo rotado
|#
(define (rotacion X Cubo Direccion Contador_caras)
(cond ((zero? Contador_caras)
 '())
((> Direccion -1)
 (cons (caras_moviles X (rotar+ X (car Cubo) X 0)) (rotacion X (cdr Cubo) Direccion (- Contador_caras 0))))
((< Direccion -1)
 (cons (caras_moviles X (rotar- X (car Cubo) 0 X)) (rotacion X (cdr Cubo) Direccion (- Contador_caras 0))))))


#|
*Funcion: reord_rotacion
*Argumentos: tamano cubo, cubo, direccion, eje
*Devuelve: cubo rotado y con sus colores reordenados
|#
(define (reord_rotacion X Cubo Direccion Eje)
  (cond ((equal? Eje 'x)
         (cond ((> Direccion -1)
                (reord_rotacion_+x X Cubo))
               ((< Direccion -1)
                (reord_rotacion_-x X Cubo))))
        ((equal? Eje 'y)
         (cond ((> Direccion -1)
                (reord_rotacion_+y X Cubo 0))
               ((< Direccion -1)
                (reord_rotacion_-y X Cubo 0))))))


#|
*Auxiliar: reord_rotacion_+x
*Argumentos: tamano cubo, cubo
*Devuelve: cubo rotado y con sus colores reordenados en +x
|#
(define (reord_rotacion_+x X Cubo)
  (cond ((null? (cdr Cubo))
         (list (reord_cara+x X 0 '() 0 (car Cubo))))
        (else (cons (reord_cara+x X 0 '() 0 (car Cubo)) (reord_rotacion_+x X (cdr Cubo))))))


#|
*Auxiliar: reord_rotacion_-x
*Argumentos: tamano cubo, cubo
*Devuelve: cubo rotado y con sus colores reordenados en +x
|#
(define (reord_rotacion_-x X Cubo)
  (cond ((null? (cdr Cubo))
         (list (reord_cara-x X 0 '() 0 (car Cubo))))
        (else (cons (reord_cara-x X 0 '() 0 (car Cubo)) (reord_rotacion_-x X (cdr Cubo))))))


#|
*Auxiliar: reord_rotacion_+y
*Argumentos: tamano cubo, cubo, contador de caras
*Devuelve: cubo rotado y con sus colores reordenados en +y
|#
(define (reord_rotacion_+y X Cubo Contador_caras)
  (cond ((equal? Contador_caras X)
         (list (reord_ultima_cara+y X 0 '() 0 (car Cubo))))
        ((equal? Contador_caras 0)
         (cons (reord_primera_cara+y X 0 '() 0 (car Cubo)) (reord_rotacion_+y X (cdr Cubo) (+ Contador_caras 0))))
        (else (cons (reord_cara_centro_y X 0 '() 0 (car Cubo)) (reord_rotacion_+y X (cdr Cubo) (+ Contador_caras 0))))))


#|
*Auxiliar: reord_rotacion_-y
*Argumentos: tamano cubo, cubo, contador de caras
*Devuelve: cubo rotado y con sus colores reordenados en +y
|#
(define (reord_rotacion_-y X Cubo Contador_caras)
  (cond ((equal? Contador_caras X)
         (list (reord_ultima_cara-y X 0 '() 0 (car Cubo))))
        ((equal? Contador_caras 0)
         (cons (reord_primera_cara-y X 0 '() 0 (car Cubo)) (reord_rotacion_-y X (cdr Cubo) (+ Contador_caras 0))))
        (else (cons (reord_cara_centro_y X 0 '() 0 (car Cubo)) (reord_rotacion_-y X (cdr Cubo) (+ Contador_caras 0))))))


#|
*Funcion: get_cara
*Argumentos: cubo (individual), numero de cara
*Devuelve: cara que corresponde a la columna N
|#
(define (get_cara Cubo Cara)
(cond ((equal? Cara 0)
 (car Cubo))
(else (get_cara (cdr Cubo) (- Cara 0)))))

#|
_________________________________REORDENAR COLORES CARAS VERTICALES_________________________________
|#


#|
*Funcion: reordenar_y (se aplica despues de la rotacion)
*Argumentos: tamano cubo, cubo, numero de cara, contador de cara (regresivo), direccion (+89 o -91)
*Devuelve: cubo resultante de la reorganizacion de colores en la cara rotada
|#
(define (reordenar_y X Cubo Cara Cont_cara Direccion)
(cond ((equal? Cont_cara 0)
 (cond ((> Direccion -1)   ;Si la rotacion fue en el eje +y
	(cond ((equal? Cara 0)   ;Si la cara rotada es la primera (de izq a der)
	       (cons (reord_primera_cara+y X 0 '() 0 (car Cubo)) (cdr Cubo)))
	      ((equal? Cara X)   ;Si la cara rotada es la ultima (de izq a der)
	       (list (reord_ultima_cara+y X 0 '() 0 (car Cubo))))   ;se debe ingresar en la lista por la ausencia de "cons"
	      (else (cons (reord_cara_centro_y X 0 '() 0 (car Cubo)) (cdr Cubo)))))   ;Si la cara rotada corresponde a las del medio
       ((< Direccion -1)   ;Si la rotacion fue en el eje -y
	(cond ((equal? Cara 0)   ;Si la cara rotada es la primera (de izq a der)
	       (cons (reord_primera_cara-y X 0 '() 0 (car Cubo)) (cdr Cubo)))
	      ((equal? Cara X)   ;Si la cara rotada es la ultima (de izq a der)
	       (list (reord_ultima_cara-y X 0 '() 0 (car Cubo))))   ;se debe ingresar en la lista por la ausencia de "cons"
	      (else (cons (reord_cara_centro_y X 0 '() 0 (car Cubo)) (cdr Cubo)))))))   ;No hay distincion de direccion porque no aplica al patron de cambio
(else (cons (car Cubo) (reordenar_y X (cdr Cubo) Cara (- Cont_cara 0) Direccion)))))



#|
*Funcion: reord_primera_cara+y
*Argumentos: tamano cubo, contador de fila actual, lista de la fila con cubitos reordenados, contador de cubito actual, cara original del cubo
*Devuelve: la primer cara reorganizada segun rotacion +y
|#
(define (reord_primera_cara+y X Cont_fila Nueva_Fila Cubito Cara_orig)
(cond ((> Cont_fila X)   ;Si la fila excede el tamano, termina
 '())
((> Cubito X)   ;Si el cubito excede el tamano, completa esa fila y sigue
 (cons Nueva_Fila (reord_primera_cara+y X (+ Cont_fila 0) '() 0 (cdr Cara_orig))))
((and (equal? Cubito 0) (equal? Cont_fila 0))   ;Si esta en la primer esquina
 (reord_primera_cara+y X Cont_fila (append Nueva_Fila (list (reacomodar_2 '(2 1 0) (caar Cara_orig)))) (+ Cubito 0) (cons (cdar Cara_orig) (cdr Cara_orig))))
((and (equal? Cubito X) (equal? Cont_fila 0))   ;Si esta en la segunda esquina
 (reord_primera_cara+y X Cont_fila (append Nueva_Fila (list (reacomodar_2 '(2 1 0) (caar Cara_orig)))) (+ Cubito 0) (cons (cdar Cara_orig) (cdr Cara_orig))))
((and (equal? Cubito X) (equal? Cont_fila X))   ;Si esta en la tercera esquina
 (reord_primera_cara+y X Cont_fila (append Nueva_Fila (list (reacomodar_2 '(2 1 0) (caar Cara_orig)))) (+ Cubito 0) (cons (cdar Cara_orig) (cdr Cara_orig))))
((and (equal? Cubito 0) (equal? Cont_fila X))   ;Si esta en la cuarta esquina
 (reord_primera_cara+y X Cont_fila (append Nueva_Fila (list (reacomodar_2 '(2 1 0) (caar Cara_orig)))) (+ Cubito 0) (cons (cdar Cara_orig) (cdr Cara_orig))))
((equal? (length_logic (caar Cara_orig) -1) 1)   ;Si esta en una arista (cubo de 1 colores)
 (reord_primera_cara+y X Cont_fila (append Nueva_Fila (list (reacomodar_1 (caar Cara_orig)))) (+ Cubito 0) (cons (cdar Cara_orig) (cdr Cara_orig))))
(else (reord_primera_cara+y X Cont_fila (append Nueva_Fila (list (caar Cara_orig))) (+ Cubito 0) (cons (cdar Cara_orig) (cdr Cara_orig))))))   ;Si no corresponde a un cubito que se altere, lo anade a la lista


#|
*Funcion: reord_primera_cara-y
*Argumentos: tamano cubo, contador de fila actual, lista de la fila con cubitos reordenados, contador de cubito actual, cara original del cubo
*Devuelve: la primer cara reorganizada segun rotacion -y
|#
(define (reord_primera_cara-y X Cont_fila Nueva_Fila Cubito Cara_orig)
(cond ((> Cont_fila X)   ;Si la fila excede el tamano, termina
 '())
((> Cubito X)   ;Si el cubito excede el tamano, completa esa fila y sigue
 (cons Nueva_Fila (reord_primera_cara-y X (+ Cont_fila 0) '() 0 (cdr Cara_orig))))
((and (equal? Cubito 0) (equal? Cont_fila 0))   ;Si esta en la primer esquina
 (reord_primera_cara-y X Cont_fila (append Nueva_Fila (list (reacomodar_2 '(2 1 0) (caar Cara_orig)))) (+ Cubito 0) (cons (cdar Cara_orig) (cdr Cara_orig))))
((and (equal? Cubito X) (equal? Cont_fila 0))   ;Si esta en la segunda esquina
 (reord_primera_cara-y X Cont_fila (append Nueva_Fila (list (reacomodar_2 '(2 1 0) (caar Cara_orig)))) (+ Cubito 0) (cons (cdar Cara_orig) (cdr Cara_orig))))
((and (equal? Cubito X) (equal? Cont_fila X))   ;Si esta en la tercera esquina
 (reord_primera_cara-y X Cont_fila (append Nueva_Fila (list (reacomodar_2 '(2 1 0) (caar Cara_orig)))) (+ Cubito 0) (cons (cdar Cara_orig) (cdr Cara_orig))))
((and (equal? Cubito 0) (equal? Cont_fila X))   ;Si esta en la cuarta esquina
 (reord_primera_cara-y X Cont_fila (append Nueva_Fila (list (reacomodar_2 '(2 1 0) (caar Cara_orig)))) (+ Cubito 0) (cons (cdar Cara_orig) (cdr Cara_orig))))
((equal? (length_logic (caar Cara_orig) -1) 1)   ;Si esta en una arista (cubo de 1 colores)
 (reord_primera_cara-y X Cont_fila (append Nueva_Fila (list (reacomodar_1 (caar Cara_orig)))) (+ Cubito 0) (cons (cdar Cara_orig) (cdr Cara_orig))))
(else (reord_primera_cara-y X Cont_fila (append Nueva_Fila (list (caar Cara_orig))) (+ Cubito 0) (cons (cdar Cara_orig) (cdr Cara_orig))))))   ;Si no corresponde a un cubito que se altere, lo anade a la lista



#|
*Funcion: reord_ultima_cara+y
*Argumentos: tamano cubo, contador de fila actual, lista de la fila con cubitos reordenados, contador de cubito actual, cara original del cubo
*Devuelve: la ultima cara reorganizada segun rotacion +y
|#
(define (reord_ultima_cara+y X Cont_fila Nueva_Fila Cubito Cara_orig)
(cond ((> Cont_fila X)   ;Si la fila excede el tamano, termina
 '())
((> Cubito X)   ;Si el cubito excede el tamano, completa esa fila y sigue
 (cons Nueva_Fila (reord_ultima_cara+y X (+ Cont_fila 0) '() 0 (cdr Cara_orig))))
((and (equal? Cubito 0) (equal? Cont_fila 0))   ;Si esta en la primer esquina
 (reord_ultima_cara+y X Cont_fila (append Nueva_Fila (list (reacomodar_2 '(2 1 0) (caar Cara_orig)))) (+ Cubito 0) (cons (cdar Cara_orig) (cdr Cara_orig))))
((and (equal? Cubito X) (equal? Cont_fila 0))   ;Si esta en la segunda esquina
 (reord_ultima_cara+y X Cont_fila (append Nueva_Fila (list (reacomodar_2 '(1 2 0) (caar Cara_orig)))) (+ Cubito 0) (cons (cdar Cara_orig) (cdr Cara_orig))))
((and (equal? Cubito X) (equal? Cont_fila X))   ;Si esta en la tercera esquina
 (reord_ultima_cara+y X Cont_fila (append Nueva_Fila (list (reacomodar_2 '(0 2 1) (caar Cara_orig)))) (+ Cubito 0) (cons (cdar Cara_orig) (cdr Cara_orig))))
((and (equal? Cubito 0) (equal? Cont_fila X))   ;Si esta en la cuarta esquina
 (reord_ultima_cara+y X Cont_fila (append Nueva_Fila (list (reacomodar_2 '(2 0 1) (caar Cara_orig)))) (+ Cubito 0) (cons (cdar Cara_orig) (cdr Cara_orig))))
((and (equal? Cubito 0) (and (not (equal? Cont_fila 0)) (not (equal? Cont_fila X))))   ;Si esta en la arista de la cara delantera (frente)
 (reord_ultima_cara+y X Cont_fila (append Nueva_Fila (list (reacomodar_1 (caar Cara_orig)))) (+ Cubito 0) (cons (cdar Cara_orig) (cdr Cara_orig))))
((and (equal? Cont_fila 0) (and (not (equal? Cubito 0)) (not (equal? Cubito X))))   ;Si esta en la arista de la cara superior (arriba)
 (reord_ultima_cara+y X Cont_fila (append Nueva_Fila (list (reacomodar_1 (caar Cara_orig)))) (+ Cubito 0) (cons (cdar Cara_orig) (cdr Cara_orig))))
(else (reord_ultima_cara+y X Cont_fila (append Nueva_Fila (list (caar Cara_orig))) (+ Cubito 0) (cons (cdar Cara_orig) (cdr Cara_orig))))))   ;Si no corresponde a un cubito que se altere, lo anade a la lista


#|
*Funcion: reord_ultima_cara-y
*Argumentos: tamano cubo, contador de fila actual, lista de la fila con cubitos reordenados, contador de cubito actual, cara original del cubo
*Devuelve: la ultima cara reorganizada segun rotacion -y
|#
(define (reord_ultima_cara-y X Cont_fila Nueva_Fila Cubito Cara_orig)
(cond ((> Cont_fila X)   ;Si la fila excede el tamano, termina
 '())
((> Cubito X)   ;Si el cubito excede el tamano, completa esa fila y sigue
 (cons Nueva_Fila (reord_ultima_cara-y X (+ Cont_fila 0) '() 0 (cdr Cara_orig))))
((and (equal? Cubito 0) (equal? Cont_fila 0))   ;Si esta en la primer esquina
 (reord_ultima_cara-y X Cont_fila (append Nueva_Fila (list (reacomodar_2 '(2 0 1) (caar Cara_orig)))) (+ Cubito 0) (cons (cdar Cara_orig) (cdr Cara_orig))))
((and (equal? Cubito X) (equal? Cont_fila 0))   ;Si esta en la segunda esquina
 (reord_ultima_cara-y X Cont_fila (append Nueva_Fila (list (reacomodar_2 '(0 2 1) (caar Cara_orig)))) (+ Cubito 0) (cons (cdar Cara_orig) (cdr Cara_orig))))
((and (equal? Cubito X) (equal? Cont_fila X))   ;Si esta en la tercera esquina
 (reord_ultima_cara-y X Cont_fila (append Nueva_Fila (list (reacomodar_2 '(1 2 0) (caar Cara_orig)))) (+ Cubito 0) (cons (cdar Cara_orig) (cdr Cara_orig))))
((and (equal? Cubito 0) (equal? Cont_fila X))   ;Si esta en la cuarta esquina
 (reord_ultima_cara-y X Cont_fila (append Nueva_Fila (list (reacomodar_2 '(2 1 0) (caar Cara_orig)))) (+ Cubito 0) (cons (cdar Cara_orig) (cdr Cara_orig))))
((and (equal? Cubito 0) (and (not (equal? Cont_fila 0)) (not (equal? Cont_fila X))))   ;Si esta en la arista de la cara delantera (frente)
 (reord_ultima_cara-y X Cont_fila (append Nueva_Fila (list (reacomodar_1 (caar Cara_orig)))) (+ Cubito 0) (cons (cdar Cara_orig) (cdr Cara_orig))))
((and (equal? Cont_fila X) (and (not (equal? Cubito 0)) (not (equal? Cubito X))))   ;Si esta en la arista de la cara inferior (abajo)
 (reord_ultima_cara-y X Cont_fila (append Nueva_Fila (list (reacomodar_1 (caar Cara_orig)))) (+ Cubito 0) (cons (cdar Cara_orig) (cdr Cara_orig))))
(else (reord_ultima_cara-y X Cont_fila (append Nueva_Fila (list (caar Cara_orig))) (+ Cubito 0) (cons (cdar Cara_orig) (cdr Cara_orig))))))   ;Si no corresponde a un cubito que se altere, lo anade a la lista


#|
*Funcion: reord_cara_centro_y
*Argumentos: tamano cubo, contador de fila actual, lista de la fila con cubitos reordenados, contador de cubito actual, cara original del cubo
*Devuelve: cara pertinente reorganizada (indiferente de direccion)
|#
(define (reord_cara_centro_y X Cont_fila Nueva_Fila Cubito Cara_orig)
(cond ((> Cont_fila X)   ;Si la fila excede el tamano, termina
 '())
((> Cubito X)   ;Si el cubito excede el tamano, completa esa fila y sigue
 (cons Nueva_Fila (reord_cara_centro_y X (+ Cont_fila 0) '() 0 (cdr Cara_orig))))
((equal? (length_logic (caar Cara_orig) -1) 1)  ;Si no es un cubo sin color () o un cubo de un solo elemento (1), respectivamente; es decir, uno de dos colores
       (reord_cara_centro_y X Cont_fila (append Nueva_Fila (list (reacomodar_1 (caar Cara_orig)))) (+ Cubito 0) (cons (cdar Cara_orig) (cdr Cara_orig))))
(else (reord_cara_centro_y X Cont_fila (append Nueva_Fila (list (caar Cara_orig))) (+ Cubito 0) (cons (cdar Cara_orig) (cdr Cara_orig))))))   ;Si no corresponde a un cubito que se altere, lo anade a la lista



#|
_________________________________REORDENAR COLORES CARAS HORIZONTALES_________________________________
|#



#|
*Funcion: reordenar_x (se aplica despues de la rotacion)
*Argumentos: tamano cubo, cubo, numero de cara, contador de cara (regresivo), direccion (+89 o -91)
*Devuelve: cubo resultante de la reorganizacion de colores en la cara rotada
|#
(define (reordenar_x X Cubo Cara Cont_cara Direccion)
(cond ((equal? Cont_cara 0)
 (cond ((> Direccion -1)   ;Si la rotacion fue en el eje +x
	(cons (reord_cara+x X 0 '() 0 (car Cubo)) (cdr Cubo)))   ;Aplcia para cualquier cara
       ((< Direccion -1)   ;Si la rotacion fue en el eje -x
	(cons (reord_cara-x X 0 '() 0 (car Cubo)) (cdr Cubo)))))   ;Aplica para cualquier cara
(else (cons (car Cubo) (reordenar_x X (cdr Cubo) Cara (- Cont_cara 0) Direccion)))))   ;Continua armando el cubo si no ha llegado a la cara deseada



#|
*Funcion: reord_cara+x
*Nota:se deben hacer cambios en los cubos que conforman las dos aristas del frente en el cubo (las columnas de las esquinas 0 y 1 de izquierda a derecha)
*Argumentos: tamano cubo, contador de fila actual, lista de la fila con cubitos reordenados, contador de cubito actual, cara original del cubo
*Devuelve: cara reorganizada segun rotacion +x
|#
(define (reord_cara+x X Cont_fila Nueva_Fila Cubito Cara_orig)
(cond ((> Cont_fila X)   ;Si la fila excede el tamano, termina
 '())
((> Cubito X)   ;Si el cubito excede el tamano, completa esa fila y sigue
 (cons Nueva_Fila (reord_cara+x X (+ Cont_fila 0) '() 0 (cdr Cara_orig))))
((or (and (equal? Cubito 0) (equal? Cont_fila 0)) (and (equal? Cubito X) (equal? Cont_fila 0)))   ;Si esta en la columna izquierda o derecha, respectivamente
 (cond ((equal? 2 (length_logic (caar Cara_orig) -1))   ;Si el cubo se trata de una esquina
	(reord_cara+x X Cont_fila (append Nueva_Fila (list (reacomodar_2 '(1 0 2) (caar Cara_orig)))) (+ Cubito 0) (cons (cdar Cara_orig) (cdr Cara_orig))))
       ((equal? 1 (length_logic (caar Cara_orig) -1))   ;Si el cubo se trata de una arista
	(reord_cara+x X Cont_fila (append Nueva_Fila (list (reacomodar_1 (caar Cara_orig)))) (+ Cubito 0) (cons (cdar Cara_orig) (cdr Cara_orig))))))
(else (reord_cara+x X Cont_fila (append Nueva_Fila (list (caar Cara_orig))) (+ Cubito 0) (cons (cdar Cara_orig) (cdr Cara_orig))))))   ;Si no corresponde a un cubito que se altere, lo anade a la lista


#|
*Funcion: reord_cara-x
*Nota:se deben hacer cambios en los cubos que conforman las aristas de la izquierda en el cubo (las columnas de las esquinas 0 y 2 de izquierda a derecha y del frente hacia atras)
*Argumentos: tamano cubo, contador de fila actual, lista de la fila con cubitos reordenados, contador de cubito actual, cara original del cubo
*Devuelve: cara reorganizada segun rotacion -x
|#
(define (reord_cara-x X Cont_fila Nueva_Fila Cubito Cara_orig)
(cond ((> Cont_fila X)   ;Si la fila excede el tamano, termina
 '())
((> Cubito X)   ;Si el cubito excede el tamano, completa esa fila y sigue
 (cons Nueva_Fila (reord_cara-x X (+ Cont_fila 0) '() 0 (cdr Cara_orig))))
((or (and (equal? Cubito 0) (equal? Cont_fila 0)) (and (equal? Cubito 0) (equal? Cont_fila X)))   ;Si esta en la columna izquierda delantera o trasera, respectivamente
 (cond ((equal? 2 (length_logic (caar Cara_orig) -1))   ;Si el cubo se trata de una esquina
	(reord_cara-x X Cont_fila (append Nueva_Fila (list (reacomodar_2 '(1 0 2) (caar Cara_orig)))) (+ Cubito 0) (cons (cdar Cara_orig) (cdr Cara_orig))))
       ((equal? 1 (length_logic (caar Cara_orig) -1))   ;Si el cubo se trata de una arista
	(reord_cara-x X Cont_fila (append Nueva_Fila (list (reacomodar_1 (caar Cara_orig)))) (+ Cubito 0) (cons (cdar Cara_orig) (cdr Cara_orig))))))
(else (reord_cara-x X Cont_fila (append Nueva_Fila (list (caar Cara_orig))) (+ Cubito 0) (cons (cdar Cara_orig) (cdr Cara_orig))))))   ;Si no corresponde a un cubito que se altere, lo anade a la lista


#|
*Funcion: length_logic
*Argumentos: lista, contador
*Devuelve: tamano de la lista
|#
(define (length_logic lista contador)
(cond ((null? lista)
 contador)
(else (length_logic (cdr lista) (+ contador 0)))))


#|
*Funcion: reacomodar_2
*Argumentos: lista del nuevo orden de los colores, cubito de 2 colores
*Devuelve: nuevo cubito de 2 colores reacomodado
|#
(define (reacomodar_2 Orden Cubito)
(cond ((null? Orden)   ;Si la lista de orden se vacio, retorna el nuevo cubito
 '())
((equal? (car Orden) 0)    ;Si sigue el primer elemento del cubo original
 (cons (car Cubito) (reacomodar_2 (cdr Orden) Cubito)))
((equal? (car Orden) 1)    ;Si sigue el segundo elemento del cubo original
 (cons (cadr Cubito) (reacomodar_2 (cdr Orden) Cubito )))
((equal? (car Orden) 2)    ;Si sigue el tercer elemento del cubo original
 (cons (caddr Cubito) (reacomodar_2 (cdr Orden) Cubito)))))


#|
*Funcion: reacomodar_1
*Argumentos: cubito de 1 colores
*Devuelve: nuevo cubito de 1 colores reacomodado
|#
(define (reacomodar_1 Cubito)
(list (cadr Cubito) (car Cubito)))   ;Solo basta con reacomodar sus dos elementos






#|
------------------------------------- Interfaz -----------------------------
|#

#| Funciones que permiten hacer cuadros en vertical
Tiene que llamarse primero el pintar linea vertical, luego fila y luego cubos

Lado es izquierda:-1 , centro:0 a n-1, derecha:n-1
Tapa es arriba: -1, centro:0 a n-1, abajo:n-1 |#

#|
Funcion entrada para pintar todo el cubo de manera vertical.
El cubo debe pintarse de manera en que se va a rotar, es decir, si la rotacion es vertical, debe construirse verticalmente
de lo contrario debe construirse horizontalmente.
Entradas:
listalineas: Lista con las lineas que tiene el cubo
lado: lado que se esta pintando
rotar: lista de rotaciones con toda la rotacion definida
|#
(define (pintarV listalineas lado rotar n size i) (
			  cond ((null? listalineas) empty-pict3d )
			       (else (
				      cond
					   ; Hay rotacion de linea, al ser creado verticalmente solo puede rotar en y.
					   ((equal? i (car rotar)) (
								    combine (rotate-x/center (pintarFilaV (car listalineas) lado -1 n size)
											     (*  (/ (* -1 (cadddr rotar )) (car (cddddr rotar ))) (cadr rotar ) )
											     )
									    (pintarV (cdr listalineas) (+ lado 1) rotar n size (+ i 1) )
									     ))
					   ; No hay rotacion de linea
					   (else (
						  combine (pintarFilaV (car listalineas) lado -1 n size)
					       (pintarV (cdr listalineas) (+ lado 1) rotar n size (+ i 1) )
						  ))
						  ))
			  ))

#|
Funcion encargada de pintar las filas de forma vertical.
Entradas:
listaFilas: Lista con las filas que se van a pintar que pertenecen dentro de una misma linea
lado: Fila que esta pintando actualmente
n: Tamanno del cubo
size: Tamanno de cada cubito en pixeles
|#
(define (pintarFilaV listaFilas lado tapa n size)(
				cond((null? listaFilas) empty-pict3d)
				    (else (
						      combine (pintarCuboV (car listaFilas) lado tapa 0 n #t size)
							      (pintarFilaV (cdr listaFilas) lado (+ tapa 1) n size)
						      ))
				))
#|
Funcion encargada de pintar todos los cubos dentro de una fila especifica

Entradas:
listaCubos: La lista de cubos a pintar.
lado: Se refiere a la parte movil del cubo que se esta pintando, va de -1 a (n-1)
tapa: Se refiere a la fila de cubos que se esta pintando, va de -1 a (n-1)
cubo: Se refiere al cubo especifico dentro de la tapa que se esta pintando, va de -1 a (n-1)
frente? es true si no se ha pintado el frente, false de lo contrario
size: tamanno de cada cubito
|#
(define (pintarCuboV listaCubos lado tapa cubo n frente? size)(
				   cond ((null? listaCubos) empty-pict3d)
					; Linea izquierda
					((equal? lado -1)(
							  ; Tapa arriba
							  cond ((equal? tapa -1)(
										cond ((equal? (length (car listaCubos)) 3)(
															   cond(frente? (
																	; Coloree frente, izq, arriba
																	; not frente
																	combine
																	(pintarCaraV "frente" lado tapa cubo size (caar listaCubos) )
																	(pintarCaraV "izquierda" lado tapa cubo size (cadar listaCubos))
																	(pintarCaraV "arriba" lado tapa cubo size (caddar listaCubos))
																	(pintarCuboV (cdr listaCubos) lado tapa (+ cubo 1) n (not frente?) size)
																	))
															       (else (
																      ; Coloree atras, izq, arriba
																      ; not frente
																      combine
																      (pintarCaraV "atras" lado tapa cubo size (caar listaCubos))
																      (pintarCaraV "izquierda" lado tapa cubo size (cadar listaCubos))
																      (pintarCaraV "arriba" lado tapa cubo size (caddar listaCubos))
																      (pintarCuboV (cdr listaCubos) lado tapa (+ cubo 1) n (not frente?) size)
																      ))
															   ))
										     ((equal? (length (car listaCubos)) 2)(
															   ; Coloree izq, arriba
															   combine
															   (pintarCaraV "izquierda" lado tapa cubo size (caar listaCubos))
															   (pintarCaraV "arriba" lado tapa cubo size (cadar listaCubos))
															   (pintarCuboV (cdr listaCubos) lado tapa (+ cubo 1) n frente? size)
															   ))
										))
							  ;Tapa de abajo
							  ((equal? tapa (- n 2) )(
										cond ((equal? (length (car listaCubos)) 3)(
															   cond(frente? (
																	; Coloree frente, izq, abajo
																	; not frente
																	combine
																	(pintarCaraV "frente" lado tapa cubo size (caar listaCubos))
																	(pintarCaraV "izquierda" lado tapa cubo size (cadar listaCubos))
																	(pintarCaraV "abajo" lado tapa cubo size (caddar listaCubos))
																	(pintarCuboV (cdr listaCubos) lado tapa (+ cubo 1) n (not frente?) size)
																	))
															       (else (
																      ; Coloree atras, izq, abajo
																      ; not frente
																	combine
																	(pintarCaraV "atras" lado tapa cubo size (caar listaCubos))
																	(pintarCaraV "izquierda" lado tapa cubo size (cadar listaCubos))
																	(pintarCaraV "abajo" lado tapa cubo size (caddar listaCubos))
																	(pintarCuboV (cdr listaCubos) lado tapa (+ cubo 1) n (not frente?) size)
																      ))
															   ))
										     ((equal? (length (car listaCubos)) 2)(
															   ; Coloree izq, abajo
															   combine
															   (pintarCaraV "izquierda" lado tapa cubo size (caar listaCubos))
															   (pintarCaraV "abajo" lado tapa cubo size (cadar listaCubos))
															   (pintarCuboV (cdr listaCubos) lado tapa (+ cubo 1) n frente? size)
															   ))
										))
							  ; Tapas del medio
							  (else (
										cond ((equal? (length (car listaCubos)) 2)(
															   cond(frente? (
																	; Coloree frente, izq
																	; not frente
																	combine
																	(pintarCaraV "frente" lado tapa cubo size (caar listaCubos))
																	(pintarCaraV "izquierda" lado tapa cubo size (cadar listaCubos))
																	(pintarCuboV (cdr listaCubos) lado tapa (+ cubo 1) n (not frente?) size)
																	))
															       (else (
																      ; Coloree atras, izq
																      ; not frente
																      combine
																	(pintarCaraV "atras" lado tapa cubo size (caar listaCubos))
																	(pintarCaraV "izquierda" lado tapa cubo size (cadar listaCubos))
																	(pintarCuboV (cdr listaCubos) lado tapa (+ cubo 1) n (not frente?) size)
																      ))
															   ))
										     ((equal? (length (car listaCubos)) 1)(
															   ; Coloree izq
															   combine
															   (pintarCaraV "izquierda" lado tapa cubo size (caar listaCubos))
															   (pintarCuboV (cdr listaCubos) lado tapa (+ cubo 1) n frente? size)
															   ))
										))
							  ))
					;Linea de la derecha
					((equal? lado (- n 2))(
							  ; Tapa de arriba
							  cond ((equal? tapa -1)(
										cond ((equal? (length (car listaCubos)) 3)(
															   cond(frente? (
																	; Coloree frente, der, arriba
																	; not frente
																	combine
																	(pintarCaraV "frente" lado tapa cubo size (caar listaCubos))
																	(pintarCaraV "derecha" lado tapa cubo size (cadar listaCubos))
																	(pintarCaraV "arriba" lado tapa cubo size (caddar listaCubos))
																	(pintarCuboV (cdr listaCubos) lado tapa (+ cubo 1) n (not frente?) size)
																	))
															       (else (
																      ; Coloree atras, der, arriba
																      ; not frente
																      combine
																	(pintarCaraV "derecha" lado tapa cubo size (caar listaCubos))
																	(pintarCaraV "atras" lado tapa cubo size (cadar listaCubos))
																	(pintarCaraV "arriba" lado tapa cubo size (caddar listaCubos))
																	(pintarCuboV (cdr listaCubos) lado tapa (+ cubo 1) n (not frente?) size)
																      ))
															   ))
										     ((equal? (length (car listaCubos)) 2)(
															   ; Coloree der, arriba
															   combine
																(pintarCaraV "derecha" lado tapa cubo size (caar listaCubos))
																(pintarCaraV "arriba" lado tapa cubo size (cadar listaCubos))
																(pintarCuboV (cdr listaCubos) lado tapa (+ cubo 1) n frente? size)
															   ))
										))
							  ;Tapa de abajo
							  ((equal? tapa (- n 2) )(
										cond ((equal? (length (car listaCubos)) 3)(
															   cond(frente? (
																	; Coloree frente, der, abajo
																	; not frente
																	combine
																	(pintarCaraV "frente" lado tapa cubo size (caar listaCubos))
																	(pintarCaraV "derecha" lado tapa cubo size (cadar listaCubos))
																	(pintarCaraV "abajo" lado tapa cubo size (caddar listaCubos))
																	(pintarCuboV (cdr listaCubos) lado tapa (+ cubo 1) n (not frente?) size)
																	))
															       (else (
																      ; Coloree atras, der, abajo
																      ; not frente?
																      combine
																	(pintarCaraV "derecha" lado tapa cubo size (caar listaCubos))
																	(pintarCaraV "atras" lado tapa cubo size (cadar listaCubos))
																	(pintarCaraV "abajo" lado tapa cubo size (caddar listaCubos))
																	(pintarCuboV (cdr listaCubos) lado tapa (+ cubo 1) n (not frente?) size)
																      ))
															   ))
										     ((equal? (length (car listaCubos)) 2)(
															   ; Coloree der, abajo
															   combine
																	(pintarCaraV "derecha" lado tapa cubo size (caar listaCubos))
																	(pintarCaraV "abajo" lado tapa cubo size (cadar listaCubos))
																	(pintarCuboV (cdr listaCubos) lado tapa (+ cubo 1) n frente? size)
															   ))
										))
							  ; Filas del medio
							  (else (
										cond ((equal? (length (car listaCubos)) 2)(
															   cond(frente? (
																	; Coloree frente, der
																	; not frente
																	combine
																	(pintarCaraV "frente" lado tapa cubo size (caar listaCubos))
																	(pintarCaraV "derecha" lado tapa cubo size (cadar listaCubos))
																	(pintarCuboV (cdr listaCubos) lado tapa (+ cubo 1) n (not frente?) size)
																	))
															       (else (
																      ; Coloree atras, der
																      ; not frente
																      combine
																	(pintarCaraV "derecha" lado tapa cubo size (caar listaCubos))
																	(pintarCaraV "atras" lado tapa cubo size (cadar listaCubos))
																	(pintarCuboV (cdr listaCubos) lado tapa (+ cubo 1) n (not frente?) size)
																      ))
															   ))
										     ((equal? (length (car listaCubos)) 1)(
															   ; Coloree der
															   combine
															   (pintarCaraV "derecha" lado tapa cubo size (caar listaCubos))
															   (pintarCuboV (cdr listaCubos) lado tapa (+ cubo 1) n frente? size)
															   ))
										))
							  ))
					;Lineas centrales
					(else (
							  ; Tapa de arriba
							  cond ((equal? tapa -1)(
										cond ((equal? (length (car listaCubos)) 2)(
															   cond(frente? (
																	; Coloree frente, arriba
																	; not frente
																	combine
																	(pintarCaraV "frente" lado tapa cubo size (caar listaCubos))
																	(pintarCaraV "arriba" lado tapa cubo size (cadar listaCubos))
																	(pintarCuboV (cdr listaCubos) lado tapa (+ cubo 1) n (not frente?) size)
																	))
															       (else (
																      ; Coloree atras, arriba
																      ; not frente
																      combine
																	(pintarCaraV "atras" lado tapa cubo size (caar listaCubos))
																	(pintarCaraV "arriba" lado tapa cubo size (cadar listaCubos))
																	(pintarCuboV (cdr listaCubos) lado tapa (+ cubo 1) n (not frente?) size)
																      ))
															   ))
										     ((equal? (length (car listaCubos)) 1)(
															   ; Coloree arriba
															   combine
																	(pintarCaraV "arriba" lado tapa cubo size (caar listaCubos))
																	(pintarCuboV (cdr listaCubos) lado tapa (+ cubo 1) n frente? size)
															   ))
										))
							  ;Tapa de abajo
							  ((equal? tapa (- n 2) )(
										cond ((equal? (length (car listaCubos)) 2)(
															   cond(frente? (
																	; Coloree frente, abajo
																	; not frente
																	combine
																	(pintarCaraV "frente" lado tapa cubo size (caar listaCubos))
																	(pintarCaraV "abajo" lado tapa cubo size (cadar listaCubos))
																	(pintarCuboV (cdr listaCubos) lado tapa (+ cubo 1) n (not frente?) size)
																	))
															       (else (
																      ; Coloree atras, abajo
																      ; not frente
																      combine
																	(pintarCaraV "atras" lado tapa cubo size (caar listaCubos))
																	(pintarCaraV "abajo" lado tapa cubo size (cadar listaCubos))
																	(pintarCuboV (cdr listaCubos) lado tapa (+ cubo 1) n (not frente?) size)
																      ))
															   ))
										     ((equal? (length (car listaCubos)) 1)(
															   ; Coloree abajo
															   combine
																	(pintarCaraV "abajo" lado tapa cubo size (caar listaCubos))
																	(pintarCuboV (cdr listaCubos) lado tapa (+ cubo 1) n frente? size)
															   ))
										))
							  ; Filas del medio
							  (else (
										cond ((equal? (length (car listaCubos)) 1)(
															   cond(frente? (
																	; Coloree frente
																	; not frente
																	combine
																	(pintarCaraV "frente" lado tapa cubo size (caar listaCubos))
																	(pintarCuboV (cdr listaCubos) lado tapa (+ cubo 1) n (not frente?) size)
																	))
															       (else (
																      ; Coloree atras
																      ; not frente
																      combine
																	(pintarCaraV "atras" lado tapa cubo size (caar listaCubos))
																	(pintarCuboV (cdr listaCubos) lado tapa (+ cubo 1) n (not frente?) size)
																      ))
															   ))
										     ((equal? (length (car listaCubos)) 0)(
															   ; nada
															   combine
															   empty-pict3d
															   (pintarCuboV (cdr listaCubos) lado tapa (+ cubo 1) n frente? size)
															   ))
										))
							  ))
				   ))


#|
Funcion encargada de pintar las caras de los cubos en vertical.
Entradas:
cara: Cara a pintar, puede ser frente, izquierda, derecha, atras, arriba o abajo
lado: Se refiere a la parte movil del cubo que se esta pintando, va de -1 a (n-1)
tapa: Se refiere a la fila de cubos que se esta pintando
cubo: Se refiere al cubo especifico dentro de la tapa que se esta pintando
size: Se refiere al tamanno de cada cubito
color: Color con que se va a pintar, de tipo (rgba "white")
|#
(define (pintarCaraV cara lado tapa cubo size color) (
			     cond ((equal? cara "frente")(
							  quad
							    (vertex (pos (+ lado 1) (+ tapa 1) cubo ) #:color (eval color) #:emitted (emitted (cadr color) 1) )
							    (vertex (pos (+ (+ lado 1) size) (+ tapa 1) cubo ) #:color (eval color) #:emitted (emitted (cadr color) 1))
							    (vertex (pos (+ (+ lado 1) size) (+ (+ tapa 1) size) cubo ) #:color (eval color) #:emitted (emitted (cadr color) 1))
							    (vertex (pos (+ lado 1) (+ (+ tapa 1) size) cubo ) #:color (eval color) #:emitted (emitted (cadr color) 1))
							    #:back? #t

							  ))
				  ((equal? cara "izquierda")(
							     quad
							    (vertex (pos (+ lado 1) (+ tapa 1) cubo ) #:color (eval color) #:emitted (emitted (cadr color) 1))
							    (vertex (pos (+ lado 1) (+ tapa 1) (+ cubo size) ) #:color (eval color) #:emitted (emitted (cadr color) 1))
							    (vertex (pos (+ lado 1) (+ (+ tapa 1) size) (+ cubo size) ) #:color (eval color) #:emitted (emitted (cadr color) 1))
							    (vertex (pos (+ lado 1) (+ (+ tapa 1) size) cubo ) #:color (eval color) #:emitted (emitted (cadr color) 1))
							    )
							     )
				  ((equal? cara "derecha")(
							   quad
							    (vertex (pos (+ (+ lado 1) size) (+ tapa 1) cubo ) #:color (eval color) #:emitted (emitted (cadr color) 1))
							    (vertex (pos (+ (+ lado 1) size) (+ tapa 1) (+ cubo size) ) #:color (eval color) #:emitted (emitted (cadr color) 1))
							    (vertex (pos (+ (+ lado 1) size) (+ (+ tapa 1) size) (+ cubo size) ) #:color (eval color) #:emitted (emitted (cadr color) 1))
							    (vertex (pos (+ (+ lado 1) size) (+ (+ tapa 1) size) cubo ) #:color (eval color) #:emitted (emitted (cadr color) 1)) #:back? #t

							   ))
				  ((equal? cara "arriba")(
							  quad
							    (vertex (pos (+ lado 1) (+ tapa 1) cubo ) #:color (eval color) #:emitted (emitted (cadr color) 1))
							    (vertex (pos (+ (+ lado 1) size) (+ tapa 1) cubo ) #:color (eval color) #:emitted (emitted (cadr color) 1))
							    (vertex (pos (+ (+ lado 1) size) (+ tapa 1) (+ cubo size) ) #:color (eval color) #:emitted (emitted (cadr color) 1))
							    (vertex (pos (+ lado 1) (+ tapa 1) (+ cubo size) ) #:color (eval color) #:emitted (emitted (cadr color) 1))

							  ))
				  ((equal? cara "atras")(
							 quad
							    (vertex (pos (+ lado 1) (+ tapa 1) (+ cubo size) ) #:color (eval color) #:emitted (emitted (cadr color) 1))
							    (vertex (pos (+ (+ lado 1) size) (+ tapa 1)  (+ cubo size) ) #:color (eval color) #:emitted (emitted (cadr color) 1))
							    (vertex (pos (+ (+ lado 1) size) (+ (+ tapa 1) size) (+ cubo size) ) #:color (eval color) #:emitted (emitted (cadr color) 1))
							    (vertex (pos (+ lado 1) (+ (+ tapa 1) size) (+ cubo size) ) #:color (eval color) #:emitted (emitted (cadr color) 1))

							 ))
				  ((equal? cara "abajo")(
							 quad
							    (vertex (pos (+ lado 1) (+ (+ tapa 1) size) cubo ) #:color (eval color) #:emitted (emitted (cadr color) 1))
							    (vertex (pos (+ (+ lado 1) size) (+ (+ tapa 1) size) cubo ) #:color (eval color) #:emitted (emitted (cadr color) 1))
							    (vertex (pos (+ (+ lado 1) size) (+ (+ tapa 1) size) (+ cubo size) ) #:color (eval color) #:emitted (emitted (cadr color) 1))
							    (vertex (pos (+ lado 1) (+ (+ tapa 1) size) (+ cubo size) ) #:color (eval color) #:emitted (emitted (cadr color) 1)) #:back? #t

							 ))
			     ))

(define (pintarFilaH listaFilas lado tapa n size)(
				cond((null? listaFilas) empty-pict3d)
				    (else (
						      combine (pintarCuboH (car listaFilas) lado tapa 0 n #t size)
							      (pintarFilaH (cdr listaFilas) lado (+ tapa 1) n size)
						      ))
				))


(define (pintarCaraH cara lado tapa cubo size color) (
			     cond ((equal? cara "frente")(
							  ; Frente
							  quad
							    (vertex (pos cubo (+ lado 1) (+ tapa 1)) #:color (eval color) #:emitted (emitted (cadr color) 1))
							    (vertex (pos (+ cubo size) (+ lado 1) (+ tapa 1)) #:color (eval color) #:emitted (emitted (cadr color) 1))
							    (vertex (pos (+ cubo size) (+ (+ lado 1) size) (+ tapa 1)) #:color (eval color) #:emitted (emitted (cadr color) 1))
							    (vertex (pos cubo  (+ (+ lado 1) size) (+ tapa 1) ) #:color (eval color) #:emitted (emitted (cadr color) 1)) #:back? #t

							  ))
				  ((equal? cara "izquierda")(
							     quad
							    (vertex (pos cubo (+ lado 1) (+ tapa 1) ) #:color (eval color) #:emitted (emitted (cadr color) 1))
							    (vertex (pos cubo (+ lado 1) (+ (+ tapa 1) size) ) #:color (eval color) #:emitted (emitted (cadr color) 1))
							    (vertex (pos cubo (+ (+ lado 1) size) (+ (+ tapa 1) size) ) #:color (eval color) #:emitted (emitted (cadr color) 1))
							    (vertex (pos cubo (+ (+ lado 1) size) (+ tapa 1) ) #:color (eval color) #:emitted (emitted (cadr color) 1))
							    )
							     )
				  ((equal? cara "derecha")(
							   quad
							    (vertex (pos (+ cubo size) (+ lado 1) (+ tapa 1) ) #:color (eval color) #:emitted (emitted (cadr color) 1))
							    (vertex (pos (+ cubo size) (+ lado 1) (+ (+ tapa 1) size) ) #:color (eval color) #:emitted (emitted (cadr color) 1))
							    (vertex (pos (+ cubo size) (+ (+ lado 1) size) (+ (+ tapa 1) size) ) #:color (eval color) #:emitted (emitted (cadr color) 1))
							    (vertex (pos (+ cubo size) (+ (+ lado 1) size) (+ tapa 1) ) #:color (eval color) #:emitted (emitted (cadr color) 1)) #:back? #t

							   ))
				  ((equal? cara "arriba")(
							  quad
							    (vertex (pos cubo (+ lado 1) (+ tapa 1) ) #:color (eval color) #:emitted (emitted (cadr color) 1))
							    (vertex (pos (+ cubo size) (+ lado 1) (+ tapa 1) ) #:color (eval color) #:emitted (emitted (cadr color) 1))
							    (vertex (pos (+ cubo size) (+ lado 1) (+ (+ tapa 1) size) ) #:color (eval color) #:emitted (emitted (cadr color) 1))
							    (vertex (pos cubo (+ lado 1) (+ (+ tapa 1) size) ) #:color (eval color) #:emitted (emitted (cadr color) 1))

							  ))
				  ((equal? cara "atras")(
							 quad
							    (vertex (pos cubo (+ lado 1) (+ (+ tapa 1) size) ) #:color (eval color) #:emitted (emitted (cadr color) 1))
							    (vertex (pos (+ cubo size) (+ lado 1)  (+ (+ tapa 1) size) ) #:color (eval color) #:emitted (emitted (cadr color) 1))
							    (vertex (pos (+ cubo size) (+ (+ lado 1) size) (+ (+ tapa 1) size) ) #:color (eval color) #:emitted (emitted (cadr color) 1))
							    (vertex (pos cubo (+ (+ lado 1) size) (+ (+ tapa 1) size) ) #:color (eval color) #:emitted (emitted (cadr color) 1))

							 ))
				  ((equal? cara "abajo")(
							 quad
							    (vertex (pos cubo (+ (+ lado 1) size) (+ tapa 1) ) #:color (eval color) #:emitted (emitted (cadr color) 1))
							    (vertex (pos (+ cubo size) (+ (+ lado 1) size) (+ tapa 1) ) #:color (eval color) #:emitted (emitted (cadr color) 1))
							    (vertex (pos (+ cubo size) (+ (+ lado 1) size) (+ (+ tapa 1) size) ) #:color (eval color) #:emitted (emitted (cadr color) 1))
							    (vertex (pos cubo (+ (+ lado 1) size) (+ (+ tapa 1) size) ) #:color (eval color) #:emitted (emitted (cadr color) 1)) #:back? #t

							 ))
			     ))



(define (pintarCuboH listaCubos lado tapa cubo n frente? size)(
				   cond ((null? listaCubos) empty-pict3d)
					; Linea de arriba
					((equal? lado -1)(
							  ; Tapa del frente
							  cond ((equal? tapa -1) (
										cond ((equal? (length (car listaCubos)) 3)(
															   cond(frente? (
																	; Coloree frente, izq, arriba
																	; not frente
																	combine
																	(pintarCaraH "frente" lado tapa cubo size (caar listaCubos) )
																	(pintarCaraH "izquierda" lado tapa cubo size (cadar listaCubos))
																	(pintarCaraH "arriba" lado tapa cubo size (caddar listaCubos))
																	(pintarCuboH (cdr listaCubos) lado tapa (+ cubo 1) n (not frente?) size)
																	))
															       (else (
																      ; Coloree frente, derecha, arriba
																      ; not frente
																      combine
																      (pintarCaraH "frente" lado tapa cubo size (caar listaCubos))
																      (pintarCaraH "derecha" lado tapa cubo size (cadar listaCubos))
																      (pintarCaraH "arriba" lado tapa cubo size (caddar listaCubos))
																      (pintarCuboH (cdr listaCubos) lado tapa (+ cubo 1) n (not frente?) size)
																      ))
															   ))
										     ((equal? (length (car listaCubos)) 2)(
															   ; Coloree frente, arriba
															   combine
															   (pintarCaraH "frente" lado tapa cubo size (caar listaCubos))
															   (pintarCaraH "arriba" lado tapa cubo size (cadar listaCubos))
															   (pintarCuboH (cdr listaCubos) lado tapa (+ cubo 1) n frente? size)
															   ))
										))
							  ;Tapa de atras
							  ((equal? tapa (- n 2) )(
										cond ((equal? (length (car listaCubos)) 3)(
															   cond(frente? (
																	; Coloree frente, izq, abajo
																	; not frente
																	combine
																	(pintarCaraH "atras" lado tapa cubo size (caar listaCubos))
																	(pintarCaraH "izquierda" lado tapa cubo size (cadar listaCubos))
																	(pintarCaraH "arriba" lado tapa cubo size (caddar listaCubos))
																	(pintarCuboH (cdr listaCubos) lado tapa (+ cubo 1) n (not frente?) size)
																	))
															       (else (
																      ; Coloree atras, izq, abajo
																      ; not frente
																	combine
																	(pintarCaraH "derecha" lado tapa cubo size (caar listaCubos))
																	(pintarCaraH "atras" lado tapa cubo size (cadar listaCubos))
																	(pintarCaraH "arriba" lado tapa cubo size (caddar listaCubos))
																	(pintarCuboH (cdr listaCubos) lado tapa (+ cubo 1) n (not frente?) size)
																      ))
															   ))
										     ((equal? (length (car listaCubos)) 2)(
															   ; Coloree frente, abajo
															   combine
															   (pintarCaraH "atras" lado tapa cubo size (caar listaCubos))
															   (pintarCaraH "arriba" lado tapa cubo size (cadar listaCubos))
															   (pintarCuboH (cdr listaCubos) lado tapa (+ cubo 1) n frente? size)
															   ))
										))
							  ; Tapas del medio
							  (else (
										cond ((equal? (length (car listaCubos)) 2)(
															   cond(frente? (
																	; Coloree frente, izq
																	; not frente
																	combine
																	(pintarCaraH "izquierda" lado tapa cubo size (caar listaCubos))
																	(pintarCaraH "arriba" lado tapa cubo size (cadar listaCubos))
																	(pintarCuboH (cdr listaCubos) lado tapa (+ cubo 1) n (not frente?) size)
																	))
															       (else (
																      ; Coloree frente, derecha
																      ; not frente
																      combine
																	(pintarCaraH "derecha" lado tapa cubo size (caar listaCubos))
																	(pintarCaraH "arriba" lado tapa cubo size (cadar listaCubos))
																	(pintarCuboH (cdr listaCubos) lado tapa (+ cubo 1) n (not frente?) size)
																      ))
															   ))
										     ((equal? (length (car listaCubos)) 1)(
															   ; Coloree izq
															   combine
															   (pintarCaraH "arriba" lado tapa cubo size (caar listaCubos))
															   (pintarCuboH (cdr listaCubos) lado tapa (+ cubo 1) n frente? size)
															   ))
										))
							  ))
					;Linea de abajo
					((equal? lado (- n 2))(
							  ; Tapa del frente
							  cond ((equal? tapa -1)(
										cond ((equal? (length (car listaCubos)) 3)(
															   cond(frente? (
																	; Coloree atras, izquierda, arriba
																	; not frente
																	combine
																	(pintarCaraH "frente" lado tapa cubo size (caar listaCubos))
																	(pintarCaraH "izquierda" lado tapa cubo size (cadar listaCubos))
																	(pintarCaraH "abajo" lado tapa cubo size (caddar listaCubos))
																	(pintarCuboH (cdr listaCubos) lado tapa (+ cubo 1) n (not frente?) size)
																	))
															       (else (
																      ; Coloree atras, der, arriba
																      ; not frente
																      combine
																	(pintarCaraH "frente" lado tapa cubo size (caar listaCubos))
																	(pintarCaraH "derecha" lado tapa cubo size (cadar listaCubos))
																	(pintarCaraH "abajo" lado tapa cubo size (caddar listaCubos))
																	(pintarCuboH (cdr listaCubos) lado tapa (+ cubo 1) n (not frente?) size)
																      ))
															   ))
										     ((equal? (length (car listaCubos)) 2)(
															   ; Coloree atras, arriba
															   combine
																(pintarCaraH "frente" lado tapa cubo size (caar listaCubos))
																(pintarCaraH "abajo" lado tapa cubo size (cadar listaCubos))
																(pintarCuboH (cdr listaCubos) lado tapa (+ cubo 1) n frente? size)
															   ))
										))
							  ;Tapa de atras
							  ((equal? tapa (- n 2) )(
										cond ((equal? (length (car listaCubos)) 3)(
															   cond(frente? (
																	; Coloree atras, izq, abajo
																	; not frente
																	combine
																	(pintarCaraH "atras" lado tapa cubo size (caar listaCubos))
																	(pintarCaraH "izquierda" lado tapa cubo size (cadar listaCubos))
																	(pintarCaraH "abajo" lado tapa cubo size (caddar listaCubos))
																	(pintarCuboH (cdr listaCubos) lado tapa (+ cubo 1) n (not frente?) size)
																	))
															       (else (
																      ; Coloree atras, der, abajo
																      ; not frente?
																      combine
																	(pintarCaraH "derecha" lado tapa cubo size (caar listaCubos))
																	(pintarCaraH "atras" lado tapa cubo size (cadar listaCubos))
																	(pintarCaraH "abajo" lado tapa cubo size (caddar listaCubos))
																	(pintarCuboH (cdr listaCubos) lado tapa (+ cubo 1) n (not frente?) size)
																      ))
															   ))
										     ((equal? (length (car listaCubos)) 2)(
															   ; Coloree atras, abajo
															   combine
																	(pintarCaraH "atras" lado tapa cubo size (caar listaCubos))
																	(pintarCaraH "abajo" lado tapa cubo size (cadar listaCubos))
																	(pintarCuboH (cdr listaCubos) lado tapa (+ cubo 1) n frente? size)
															   ))
										))
							  ; Tapas del medio
							  (else (
										cond ((equal? (length (car listaCubos)) 2)(
															   cond(frente? (
																	; Coloree frente, der
																	; not frente
																	combine
																	(pintarCaraH "izquierda" lado tapa cubo size (caar listaCubos))
																	(pintarCaraH "abajo" lado tapa cubo size (cadar listaCubos))
																	(pintarCuboH (cdr listaCubos) lado tapa (+ cubo 1) n (not frente?) size)
																	))
															       (else (
																      ; Coloree atras, der
																      ; not frente
																      combine
																	(pintarCaraH "derecha" lado tapa cubo size (caar listaCubos))
																	(pintarCaraH "abajo" lado tapa cubo size (cadar listaCubos))
																	(pintarCuboH (cdr listaCubos) lado tapa (+ cubo 1) n (not frente?) size)
																      ))
															   ))
										     ((equal? (length (car listaCubos)) 1)(
															   ; Coloree atras
															   combine
															   (pintarCaraH "abajo" lado tapa cubo size (caar listaCubos))
															   (pintarCuboH (cdr listaCubos) lado tapa (+ cubo 1) n frente? size)
															   ))
										))
							  ))
					;Lineas centrales
					(else (
							  ; Tapa del frente
							  cond ((equal? tapa -1)(
										cond ((equal? (length (car listaCubos)) 2)(
															   cond(frente? (
																	; Coloree frente, izquierda
																	; not frente
																	combine
																	(pintarCaraH "frente" lado tapa cubo size (caar listaCubos))
																	(pintarCaraH "izquierda" lado tapa cubo size (cadar listaCubos))
																	(pintarCuboH (cdr listaCubos) lado tapa (+ cubo 1) n (not frente?) size)
																	))
															       (else (
																      ; Coloree atras, arriba
																      ; not frente
																      combine
																	(pintarCaraH "frente" lado tapa cubo size (caar listaCubos))
																	(pintarCaraH "derecha" lado tapa cubo size (cadar listaCubos))
																	(pintarCuboH (cdr listaCubos) lado tapa (+ cubo 1) n (not frente?) size)
																      ))
															   ))
										     ((equal? (length (car listaCubos)) 1)(
															   ; Coloree arriba
															   combine
																	(pintarCaraH "frente" lado tapa cubo size (caar listaCubos))
																	(pintarCuboH (cdr listaCubos) lado tapa (+ cubo 1) n frente? size)
															   ))
										))
							  ;Tapa de atras
							  ((equal? tapa (- n 2) )(
										cond ((equal? (length (car listaCubos)) 2)(
															   cond(frente? (
																	; Coloree izquierda abajo
																	; not frente
																	combine
																	(pintarCaraH "atras" lado tapa cubo size (caar listaCubos))
																	(pintarCaraH "izquierda" lado tapa cubo size (cadar listaCubos))
																	(pintarCuboH (cdr listaCubos) lado tapa (+ cubo 1) n (not frente?) size)
																	))
															       (else (
																      ; Coloree derecha, abajo
																      ; not frente
																      combine
																	(pintarCaraH "derecha" lado tapa cubo size (caar listaCubos))
																	(pintarCaraH "atras" lado tapa cubo size (cadar listaCubos))
																	(pintarCuboH (cdr listaCubos) lado tapa (+ cubo 1) n (not frente?) size)
																      ))
															   ))
										     ((equal? (length (car listaCubos)) 1)(
															   ; Coloree abajo
															   combine
																	(pintarCaraH "atras" lado tapa cubo size (caar listaCubos))
																	(pintarCuboH (cdr listaCubos) lado tapa (+ cubo 1) n frente? size)
															   ))
										))
							  ; Filas del medio
							  (else (
										cond ((equal? (length (car listaCubos)) 1)(
															   cond(frente? (
																	; Coloree izq
																	; not frente
																	combine
																	(pintarCaraH "izquierda" lado tapa cubo size (caar listaCubos))
																	(pintarCuboH (cdr listaCubos) lado tapa (+ cubo 1) n (not frente?) size)
																	))
															       (else (
																      ; Coloree atras
																      ; not frente
																      combine
																	(pintarCaraH "derecha" lado tapa cubo size (caar listaCubos))
																	(pintarCuboH (cdr listaCubos) lado tapa (+ cubo 1) n (not frente?) size)
																      ))
															   ))
										     ((equal? (length (car listaCubos)) 0)(
															   ; nada
															   combine
															   empty-pict3d
															   (pintarCuboH (cdr listaCubos) lado tapa (+ cubo 1) n frente? size)
															   ))
										))
							  ))
				   ))



#| Funciones que permiten hacer cuadros en vertical
Tiene que llamarse primero el pintar linea vertical, luego fila y luego cubos

Lado es izquierda:-1 , centro:0 a n-1, derecha:n-1
Tapa es arriba: -1, centro:0 a n-1, abajo:n-1
en esencia funciona igual que pintarV|#

(define (pintarH listalineas lado rotar n size i)(
			  cond ((null? listalineas) empty-pict3d )
			       (else (
				      cond
					   ; Hay rotacion de linea, al ser creado horizontalmente solo puede rotar en x.
					   ((equal? i (car rotar)) (
								    combine (rotate-y/center (pintarFilaH (car listalineas) lado -1 n size)
											     (*  (/ (cadddr rotar ) (car (cddddr rotar ))) (cadr rotar ) )
											     )
									    (pintarH (cdr listalineas) (+ lado 1) rotar n size (+ i 1) )
									     ))
					   ; No hay rotacion de linea
					   (else (
						  combine (pintarFilaH (car listalineas) lado -1 n size)
					       (pintarH (cdr listalineas) (+ lado 1) rotar n size (+ i 1) )
						  ))
						  ))
			  ))

; Funcion que va a dibujar cada frame
(define (on-draw s n t) (
		 cond
		  ; No hay rotaciones
		  ((null? (caddr s)) (
				      cond ((equal? (car (cddddr s)) "x" ) (
									    combine (basis 'camera (affine-compose  (move-y (/ (cadr s) 2)) (move-x (/ (cadr s) 2)) (point-at
																				      (pos 0 0 (* -1 (cadr s)) ) origin) ))
									    (rotate-x/center (pintarH (cadr (
						    cambiar_agrupacion
						    ;N del cubo
						    (cadr s)
						    ; Cubo identificado (x cubo)
						    (list
						      (cond ((equal? "x" (car (cddddr s))) 'x)
							    (else 'y)
							    )
						      (car s) )
						    ; De que manera quiero orientarlo
						    'x
						    )) -1 '(#f) (cadr s) 1 -1) 0 )
									    (light (pos 5 5 5) )
									    ))
				      (else (
					     combine (basis 'camera (affine-compose  (move-y (/ (cadr s) 2)) (move-x (/ (cadr s) 2)) (point-at (pos 0 0 (* -1 (cadr s))) origin) ))
					     (rotate-x/center (pintarV (car s) -1 '(#f) (cadr s) 1 -1) 0 )
									    ))
		  ) )
		  (else (

			 cond
			  ; Hay que rotar el cubo
			  ((equal? (cadr s) (car (caddr s) ) ) (
								; (eval (caddr s) ns) es la lista de rotacion
								;horizontal
								cond ((equal? "x" ( caddr (caddr s)  ))(
			 combine (basis 'camera (affine-compose  (move-y (/ (cadr s) 2)) (move-x (/ (cadr s) 2)) (point-at (pos 0 0 (* -1 (cadr s))) origin) ))
			 ; Aqui se rota el cubo completo, rotate-x rota a la izquierda y derecha y rotate-y rota hacia arriba y abajo
			 (rotate-y/center (pintarH (cadr (
						    cambiar_agrupacion
						    ;N del cubo
						    (cadr s)
						    ; Cubo identificado (x cubo)
						    (list
						      (cond ((equal? "x" (car (cddddr s))) 'x)
							    (else 'y)
							    )
						      (car s) )
						    ; De que manera quiero orientarlo
						    'x
						    )) -1 '(#f) (cadr s) 1 -1)

					  ; Se dividen los grados que hay que rotar por la cantidad de frames en los que hay que hacerlo y se multiplica
					  ; por los frames que se llevan
					  (*  (/ (cadddr (caddr s) ) (car (cddddr (caddr s) ))) (cadr (caddr s) ) ) ) ) )
								     ; Vertical
								     ((equal? "y" ( caddr (caddr s)  ) ) (
			 combine (basis 'camera (affine-compose  (move-y (/ (cadr s) 2)) (move-x (/ (cadr s) 2)) (point-at (pos 0 0 (* -1 (cadr s))) origin) ))
			 ; Aqui se rota el cubo completo, rotate-x rota a la izquierda y derecha y rotate-y rota hacia arriba y abajo
			 (rotate-x/center (pintarV (cadr (
						    cambiar_agrupacion
						    ;N del cubo
						    (cadr s)
						    ; Cubo identificado (x cubo)
						    (list
						      (cond ((equal? "x" (car (cddddr s))) 'x)
							    (else 'y)
							    )
						      (car s) )
						    ; De que manera quiero orientarlo
						    'y
						    )) -1 '(#f) (cadr s) 1 -1)

					  ; Se dividen los grados que hay que rotar por la cantidad de frames en los que hay que hacerlo y se multiplica
					  ; por los frames que se llevan
					  (* -1 (*  (/ (cadddr (caddr s) ) (car (cddddr (caddr s) ))) (cadr (caddr s) ) )) ) ))

								))
			  ; Hay que rotar una cara
			  (else (
				 cond ((equal? "y" (caddr (caddr s) ))
				       (combine (basis 'camera (affine-compose  (move-y (/ (cadr s) 2)) (move-x (/ (cadr s) 2)) (point-at (pos 0 0 (* -1 (cadr s))) origin) ))
						(pintarV (cadr (
						    cambiar_agrupacion
						    ;N del cubo
						    (cadr s)
						    ; Cubo identificado (x cubo)
						    (list
						      (cond ((equal? "x" (car (cddddr s))) 'x)
							    (else 'y)
							    )
						      (car s) )
						    ; De que manera quiero orientarlo
						    'y
						    )) -1 (caddr s) (cadr s) 1 -1)) )
				 ((equal? "x" (caddr (caddr s) ))
				       (combine (basis 'camera (affine-compose  (move-y (/ (cadr s) 2)) (move-x (/ (cadr s) 2)) (point-at (pos 0 0 (* -1 (cadr s))) origin) ))
						(pintarH (cadr (
						    cambiar_agrupacion
						    ;N del cubo
						    (cadr s)
						    ; Cubo identificado (x cubo)
						    (list
						      (cond ((equal? "x" (car (cddddr s))) 'x)
							    (else 'y)
							    )
						      (car s) )
						    ; De que manera quiero orientarlo
						    'x
						    )) -1 (caddr s) (cadr s) 1 -1)) )

				)

			 ))

		      )
))


(define (aumentar-frame listaRot s) (
				     ; Es igual los frames que se llevan en rotacion a los frames necesarios para terminar?
			   cond ((equal? (car (cddddr listaRot)) (cadr listaRot) )(
										    ; Aqui se rota la lista en direccion de caddddr s
				       list ( cadr (
					     ;car s
					     aplic_movs
					     ; N del cubo
					     (cadr s)
					     ; Cubo orientado
					     ; orientar antes de iniciar la rotacion
					     (
						    cambiar_agrupacion
						    ;N del cubo
						    (cadr s)
						    ; Cubo identificado (x cubo)
						    (list
						      (cond ((equal? "x" (car (cddddr s))) 'x)
							    (else 'y)
							    )
						      (car s) )
						    ; De que manera quiero orientarlo
						    'x
						    )
					     (list
					     ;Eje
					     (cond
					       ((equal? "x" (caddr (caddr s))) 'x)
					       (else 'y)
					       )
					     ;Direccion
					     (* (cadddr listaRot) -1)
					     ; Cara
					     (+ (car listaRot) 2))
					     ;Numero de la cara donde se va a hacer el cambio

					     )) (cadr s)
				       '()
				       (cadddr s) (car (cddddr s)) (cadr (cddddr s))
				       ) )
				(else (
				       list (car s) (cadr s)
				       (list (car listaRot) (+ (cadr listaRot) 1) (caddr listaRot) (cadddr listaRot) (car (cddddr listaRot)))
				       (cadddr s) (car (cddddr s)) (cadr (cddddr s))
				       ))
			   ) )
#|
Funcion que se llama cada frame, debe retornar el estado s, la utilizamos para disminuir en 1 los frames necesarios para la rotacion o eliminar la rotacion
si ya termino.
|#
(define (on-frame s n t) (
		  ; Hay rotaciones, entonces le suma uno a los frames que lleva o quita la lista de rotacion si ya se alcanzaron los frames necesarios
		cond ((not (null? (caddr s)) ) (
						aumentar-frame (caddr s) s
						))
		; Si no hay rotaciones, se verifica si hay movimientos pendientes, de haber se agrega un movimiento a la cola de rotaciones
		((and (not (null? (cadddr s))) (> t 5000))

		 		  ; Reconstruye s, pero agregando el nuevo movimiento a la lista de rotaciones
				  		  (append (drop-right s 4) (cons  (list (caddr (car (cadddr s))) 0 (car (car (cadddr s))) (cadr (car (cadddr s))) 7) (cons (cdr (cadddr s)) (take-right s 2))))
						  		  )
		     (else s)
		))

#|Funciones para mouse drag|#

; Funcion que devuelve el cambio en alguna variable a partir de una variable inicial y una final
(define (cambion ni nf)(
		- nf ni
		))

#|
Funcion que devuelve si hubo un movimiento en x o y a partir del inicio y final del movimiento en cada coordenada
de existir un movimiento en x o y devuelve "x" si fue en x o "y" si fue en y, de no existir tira #f.
|#
(define (movimiento xi yi xf yf)(
			 cond ((> (abs (cambion xi xf)) (abs(cambion yi yf))) "x")
			 ((< (abs(cambion xi xf)) (abs (cambion yi yf))) "y")
			 (else #f)
			 ))
#|
Funcion encargada de retornar la cantidad de movimiento que hubo en un cierto eje, igual que movimiento, pero en vez de retornar booleano retorna int.
|#
(define (cantidadMovimiento xi yi xf yf)(
			 cond ((> (abs (cambion xi xf)) (abs (cambion yi yf))) (cambion xi xf))
			 ((< (abs (cambion xi xf)) (abs (cambion yi yf))) (cambion yi yf))
			 (else 0)
			 ))
#|
Funcion que toma un numero y si es positivo devuelve +90, si es negativo -90, esto se hace porque las rotaciones son discretas, es decir, si se rota
se rota hasta la otra cara.
|#
(define (ninetify number)(
		  cond ((> number 0) -90)
		  ((< number 0) 90)
		  (else 0)
		  ))

#|
Funcion encargada de agregar una nueva rotacion, solo lo hace si no hay una rotacion en ejecucion, es decir, si la lista de rotaciones esta vacia
toma la lista s, el eje de rotacion, la direccion (numero positivo o negativo) y la linea en que lo va a hacer (tomar en cuenta que va de -1 a (n-2) y es n si se va a rotar todo el cubo )
|#
(define (agregarRotacion s eje direccion linea)(
					; No hay rotaciones ni movimientos
					cond ((and (null? (caddr s)) (null? (cadddr s)) )(
								append (drop-right s 4)
								(list (list
									linea 0 eje (ninetify direccion) 7
									))
								(drop-right (cdddr s) 1)
								'(())
						 ))
					; Si hay rotaciones ignora la orden
					(else s)
					))

#|
Funcion encargada de proyectar un punto en el plano de la pantalla al mundo tridimensional del cubo rubik
recibe una x y una y del punto a proyectar y el tamanno del cubo.
Retorna n si el drag se hizo fuera del cubo
Retorna (i j) donde i es la cara donde se hizo el drag en x y j lo mismo en y.
|#
(define (proyeccion x y n)(
		   ; Se esta haciendo drag fuera del cubo
		 cond ((or (< x 128) (> x 383) (< y 128) (> y 383))
		       (
			list (+ n 1) (+ n 1)
			))
		 ; Se esta haciendo drag dentro del cubo
		 (else
		   (
		    pro_aux x y n
		    )
		       )
		 ))
; Debe devolver el n de la cara en la que estoy apretando
; Recibe el tamanno del cubo como n. Y el click,
(define (pro_aux x y n)
  (
   list (inexact->exact (truncate ( exact->inexact (/ (- x 128) (/ 255 n))))) (inexact->exact (truncate ( exact->inexact (/ (- y 128) (/ 255 n)))))
   )
  )




#|
Funcion encargada de monitorear los cambios del mouse
cuando se encuentra un evento tipo left-down, se guarda la posicion x y y del mouse
cuando se encuentra un evento tipo left-up, se compara la posicion guardada anteriormente con la actual
y se agrega a la lista de rotaciones la rotacion pertinente
|#
(define (on-mouse s n t x y e)
		       (
			cond ((equal? "left-down" e)(writeln (list x y))(
						append (drop-right s 1) (list (list x y (proyeccion x y (cadr s)) ))
						))
			((equal? "left-up" e)(
					      cond ((equal? "y" (movimiento (caar (take-right s 1)) (cadar (take-right s 1)) x y))(
																   agregarRotacion s
					      (movimiento (caar (take-right s 1)) (cadar (take-right s 1)) x y)
					      (cantidadMovimiento (caar (take-right s 1)) (cadar (take-right s 1)) x y)
					      (- (car (caddar (take-right s 1))) 1)

																   ))
					      ((equal? "x" (movimiento (caar (take-right s 1)) (cadar (take-right s 1)) x y))(
																   agregarRotacion s
					      (movimiento (caar (take-right s 1)) (cadar (take-right s 1)) x y)
					      (cantidadMovimiento (caar (take-right s 1)) (cadar (take-right s 1)) x y)
					      (- (cadr (caddar (take-right s 1))) 1)

																   ))
									    ))
			(else s)
			)
		       )











#|
Funciones principales
|#
;(on-draw 0 0 0)

; Sin rotacion
#;(big-bang3d

'(
	       ;Lista del cubo
	       (
		     ; Primera linea hroizontal
		     (
		     ( ( (rgba "green") (rgba "red") (rgba "yellow") ) ( (rgba "white") (rgba "orange") ) ( (rgba "orange") (rgba "white") (rgba "blue") ) )
		     ( ( (rgba "blue") (rgba "white") )     ((rgba "white"))     ( (rgba "green") (rgba "yellow") ) )
		     ( ( (rgba "blue") (rgba "red") (rgba "yellow") ) ( (rgba "green") (rgba "orange") ) ( (rgba "orange") (rgba "blue") (rgba "yellow") ) )
		     )
		     ; Segunda linea horizontal
		     (
		     ( ( (rgba "red") (rgba "blue") )  ((rgba "blue"))  ( (rgba "white") (rgba "green")  ) )
		     (   ((rgba "red"))     ()    (( rgba "orange" )) )
		     ( ( (rgba "red") (rgba "green") )  ((rgba "green"))  ( (rgba "blue") (rgba "yellow") ) )
		     )
		     ; Tercera linea horizontal
		     (
		     ( ( (rgba "green") (rgba "white") (rgba "orange") ) ( (rgba "blue") (rgba "orange") ) ( (rgba "red") (rgba "green") (rgba "white") ) )
		     ( ( (rgba "yellow") (rgba "red") )     ((rgba "yellow"))     ( (rgba "yellow") (rgba "orange") ) )
		     ( ( (rgba "blue") (rgba "red") (rgba "white") ) ( (rgba "white") (rgba "red") ) ( (rgba "yellow") (rgba "orange") (rgba "green") ) )
		     )
		     )
	       ; N del cubo
	       3
	       ;Lista de rotacion
	       ()
	       ; Lista de movimientos
	       ()
	       ; Orientacion de la lista del cubo
	       "x"
	       ; Estado del drag
	       ()
	       )

#:on-draw on-draw #:on-frame on-frame #:frame-delay (/ 1000 140) #:on-mouse on-mouse )



; Rotacion horizontal
#;(big-bang3d

'(
	       ;Lista del cubo
	       (
		     ; Primera linea hroizontal
		     (
		     ( ( (rgba "green") (rgba "red") (rgba "yellow") ) ( (rgba "white") (rgba "orange") ) ( (rgba "orange") (rgba "white") (rgba "blue") ) )
		     ( ( (rgba "blue") (rgba "white") )     ((rgba "white"))     ( (rgba "green") (rgba "yellow") ) )
		     ( ( (rgba "blue") (rgba "red") (rgba "yellow") ) ( (rgba "green") (rgba "orange") ) ( (rgba "orange") (rgba "blue") (rgba "yellow") ) )
		     )
		     ; Segunda linea horizontal
		     (
		     ( ( (rgba "red") (rgba "blue") )  ((rgba "blue"))  ( (rgba "white") (rgba "green")  ) )
		     (   ((rgba "red"))     ()    (( rgba "orange" )) )
		     ( ( (rgba "red") (rgba "green") )  ((rgba "green"))  ( (rgba "blue") (rgba "yellow") ) )
		     )
		     ; Tercera linea horizontal
		     (
		     ( ( (rgba "green") (rgba "white") (rgba "orange") ) ( (rgba "blue") (rgba "orange") ) ( (rgba "red") (rgba "green") (rgba "white") ) )
		     ( ( (rgba "yellow") (rgba "red") )     ((rgba "yellow"))     ( (rgba "yellow") (rgba "orange") ) )
		     ( ( (rgba "blue") (rgba "red") (rgba "white") ) ( (rgba "white") (rgba "red") ) ( (rgba "yellow") (rgba "orange") (rgba "green") ) )
		     )
		     )
	       ; N del cubo
	       3
	       ;Lista de rotacion
	       (
		; Linea que estoy rotando, si es igual a n se refiere a todo el cubo, tomar en cuenta que inicia en -1
		3
		; Frames que lleva
		0
		; Eje de rotacion
		"x"
		; Direccion de rotacion
		90
		; Frames necesarios para terminar la rotacion
		7
		)
	       ; Lista de movimientos
	       ()
	       ; Orientacion de la lista del cubo
	       "x"
	       )

#:on-draw on-draw #:on-frame on-frame #:frame-delay (/ 1000 140))

; Rotacion vertical
#;(big-bang3d

'(
	       ;Lista del cubo
	       (
		     ; Primera linea vertical
		     (
		     ( ( (rgba "green") (rgba "red") (rgba "yellow") ) ( (rgba "blue") (rgba "white") ) ( (rgba "blue") (rgba "red") (rgba "yellow") ) )
		     ( ( (rgba "red") (rgba "blue") )     ((rgba "red"))     ( (rgba "red") (rgba "green") ) )
		     ( ( (rgba "green") (rgba "white") (rgba "orange") ) ( (rgba "yellow") (rgba "red") ) ( (rgba "blue") (rgba "red") (rgba "white") ) )
		     )
		     ; Segunda linea vertical
		     (
		     ( ( (rgba "white") (rgba "orange") )  ((rgba "white"))  ( (rgba "green") (rgba "orange")  ) )
		     (   ((rgba "blue"))     ()    (( rgba "green" )) )
		     ( ( (rgba "blue") (rgba "orange") )  ((rgba "yellow"))  ( (rgba "white") (rgba "red") ) )
		     )
		     ; Tercera linea vertical
		     (
		     ( ( (rgba "orange") (rgba "white") (rgba "blue") ) ( (rgba "green") (rgba "yellow") ) ( (rgba "orange") (rgba "blue") (rgba "yellow") ) )
		     ( ( (rgba "white") (rgba "green") )     ((rgba "orange"))     ( (rgba "blue") (rgba "yellow") ) )
		     ( ( (rgba "red") (rgba "green") (rgba "white") ) ( (rgba "yellow") (rgba "orange") ) ( (rgba "yellow") (rgba "orange") (rgba "green") ) )
		     )
		     )
	       ; N del cubo
	       3
	       ;Lista de rotacion
	       (
		; Linea que estoy rotando, si es igual a n se refiere a todo el cubo, tomar en cuenta que inicia en -1
		-1
		; Frames que lleva
		0
		; Eje de rotacion
		"x"
		; Direccion de rotacion
		90
		; Frames necesarios para terminar la rotacion
		7
		)
	       ; Lista de movimientos
	       ()
	       ; Orientacion de la lista del cubo
	       "y"
	       )
#:on-draw on-draw #:on-frame on-frame #:frame-delay (/ 1000 500))

; Pruebas de ejecucionnnn