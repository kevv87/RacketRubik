#lang racket
(define lista '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54))
(define cubo_3x3 '((rgba "orange") (rgba "white") (rgba "blue") (rgba "white") (rgba "blue") (rgba "white") (rgba "red") (rgba "blue") (rgba "orange") (rgba "blue") (rgba "blue") (rgba "red") (rgba "blue") (rgba "orange") (rgba "yellow") (rgba "blue") (rgba "yellow") (rgba "blue") (rgba "yellow") (rgba "red") (rgba "blue") (rgba "white") (rgba "orange") (rgba "white") (rgba "white") (rgba "red") (rgba "orange") () (rgba "red") (rgba "orange") (rgba "yellow") (rgba "yellow") (rgba "yellow") (rgba "red") (rgba "white") (rgba "orange") (rgba "green") (rgba "white") (rgba "green") (rgba "white") (rgba "red") (rgba "green") (rgba "orange") (rgba "green") (rgba "green") (rgba "red") (rgba "green") (rgba "orange") (rgba "yellow") (rgba "green") (rgba "yellow") (rgba "green") (rgba "yellow") (rgba "red") (rgba "green")))
(define cubo_4x4 '((rgba "orange") (rgba "white") (rgba "blue") (rgba "white") (rgba "blue") (rgba "white") (rgba "blue") (rgba "white") (rgba "red") (rgba "blue") (rgba "orange") (rgba "blue") (rgba "blue") (rgba "blue") (rgba "red") (rgba "blue") (rgba "orange") (rgba "blue") (rgba "blue") (rgba "blue") (rgba "red") (rgba "blue") (rgba "orange") (rgba "yellow") (rgba "blue") (rgba "yellow") (rgba "blue") (rgba "yellow") (rgba "blue") (rgba "yellow") (rgba "red") (rgba "blue") (rgba "white") (rgba "orange") (rgba "white") (rgba "white") (rgba "white") (rgba "red") (rgba "orange") () () (rgba "red") (rgba "orange") () () (rgba "red") (rgba "orange") (rgba "yellow") (rgba "yellow") (rgba "yellow") (rgba "yellow") (rgba "red") (rgba "white") (rgba "orange") (rgba "white") (rgba "white") (rgba "white") (rgba "red") (rgba "orange") () () (rgba "red") (rgba "orange") () () (rgba "red") (rgba "orange") (rgba "yellow") (rgba "yellow") (rgba "yellow") (rgba "yellow") (rgba "red") (rgba "white") (rgba "orange") (rgba "green") (rgba "white") (rgba "green") (rgba "white") (rgba "green") (rgba "white") (rgba "red") (rgba "green") (rgba "orange") (rgba "green") (rgba "green") (rgba "green") (rgba "green") (rgba "red") (rgba "orange") (rgba "green") (rgba "green") (rgba "green") (rgba "red") (rgba "green") (rgba "orange") (rgba "yellow") (rgba "green") (rgba "yellow") (rgba "green") (rgba "yellow") (rgba "green") (rgba "yellow") (rgba "red") (rgba "green")))
(define cubo_kevin '((rgba "green") (rgba "red") (rgba "yellow") (rgba "white") (rgba "orange") (rgba "orange") (rgba "white") (rgba "blue") (rgba "blue") (rgba "white") (rgba "white") (rgba "green") (rgba "yellow") (rgba "blue") (rgba "red") (rgba "yellow") (rgba "green") (rgba "orange")  (rgba "orange") (rgba "blue") (rgba "yellow") (rgba "red") (rgba "blue") (rgba "blue")  (rgba "white") (rgba "green") (rgba "red") () (rgba "orange")  (rgba "red") (rgba "green") (rgba "green")  (rgba "blue") (rgba "yellow") (rgba "green") (rgba "white") (rgba "orange")  (rgba "blue") (rgba "orange") (rgba "red") (rgba "green") (rgba "white") (rgba "yellow") (rgba "red") (rgba "yellow") (rgba "yellow") (rgba "orange")  (rgba "blue") (rgba "red") (rgba "white") (rgba "white") (rgba "red") (rgba "yellow") (rgba "orange") (rgba "green")))

#|                                                       ___________________
________________________________________________________/Ejecucion del juego\________________________________________________________
|#
;Funcion que simula un cubo de Rubik
(define (RS X Cubo Movs)
  (cond ((> (round X) 1)
          (cond ((null? Cubo)
                 (aplic_movs X (cubo_standard (round X)) Movs))   ;Si no ingresa un cubo, genera un cubo NxN estandar
                (else (aplic_movs X (identificar 'x (caras_moviles X (format_x X Cubo 1 1))) Movs))))
        (else #f)))

;Funcion que aplica los movimientos sobre el cubo
(define (aplic_movs X Cubo_identificado Movs)
  (cond ((null? Movs)
         Cubo_identificado)
        ((equal? (substring (car Movs) 0 1) "F")   ;Para el cubo agrupado horizontalmente
         (cond ((equal? (substring (car Movs) 2 3) "D")   ;Para direccion positiva de rotacion
                (rotar X (cadr (cambiar_agrupacion X Cubo_identificado 'x)) (string->number (substring (car Movs) 1 2)) 90)   ;Aplica rotacion + en X
                (aplic_movs X (identificar 'x (rotar X (cadr (cambiar_agrupacion X Cubo_identificado 'x)) (string->number (substring (car Movs) 1 2)) 90)) (cdr Movs)))
               ((equal? (substring (car Movs) 2 3) "I")   ;Para direccion negativa de rotacion
                (rotar X (cadr (cambiar_agrupacion X Cubo_identificado 'x)) (string->number (substring (car Movs) 1 2)) -90)   ;;Aplica rotacion - en X
                (aplic_movs X (identificar 'x (rotar X (cadr (cambiar_agrupacion X Cubo_identificado 'x)) (string->number (substring (car Movs) 1 2)) -90)) (cdr Movs)))))
        ((equal? (substring (car Movs) 0 1) "C")   ;Para el cubo agrupado verticalmente
         (cond ((equal? (substring (car Movs) 2 3) "A")   ;Para direccion positiva de rotacion
                (rotar X (cadr (cambiar_agrupacion X Cubo_identificado 'y)) (string->number (substring (car Movs) 1 2)) 90)   ;Aplica rotacion + en Y
                (aplic_movs X (cambiar_agrupacion X (identificar 'y (rotar X (cadr (cambiar_agrupacion X Cubo_identificado 'y)) (string->number (substring (car Movs) 1 2)) 90)) 'x) (cdr Movs)))
               ((equal? (substring (car Movs) 2 3) "B")   ;Para direccion negativa de rotacion
                (rotar X (cadr (cambiar_agrupacion X Cubo_identificado 'y)) (string->number (substring (car Movs) 1 2)) -90)   ;;Aplica rotacion - en X
                (aplic_movs X (cambiar_agrupacion X (identificar 'y (rotar X (cadr (cambiar_agrupacion X Cubo_identificado 'y)) (string->number (substring (car Movs) 1 2)) -90)) 'x) (cdr Movs)))))))

;Function that creates a cube of nxn if there's no cube input
(define (cubo_standard X)
  #t)



#|                                                       _____________________________________
________________________________________________________/Funciones para la creacion de un cubo\________________________________________________________
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
________________________________________________________/ Funciones para darle formato al cubo\________________________________________________________
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
*Argumentos: tamano cubo, cubo, contador cara, direccion (+ o -) '+
*Devuelve: cubo resultante de la rotacion a una de sus caras
|#
(define (rotar X Cubo Num_cara Direccion)
  (cond ((>= X Num_cara)  ;Si cara menor al tamano, es rotacion de una sola cara
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
         

(define cubo3_format (format_x 3 cubo_3x3 1 1))
(define cubo3_caras (caras_moviles 3 cubo3_format))
(define cubo3_id (identificar 'x cubo3_caras))
;(define cubo3_id (identificar 3 'x cubo_3x3))
(define cubo3_en_Y '(y
  ((((naran blanc azul) (naran azul) (naran amar azul)) ((blanc naran) (naran) (naran amar)) ((blanc naran verd) (naran verde) (naran amar verd)))
   (((blanc azul) (azul) (amar azul)) ((blanc) (X) (amar)) ((blanc verd) (verde) (amar verd)))
   (((blanc rojo azul) (rojo azul) (amar rojo azul)) ((blanc rojo) (rojo) (amar rojo)) ((blanc rojo verd) (rojo verd) (amar rojo verd))))))
;(define cubo4_id (identificar 4 'x cubo_4x4))
(define cubo4_en_Y '(y
  ((((naran blanc azul) (naran azul) (naran azul) (naran amar azul)) ((blanc naran) (naran) (naran) (naran amar)) ((blanc naran) (naran) (naran) (naran amar)) ((blanc naran verd) (naran verd) (naran verd) (naran amar verd)))
   (((blanc azul) (azul) (azul) (amar azul)) ((blanc) (X) (X) (amar)) ((blanc) (X) (X) (amar)) ((blanc verd) (verd) (verd) (amar verd)))
   (((blanc azul) (azul) (azul) (amar azul)) ((blanc) (X) (X) (amar)) ((blanc) (X) (X) (amar)) ((blanc verd) (verd) (verd) (amar verd)))
   (((blanc rojo azul) (rojo azul) (rojo azul) (amar rojo azul)) ((blanc rojo) (rojo) (rojo) (amar rojo)) ((blanc rojo) (rojo) (rojo) (amar rojo)) ((blanc rojo verd) (verd rojo) (rojo verd) (amar rojo verd))))))
