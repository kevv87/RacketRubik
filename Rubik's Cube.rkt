#lang racket
(define lista '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54))
(define cubo_3x3 '(naran blanc azul blanc azul blanc rojo azul naran azul azul rojo azul naran amar azul amar azul amar rojo azul blanc naran blanc blanc rojo naran X rojo naran amar amar amar rojo blanc naran verd blanc verd blanc rojo verd naran verde verde rojo verd naran amar verd amar verd amar rojo verd))
(define cubo_4x4 '(naran blanc azul blanc azul blanc azul blanc rojo azul naran azul azul azul rojo azul naran azul azul azul rojo azul naran amar azul amar azul amar azul amar rojo azul blanc naran blanc blanc blanc rojo naran X X rojo naran X X rojo naran amar amar amar amar rojo blanc naran blanc blanc blanc rojo naran X X rojo naran X X rojo naran amar amar amar amar rojo blanc naran verd blanc verd blanc verd blanc rojo verd naran verd verd verd verd rojo naran verd verd verd rojo verd naran amar verd amar verd amar verd amar rojo verd))

;Colores: [(vacio, 0), (rojo, 1), (azul, 2), (naranja, 3), (verde, 4), (blanco, 5), (amarillo, 6)]
;Orden de caras: frente, derecha, reverso, izquierda, superior, inferior.
;Funcion que simula un cubo de Rubik
(define (RS X Cubo Movs)
  (cond ((> (round X) 1)
          (cond ((null? Cubo)
                 (aplic_movs X (cubo_standard (round X)) Movs))
                (else (aplic_movs X Cubo Movs))))
        (else #f)))

;Funcion que aplica los movimientos sobre el cubo
(define (aplic_movs X Cubo Movs)
  #t)

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
*Argumentos: eje de orientacion, cubo agrupado por caras moviles
*Devuelve: pareja de eje y cubo agrupado por caras
|#
(define (identificar eje Cubo)
  (list eje Cubo))



#|                                                       ______________________________________
________________________________________________________/Funciones para aplicar cambios al cubo\________________________________________________________
|#


#|
*Funcion: cambair_agrupacion
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
*Argumentos: tamano cubo, cubo identificado por su eje de orientacion, contador cara, direccion (+ o -)
*Devuelve: cubo resultante de la rotacion a una de sus caras
|#
(define (rotar X Cubo Num_cara Direccion)
  (cond ((equal? Num_cara 1)
         (cond ((equal? Direccion '+)
                (cons (caras_moviles X (rotar+ X (car Cubo) X 1)) (cdr Cubo)))
               ((equal? Direccion '-)
                (cons (caras_moviles X (rotar- X (car Cubo) 1 X)) (cdr Cubo)))))
        (else (cons (car Cubo) (rotar X (cdr Cubo) (- Num_cara 1) Direccion)))))

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


(define cubo3_format (format_x 3 cubo_3x3 1 1))
(define cubo3_caras (caras_moviles 3 cubo3_format))
(define cubo3_id (identificar 'x cubo3_caras))
(define cubo3_en_Y '(y
  ((((naran blanc azul) (naran azul) (naran amar azul)) ((blanc naran) (naran) (naran amar)) ((blanc naran verd) (naran verde) (naran amar verd)))
   (((blanc azul) (azul) (amar azul)) ((blanc) (X) (amar)) ((blanc verd) (verde) (amar verd)))
   (((blanc rojo azul) (rojo azul) (amar rojo azul)) ((blanc rojo) (rojo) (amar rojo)) ((blanc rojo verd) (rojo verd) (amar rojo verd))))))
(define cubo4_format (format_x 4 cubo_4x4 1 1))
(define cubo4_caras (caras_moviles 4 cubo4_format))
(define cubo4_id (identificar 'x cubo4_caras))
(define cubo4_en_Y '(y
  ((((naran blanc azul) (naran azul) (naran azul) (naran amar azul)) ((blanc naran) (naran) (naran) (naran amar)) ((blanc naran) (naran) (naran) (naran amar)) ((blanc naran verd) (naran verd) (naran verd) (naran amar verd)))
   (((blanc azul) (azul) (azul) (amar azul)) ((blanc) (X) (X) (amar)) ((blanc) (X) (X) (amar)) ((blanc verd) (verd) (verd) (amar verd)))
   (((blanc azul) (azul) (azul) (amar azul)) ((blanc) (X) (X) (amar)) ((blanc) (X) (X) (amar)) ((blanc verd) (verd) (verd) (amar verd)))
   (((blanc rojo azul) (rojo azul) (rojo azul) (amar rojo azul)) ((blanc rojo) (rojo) (rojo) (amar rojo)) ((blanc rojo) (rojo) (rojo) (amar rojo)) ((blanc rojo verd) (verd rojo) (rojo verd) (amar rojo verd))))))
