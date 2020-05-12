#lang racket
(require pict3d
         pict3d/universe)

(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))

#|
------------------------------------- Logica ----------------------------
|#

                                                       
#|                                                       ___________________
________________________________________________________/Ejecucion del juego\________________________________________________________
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
                (aplic_movs X (cambiar_agrupacion X (identificar 'y (reordenar X (rotar X (cadr (cambiar_agrupacion X Cubo_identificado 'y)) (string->number (substring (car Movs) 1 2)) 90) (string->number (substring (car Movs) 1 2)) (string->number (substring (car Movs) 1 2)) 90)) 'x) (cdr Movs)))   ;(llamada recursiva (cambia agrupacion de caras moviles a eje x(identifica la agrupacion de caras moviles y(reordena colores +y (aplica rotacion)))))
               ((equal? (substring (car Movs) 2 3) "B")   ;Para direccion negativa de rotacion
                (rotar X (cadr (cambiar_agrupacion X Cubo_identificado 'y)) (string->number (substring (car Movs) 1 2)) -90)   ;;Aplica rotacion - en X
                (aplic_movs X (cambiar_agrupacion X (identificar 'y (reordenar X (rotar X (cadr (cambiar_agrupacion X Cubo_identificado 'y)) (string->number (substring (car Movs) 1 2)) -90) (string->number (substring (car Movs) 1 2)) (string->number (substring (car Movs) 1 2)) -90)) 'x) (cdr Movs)))))))   ;(llamada recursiva (cambia agrupacion de caras moviles a eje x(identifica la agrupacion de caras moviles y(reordena colores -y (aplica rotacion)))))



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
    (cons '() (cubo_standard_aux i j (+ k 1) x)))

   ;Esquinas
   ((and (equal? i 1) (equal? j 1) (equal? k 1))
    (cons '((rgba "white") (rgba "green") (rgba "orange")) (cubo_standard_aux i j (+ k 1) x) ))
   ((and (equal? i 1) (equal? j 1) (equal? k x)) 
    (cons '((rgba "yellow") (rgba "green") (rgba "orange")) (cubo_standard_aux i j (+ k 1) x) ))
   ((and (equal? i 1) (equal? j x) (equal? k 1)) 
    (cons '((rgba "white") (rgba "blue") (rgba "orange")) (cubo_standard_aux i j (+ k 1) x) ))
   ((and (equal? i 1) (equal? j x) (equal? k x)) 
    (cons '((rgba "blue") (rgba "yellow") (rgba "orange")) (cubo_standard_aux i j (+ k 1) x) ))
   ((and (equal? i x) (equal? j 1) (equal? k 1)) 
    (cons '((rgba "white") (rgba "green") (rgba "red")) (cubo_standard_aux i j (+ k 1) x) ))
   ((and (equal? i x) (equal? j 1) (equal? k x)) 
    (cons '((rgba "red") (rgba "yellow") (rgba "orange")) (cubo_standard_aux i j (+ k 1) x) ))
   ((and (equal? i x) (equal? j x) (equal? k 1)) 
    (cons '((rgba "white") (rgba "blue") (rgba "red")) (cubo_standard_aux i j (+ k 1) x) ))
   ((and (equal? i x) (equal? j x) (equal? k x)) 
    (cons '((rgba "blue") (rgba "yellow") (rgba "red")) (cubo_standard_aux i j (+ k 1) x) ))

   ;Colores
   ((equal? i 1)
    (
     cond
     ((equal? j 1)
      (cons '((rgba "green") (rgba "orange")) (cubo_standard_aux i j (+ k 1) x) ))
     ((equal? j x)
      (cons '((rgba "blue") (rgba "orange")) (cubo_standard_aux i j (+ k 1) x) ))
     (else (
	    cond
	    ((equal? k 1) (cons '((rgba "white") (rgba "orange")) (cubo_standard_aux i j (+ k 1) x) ))
	    ((equal? k x) (cons '((rgba "yellow") (rgba "orange")) (cubo_standard_aux i j (+ k 1) x) ))
	    (else (cons '((rgba "orange")) (cubo_standard_aux i j (+ k 1) x) ))
	    ))
     ))
   ((equal? i x)
    (
     cond
     ((equal? j 1) (cons '((rgba "green") (rgba "red")) (cubo_standard_aux i j (+ k 1) x) ))
     ((equal? j x) (cons '((rgba "blue") (rgba "red")) (cubo_standard_aux i j (+ k 1) x) ))
     (else (
	    cond
	    ((equal? k 1) (cons '((rgba "white") (rgba "red")) (cubo_standard_aux i j (+ k 1) x) ))
	    ((equal? k x) (cons '((rgba "yellow") (rgba "orange")) (cubo_standard_aux i j (+ k 1) x) ))
	    (else (cons '((rgba "red") ) (cubo_standard_aux i j (+ k 1) x) ))
	    ))
     ))
   (else (
	  cond
	  ((equal? j 1)
	   (
	    cond
	    ((equal? k 1) (cons '((rgba "white") (rgba "green")) (cubo_standard_aux i j (+ k 1) x) ))
	    ((equal? k x) (cons '((rgba "yellow") (rgba "green")) (cubo_standard_aux i j (+ k 1) x) ))
	    (else (cons '( (rgba "green")) (cubo_standard_aux i j (+ k 1) x) ))
	    ))
	  ((equal? j x)
	   (
	    cond
	    ((equal? k 1) (cons '((rgba "white") (rgba "blue")) (cubo_standard_aux i j (+ k 1) x) ))
	    ((equal? k x) (cons '((rgba "blue") (rgba "yellow")) (cubo_standard_aux i j (+ k 1) x) ))
	    (else (cons '((rgba "blue") ) (cubo_standard_aux i j (+ k 1) x) ))
	    ))
	  (else
	    (
	     cond
	     ((equal? k 1) (cons '((rgba "white") ) (cubo_standard_aux i j (+ k 1) x) ))
	     ((equal? k x) (cons '((rgba "yellow") ) (cubo_standard_aux i j (+ k 1) x) )) 
	     )
	    )
	  ))
   )
  )





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


;Function that creates a cube of nxn if there's no cube input
(define (cubo_standard X)
  (cond
    ((> X 0) ( ; Cubo valido
	       cubo_standard_aux 1 1 1 X
	      ))
    (else #f)
    )
  )

(define (RSP X Cubo Movs)
  (cond ((> (round X) 1)
          (cond ((null? Cubo)
                 (list  (cubo_standard (round X)) X (traducirMovs Movs) '() "x" '()  ))   ;Si no ingresa un cubo, genera un cubo NxN estandar
                (else (aplic_movs X (identificar 'x (caras_moviles X (format_x X Cubo 1 1))) Movs))))
        (else #f)))
(RSP 3 '() '("C3A" "F2D"))

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
*Argumentos: tamano cubo, cubo, contador cara, direccion (+90 o -90) '+
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


#|
*Funcion: reordenar (se aplica despues de la rotacion)
*Argumentos: tamano cubo, cubo, numero de cara, contador de cara (regresivo), direccion (+90 o -90)
*Devuelve: cubo resultante de la reorganizacion de colores en la cara rotada
|#
(define (reordenar X Cubo Cara Cont_cara Direccion)
  (cond ((equal? Cont_cara 1)
         (cond ((> Direccion 0)   ;Si la rotacion fue en el eje +y
                (cond ((equal? Cara 1)   ;Si la cara rotada es la primera (de izq a der)
                       (cons (reord_primera_cara+ X 1 '() 1 (car Cubo)) (cdr Cubo)))
                      ((equal? Cara X)   ;Si la cara rotada es la ultima (de izq a der)
                       (reord_ultima_cara+ X 1 '() 1 (car Cubo)))
                      (else (cons (reord_cara_centro X 1 '() 1 (car Cubo)) (cdr Cubo)))))   ;Si la cara rotada corresponde a las del medio
               ((< Direccion 0)   ;Si la rotacion fue en el eje -y
                (cond ((equal? Cara 1)   ;Si la cara rotada es la primera (de izq a der)
                       (cons (reord_primera_cara- X 1 '() 1 (car Cubo)) (cdr Cubo)))   
                      ((equal? Cara X)   ;Si la cara rotada es la ultima (de izq a der)
                       (reord_ultima_cara- X 1 '() 1 (car Cubo)))
                      (else (cons (reord_cara_centro X 1 '() 1 (car Cubo)) (cdr Cubo)))))))   ;No hay distincion de direccion porque no aplica al patron de cambio
        (else (cons (car Cubo) (reordenar X (cdr Cubo) Cara (- Cont_cara 1) Direccion)))))

         
#|
*Funcion: reord_primera_cara+
*Argumentos: tamano cubo, contador de fila, lista con los cubos reorientados, contador de cubo en la fila, cara original del cubo rubik por alterar
*Devuelve: primer cara del cubo con colores reorganizados segun rotacion +y
|#      
(define (reord_primera_cara+ X Cont_fila Nueva_Fila Cubito Cara_orig)
  (cond ((> Cont_fila X)   ;Si la fila excede el tamano, termina
         '())
        ((> Cubito X)   ;Si el cubito excede el tamano, completa esa fila y sigue
         (cons Nueva_Fila (reord_primera_cara+ X (+ Cont_fila 1) '() 1 (cdr Cara_orig))))
        ((and (equal? Cubito 1) (equal? Cont_fila 1))   ;Si esta en la primer esquina
         (reord_primera_cara+ X Cont_fila (append Nueva_Fila (list (reacomodar_3 '(1 3 2) (caar Cara_orig)))) (+ Cubito 1) (cons (cdar Cara_orig) (cdr Cara_orig))))
        ((and (equal? Cubito X) (equal? Cont_fila 1))   ;Si esta en la segunda esquina
         (reord_primera_cara+ X Cont_fila (append Nueva_Fila (list (reacomodar_3 '(3 1 2) (caar Cara_orig)))) (+ Cubito 1) (cons (cdar Cara_orig) (cdr Cara_orig))))
        ((and (equal? Cubito X) (equal? Cont_fila X))   ;Si esta en la tercera esquina
         (reord_primera_cara+ X Cont_fila (append Nueva_Fila (list (reacomodar_3 '(3 2 1) (caar Cara_orig)))) (+ Cubito 1) (cons (cdar Cara_orig) (cdr Cara_orig))))
        ((and (equal? Cubito 1) (equal? Cont_fila X))   ;Si esta en la cuarta esquina
         (reord_primera_cara+ X Cont_fila (append Nueva_Fila (list (reacomodar_3 '(2 3 1) (caar Cara_orig)))) (+ Cubito 1) (cons (cdar Cara_orig) (cdr Cara_orig))))
        ((and (equal? Cubito X) (and (not (equal? Cont_fila 1)) (not (equal? Cont_fila X))))   ;Si esta en la arista de la cara trasera (fondo)
         (reord_primera_cara+ X Cont_fila (append Nueva_Fila (list (reacomodar_2 (caar Cara_orig)))) (+ Cubito 1) (cons (cdar Cara_orig) (cdr Cara_orig))))
        ((and (equal? Cont_fila X) (and (not (equal? Cubito 1)) (not (equal? Cubito X))))   ;Si esta en la arista de la cara base (suelo)
         (reord_primera_cara+ X Cont_fila (append Nueva_Fila (list (reacomodar_2 (caar Cara_orig)))) (+ Cubito 1) (cons (cdar Cara_orig) (cdr Cara_orig))))
        (else (reord_primera_cara+ X Cont_fila (append Nueva_Fila (list (caar Cara_orig))) (+ Cubito 1) (cons (cdar Cara_orig) (cdr Cara_orig))))))   ;Si no corresponde a un cubito que se altere, lo anade a la lista


#|
*Funcion: reord_primer_cara-
*Argumentos: tamano cubo, contador de fila, lista con los cubos reorientados, contador de cubo en la fila, cara original del cubo rubik por alterar
*Devuelve: primer cara del cubo con colores reorganizados segun rotacion -y
|#
(define (reord_primera_cara- X Cont_fila Nueva_Fila Cubito Cara_orig)
  (cond ((> Cont_fila X)   ;Si la fila excede el tamano, termina
         '())
        ((> Cubito X)   ;Si el cubito excede el tamano, completa esa fila y sigue
         (cons Nueva_Fila (reord_primera_cara+ X (+ Cont_fila 1) '() 1 (cdr Cara_orig))))
        ((and (equal? Cubito 1) (equal? Cont_fila 1))   ;Si esta en la primer esquina
         (reord_primera_cara- X Cont_fila (append Nueva_Fila (list (reacomodar_3 '(2 3 1) (caar Cara_orig)))) (+ Cubito 1) (cons (cdar Cara_orig) (cdr Cara_orig))))
        ((and (equal? Cubito X) (equal? Cont_fila 1))   ;Si esta en la segunda esquina
         (reord_primera_cara- X Cont_fila (append Nueva_Fila (list (reacomodar_3 '(1 3 2) (caar Cara_orig)))) (+ Cubito 1) (cons (cdar Cara_orig) (cdr Cara_orig))))
        ((and (equal? Cubito X) (equal? Cont_fila X))   ;Si esta en la tercera esquina
         (reord_primera_cara- X Cont_fila (append Nueva_Fila (list (reacomodar_3 '(3 1 2) (caar Cara_orig)))) (+ Cubito 1) (cons (cdar Cara_orig) (cdr Cara_orig))))
        ((and (equal? Cubito 1) (equal? Cont_fila X))   ;Si esta en la cuarta esquina
         (reord_primera_cara- X Cont_fila (append Nueva_Fila (list (reacomodar_3 '(3 2 1) (caar Cara_orig)))) (+ Cubito 1) (cons (cdar Cara_orig) (cdr Cara_orig))))
        ((and (equal? Cubito X) (and (not (equal? Cont_fila 1)) (not (equal? Cont_fila X))))   ;Si esta en la arista de la cara trasera (fondo)
         (reord_primera_cara- X Cont_fila (append Nueva_Fila (list (reacomodar_2 (caar Cara_orig)))) (+ Cubito 1) (cons (cdar Cara_orig) (cdr Cara_orig))))
        ((and (equal? Cont_fila X) (and (not (equal? Cubito 1)) (not (equal? Cubito X))))   ;Si esta en la arista de la cara base (suelo)
         (reord_primera_cara- X Cont_fila (append Nueva_Fila (list (reacomodar_2 (caar Cara_orig)))) (+ Cubito 1) (cons (cdar Cara_orig) (cdr Cara_orig))))
        (else (reord_primera_cara- X Cont_fila (append Nueva_Fila (list (caar Cara_orig))) (+ Cubito 1) (cons (cdar Cara_orig) (cdr Cara_orig))))))   ;Si no corresponde a un cubito que se altere, lo anade a la lista


#|
*Funcion: reord_ultima_cara+
*Argumentos: tamano cubo, contador de fila, lista con los cubos reorientados, contador de cubo en la fila, cara original del cubo rubik por alterar
*Devuelve: ultima cara del cubo con colores reorganizados segun rotacion +y
|#  
(define (reord_ultima_cara+ X Cont_fila Nueva_Fila Cubito Cara_orig)
  (cond ((> Cont_fila X)   ;Si la fila excede el tamano, termina
         '())
        ((> Cubito X)   ;Si el cubito excede el tamano, completa esa fila y sigue
         (cons Nueva_Fila (reord_ultima_cara+ X (+ Cont_fila 1) '() 1 (cdr Cara_orig))))
        ((and (equal? Cubito 1) (equal? Cont_fila 1))   ;Si esta en la primer esquina
         (reord_ultima_cara+ X Cont_fila (append Nueva_Fila (list (reacomodar_3 '(3 2 1) (caar Cara_orig)))) (+ Cubito 1) (cons (cdar Cara_orig) (cdr Cara_orig))))
        ((and (equal? Cubito X) (equal? Cont_fila 1))   ;Si esta en la segunda esquina
         (reord_ultima_cara+ X Cont_fila (append Nueva_Fila (list (reacomodar_3 '(2 3 1) (caar Cara_orig)))) (+ Cubito 1) (cons (cdar Cara_orig) (cdr Cara_orig))))
        ((and (equal? Cubito X) (equal? Cont_fila X))   ;Si esta en la tercera esquina
         (reord_ultima_cara+ X Cont_fila (append Nueva_Fila (list (reacomodar_3 '(1 3 2) (caar Cara_orig)))) (+ Cubito 1) (cons (cdar Cara_orig) (cdr Cara_orig))))
        ((and (equal? Cubito 1) (equal? Cont_fila X))   ;Si esta en la cuarta esquina
         (reord_ultima_cara+ X Cont_fila (append Nueva_Fila (list (reacomodar_3 '(3 1 2) (caar Cara_orig)))) (+ Cubito 1) (cons (cdar Cara_orig) (cdr Cara_orig))))
        ((and (equal? Cubito 1) (and (not (equal? Cont_fila 1)) (not (equal? Cont_fila X))))   ;Si esta en la arista de la cara delantera (frente)
         (reord_ultima_cara+ X Cont_fila (append Nueva_Fila (list (reacomodar_2 (caar Cara_orig)))) (+ Cubito 1) (cons (cdar Cara_orig) (cdr Cara_orig))))
        ((and (equal? Cont_fila 1) (and (not (equal? Cubito 1)) (not (equal? Cubito X))))   ;Si esta en la arista de la cara superior (arriba)
         (reord_ultima_cara+ X Cont_fila (append Nueva_Fila (list (reacomodar_2 (caar Cara_orig)))) (+ Cubito 1) (cons (cdar Cara_orig) (cdr Cara_orig))))
        (else (reord_ultima_cara+ X Cont_fila (append Nueva_Fila (list (caar Cara_orig))) (+ Cubito 1) (cons (cdar Cara_orig) (cdr Cara_orig))))))   ;Si no corresponde a un cubito que se altere, lo anade a la lista


#|
*Funcion: reord_ultima_cara-
*Argumentos: tamano cubo, contador de fila, lista con los cubos reorientados, contador de cubo en la fila, cara original del cubo rubik por alterar
*Devuelve: ultima cara del cubo con colores reorganizados segun rotacion -y
|# 
(define (reord_ultima_cara- X Cont_fila Nueva_Fila Cubito Cara_orig)
  (cond ((> Cont_fila X)   ;Si la fila excede el tamano, termina
         '())
        ((> Cubito X)   ;Si el cubito excede el tamano, completa esa fila y sigue
         (cons Nueva_Fila (reord_ultima_cara- X (+ Cont_fila 1) '() 1 (cdr Cara_orig))))
        ((and (equal? Cubito 1) (equal? Cont_fila 1))   ;Si esta en la primer esquina
         (reord_ultima_cara- X Cont_fila (append Nueva_Fila (list (reacomodar_3 '(3 1 2) (caar Cara_orig)))) (+ Cubito 1) (cons (cdar Cara_orig) (cdr Cara_orig))))
        ((and (equal? Cubito X) (equal? Cont_fila 1))   ;Si esta en la segunda esquina
         (reord_ultima_cara- X Cont_fila (append Nueva_Fila (list (reacomodar_3 '(3 2 1) (caar Cara_orig)))) (+ Cubito 1) (cons (cdar Cara_orig) (cdr Cara_orig))))
        ((and (equal? Cubito X) (equal? Cont_fila X))   ;Si esta en la tercera esquina
         (reord_ultima_cara- X Cont_fila (append Nueva_Fila (list (reacomodar_3 '(2 3 1) (caar Cara_orig)))) (+ Cubito 1) (cons (cdar Cara_orig) (cdr Cara_orig))))
        ((and (equal? Cubito 1) (equal? Cont_fila X))   ;Si esta en la cuarta esquina
         (reord_ultima_cara- X Cont_fila (append Nueva_Fila (list (reacomodar_3 '(1 3 2) (caar Cara_orig)))) (+ Cubito 1) (cons (cdar Cara_orig) (cdr Cara_orig))))
        ((and (equal? Cubito 1) (and (not (equal? Cont_fila 1)) (not (equal? Cont_fila X))))   ;Si esta en la arista de la cara delantera (frente)
         (reord_ultima_cara- X Cont_fila (append Nueva_Fila (list (reacomodar_2 (caar Cara_orig)))) (+ Cubito 1) (cons (cdar Cara_orig) (cdr Cara_orig))))
        ((and (equal? Cont_fila 1) (and (not (equal? Cubito 1)) (not (equal? Cubito X))))   ;Si esta en la arista de la cara superior (arriba)
         (reord_ultima_cara- X Cont_fila (append Nueva_Fila (list (reacomodar_2 (caar Cara_orig)))) (+ Cubito 1) (cons (cdar Cara_orig) (cdr Cara_orig))))
        (else (reord_ultima_cara- X Cont_fila (append Nueva_Fila (list (caar Cara_orig))) (+ Cubito 1) (cons (cdar Cara_orig) (cdr Cara_orig))))))   ;Si no corresponde a un cubito que se altere, lo anade a la lista


#|
*Funcion: reord_cara_centro
*Argumentos: tamano cubo, contador de fila, lista con los cubos reorientados, contador de cubo en la fila, cara original del cubo rubik por alterar
*Devuelve: cara intermedia pertinente reorganizada (indiferente de direccion)
|#  
(define (reord_cara_centro X Cont_fila Nueva_Fila Cubito Cara_orig)
  (cond ((> Cont_fila X)   ;Si la fila excede el tamano, termina
         '())
        ((> Cubito X)   ;Si el cubito excede el tamano, completa esa fila y sigue
         (cons Nueva_Fila (reord_cara_centro X (+ Cont_fila 1) '() 1 (cdr Cara_orig))))
        ((equal? (length_logic (caar Cara_orig) 0) 2)  ;Si no es un cubo sin color () o un cubo de un solo elemento (2), respectivamente; es decir, uno de dos colores
               (reord_cara_centro X Cont_fila (append Nueva_Fila (list (reacomodar_2 (caar Cara_orig)))) (+ Cubito 1) (cons (cdar Cara_orig) (cdr Cara_orig))))
        (else (reord_cara_centro X Cont_fila (append Nueva_Fila (list (caar Cara_orig))) (+ Cubito 1) (cons (cdar Cara_orig) (cdr Cara_orig))))))   ;Si no corresponde a un cubito que se altere, lo anade a la lista


#|
*Funcion: length
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
Nota: los colores en Cubito son la referencia (123), los elementos en orden (por ejemplo 3 1 2) significan que los elementos '(1 2 3) en Cubito deben reorganizarse como '(3 1 2)
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
								    (vertex (pos (+ lado 1) (+ tapa 1) cubo ) #:color (eval color ns) #:emitted (emitted (cadr color) 1) )
                                                                    (vertex (pos (+ (+ lado 1) size) (+ tapa 1) cubo ) #:color (eval color ns) #:emitted (emitted (cadr color) 1))
                                                                    (vertex (pos (+ (+ lado 1) size) (+ (+ tapa 1) size) cubo ) #:color (eval color ns) #:emitted (emitted (cadr color) 1))
                                                                    (vertex (pos (+ lado 1) (+ (+ tapa 1) size) cubo ) #:color (eval color ns) #:emitted (emitted (cadr color) 1)) 
								    #:back? #t
								    
								  ))
				          ((equal? cara "izquierda")(
								     quad
								    (vertex (pos (+ lado 1) (+ tapa 1) cubo ) #:color (eval color ns) #:emitted (emitted (cadr color) 1))
                                                                    (vertex (pos (+ lado 1) (+ tapa 1) (+ cubo size) ) #:color (eval color ns) #:emitted (emitted (cadr color) 1))
                                                                    (vertex (pos (+ lado 1) (+ (+ tapa 1) size) (+ cubo size) ) #:color (eval color ns) #:emitted (emitted (cadr color) 1))
                                                                    (vertex (pos (+ lado 1) (+ (+ tapa 1) size) cubo ) #:color (eval color ns) #:emitted (emitted (cadr color) 1))
								    )
								     )
					  ((equal? cara "derecha")(
								   quad
								    (vertex (pos (+ (+ lado 1) size) (+ tapa 1) cubo ) #:color (eval color ns) #:emitted (emitted (cadr color) 1))
                                                                    (vertex (pos (+ (+ lado 1) size) (+ tapa 1) (+ cubo size) ) #:color (eval color ns) #:emitted (emitted (cadr color) 1))
                                                                    (vertex (pos (+ (+ lado 1) size) (+ (+ tapa 1) size) (+ cubo size) ) #:color (eval color ns) #:emitted (emitted (cadr color) 1))
                                                                    (vertex (pos (+ (+ lado 1) size) (+ (+ tapa 1) size) cubo ) #:color (eval color ns) #:emitted (emitted (cadr color) 1)) #:back? #t
								    
								   ))
					  ((equal? cara "arriba")(
								  quad
								    (vertex (pos (+ lado 1) (+ tapa 1) cubo ) #:color (eval color ns) #:emitted (emitted (cadr color) 1))
                                                                    (vertex (pos (+ (+ lado 1) size) (+ tapa 1) cubo ) #:color (eval color ns) #:emitted (emitted (cadr color) 1))
                                                                    (vertex (pos (+ (+ lado 1) size) (+ tapa 1) (+ cubo size) ) #:color (eval color ns) #:emitted (emitted (cadr color) 1))
                                                                    (vertex (pos (+ lado 1) (+ tapa 1) (+ cubo size) ) #:color (eval color ns) #:emitted (emitted (cadr color) 1))
								    
								  ))
					  ((equal? cara "atras")(
								 quad
								    (vertex (pos (+ lado 1) (+ tapa 1) (+ cubo size) ) #:color (eval color ns) #:emitted (emitted (cadr color) 1))
                                                                    (vertex (pos (+ (+ lado 1) size) (+ tapa 1)  (+ cubo size) ) #:color (eval color ns) #:emitted (emitted (cadr color) 1))
                                                                    (vertex (pos (+ (+ lado 1) size) (+ (+ tapa 1) size) (+ cubo size) ) #:color (eval color ns) #:emitted (emitted (cadr color) 1))
                                                                    (vertex (pos (+ lado 1) (+ (+ tapa 1) size) (+ cubo size) ) #:color (eval color ns) #:emitted (emitted (cadr color) 1))
								    
								 ))
					  ((equal? cara "abajo")(
								 quad
								    (vertex (pos (+ lado 1) (+ (+ tapa 1) size) cubo ) #:color (eval color ns) #:emitted (emitted (cadr color) 1))
                                                                    (vertex (pos (+ (+ lado 1) size) (+ (+ tapa 1) size) cubo ) #:color (eval color ns) #:emitted (emitted (cadr color) 1))
                                                                    (vertex (pos (+ (+ lado 1) size) (+ (+ tapa 1) size) (+ cubo size) ) #:color (eval color ns) #:emitted (emitted (cadr color) 1))
                                                                    (vertex (pos (+ lado 1) (+ (+ tapa 1) size) (+ cubo size) ) #:color (eval color ns) #:emitted (emitted (cadr color) 1)) #:back? #t
								    
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
								    (vertex (pos cubo (+ lado 1) (+ tapa 1)) #:color (eval color ns) #:emitted (emitted (cadr color) 1))
                                                                    (vertex (pos (+ cubo size) (+ lado 1) (+ tapa 1)) #:color (eval color ns) #:emitted (emitted (cadr color) 1))
                                                                    (vertex (pos (+ cubo size) (+ (+ lado 1) size) (+ tapa 1)) #:color (eval color ns) #:emitted (emitted (cadr color) 1))
                                                                    (vertex (pos cubo  (+ (+ lado 1) size) (+ tapa 1) ) #:color (eval color ns) #:emitted (emitted (cadr color) 1)) #:back? #t
								    
								  ))
				          ((equal? cara "izquierda")(
								     quad
								    (vertex (pos cubo (+ lado 1) (+ tapa 1) ) #:color (eval color ns) #:emitted (emitted (cadr color) 1))
                                                                    (vertex (pos cubo (+ lado 1) (+ (+ tapa 1) size) ) #:color (eval color ns) #:emitted (emitted (cadr color) 1))
                                                                    (vertex (pos cubo (+ (+ lado 1) size) (+ (+ tapa 1) size) ) #:color (eval color ns) #:emitted (emitted (cadr color) 1))
                                                                    (vertex (pos cubo (+ (+ lado 1) size) (+ tapa 1) ) #:color (eval color ns) #:emitted (emitted (cadr color) 1))
								    )
								     )
					  ((equal? cara "derecha")(
								   quad
								    (vertex (pos (+ cubo size) (+ lado 1) (+ tapa 1) ) #:color (eval color ns) #:emitted (emitted (cadr color) 1))
                                                                    (vertex (pos (+ cubo size) (+ lado 1) (+ (+ tapa 1) size) ) #:color (eval color ns) #:emitted (emitted (cadr color) 1))
                                                                    (vertex (pos (+ cubo size) (+ (+ lado 1) size) (+ (+ tapa 1) size) ) #:color (eval color ns) #:emitted (emitted (cadr color) 1))
                                                                    (vertex (pos (+ cubo size) (+ (+ lado 1) size) (+ tapa 1) ) #:color (eval color ns) #:emitted (emitted (cadr color) 1)) #:back? #t
								    
								   ))
					  ((equal? cara "arriba")(
								  quad
								    (vertex (pos cubo (+ lado 1) (+ tapa 1) ) #:color (eval color ns) #:emitted (emitted (cadr color) 1))
                                                                    (vertex (pos (+ cubo size) (+ lado 1) (+ tapa 1) ) #:color (eval color ns) #:emitted (emitted (cadr color) 1))
                                                                    (vertex (pos (+ cubo size) (+ lado 1) (+ (+ tapa 1) size) ) #:color (eval color ns) #:emitted (emitted (cadr color) 1))
                                                                    (vertex (pos cubo (+ lado 1) (+ (+ tapa 1) size) ) #:color (eval color ns) #:emitted (emitted (cadr color) 1))
								    
								  ))
					  ((equal? cara "atras")(
								 quad
								    (vertex (pos cubo (+ lado 1) (+ (+ tapa 1) size) ) #:color (eval color ns) #:emitted (emitted (cadr color) 1))
                                                                    (vertex (pos (+ cubo size) (+ lado 1)  (+ (+ tapa 1) size) ) #:color (eval color ns) #:emitted (emitted (cadr color) 1))
                                                                    (vertex (pos (+ cubo size) (+ (+ lado 1) size) (+ (+ tapa 1) size) ) #:color (eval color ns) #:emitted (emitted (cadr color) 1))
                                                                    (vertex (pos cubo (+ (+ lado 1) size) (+ (+ tapa 1) size) ) #:color (eval color ns) #:emitted (emitted (cadr color) 1))
								    
								 ))
					  ((equal? cara "abajo")(
								 quad
								    (vertex (pos cubo (+ (+ lado 1) size) (+ tapa 1) ) #:color (eval color ns) #:emitted (emitted (cadr color) 1))
                                                                    (vertex (pos (+ cubo size) (+ (+ lado 1) size) (+ tapa 1) ) #:color (eval color ns) #:emitted (emitted (cadr color) 1))
                                                                    (vertex (pos (+ cubo size) (+ (+ lado 1) size) (+ (+ tapa 1) size) ) #:color (eval color ns) #:emitted (emitted (cadr color) 1))
                                                                    (vertex (pos cubo (+ (+ lado 1) size) (+ (+ tapa 1) size) ) #:color (eval color ns) #:emitted (emitted (cadr color) 1)) #:back? #t
								    
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
                                   cond ((equal? (car (cddddr listaRot)) (cadr listaRot) ) (writeln (
						     ;car s
						     rotar
						     ; N del cubo
						     (cadr s)
						     ; Cubo orientado
						     ; orientar antes de iniciar la rotacion
						     (car s)
						     ;Numero de la cara donde se va a hacer el cambio
						     (+ (car listaRot) 2)
						     ;Direccion
						     (* (cadddr listaRot) -1)
						     ))(
											    ; Aqui se rota la lista en direccion de caddddr s
                                               list (
						     ;car s
						     rotar
						     ; N del cubo
						     (cadr s)
						     ; Cubo orientado
						     ; orientar antes de iniciar la rotacion
						     (car s)
						     ;Numero de la cara donde se va a hacer el cambio
						     (+ (car listaRot) 2)
						     ;Direccion
						     (* (cadddr listaRot) -1)
						     ) (cadr s) 
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
						; No hay rotaciones
						cond ((null? (caddr s))(
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
			 cond ((or (< (exact->inexact (/ x (/ 253 n)) ) 1.5) (> (exact->inexact (/ x (/ 253 n)) )  (+ n 1.5)) (< (exact->inexact (/ y (/ 253 n)) ) 1.5) (> (exact->inexact (/ y (/ 253 n)) ) (+ n 1.5))) 
			       (
				list (+ n 1) (+ n 1)
				))
			 ; Se esta haciendo drag dentro del cubo
			 (else (
				list (inexact->exact (truncate (- (exact->inexact (/ x (/ 253 n)) ) 1.5) )) (inexact->exact (truncate (- (exact->inexact (/ y (/ 253 n)) ) 1.5) ))
				))
			 ))


#|
Funcion encargada de monitorear los cambios del mouse
cuando se encuentra un evento tipo left-down, se guarda la posicion x y y del mouse
cuando se encuentra un evento tipo left-up, se compara la posicion guardada anteriormente con la actual 
y se agrega a la lista de rotaciones la rotacion pertinente
|#
(define (on-mouse s n t x y e)
                               (
                                cond ((equal? "left-down" e)(
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
                        "y"
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
