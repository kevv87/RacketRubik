#lang racket
(define lista '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54))

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

;Funciones para arreglo horizontal
;Funcion que crea la arista de la primer cara de un cubo
(define (arista_a Colores Vert)
  (cond ((null? (cdddr Colores))
         (list (list (car Colores) (cadr Colores) (caddr Colores))))
        ((false? Vert)
         (cons (list (car Colores) (cadr Colores) (caddr Colores)) (arista_a (cdddr Colores) #t)))
        (else (cons (list (car Colores) (cadr Colores)) (arista_a (cddr Colores) Vert)))))

;Funcion que crea una linea central de N elementos
(define (centro_cara Colores Borde)
  (cond ((null? (cddr Colores))
         (list (list (car Colores) (cadr Colores))))
        ((false? Borde)
         (cons (list (car Colores) (cadr Colores)) (centro_cara (cddr Colores) #t)))
        (else (cons (list (car Colores)) (centro_cara (cdr Colores) Borde)))))

;Funcion que crea una linea central de 2 elementos
(define (centro_interior X Colores)
  (cond ((equal? X 1)
         '())
        (else (cons (list (car Colores)) (centro_interior (- X 1) (cdr Colores))))))

;Funcion que obtiene N elementos de una lista
(define (get_elementos Cantidad Lista)
  (cond ((equal? Cantidad 0)
         '())
        (else (cons (car Lista) (get_elementos (- Cantidad 1) (cdr Lista))))))

;Funcion que elimina N elementos de una lista
(define (borrar_elementos Cantidad Lista)
  (cond ((equal? Cantidad 0)
         Lista)
        (else (borrar_elementos (- Cantidad 1) (cdr Lista)))))

;Funcion que a partir de una lista de colores crea un cubo (horizontal)
(define (format_x X Colores Fila Columna)
  (cond ((> Columna X)   ;Columna se excede => termina
         '())
        ((> Fila X)   ;Fila se excede => Columna+1
         (format_x X Colores 1 (+ Columna 1)))
        ((or (equal? Fila 1) (equal? Fila X))   ;Fila 1 o N y Columna 1 o N => arista
         (cond ((or (equal? Columna 1) (equal? Columna X))
                (cons (arista_a (get_elementos (+ 6 (* 2 (- X 2))) Colores) #f) (format_x X (borrar_elementos (+ 6 (* 2 (- X 2))) Colores) (+ Fila 1) Columna)))
               ((and (> Columna 1) (< Columna X))   ;Fila 1 o N y 1<Columna<N => anade centro_cara
                (cons (centro_cara (get_elementos (+ 4 (- X 2)) Colores) #f) (format_x X (borrar_elementos (+ 4 (- X 2)) Colores) (+ Fila 1) Columna)))))
        ((and (or (equal? Columna 1) (equal? Columna X)) (and (> Fila 1) (< Fila X)))   ;Columna 1 o N y 1<Fila<X => anade centro_cara
         (cons (centro_cara (get_elementos (+ 4 (- X 2)) Colores) #f) (format_x X (borrar_elementos (+ 4 (- X 2)) Colores) (+ Fila 1) Columna)))
        ((and (and (> Fila 1) (< Fila X)) (and (> Columna 1) (< Columna X)))
         (cons (centro_interior X (get_elementos 2 Colores)) (format_x X (borrar_elementos 2 Colores) (+ Fila 1) Columna)))))