#lang racket
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

;Funcion que crea la arista de la primer cara de un cubo
(define (arista_a X Colores Cont_vert)
  (cond ((null? (cdddr Colores))
         (list (list (car Colores) (cadr Colores) (caddr Colores))))
        ((zero? Cont_vert)
         (cons (list (car Colores) (cadr Colores) (caddr Colores)) (arista_a X (cdddr Colores) (+ Cont_vert 1))))
        (else (cons (list (car Colores) (cadr Colores)) (arista_a X (cddr Colores) Cont_vert)))))