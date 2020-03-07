#lang racket
(require pict3d
         pict3d/universe)

; Esta funcion crea un cubito y pinta sus caras
; Nota: Al definir los vertices de la cara se debe hacer en sentido contrario a las agujas del reloj
;       Para evitar traslapar se tiene que sumar un poquito, 0.001 esta bien.
;       Todos los vertex deben tener color.
(define (createcube x y z size) (
                                 combine
                                         ;Cubo
                                         (cube (pos x y z) size)
                                         ;Cara1
                                         (quad
                                          (vertex (pos (- (- x 0.001) size) (- y size) (- z size) ) #:color (rgba "red") )
                                          (vertex (pos (- (- x 0.001) size) (- y size) (+ z size) ) #:color (rgba "red") )
                                          (vertex (pos (- (- x 0.001) size) (+ y size) (+ z size) ) #:color (rgba "red") )
                                          (vertex (pos (- (- x 0.001) size) (+ y size) (- z size) ) #:color (rgba "red") )
                                          )
                                         ;Cara2
                                         (quad
                                          (vertex (pos (+ x size) (- (- y 0.001) size) (- z size) ) #:color (rgba "yellow") )
                                          (vertex (pos (+ x size) (- (- y 0.001) size) (+ z size) ) #:color (rgba "yellow") )
                                          (vertex (pos (- x size) (- (- y 0.001) size) (+ z size) ) #:color (rgba "yellow") )
                                          (vertex (pos (- x size) (- (- y 0.001) size) (- z size) ) #:color (rgba "yellow") )
                                          )
                                         ;Cara3
                                         (quad
                                          (vertex (pos (+ x size) (- y size) (+ (+ z 0.001) size) ) #:color (rgba "blue") )
                                          (vertex (pos (+ x size) (+ y size) (+ (+ z 0.001) size) ) #:color (rgba "blue") )
                                          (vertex (pos (- x size) (+ y size) (+ (+ z 0.001) size) ) #:color (rgba "blue") )
                                          (vertex (pos (- x size) (- y size) (+ (+ z 0.001) size) ) #:color (rgba "blue") )
                                          )
                                         ;Cara4
                                         (quad
                                          (vertex (pos (+ x size) (- y size) (- (- z 0.001) size) ) #:color (rgba "green") )
                                          (vertex (pos (+ x size) (+ y size) (- (- z 0.001) size) ) #:color (rgba "green") )
                                          (vertex (pos (- x size) (+ y size) (- (- z 0.001) size) ) #:color (rgba "green") )
                                          (vertex (pos (- x size) (- y size) (- (- z 0.001) size) ) #:color (rgba "green") )
                                          #:back? #t
                                          )
                                         ;Cara5
                                         (quad
                                          (vertex (pos (+ x size) (+ (+ y 0.001) size) (- z size) ) #:color (rgba "orange") )
                                          (vertex (pos (+ x size) (+ (+ y 0.001) size) (+ z size) ) #:color (rgba "orange") )
                                          (vertex (pos (- x size) (+ (+ y 0.001) size) (+ z size) ) #:color (rgba "orange") )
                                          (vertex (pos (- x size) (+ (+ y 0.001) size) (- z size) ) #:color (rgba "orange") )
                                          #:back? #t
                                          )
                                         ;Cara6
                                         (quad
                                          (vertex (pos (+ (+ x 0.001) size) (- y size) (- z size) ) #:color (rgba "white") )
                                          (vertex (pos (+ (+ x 0.001) size) (- y size) (+ z size) ) #:color (rgba "white") )
                                          (vertex (pos (+ (+ x 0.001) size) (+ y size) (+ z size) ) #:color (rgba "white") )
                                          (vertex (pos (+ (+ x 0.001) size) (+ y size) (- z size) ) #:color (rgba "white") )
                                          #:back? #t
                                          )
                                 ))

; Funcion que crea el cubo rubik de tamanno nxn
(define (createrubik n) (
                     createrubik-aux n 0 0 0
                     ))
(define (createrubik-aux n x y z) (
                                
                         cond ; Z sobrepasa, entonces finaliza
                               ((equal? z n) empty-pict3d)
                               ; Y sobrepasa, entonces sigue poniendo en z+1, reinicia x ^ y
                               ((equal? y n) (createrubik-aux n 0 0 (+ z 1)))
                               ; X sobrepasa, sigue poniendo en y+1, reinicia x
                               ((equal? x n) (createrubik-aux n 0 (+ y 1) z))
                               ; X en el limite, coloca bloques sumando en X
                               (else (combine (createcube x y z 0.5) (createrubik-aux n (+ x 1) y z) ) )
                              ))

(define camera (basis 'camera (point-at (pos 6 6 6) (pos 0 0 0) )) )

(define (on-draw s n t) (combine camera (createrubik 3) ))

(big-bang3d 0 #:on-draw on-draw)

