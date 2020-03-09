#lang racket
(require pict3d
         pict3d/universe)
(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))


; Funcion que pinta cada cubito segun su lista de colores
(define (colorcube x y z size colorlist cara)(
                                         cond ((null? colorlist) empty-pict3d)
                                              ((equal? cara 1) 
                                                                (combine (quad
                                          (vertex (pos (+ x size) (- y size) (+ (+ z 0.001) size) ) #:color (eval (car colorlist) ns) )
                                          (vertex (pos (+ x size) (+ y size) (+ (+ z 0.001) size) ) #:color (eval (car colorlist) ns) )
                                          (vertex (pos (- x size) (+ y size) (+ (+ z 0.001) size) ) #:color (eval (car colorlist) ns))
                                          (vertex (pos (- x size) (- y size) (+ (+ z 0.001) size) ) #:color (eval (car colorlist) ns) )
                                          ) (colorcube x y z size (cdr colorlist) (+ cara 1) ) 
                                                                ))
                                              ((equal? cara 2) 
                                                                (combine (quad
                                          (vertex (pos (+ x size) (- (- y 0.001) size) (- z size) ) #:color (eval (car colorlist) ns) )
                                          (vertex (pos (+ x size) (- (- y 0.001) size) (+ z size) ) #:color (eval (car colorlist) ns) )
                                          (vertex (pos (- x size) (- (- y 0.001) size) (+ z size) ) #:color (eval (car colorlist) ns) )
                                          (vertex (pos (- x size) (- (- y 0.001) size) (- z size) ) #:color (eval (car colorlist) ns) )
                                          ) (colorcube x y z size (cdr colorlist) (+ cara 1) ))  
                                                                )
                                              ((equal? cara 3) (
                                                                combine (quad
                                          (vertex (pos (+ x size) (- y size) (- (- z 0.001) size) ) #:color (eval (car colorlist) ns) )
                                          (vertex (pos (+ x size) (+ y size) (- (- z 0.001) size) ) #:color (eval (car colorlist) ns) )
                                          (vertex (pos (- x size) (+ y size) (- (- z 0.001) size) ) #:color (eval (car colorlist) ns) )
                                          (vertex (pos (- x size) (- y size) (- (- z 0.001) size) ) #:color (eval (car colorlist) ns) )
                                          #:back? #t
                                          ) (colorcube x y z size (cdr colorlist) (+ cara 1) ) 
                                                                ))
                                              ((equal? cara 4) 
                                                                (combine (quad
                                          (vertex (pos (+ x size) (+ (+ y 0.001) size) (- z size) ) #:color (eval (car colorlist) ns) )
                                          (vertex (pos (+ x size) (+ (+ y 0.001) size) (+ z size) ) #:color (eval (car colorlist) ns) )
                                          (vertex (pos (- x size) (+ (+ y 0.001) size) (+ z size) ) #:color (eval (car colorlist) ns) )
                                          (vertex (pos (- x size) (+ (+ y 0.001) size) (- z size) ) #:color (eval (car colorlist) ns) )
                                          #:back? #t) (colorcube x y z size (cdr colorlist) (+ cara 1) )) 
                                                                )
                                              ((equal? cara 5) 
                                                                (combine (quad
                                          (vertex (pos (+ (+ x 0.001) size) (- y size) (- z size) ) #:color (eval (car colorlist) ns) )
                                          (vertex (pos (+ (+ x 0.001) size) (- y size) (+ z size) ) #:color (eval (car colorlist) ns) )
                                          (vertex (pos (+ (+ x 0.001) size) (+ y size) (+ z size) ) #:color (eval (car colorlist) ns) )
                                          (vertex (pos (+ (+ x 0.001) size) (+ y size) (- z size) ) #:color (eval (car colorlist) ns) )
                                          #:back? #t
                                          ) (colorcube x y z size (cdr colorlist) (+ cara 1) ) )
                                                                )
                                              ((equal? cara 6) 
                                                                (combine (quad
                                          (vertex (pos (- (- x 0.001) size) (- y size) (- z size) ) #:color (eval (car colorlist) ns) )
                                          (vertex (pos (- (- x 0.001) size) (- y size) (+ z size) ) #:color (eval (car colorlist) ns) )
                                          (vertex (pos (- (- x 0.001) size) (+ y size) (+ z size) ) #:color (eval (car colorlist) ns) )
                                          (vertex (pos (- (- x 0.001) size) (+ y size) (- z size) ) #:color (eval (car colorlist) ns) )
                                          ) (colorcube x y z size (cdr colorlist) (+ cara 1) ) )
                                                                )
                                         )
  )

(define (createcube x y z size colorlist)(
                                          combine (cube (pos x y z) size) (colorcube x y z size colorlist 1)
                                          ))

; Funcion que crea el cubo rubik de tamanno nxn y centra la camara
(define (createrubik n cubelist) (
                     combine (basis 'camera (point-at (pos (* (/ n 2) 0.5) (* (/ n 2) 0.5) (* n 2)) (pos (* (/ n 2) 0.5) (* (/ n 2) 0.5) 0) ))
                     ( createrubik-aux n 0 0 0 cubelist)
                     ))
(define (createrubik-aux n x y z cubelist) (
                                
                         cond ; Z sobrepasa  entonces finaliza
                               ((equal? z n) empty-pict3d)
                               ; Y sobrepasa  entonces sigue poniendo en z+1  reinicia x ^ y
                               ((equal? y n) (createrubik-aux n 0 0 (+ z 1) cubelist))
                               ; X sobrepasa, sigue poniendo en y+1, reinicia x
                               ((equal? x n) (createrubik-aux n 0 (+ y 1) z cubelist))
                               ; X en el limite, coloca bloques sumando en X
                               (else (combine (createcube x y z 0.5 (car cubelist)) (createrubik-aux n (+ x 1) y z (cdr cubelist)) ) )
                              ))




(define validcube3 '(
                    ;Cubos primera cara
                    ((rgba "green") (rgba "purple") (rgba "purple") (rgba "red") (rgba "yellow") (rgba "purple"))
                    ((rgba "white") (rgba "purple") (rgba "purple") (rgba "purple") (rgba "blue")(rgba "purple"))
                    ((rgba "yellow") (rgba "orange") (rgba "purple") (rgba "purple") (rgba "blue") (rgba "purple"))
                    ((rgba "blue") (rgba "purple") (rgba "purple") (rgba "orange") (rgba "purple") (rgba "purple"))
                    ((rgba "red") (rgba "purple") (rgba "purple") (rgba "purple") (rgba "purple") (rgba "purple"))
                    ((rgba "yellow") (rgba "green") (rgba "purple") (rgba "purple") (rgba "purple") (rgba "purple"))
                    ((rgba "green") (rgba "purple") (rgba "purple") (rgba "red") (rgba "purple") (rgba "white"))
                    ((rgba "white") (rgba "purple") (rgba "purple") (rgba "purple") (rgba "purple") (rgba "green"))
                    ((rgba "red") (rgba "white") (rgba "purple") (rgba "purple") (rgba "purple") (rgba "blue"))
                    ;Cubos del medio
                    ((rgba "purple") (rgba "purple") (rgba "purple") (rgba "purple") (rgba "white") (rgba "red"))
                    ((rgba "purple") (rgba "purple") (rgba "purple") (rgba "purple") (rgba "blue") (rgba "purple"))
                    ((rgba "purple") (rgba "blue") (rgba "purple") (rgba "purple") (rgba "red") (rgba "purple"))
                    ((rgba "purple") (rgba "purple") (rgba "purple") (rgba "white") (rgba "purple") (rgba "purple"))
                    ((rgba "purple") (rgba "purple") (rgba "purple") (rgba "purple") (rgba "purple") (rgba "purple"))
                    ((rgba "purple") (rgba "yellow") (rgba "purple") (rgba "purple") (rgba "purple") (rgba "purple"))
                    ((rgba "purple") (rgba "purple") (rgba "purple") (rgba "orange") (rgba "purple") (rgba "yellow"))
                    ((rgba "purple") (rgba "purple") (rgba "purple") (rgba "purple") (rgba "purple") (rgba "green"))
                    ((rgba "purple") (rgba "orange") (rgba "purple") (rgba "purple") (rgba "purple") (rgba "white"))
                    ; Cubos del fondo
                    ((rgba "purple") (rgba "purple") (rgba "blue") (rgba "orange") (rgba "white") (rgba "purple"))
                    ((rgba "purple") (rgba "purple") (rgba "blue") (rgba "purple") (rgba "yellow") (rgba "purple"))
                    ((rgba "purple") (rgba "yellow") (rgba "blue") (rgba "purple") (rgba "red") (rgba "purple"))
                    ((rgba "purple") (rgba "purple") (rgba "orange") (rgba "green") (rgba "purple") (rgba "purple"))
                    ((rgba "purple") (rgba "purple") (rgba "orange") (rgba "purple") (rgba "purple") (rgba "purple"))
                    ((rgba "purple") (rgba "red") (rgba "yellow") (rgba "purple") (rgba "purple") (rgba "purple"))
                    ((rgba "purple") (rgba "purple") (rgba "orange") (rgba "white") (rgba "purple") (rgba "green"))
                    ((rgba "purple") (rgba "purple") (rgba "green") (rgba "purple") (rgba "purple") (rgba "red"))
                    ((rgba "purple") (rgba "yellow") (rgba "orange") (rgba "purple") (rgba "purple") (rgba "green"))
                    )
  )

(define validcube '(
                    ((rgba "white") (rgba "red") (rgba "yellow") (rgba "blue") (rgba "orange") (rgba "green"))
                    ))

; Funcion que va a dibujar cada frame
(define (on-draw s n t) ( createrubik 3 validcube3))

; Funcion para rotar el cubo
; Entradas: cubo: true si hay que mover todo el cubo, false si no.
;           eje: x y o z, de querer rotar todo el cubo, inf, sup, der, izq, indicando la cara a rotar si se quiere rotar solo una cara
;           dir: true si es hacia la derecha o arriba, false si es hacia izquierda o abajo
#;(define (rotar cubo eje dir) (
            
            )
  )
(on-draw 0 0 0)
;(big-bang3d 0 #:on-draw on-draw)
