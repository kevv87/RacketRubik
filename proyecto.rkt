#lang racket
(require pict3d
         pict3d/universe)

(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))



; Funcion que pinta cada cubito segun su lista de colores
#;(define (colorcube x y z size colorlist cara)(
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

#;(define (createcube x y z size colorlist)(
                                          combine (cube (pos x y z) size) (colorcube x y z size colorlist 1)
                                          ))

; Funcion que crea el cubo rubik de tamanno nxn y centra la camara
#;(define (createrubik n cubelist) (
                     combine (basis 'camera (point-at (pos (* (/ n 2) 0.5) (* (/ n 2) 0.5) (* n 2)) (pos (* (/ n 2) 0.5) (* (/ n 2) 0.5) 0) ))
                     ( createrubik-aux n 0 0 0 cubelist)
                     ))
#;(define (createrubik-aux n x y z cubelist) (
                                
                         cond ; Z sobrepasa  entonces finaliza
                               ((equal? z n) empty-pict3d)
                               ; Y sobrepasa  entonces sigue poniendo en z+1  reinicia x ^ y
                               ((equal? y n) (createrubik-aux n 0 0 (+ z 1) cubelist))
                               ; X sobrepasa, sigue poniendo en y+1, reinicia x
                               ((equal? x n) (createrubik-aux n 0 (+ y 1) z cubelist))
                               ; X en el limite, coloca bloques sumando en X
                               (else (combine (createcube x y z 0.5 (car cubelist)) (createrubik-aux n (+ x 1) y z (cdr cubelist)) ) )
                              ))


; Funciones que permiten hacer cuadros en vertical
; Tiene que llamarse primero el pintar linea vertical, luego fila y luego cubos

; Lado es izquierda:-1 , centro:0 a n-1, derecha:n-1
; Rotar es una transformada sobre la cual va a rotar una linea vertical en especifico
(define (pintarV listalineas lado rotar n)(
				  cond ((null? listalineas)(empty-pict3d))
				       (else  (
							  combine (pintarFilaV (car listalineas) -1 n) (pintarV (cdr listalineas) (+ lado 1) n)
							  ))
				  ))

; Tapa es arriba: -1, centro:0 a n-1, abajo:n-1
(define (pintarFilaV listaFilas lado tapa n)(
					cond((null? listaFilas)(empty-pict3d))
					    (else (
							      combine (pintarCuboV (car listaFilas) lado tapa 0 n #t) (pintarFilaV (cdr listaFilas) (+ tapa 1) n)
							      )) 
					))

; Frente es true si no se ha pintado el frente, false de lo contrario
; cubo va de 0 a n-1
(define (pintarCuboV listaCubos lado tapa cubo n frente?)(
					   cond ((null? listaCubos)(empty-pict3d))
					        ; Linea izquierda
					        ((equal? lado -1)(
								  ; Tapa de arriba
								  cond ((equal? tapa -1)(
											cond ((equal? (length (car listaCubos)) 3)(
																   cond(frente? (
																		; Coloree frente, izq, arriba
																		; not frente
																		combine 
																		; Frente
																		(quad
																			  (vertex (pos (* x cubo) (* y cubo) (* z cubo) ) #:color (eval (caar listaCubos) ns) )
                                                                                                                                                          (vertex (pos (+ (* x cubo) size) (+ (* y cubo) size) (* z cubo) ) #:color (eval (caar listaCubos) ns) )
                                                                                                                                                          (vertex (pos (- x size) (+ (+ y 0.001) size) (+ z size) ) #:color (eval (car colorlist) ns) )
                                                                                                                                                          (vertex (pos (- x size) (+ (+ y 0.001) size) (- z size) ) #:color (eval (car colorlist) ns) )
																			  #:back? #t)
																		))
																       (else (
																	      ; Coloree atras, izq, arriba
																	      ; not frente
																	      ))
																   ))
											     ((equal? (length (car listaCubos)) 2)(
																   ; Coloree izq, arriba
																   ))									  
											)) 
								  ;Tapa de abajo
								  ((equal? tapa (- n 1) )(
											cond ((equal? (length (car listaCubos)) 3)(
																   cond(frente? (
																		; Coloree frente, izq, abajo
																		; not frente
																		))
																       (else (
																	      ; Coloree atras, izq, abajo
																	      ; not frente
																	      ))
																   ))
											     ((equal? (length (car listaCubos)) 2)(
																   ; Coloree izq, abajo
																   ))									  
											))
								  ; Filas del medio
								  (else (
											cond ((equal? (length (car listaCubos)) 2)(
																   cond(frente? (
																		; Coloree frente, izq
																		; not frente
																		))
																       (else (
																	      ; Coloree atras, izq
																	      ; not frente
																	      ))
																   ))
											     ((equal? (length (car listaCubos)) 1)(
																   ; Coloree izq 
																   ))									  
											))
								  ))
						;Linea derecha
						((equal? lado (- n 1))(
								  ; Tapa de arriba
								  cond ((equal? tapa -1)(
											cond ((equal? (length (car listaCubos)) 3)(
																   cond(frente? (
																		; Coloree frente, der, arriba
																		; not frente
																		))
																       (else (
																	      ; Coloree atras, der, arriba
																	      ; not frente
																	      ))
																   ))
											     ((equal? (length (car listaCubos)) 2)(
																   ; Coloree der, arriba
																   ))									  
											)) 
								  ;Tapa de abajo
								  ((equal? tapa (- n 1) )(
											cond ((equal? (length (car listaCubos)) 3)(
																   cond(frente? (
																		; Coloree frente, der, abajo
																		; not frente
																		))
																       (else (
																	      ; Coloree atras, der, abajo
																	      ; not frente
																	      ))
																   ))
											     ((equal? (length (car listaCubos)) 2)(
																   ; Coloree der, abajo
																   ))									  
											))
								  ; Filas del medio
								  (else (
											cond ((equal? (length (car listaCubos)) 2)(
																   cond(frente? (
																		; Coloree frente, der
																		; not frente
																		))
																       (else (
																	      ; Coloree atras, der
																	      ; not frente
																	      ))
																   ))
											     ((equal? (length (car listaCubos)) 1)(
																   ; Coloree der
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
																		))
																       (else (
																	      ; Coloree atras, arriba
																	      ; not frente
																	      ))
																   ))
											     ((equal? (length (car listaCubos)) 1)(
																   ; Coloree arriba
																   ))									  
											)) 
								  ;Tapa de abajo
								  ((equal? tapa (- n 1) )(
											cond ((equal? (length (car listaCubos)) 2)(
																   cond(frente? (
																		; Coloree frente, abajo
																		; not frente
																		))
																       (else (
																	      ; Coloree atras, abajo
																	      ; not frente
																	      ))
																   ))
											     ((equal? (length (car listaCubos)) 1)(
																   ; Coloree abajo
																   ))									  
											))
								  ; Filas del medio
								  (else (
											cond ((equal? (length (car listaCubos)) 1)(
																   cond(frente? (
																		; Coloree frente
																		; not frente
																		))
																       (else (
																	      ; Coloree atras
																	      ; not frente
																	      ))
																   ))
											     ((equal? (length (car listaCubos)) 0)(
																   ; nada
																   ))									  
											))
								  ))
					   ))

(define validcubeVertical3 '(
			     ; Primera linea vertical
			     (
			     ( ( (rgba "blue") (rgba "white") (rgba "orange") ) ( (rgba "white") (rgba "orange") ) ( (rgba "blue") (rgba "white") (rgba "red") ) ) 
			     ( ( (rgba "yellow") (rgba "blue") )     (rgba "green")     ( (rgba "red") (rgba "blue") ) ) 
			     ( ( (rgba "yellow") (rgba "orange") (rgba "blue") ) ( (rgba "white") (rgba "blue") ) ( (rgba "orange") (rgba "white") (rgba "green") ) )  
			     )
			     ; Segunda linea vertical
			     (
			     ( ( (rgba "orange") (rgba "blue") )  (rgba "red")  ( (rgba "yellow") (rgba "orange")  ) ) 
			     (   (rgba "yellow")     ( )    ( rgba "white" ) ) 
			     ( ( (rgba "green") (rgba "white") )  (rgba "orange")  ( (rgba "white") (rgba "red") ) )  
			     )
			     ; Tercera linea vertical
			     (
			     ( ( (rgba "green") (rgba "orange") (rgba "yellow") ) ( (rgba "red") (rgba "green") ) ( (rgba "yellow") (rgba "blue") (rgba "red") ) ) 
			     ( ( (rgba "yellow") (rgba "red") )     (rgba "blue")     ( (rgba "green") (rgba "yellow") ) ) 
			     ( ( (rgba "yellow") (rgba "green") (rgba "red") ) ( (rgba "green") (rgba "orange") ) ( (rgba "green") (rgba "white") (rgba "red") ) )  
			     )
			     )
  )

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
