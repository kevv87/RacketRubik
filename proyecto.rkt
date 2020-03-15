#lang racket
(require pict3d
         pict3d/universe)

(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))


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

(define (pintarH listalineas lado rotar n size i) (
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
										    (rotate-x/center (pintarH (car s) -1 '(#f) (cadr s) 1 -1) 0 )
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
                                 (rotate-y/center (pintarH (car s) -1 '(#f) (cadr s) 1 -1)

                                                  ; Se dividen los grados que hay que rotar por la cantidad de frames en los que hay que hacerlo y se multiplica
                                                  ; por los frames que se llevan
                                                  (*  (/ (cadddr (caddr s) ) (car (cddddr (caddr s) ))) (cadr (caddr s) ) ) ) ) )
                                                                             ; Vertical
                                                                             ((equal? "y" ( caddr (caddr s)  ) ) (
                                 combine (basis 'camera (affine-compose  (move-y (/ (cadr s) 2)) (move-x (/ (cadr s) 2)) (point-at (pos 0 0 (* -1 (cadr s))) origin) ))
                                 ; Aqui se rota el cubo completo, rotate-x rota a la izquierda y derecha y rotate-y rota hacia arriba y abajo
                                 (rotate-x/center (pintarV (car s)  -1 '(#f) (cadr s) 1 -1)
                                                  ; Se dividen los grados que hay que rotar por la cantidad de frames en los que hay que hacerlo y se multiplica
                                                  ; por los frames que se llevan
                                                  (*  (/ (* -1 (cadddr (caddr s) )) (car (cddddr (caddr s) ))) (cadr (caddr s) ) )
                                                  ) ))
                                                                             
                                                                        ))
                                  ; Hay que rotar una cara
                                  (else (
					 cond ((equal? "y" (caddr (caddr s) )) 
					       (combine (basis 'camera (affine-compose  (move-y (/ (cadr s) 2)) (move-x (/ (cadr s) 2)) (point-at (pos 0 0 (* -1 (cadr s))) origin) ))
							(pintarV (car s) -1 (caddr s) (cadr s) 1 -1)) )
					 ((equal? "x" (caddr (caddr s) )) 
					       (combine (basis 'camera (affine-compose  (move-y (/ (cadr s) 2)) (move-x (/ (cadr s) 2)) (point-at (pos 0 0 (* -1 (cadr s))) origin) ))
							(pintarH (car s) -1 (caddr s) (cadr s) 1 -1)) )
                                   
					)

                                 ))
                          
                              )
  ))


(define (aumentar-frame listaRot s) (
                                   cond ((equal? (car (cddddr listaRot)) (cadr listaRot) ) (
											    ; Aqui se rota la lista en direccion de caddddr s
                                               list (car s) (cadr s) 
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
(big-bang3d
 
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
