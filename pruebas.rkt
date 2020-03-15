#lang racket
(require pict3d)
(require pict3d/universe)



(define (on-draw s n t)(combine 
			 (cube origin 1)
			 (basis 'camera (point-at (pos 0 0 -5) origin) )
            (light (pos 0 0 -4) #:range +inf.0  )))

(big-bang3d 0 #:on-draw on-draw)
