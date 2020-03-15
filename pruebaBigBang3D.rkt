#lang racket

(require pict3d)
(require pict3d/universe)


(define (on-draw s n t)(
			combine (cube origin 1) (basis 'camera (point-at (pos 0 0 -2) origin))
			))

(define (on-mouse s n t x y e)(
			       cond ((equal? e "left-down")(writeln x)(writeln y))
			       ))

(big-bang3d 0 #:on-draw on-draw #:on-mouse on-mouse)
