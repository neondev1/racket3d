;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname common) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)
(require 2htdp/image)

(require "provide.rkt")
(provide (all-defined-out))

;;
;; COMMON.rkt
;;
;; Basic data types and functions used throughout Racket3D.
;;


;; For use with inexact numbers to account for floating-point errors
(define APPROX (expt 10 -12))


(@htdd Colour)
;; Colour is Color
;; interp. the Color primitive data type provided by 2htdp/image,
;;         but with a Canadian English name.
;;         Used in place of Color in this program. 

(define COLOUR1 "black")
(define COLOUR2 "red")
(define COLOUR3 (make-color 255 0 0))

(@dd-template-rules atomic-non-distinct) ;treat as simple atomic data
;                                        ;like with Color

(define (fn-for-colour c) ;don't need to bother using ref rule
  (... c))



(@htdf make-colour)
(@signature Natural Natural Natural -> Colour)
;; produce colour value with RGB value (r, g, b), used in place of make-color
(check-expect (make-colour 0 0 0) (make-color 0 0 0))
(check-expect (make-colour 76 84 74) (make-color 76 84 74))

;(define (make-colour r g b) "transparent") ;stub

(@template-origin Natural)

(@template
 (define (make-colour r g b)
   (... r g b)))

(define (make-colour r g b)
  (make-color r g b))



(@htdd Point)
(define-struct point (x y z))
;; Point is (make-point Number Number Number)
;; interp. the x, y and z coordinates of a point
(define ORIGIN (make-point 0 0 0))
(define POINT1 (make-point 1 1 1))
(define POINT2 (make-point 0.1 1.2 2.3))
(define POINT3 (make-point -0.1 0 -1))

(@dd-template-rules compound) ;3 fields

(define (fn-for-point p)
  (... (point-x p)   ;Number
       (point-y p)   ;Number
       (point-z p))) ;Number


(@htdd Euler)
(define-struct euler (alpha beta gamma))
;; Euler is (make-euler Number Number Number)
;; interp. the Euler angles, in degrees, representing an orientation
(define EULER1 (make-euler 0 0 0))
(define EULER2 (make-euler 60 90 180))
(define EULER3 (make-euler 12.3 45.6 78.9))
(define EULER4 (make-euler -60 -45.6 0))

(@dd-template-rules compound) ;3 fields

(define (fn-for-euler e)
  (... (euler-alpha e)   ;Number
       (euler-beta  e)   ;Number
       (euler-gamma e))) ;Number



(@htdd Triangle)
(define-struct r3d-triangle (v0 v1 v2))
;; Triangle is (make-r3d-triangle Point Point Point)
;; interp. the three vertices of a triangle
;; CONSTRAINT: Triangle must be non-degenerate
(define TRIANGLE1 (make-r3d-triangle (make-point 0 1 1)
                                     (make-point 1 0 1)
                                     (make-point 0 0 1))) ;triangles for a
(define TRIANGLE2 (make-r3d-triangle (make-point 0 1 1)   ;rectangular mesh
                                     (make-point 1 1 1)
                                     (make-point 1 0 1)))
(define TRIANGLE3 (make-r3d-triangle (make-point -1 1 1)
                                     (make-point 1 1 2)
                                     (make-point 0 0 3)))

(@dd-template-rules compound ;4 fields
                    ref      ;(r3d-triangle-v0 Triangle) is Point
                    ref      ;(r3d-triangle-v1 Triangle) is Point
                    ref)     ;(r3d-triangle-v2 Triangle) is Point

(define (fn-for-triangle t)
  (... (fn-for-point (r3d-triangle-v0 t)) 
       (fn-for-point (r3d-triangle-v1 t))
       (fn-for-point (r3d-triangle-v2 t))))


;;
;; FUNCTIONS
;;


(@htdf take take--acc)
(@signature (listof X) Natural -> (listof X))
;; produce the first n elements of the given list
;; CONSTRAINT: n must be less than or equal to the length of the list
(check-expect (take empty 0) empty)
(check-expect (take (list 0 1 2 3 4) 0) empty)
(check-expect (take (list 0 1 2 3 4) 3) (list 0 1 2))
(check-expect (take (list 0 1 2 3 4) (length (list 0 1 2 3 4)))
              (list 0 1 2 3 4))

;(define (take lst n) empty) ;stub

(@template-origin accumulator)

(@template
 (define (take lst n)
   (... (take--acc (... lst n) (... lst n) (... lst n)))))

(define (take lst n)
  (take--acc lst n empty))

(@template-origin Natural accumulator)

(@template
 (define (take--acc lst n rsf)
   (cond [(zero? n)
          (... lst rsf)]
         [else
          (... lst n rsf
               (take--acc (... lst) (sub1 n) (... rsf)))])))

;; lst is (listof X)
;; INVARIANT: the list of all elements that have not yet been seen
;;
;; rsf is (listof X)
;; INVARIANT: the list of all selected elements so far, in reverse order
(define (take--acc lst n rsf)
  (cond [(zero? n)
         (reverse rsf)]
        [else
         (take--acc (rest lst) (sub1 n) (cons (first lst) rsf))]))



(@htdf drop)
(@signature (listof X) Natural -> (listof X))
;; produce the given list with the first n elements removed
;; CONSTRAINT: n must be less than or equal to the length of the list
(check-expect (drop empty 0) empty)
(check-expect (drop (list 0 1 2 3 4) 0) (list 0 1 2 3 4))
(check-expect (drop (list 0 1 2 3 4) 3) (list 3 4))
(check-expect (drop (list 0 1 2 3 4) (length (list 0 1 2 3 4))) empty)

(@template-origin Natural accumulator)

(@template
 (define (drop lst n)
   (cond [(zero? n)
          (... lst)]
         [else
          (... lst n (drop (... lst) (sub1 n)))])))

;; lst is (listof X)
;; INVARIANT: the list of all remaining elements
(define (drop lst n)
  (cond [(zero? n)
         lst]
        [else
         (drop (rest lst) (sub1 n))]))
