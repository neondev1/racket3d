;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname common) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)
(require 2htdp/image)

(require "provide.rkt")
(provide (matching-identifiers-out #rx"^((?!--).)*$" (all-defined-out)))

;;
;; COMMON.rkt
;;
;; Basic data types and functions used throughout Racket3D.
;;


;; For use with inexact number tests to account for floating-point imprecision
(define DELTA (expt 10 -12))


(@htdd Colour)
;; Colour is Color
;; interp. the Color primitive data type provided by 2htdp/image,
;;         but with a Canadian English name. Used in place of Color.

(define COLOUR1 "black")
(define COLOUR2 "red")
(define COLOUR3 (make-color 255 0 0))

(@dd-template-rules atomic-non-distinct) ;treat as simple atomic data
;                                        ;like with Color

(define (fn-for-colour c) ;don't need to bother using ref rule
  (... c)) ;Color



(@htdf make-colour)
(@signature Natural Natural Natural -> Colour)
;; produce colour value with RGB value (r, g, b), used in place of make-color
;; CONSTRAINT: r, g, b must all be less than or equal to 255
(check-expect (make-colour 0 0 0) (make-color 0 0 0))
(check-expect (make-colour 76 84 74) (make-color 76 84 74))

(@template-origin Natural)

(define (make-colour r g b)
  (make-color r g b))



(@htdd Vector)
(define-struct vector (x y z))
;; Vector is (make-vector Number Number Number)
;; interp. the x, y and z components of a 3-dimensional vector
(define ZERO-VECTOR (make-vector 0 0 0)) ;zero vector
(define ORIGIN ZERO-VECTOR)
(define VECTOR1 (make-vector 0 0 1))     ;unit vector normal to xy plane
(define VECTOR2 (make-vector 1.3 3.5 5.7))
(define VECTOR3 (make-vector -1.3 -3.5 -5.7))

(@dd-template-rules compound) ;3 fields

(define (fn-for-vector v)
  (... (vector-x v)   ;Number
       (vector-y v)   ;Number
       (vector-z v))) ;Number



(@htdd Euler)
(define-struct euler (alpha beta gamma))
;; Euler is (make-euler Number Number Number)
;; interp. the Euler angles, in degrees, representing an orientation
(define EULER0 (make-euler 0 0 0))
(define EULER1 (make-euler 60 90 180))
(define EULER2 (make-euler 12.3 45.6 78.9))
(define EULER3 (make-euler -60 -45.6 0))

(@dd-template-rules compound) ;3 fields

(define (fn-for-euler e)
  (... (euler-alpha e)   ;Number
       (euler-beta  e)   ;Number
       (euler-gamma e))) ;Number



(@htdd Triangle)
(define-struct poly (v0 v1 v2))
;; Triangle is (make-poly Vector Vector Vector)
;; interp. the position vectors of the three vertices of a triangle
;; CONSTRAINT: triangle must be non-degenerate
(define TRIANGLE1 (make-poly (make-vector 0 1 1)
                             (make-vector 1 0 1)
                             (make-vector 0 0 1))) ;triangles for a
(define TRIANGLE2 (make-poly (make-vector 0 1 1)   ;rectangular mesh
                             (make-vector 1 1 1)
                             (make-vector 1 0 1)))
(define TRIANGLE3 (make-poly (make-vector -1 1 1)
                             (make-vector 1 1 2)
                             (make-vector 0 0 3)))

(@dd-template-rules compound ;4 fields
                    ref      ;(poly-v0 Triangle) is Vector
                    ref      ;(poly-v1 Triangle) is Vector
                    ref)     ;(poly-v2 Triangle) is Vector

(define (fn-for-triangle t)
  (... (fn-for-vector (poly-v0 t))
       (fn-for-vector (poly-v1 t))
       (fn-for-vector (poly-v2 t))))


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

(@template-origin accumulator)

(define (take lst n)
  (take--acc lst n empty))

(@template-origin Natural accumulator)

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

;; lst is (listof X)
;; INVARIANT: the list of all remaining elements
(define (drop lst n)
  (cond [(zero? n)
         lst]
        [else
         (drop (rest lst) (sub1 n))]))
