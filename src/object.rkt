;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname object) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)
(require 2htdp/image)

(require "provide.rkt")
(provide (all-defined-out))

(require "common.rkt")
(@htdd Colour Point Euler Triangle)

;;
;; OBJECT.rkt
;;
;; Data types for representing general objects
;;


(@htdd Cuboid)
(define-struct cuboid (position rotation x-scale y-scale z-scale colour))
;; Cuboid is (make-cuboid Point Euler Number Number Number Colour)
;; interp. the position, orientation, x, y, z scales and colour of a cuboid
(define CUBOID1 (make-cuboid ORIGIN ;Unit cube
                             (make-euler 0 0 0)
                             1 1 1 "black")) 
(define CUBOID2 (make-cuboid (make-point 1 2 3)
                             (make-euler 0 0 0)
                             2 4 6 "black"))
(define CUBOID3 (make-cuboid (make-point 1 2 1)
                             (make-euler 45 45 45)
                             2 3 4 "black"))
(define CUBOID4 (make-cuboid (make-point -1 -2 -3)
                             (make-euler -50 -30 -15)
                             -5 0 -4 "red"))

(@dd-template-rules compound ;6 fields
                    ref      ;(cuboid-position Cuboid) is Point
                    ref)     ;(cuboid-rotation Cuboid) is Euler

(define (fn-for-cuboid c)
  (... (fn-for-point (cuboid-position c))
       (fn-for-euler (cuboid-rotation c))
       (cuboid-x-scale c)                 ;Number
       (cuboid-y-scale c)                 ;Number
       (cuboid-z-scale c)                 ;Number
       (cuboid-colour c)))                ;Colour



(@htdd Icosphere)
(define-struct icosphere (position rotation x-scale y-scale z-scale colour))
;; Icosphere is (make-icosphere Point Euler Number Number Number Colour)
;; interp. the position, orientation, x, y, z scales and colour of an icosphere
(define ICOSPHERE1 (make-icosphere ORIGIN ;sphere
                                   (make-euler 0 0 0)
                                   1 1 1 "black")) 
(define ICOSPHERE2 (make-icosphere ORIGIN                ;rotated sphere is
                                   (make-euler 23 37 79) ;nearly identical
                                   1 1 1 "black")) 
(define ICOSPHERE3 (make-icosphere (make-point 1 3 5)
                                   (make-euler 100 120 140)
                                   3 4 5 "black"))
(define ICOSPHERE4 (make-icosphere (make-point -1 -3 -5)
                                   (make-euler -100 -120 140)
                                   -3 -4 -5 "red"))

(@dd-template-rules compound ;6 fields
                    ref      ;(icosphere-position Icosphere) is Point
                    ref)     ;(icosphere-rotation Icosphere) is Euler

(define (fn-for-icosphere i)
  (... (fn-for-point (icosphere-position i))
       (fn-for-euler (icosphere-rotation i))
       (icosphere-x-scale i)                 ;Number
       (icosphere-y-scale i)                 ;Number
       (icosphere-z-scale i)                 ;Number
       (icosphere-colour i)))                ;Colour



(@htdd Mesh)
;; Mesh is one of:
;;  - empty
;;  - (cons Triangle Mesh)
;; interp. a mesh composed of triangular faces
(define MESH1 empty)
(define MESH2 (list (make-r3d-triangle (make-point 2 0 0) ;Tetrahedron example
                                       (make-point -1 -1 (/ (sqrt 13) 2))
                                       (make-point -1 -1 (/ (sqrt 13) -2))
                                       "black")
                    (make-r3d-triangle (make-point -1 2 0)
                                       (make-point -1 -1 (/ (sqrt 13) 2))
                                       (make-point -1 -1 (/ (sqrt 13) -2))
                                       "black")
                    (make-r3d-triangle (make-point 2 0 0)
                                       (make-point -1 2 0)
                                       (make-point -1 -1 (/ (sqrt 13) 2))
                                       "black")
                    (make-r3d-triangle (make-point 2 0 0)
                                       (make-point -1 2 0)
                                       (make-point -1 -1 (/ (sqrt 13) -2))
                                       "black")))

(@dd-template-rules one-of          ;2 cases
                    atomic-distinct ;empty
                    compound        ;(cons Triangle Mesh)
                    ref             ;(first Mesh) is Triangle
                    self-ref)       ;(rest Mesh) is Mesh

(define (fn-for-mesh m)
  (cond [(empty? m)
         (...)]
        [else
         (... (fn-for-triangle (first m))
              (fn-for-mesh (rest m)))]))



(@htdd Object)
;; Object is one of:
;;  - Cuboid
;;  - Icosphere
;;  - Mesh
;; interp. the position, orientation and size info of an object
(define OBJECT1 CUBOID1)
(define OBJECT2 ICOSPHERE1)
(define OBJECT3 MESH2)

(@dd-template-rules one-of   ;3 cases
                    ref      ;Cuboid
                    ref      ;Icosphere
                    compound ;Mesh
                    ref)     ;Mesh

(define (fn-for-object o)
  (cond [(cuboid? o)
         (... (fn-for-cuboid o))]
        [(icosphere? o)
         (... (fn-for-icosphere o))]
        [else
         (... (fn-for-mesh o))]))



(@htdd ListOfObject)
;; ListOfObject is one of:
;;  - empty
;;  - (cons Object ListOfObject)
;; interp. a list of all objects to be rendered
(define LOO1 empty)
(define LOO2 (cons OBJECT1 (cons OBJECT2 (cons OBJECT3 empty))))

(@dd-template-rules one-of          ;2 cases
                    atomic-distinct ;empty
                    compound        ;(cons Object ListOfObject)
                    ref             ;(first ListOfObject) is Object
                    self-ref)       ;(rest ListOfObject) is ListOfObject

(define (fn-for-loo loo)
  (cond [(empty? loo)
         (...)]
        [else
         (... (fn-for-object (first loo))
              (fn-for-loo (rest loo)))]))
