;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname object) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)

(require "provide.rkt")
(provide (all-defined-out))

(require "common.rkt")
(@htdd Colour Vector Euler Triangle)

(require "vector.rkt")
(@htdd Plane Line)

(require "buffers.rkt")
(@htdd VertexBuffer Element ElementBuffer)

(require "geodesic.rkt")
(@htdd Edge)

;;
;; OBJECT.rkt
;;
;; Data types for representing objects, and functions to convert them to meshes
;;


;;
;; DATA DEFINITIONS
;;


(@htdd Cuboid)
(define-struct cuboid (position rotation x-scale y-scale z-scale colour))
;; Cuboid is (make-cuboid Vector Euler Number Number Number Colour)
;; interp. the position, orientation, x, y, z scales and colour of a cuboid
(define CUBOID1 (make-cuboid ORIGIN ;unit cube
                             (make-euler 0 0 0)
                             1 1 1 "black")) 
(define CUBOID2 (make-cuboid (make-vector 1 2 3)
                             (make-euler 0 0 0)
                             2 4 6 "black"))
(define CUBOID3 (make-cuboid (make-vector 1 2 1)
                             (make-euler 45 45 45)
                             2 3 4 "black"))
(define CUBOID4 (make-cuboid (make-vector -1 -2 -3)
                             (make-euler -50 -30 -15)
                             -5 0 -4 "red"))

(@dd-template-rules compound ;6 fields
                    ref      ;(cuboid-position Cuboid) is Vector
                    ref)     ;(cuboid-rotation Cuboid) is Euler

(define (fn-for-cuboid c)
  (... (fn-for-vector (cuboid-position c))
       (fn-for-euler (cuboid-rotation c))
       (cuboid-x-scale c)                  ;Number
       (cuboid-y-scale c)                  ;Number
       (cuboid-z-scale c)                  ;Number
       (cuboid-colour c)))                 ;Colour



(@htdd Icosphere)
(define-struct icosphere
  (position rotation x-scale y-scale z-scale resolution colour))
;; Icosphere is (make-icosphere Vector Euler Number Number Number Number Colour)
;; interp. the position, orientation, x, y, z scales, resolution and colour of
;;         an icosphere; resolution determines the maximum allowable side length
(define ICOSPHERE1 (make-icosphere ORIGIN ;sphere
                                   (make-euler 0 0 0)
                                   1 1 1 0.1 "black")) 
(define ICOSPHERE2 (make-icosphere ORIGIN                ;rotated (ico)sphere
                                   (make-euler 23 37 79) ;is nearly identical
                                   1 1 1 0.1 "black")) 
(define ICOSPHERE3 (make-icosphere (make-vector 1 3 5)
                                   (make-euler 100 120 140)
                                   3 4 5 0.1 "black"))
(define ICOSPHERE4 (make-icosphere (make-vector -1 -3 -5)
                                   (make-euler -100 -120 140)
                                   -3 -4 -5 0.05 "red"))

(@dd-template-rules compound ;7 fields
                    ref      ;(icosphere-position Icosphere) is Vector
                    ref)     ;(icosphere-rotation Icosphere) is Euler

(define (fn-for-icosphere i)
  (... (fn-for-vector (icosphere-position i))
       (fn-for-euler (icosphere-rotation i))
       (icosphere-x-scale i)                 ;Number
       (icosphere-y-scale i)                 ;Number
       (icosphere-z-scale i)                 ;Number
       (icosphere-resolution i)              ;Number
       (icosphere-colour i)))                ;Colour



(@htdd Mesh)
(define-struct mesh (vertices elements))
;; Mesh is (make-mesh VertexBuffer ElementBuffer)
;; interp. the unique vertices and triangular elements of a mesh
(define MESH0 (make-mesh empty empty))
(define MESH1 (make-mesh VBUF1 EBUF1))

(@dd-template-rules compound ;2 fields
                    ref      ;(mesh-vertices Mesh) is VertexBuffer
                    ref)     ;(mesh-elements Mesh) is ElementBuffer

(define (fn-for-mesh m)
  (... (fn-for-vbuf (mesh-vertices m))
       (fn-for-ebuf (mesh-elements m))))



(@htdd Object)
;; Object is one of:
;;  - Cuboid
;;  - Icosphere
;;  - Mesh
;; interp. the position, orientation and size info of an object
(define OBJECT1 CUBOID1)
(define OBJECT2 ICOSPHERE1)
(define OBJECT3 MESH1)

(@dd-template-rules one-of   ;3 cases
                    compound ;Cuboid
                    ref      ;Cuboid
                    compound ;Icosphere
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


;;
;; FUNCTIONS
;;


(@htdf object->mesh)
(@signature Object -> Mesh)
;; produce mesh for rendering given object
;!!! tests

(@template-origin Object)

(define (object->mesh o)
  (cond [(cuboid? o)
         (... (fn-for-cuboid o))]
        [(icosphere? o)
         (... (fn-for-icosphere o))]
        [else o]))



(@htdf icosphere->mesh)
(@signature Icosphere -> Mesh)
;; produce mesh from icosphere
;!!! tests

(define (icosphere->mesh i) MESH0) ;stub
