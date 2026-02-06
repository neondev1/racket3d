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
;; Data types for representing general objects, and functions operating on them
;;


;;
;; DATA DEFINITIONS
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
(define-struct icosphere
  (position rotation x-scale y-scale z-scale resolution colour))
;; Icosphere is (make-icosphere Point Euler Number Number Number Number Colour)
;; interp. the position, orientation, x, y, z scales, resolution and colour of
;;         an icosphere; resolution determines the maximum allowable side length
(define ICOSPHERE1 (make-icosphere ORIGIN ;sphere
                                   (make-euler 0 0 0)
                                   1 1 1 0.1 "black")) 
(define ICOSPHERE2 (make-icosphere ORIGIN                ;rotated (ico)sphere
                                   (make-euler 23 37 79) ;is nearly identical
                                   1 1 1 0.1 "black")) 
(define ICOSPHERE3 (make-icosphere (make-point 1 3 5)
                                   (make-euler 100 120 140)
                                   3 4 5 0.1 "black"))
(define ICOSPHERE4 (make-icosphere (make-point -1 -3 -5)
                                   (make-euler -100 -120 140)
                                   -3 -4 -5 0.05 "red"))

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



(@htdd VertexBuffer)
;; VertexBuffer is one of:
;;  - empty
;;  - (cons Point VertexBuffer)
;; interp. a list of unique vertices of a mesh, similar to a VBO in OpenGL
;; CONSTRAINT: No duplicate vertices should be present in the list
(define VBUF0 empty)
(define VBUF1 (list (make-point 2 0 0)
                    (make-point -1 2 0)
                    (make-point -1 -1 (/ (sqrt 13) 2))
                    (make-point -1 -1 (/ (sqrt 13) -2)))) ;tetrahedron example

(@dd-template-rules one-of          ;2 cases
                    atomic-distinct ;empty
                    compound        ;(cons Point VertexBuffer)
                    ref             ;(first VertexBuffer) is Point
                    self-ref)       ;(rest VertexBuffer) is VertexBuffer

(define (fn-for-vbuf vbuf)
  (cond [(empty? vbuf)
         (...)]
        [else
         (... (fn-for-point (first vbuf))
              (fn-for-vbuf (rest vbuf)))]))



(@htdd Element)
(define-struct element (v0 v1 v2 colour))
;; Element is (make-element Natural Natural Natural Colour)
;; interp. the indices of the three vertices of a triangular mesh element
;;         in a VertexBuffer, as well as the element's colour
;; CONSTRAINT: No two vertices can be the same, and the resulting
;;             triangular element must not be degenerate; v0, v1, v2 must
;;             be less than the length of the corresponding VertexBuffer
(define ELEMENT1 (make-element 0 1 2 "green"))
(define ELEMENT2 (make-element 2 1 0 "black"))

(@dd-template-rules compound) ;4 fields

(define (fn-for-element e)
  (... (element-v0 e)
       (element-v1 e)
       (element-v2 e)
       (element-colour e)))



(@htdd ElementBuffer)
;; ElementBuffer is one of:
;;  - empty
;;  - (cons Element ElementBuffer)
;; interp. a list of VertexBuffer indices of vertices of triangular elements
;;         constituting a mesh, similar to an EBO in OpenGL
(define EBUF0 empty)
(define EBUF1 (list (make-element 0 1 2 "blue")
                    (make-element 0 2 3 "blue")
                    (make-element 0 3 1 "blue")
                    (make-element 1 3 2 "blue")))

(@dd-template-rules one-of          ;2 cases
                    atomic-distinct ;empty
                    compound        ;(cons Element ElementBuffer)
                    ref             ;(first ElementBuffer) is Element
                    self-ref)       ;(rest ElementBuffer) is ElementBuffer

(define (fn-for-ebuf ebuf)
  (cond [(empty? ebuf)
         (...)]
        [else
         (... (fn-for-element (first ebuf))
              (fn-for-ebuf (rest ebuf)))]))



(@htdd Mesh)
(define-struct mesh (vertices elements vcount))
;; Mesh is (make-mesh VertexBuffer ElementBuffer)
;; interp. the unique vertices and triangular elements of a mesh,
;;         and its vertex count
;; CONSTRAINT: vcount must be equal to (length vertices)
(define MESH0 (make-mesh empty empty))
(define MESH1 (make-mesh VBUF1 EBUF1))

(@dd-template-rules compound ;2 cases
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


;;
;; FUNCTIONS
;;


(@htdf object->mesh)
(@signature Object -> Mesh)
;; produce mesh for rendering given object

(@template-origin Object)

(@template
 (define (object->mesh o)
   (cond [(cuboid? o)
          (... (fn-for-cuboid o))]
         [(icosphere? o)
          (... (fn-for-icosphere o))]
         [else
          (... (fn-for-mesh o))])))

(define (object->mesh o)
  (cond [(cuboid? o)
         (... (fn-for-cuboid o))]
        [(icosphere? o)
         (... (fn-for-icosphere o))]
        [else o]))
