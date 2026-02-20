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

;;
;; OBJECT.rkt
;;
;; Data types for representing general objects,
;; and functions to convert them to meshes
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



(@htdd VertexBuffer)
;; VertexBuffer is one of:
;;  - empty
;;  - (cons Vector VertexBuffer)
;; interp. a list of unique vertices of a mesh, similar to a VBO in OpenGL
;; CONSTRAINT: No duplicate vertices should be present in the list
(define VBUF0 empty)
(define VBUF1 (list (make-vector 2 0 0)
                    (make-vector -1 2 0)
                    (make-vector -1 -1 (/ (sqrt 13) 2))
                    (make-vector -1 -1 (/ (sqrt 13) -2)))) ;tetrahedron example

(@dd-template-rules one-of          ;2 cases
                    atomic-distinct ;empty
                    compound        ;(cons Vector VertexBuffer)
                    ref             ;(first VertexBuffer) is Vector
                    self-ref)       ;(rest VertexBuffer) is VertexBuffer

(define (fn-for-vbuf vbuf)
  (cond [(empty? vbuf)
         (...)]
        [else
         (... (fn-for-vector (first vbuf))
              (fn-for-vbuf (rest vbuf)))]))



(@htdd Element)
(define-struct element (v0 v1 v2))
;; Element is (make-element Natural Natural Natural)
;; interp. the indices of the three vertices of a triangular mesh element
;;         in the VertexBuffer to which this element corresponds
;; CONSTRAINT: No two vertices can be the same, and the resulting
;;             triangular element must not be degenerate; v0, v1, v2 must
;;             be less than the length of the corresponding VertexBuffer
(define ELEMENT1 (make-element 0 1 2))
(define ELEMENT2 (make-element 2 1 0))

(@dd-template-rules compound) ;3 fields

(define (fn-for-element e)
  (... (element-v0 e)
       (element-v1 e)
       (element-v2 e)))



(@htdd ElementBuffer)
;; ElementBuffer is one of:
;;  - empty
;;  - (cons Element ElementBuffer)
;; interp. a list of VertexBuffer indices of vertices of triangular elements
;;         constituting a mesh, similar to an EBO in OpenGL
;; N.B. The purpose of this type is to reduce the number of (per-frame) matrix
;;      transformations required, unlike in OpenGL where EBOs are primarily
;;      used for optimizing memory consumption (which is not a focus here).
(define EBUF0 empty)
(define EBUF1 (list (make-element 0 1 2)
                    (make-element 0 2 3)
                    (make-element 0 3 1)
                    (make-element 1 3 2)))

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
(define-struct mesh (vertices elements count))
;; Mesh is (make-mesh VertexBuffer ElementBuffer Natural)
;; interp. the unique vertices and triangular elements of a mesh,
;;         and its vertex count
;; CONSTRAINT: count must be equal to (length vertices)
(define MESH0 (make-mesh empty empty 0))
(define MESH1 (make-mesh VBUF1 EBUF1 (length VBUF1)))

(@dd-template-rules compound ;3 fields
                    ref      ;(mesh-vertices Mesh) is VertexBuffer
                    ref)     ;(mesh-elements Mesh) is ElementBuffer

(define (fn-for-mesh m)
  (... (fn-for-vbuf (mesh-vertices m))
       (fn-for-ebuf (mesh-elements m))
       (mesh-count m)))



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



(@htdd Edge)
(define-struct edge (v0 v1))
;; Edge is (make-edge Natural Natural)
;; interp. the indices, in ICOSAHEDRON-VERTICES, of the two vertices defining
;;         the edge of an icosahedron
(define EDGE1 (make-edge 0 1))  ;valid edge
(define EDGE2 (make-edge 10 4)) ;another valid edge
(define EDGE3 (make-edge 10 7)) ;"invalid" edge

(@dd-template-rules compound) ;2 fields

(define (fn-for-edge e)
  (... (edge-v0 e)   ;Natural
       (edge-v1 e))) ;Natural


;;
;; CONSTANTS
;;


(define ICOSAHEDRON-VERTICES
  (list (make-vector (/ 2 (sqrt 5)) (/ 1 (sqrt 5)) 0)
        (make-vector (/ (+ 5 (sqrt 5)) 10) (/ -1 (sqrt 5))
                     (sqrt (/ (- 5 (sqrt 5)) 10)))
        (make-vector (/ (- 5 (sqrt 5)) 10) (/ 1 (sqrt 5))
                     (sqrt (/ (+ 5 (sqrt 5)) 10)))
        (make-vector (/ (- (sqrt 5) 5) 10) (/ -1 (sqrt 5))
                     (sqrt (/ (+ 5 (sqrt 5)) 10)))
        (make-vector (/ (- -5 (sqrt 5)) 10) (/ 1 (sqrt 5))
                     (sqrt (/ (- 5 (sqrt 5)) 10)))
        (make-vector (/ -2 (sqrt 5)) (/ -1 (sqrt 5)) 0)
        (make-vector (/ (- -5 (sqrt 5)) 10) (/ 1 (sqrt 5))
                     (- (sqrt (/ (- 5 (sqrt 5)) 10))))
        (make-vector (/ (- (sqrt 5) 5) 10) (/ -1 (sqrt 5))
                     (- (sqrt (/ (+ 5 (sqrt 5)) 10))))
        (make-vector (/ (- 5 (sqrt 5)) 10) (/ 1 (sqrt 5))
                     (- (sqrt (/ (+ 5 (sqrt 5)) 10))))
        (make-vector (/ (+ 5 (sqrt 5)) 10) (/ -1 (sqrt 5))
                     (- (sqrt (/ (- 5 (sqrt 5)) 10))))
        (make-vector 0 1 0)
        (make-vector 0 -1 0)))

(define ICOSAHDRON-ELEMENTS
  (list (make-element 8 0 9) (make-element 9 1 0)
        (make-element 0 2 1) (make-element 1 3 2)
        (make-element 2 4 3) (make-element 3 5 4)
        (make-element 4 6 5) (make-element 5 7 6)
        (make-element 6 8 7) (make-element 7 9 8)
        (make-element 0 10 2) (make-element 2 10 4) (make-element 4 10 6)
        (make-element 6 10 8) (make-element 8 10 0)
        (make-element 1 11 3) (make-element 3 11 5) (make-element 5 11 7)
        (make-element 7 11 9) (make-element 9 11 1)))

(define ICOSAHEDRON-EDGES
  (list (make-edge 0 8) (make-edge 0 9) (make-edge 1 9) (make-edge 1 0)
        (make-edge 2 0) (make-edge 2 1) (make-edge 3 1) (make-edge 3 2)
        (make-edge 4 2) (make-edge 4 3) (make-edge 5 3) (make-edge 5 4)
        (make-edge 6 4) (make-edge 6 5) (make-edge 7 5) (make-edge 7 6)
        (make-edge 8 6) (make-edge 8 7) (make-edge 9 7) (make-edge 9 8)
        (make-edge 10 0) (make-edge 10 2) (make-edge 10 4)
        (make-edge 10 6) (make-edge 10 8)
        (make-edge 11 1) (make-edge 11 3) (make-edge 11 5)
        (make-edge 11 7) (make-edge 11 9)))


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



(@htdf subdivision-vertices)
(@signature Natural -> (listof Vector))
;; produces vertices of n-frequency subdivision of an icosahedron
;; CONSTRAINT: n must be nonzero
#;(check-within (subdivision-vertices 1) ICOSAHEDRON-VERTICES DELTA)
;!!! more tests

(define (subdivision-vertices n) empty) ;stub



(@htdf all-edge-vertices all-edge-vertices--acc)
(@signature Natural -> (listof Vector))
;; produce all edge vertices of n-frequency subdivision of an icosahedron
;; CONSTRAINT: n must be nonzero
;!!! tests

(@template-origin accumulator)

(define (all-edge-vertices n)
  (all-edge-vertices--acc n ICOSAHEDRON-EDGES empty))

(@template-origin (listof Edge) accumulator)

;; edges is (listof Edge)
;; INVARIANT: the list of edges for which vertices have not yet been generated
;;
;; rsf is (listof Vector)
;; INVARIANT: the list of all vertices generated so far
(define (all-edge-vertices--acc n edges rsf)
  (cond [(empty? edges)
         rsf]
        [else
         (all-edge-vertices--acc
          n (rest edges)
          (append (edge-vertices n (first edges)) rsf))]))



(@htdf edge-vertices)
(@signature Natural Edge -> (listof Vector))
;; produce all nonterminal vertices from subdividing given edge
;; CONSTRAINT: n must be nonzero
;!!! tests

(@template-origin Edge)

(define (edge-vertices n e)
  (vertex-combinations n (get-vertex (edge-v0 e)) (get-vertex (edge-v1 e))))



(@htdf vertex-combinations vertex-combinations--acc)
(@signature Natural Vector Vector -> (listof Vector))
;; produce n-1 equispaced linear combinations of the given vertex vectors
;; CONSTRAINT: n must be nonzero
;!!! tests

(@template-origin accumulator)

(define (vertex-combinations n v0 v1)
  (vertex-combinations--acc n v0 v1 (sub1 n) empty))

(@template-origin Natural accumulator)

;; next is Natural
;; INVARIANT: the coefficient of the first vector in the next linear combination
;;            to be generated (given by (v0 * next + v1 * (n - next)) / n)
;;
;; rsf is (listof Vertex)
;; INVARIANT: the list of all vertices generated so far
(define (vertex-combinations--acc n v0 v1 next rsf)
  (cond [(zero? next)
         rsf]
        [else
         (vertex-combinations--acc
          n v0 v1 (sub1 next)
          (cons (scalar-divide (add (scalar-multiply v1 (- n next))
                                    (scalar-multiply v0 next)) n) rsf))]))



(@htdf get-vertex)
(@signature Natural -> Vertex)
;; produce the vertex of the regular icosahedron with given index
(check-within (get-vertex 0)  (make-vector (/ 2 (sqrt 5)) (/ 1 (sqrt 5)) 0)
              DELTA)
(check-expect (get-vertex 10) (make-vector 0 1 0))

(@template-origin Natural)

(define (get-vertex index)
  (list-ref ICOSAHEDRON-VERTICES index))
