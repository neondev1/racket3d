;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname object) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)

(require "provide.rkt")
(provide (matching-identifiers-out #rx"^((?!--).)*$" (all-defined-out)))

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



(@htdd Edge)
(define-struct edge (v0 v1))
;; Edge is (make-edge Natural Natural)
;; interp. the indices, in ICOSAHEDRON-VERTICES, of the two vertices defining
;;         an edge of an icosahedron
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

(define ICOSAHEDRON-FACES
  (list (make-element 0 9 8) (make-element 1 0 9)
        (make-element 2 1 0) (make-element 3 2 1)
        (make-element 4 3 2) (make-element 5 4 3)
        (make-element 6 5 4) (make-element 7 6 5)
        (make-element 8 7 6) (make-element 9 8 7)
        (make-element 10 2 0) (make-element 10 4 2) (make-element 10 6 4)
        (make-element 10 8 6) (make-element 10 0 8)
        (make-element 11 3 1) (make-element 11 5 3) (make-element 11 7 5)
        (make-element 11 9 7) (make-element 11 1 9)))

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
(@signature Natural -> VertexBuffer)
;; produce vertices of n-frequency subdivision of an icosahedron
;; CONSTRAINT: n must be nonzero
(check-within (subdivision-vertices 1) ICOSAHEDRON-VERTICES DELTA)
;!!! more tests

(@template-origin Natural)

(define (subdivision-vertices n)
  (append ICOSAHEDRON-VERTICES (all-edge-vertices n) (all-internal-vertices n)))



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
;; INVARIANT: the list of edges for which vertices remain to be generated
;;
;; rsf is (listof Vector)
;; INVARIANT: the list of all vertices generated so far
(define (all-edge-vertices--acc n edges rsf)
  (cond [(empty? edges)
         (reverse rsf)]
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



(@htdf all-internal-vertices all-internal-vertices--acc)
(@signature Natural -> (listof Vector))
;; produce all internal vertices of n-frequency subdivision of an icosahedron
;; CONSTRAINT: n must be nonzero
;!!! tests

(@template-origin accumulator)

(define (all-internal-vertices n)
  (all-internal-vertices--acc n ICOSAHEDRON-FACES empty))

(@template-origin (listof Element) accumulator)

;; faces is (listof Element)
;; INVARIANT: the list of faces for which vertices remain to be generated
;;
;; rsf is (listof Vector)
;; INVARIANT: the list of all vertices generated so far
(define (all-internal-vertices--acc n faces rsf)
  (cond [(empty? faces)
         (reverse rsf)]
        [else
         (all-internal-vertices--acc
          n (rest faces) (append (internal-vertices n (first faces)) rsf))]))



(@htdf internal-vertices)
(@signature Natural Element -> (listof Vector))
;; produce all non-edge vertices from subdividing given face
;; CONSTRAINT: n must be nonzero
;!!! tests

(@template-origin Element)

(define (internal-vertices n f)
  (edge-combinations n
                     (vertex-combinations n
                                          (get-vertex (element-v2 f))
                                          (get-vertex (element-v0 f)))
                     (vertex-combinations n
                                          (get-vertex (element-v1 f))
                                          (get-vertex (element-v0 f)))))



(@htdf edge-combinations edge-combinations--acc)
(@signature Natural (listof Vector) (listof Vector) -> (listof Vector))
;; produce non-edge vertices as linear combinations of the given edge vertices
;; CONSTRAINT: n must be nonzero; length of both lists must be n-1
;!!! tests

(@template-origin accumulator)

(define (edge-combinations n e0 e1)
  (edge-combinations--acc n e0 e1 (sub1 n) empty))

(@template-origin Natural accumulator)

;; e0 is (listof Vector)
;; INVARIANT: the list of vertices on the first edge for which linear
;;            combinations remain to be generated
;;
;; e1 is (listof Vector)
;; INVARIANT: the list of vertices on the second edge for which linear
;;            combinations remain to be generated
;;
;; count is Natural
;; INVARIANT: equal to both (length e0) and (length e1)
;;
;; rsf is (listof Vector)
;; INVARIANT: the list of all vertices generated so far
(define (edge-combinations--acc n e0 e1 count rsf)
  (cond [(zero? count)
         rsf]
        [else
         (edge-combinations--acc
          n (rest e0) (rest e1) (sub1 count)
          (append (vertex-combinations (- n count) (first e0) (first e1))
                  rsf))]))



(@htdf get-vertex)
(@signature Natural -> Vertex)
;; produce the vertex of the regular icosahedron with given index
(check-within (get-vertex 0)  (make-vector (/ 2 (sqrt 5)) (/ 1 (sqrt 5)) 0)
              DELTA)
(check-expect (get-vertex 10) (make-vector 0 1 0))

(@template-origin Natural)

(define (get-vertex index)
  (list-ref ICOSAHEDRON-VERTICES index))



(@htdf subdivision-elements subdivision-elements--acc)
(@signature Natural -> ElementBuffer)
;; produce the elements corresponding to (subdivision-vertices n)
;; CONSTRAINT: n must be nonzero
(check-expect (subdivision-elements 1) ICOSAHEDRON-FACES)
;!!! more tests

(@template-origin accumulator)

(define (subdivision-elements n)
  (if (= n 1)
      ICOSAHEDRON-FACES
      (subdivision-elements--acc n ICOSAHEDRON-FACES empty)))

(@template-origin (listof Element) accumulator)

;; faces is (listof Element)
;; INVARIANT: the list of faces for which vertices remain to be generated
;;
;; rsf is (listof Vector)
;; INVARIANT: the list of all vertices generated so far
(define (subdivision-elements--acc n faces rsf)
  (cond [(empty? faces)
         rsf]
        [else
         (subdivision-elements--acc
          n (rest faces)
          (append (face->elements n (first faces)) rsf))]))



(@htdf face->elements)
(@signature Natural Element -> (listof Element))
;; produce the elements of the n-frequency subdivision of the given face
;; CONSTRAINT: n must be greater than 1
;!!! tests

(@template-origin Element)

(define (face->elements n f)
  (offsets->elements n (element-v0 f) (element-v1 f) (element-v2 f)
                     (get-edge-offset n (element-v0 f) (element-v1 f))
                     (get-edge-offset n (element-v0 f) (element-v2 f))
                     (get-edge-offset n (element-v1 f) (element-v2 f))
                     (get-internal-offset n
                                          (element-v0 f)
                                          (element-v1 f)
                                          (element-v2 f))))



(@htdf get-edge-offset)
(@signature Natural Natural Natural -> Natural)
;; produce the index of the first nonterminal vertex on the given edge
;; CONSTRAINT: n must be nonzero
;!!! tests

(@template-origin Natural)

(define (get-edge-offset n v0 v1)
  (cond [(= v0 10)
         (+ 20 (* (sub1 n) (+ 20 (/ v1 2))))]
        [(= v0 11)
         (+ 20 (* (sub1 n) (+ 25 (/ (sub1 v1) 2))))]
        [else
         (+ 20 (* (sub1 n) (+ (* 2 v0) (- 2 (modulo (- v0 v1) 10)))))]))



(@htdf get-internal-offset)
(@signature Natural Natural Natural Natural -> Natural)
;; produce the index of the first internal vertex of the given face
;; CONSTRAINT: n must be nonzero
;!!! tests

(@template-origin Natural)

(define (get-internal-offset n v0 v1 v2)
  (cond [(= v0 10)
         (+ 20 (* (sub1 n) 30) (* (triangular (- n 2)) (+ 10 (/ v2 2))))]
        [(= v0 11)
         (+ 20 (* (sub1 n) 30) (* (triangular (- n 2)) (+ 15 (/ (sub1 v2) 2))))]
        [else
         (+ 20 (* (sub1 n) 30) (* (triangular (- n 2)) v0))]))



(@htdf offsets->elements)
(@signature Natural Natural Natural Natural
            Natural Natural Natural Natural -> (listof Element))
;; produce the elements of the subdivision given the index offsets of all edges
;; CONSTRAINT: n must be greater than 1
;!!! tests

(@template-origin Natural)

(define (offsets->elements n v0 v1 v2 e0 e1 e2 int)
  (if (= n 2)
      (list (make-element v0 e0 e1)
            (make-element e0 v1 e2)
            (make-element e1 e2 v2)
            (make-element e0 e1 e2))
      (append (list (make-element v0 e0 e1)
                    (make-element (+ e0 n -2) v1 e2)
                    (make-element (+ e1 n -2) (+ e2 n -2) v2)
                    (make-element e0 e1 int)
                    (make-element (+ e0 n -2) (+ int (triangular (- n 3))) e2)
                    (make-element (+ int (triangular (- n 2)) -1)
                                  (+ e1 n -2) (+ e2 n -2)))
              (edge-elements n v0 v1 v2 e0 e1 e2 int)
              (internal-elements n int))))



(@htdf edge-elements edge-elements--acc)
(@signature Natural Natural Natural Natural
            Natural Natural Natural Natural -> (listof Element))
;; produce the edge elements of the subdivided icosahedron
;; CONSTRAINT: n must be greater than 2
;!!! tests

(@template-origin accumulator)

(define (edge-elements n v0 v1 v2 e0 e1 e2 int)
  (edge-elements--acc n (- n 3) v0 v1 v2 e0 e1 e2 int empty))

(@template-origin Natural accumulator)

;; count is Natural
;; INVARIANT: the number of elements parallel to the face that remain to
;;            be generated (one more than the number of antiparallel ones)
;;
;; rsf is (listof Element)
;; INVARIANT: the list of all elements generated so far
(define (edge-elements--acc n count v0 v1 v2 e0 e1 e2 int rsf)
  (cond [(zero? count)
         (append (list (make-element v0 (add1 v0) int)
                       (make-element v1 int (add1 v1))
                       (make-element (+ int (triangular (- n 3))) v2 (add1 v2)))
                 rsf)]
        [else
         (edge-elements--acc
          n (sub1 count) v0 v1 v2 e0 e1 e2 int
          (append (list (make-element (+ v0 count) (+ v0 count 1)
                                      (+ int (triangular count)))
                        (make-element (+ v1 count)
                                      (+ int (triangular (add1 count)) -1)
                                      (+ v1 count 1))
                        (make-element (+ int (triangular (- n 3)) count)
                                      (+ v2 count) (+ v2 count 1))
                        (make-element (+ v0 count)
                                      (+ int (triangular count) -1)
                                      (+ int (triangular count)))
                        (make-element (+ int (triangular count) -1)
                                      (+ v1 count)
                                      (+ int (triangular (add1 count)) -1))
                        (make-element (+ int (triangular (- n 3)) count -1)
                                      (+ int (triangular (- n 3)) count)
                                      (+ v2 count))) rsf))]))



(@htdf internal-elements internal-elements--acc)
(@signature Natural Natural -> (listof Element))
;; produce the internal elements of the subdivided icosahedron
;; CONSTRAINT: n must be greater than 2
;!!! tests

(@template-origin accumulator)

(define (internal-elements n int)
  (if (= n 3)
      empty
      (internal-elements--acc (- n 4) int empty)))

(@template-origin Natural accumulator)

;; count is Natural
;; INVARIANT: the number of rows of elements parallel to the face that remain to
;;            be generated (one more than the number of antiparallel ones)
;;
;; rsf is (listof Element)
;; INVARIANT: the list of all elements generated so far
(define (internal-elements--acc count int rsf)
  (cond [(zero? count)
         (append (parallel-row 0 int) rsf)]
        [else
         (internal-elements--acc
          (sub1 count) int (append (parallel-row count int)
                                   (antiparallel-row (sub1 count) int) rsf))]))



(@htdf parallel-row parallel-row--acc)
(@signature Natural Natural -> (listof Element))
;; produce the specified row of elements parallel to the icosahedron face
;!!! tests

(@template-origin accumulator)

(define (parallel-row row int)
  (parallel-row--acc row row int empty))

(@template-origin Natural accumulator)

;; count is Natural
;; INVARIANT: the number of elements that remain to be generated
;;
;; rsf is (listof Element)
;; INVARIANT: the list of all elements generated so far
(define (parallel-row--acc row count int rsf)
  (cond [(zero? count)
         (cons (make-element (+ int (triangular row))
                             (+ int (triangular (add1 row)))
                             (+ int (triangular (add1 row)) 1)) rsf)]
        [else
         (parallel-row--acc
          row (sub1 count) int
          (cons (make-element (+ int (triangular row) count)
                              (+ int (triangular (add1 row)) count)
                              (+ int (triangular (add1 row)) count 1)) rsf))]))



(@htdf antiparallel-row antiparallel-row--acc)
(@signature Natural Natural -> (listof Element))
;; produce the specified row of elements antiparallel to the icosahedron face
;!!! tests

(@template-origin accumulator)

(define (antiparallel-row row int)
  (antiparallel-row--acc row row int empty))

(@template-origin Natural accumulator)

;; count is Natural
;; INVARIANT: the number of elements that remain to be generated
;;
;; rsf is (listof Element)
;; INVARIANT: the list of all elements generated so far
(define (antiparallel-row--acc row count int rsf)
  (cond [(zero? count)
         (cons (make-element (+ int (triangular (add1 row)))
                             (+ int (triangular (add1 row)) 1)
                             (+ int (triangular (+ row 2)) 1)) rsf)]
        [else
         (parallel-row--acc
          row (sub1 count) int
          (cons (make-element (+ int (triangular (add1 row)) count)
                              (+ int (triangular (add1 row)) count 1)
                              (+ int (triangular (+ row 2)) count 1)) rsf))]))



(@htdf triangular triangular--acc)
(@signature Natural -> Natural)
;; produce the nth triangular number T_n=0+1+2+...+n
(check-expect (triangular 0)   0)
(check-expect (triangular 1)   1)
(check-expect (triangular 3)   6)
(check-expect (triangular 100) 5050)

(@template-origin accumulator)

(define (triangular n)
  (triangular--acc n 0))

(@template-origin Natural accumulator)

;; n is Natural
;; INVARIANT: the next number to be added
;;
;; rsf is Natural
;; INVARIANT: the sum of all numbers between the current value of n (exclusive)
;;            and the original value of n (inclusive)
(define (triangular--acc n rsf)
  (cond [(zero? n)
         rsf]
        [else
         (triangular--acc (sub1 n) (+ rsf n))]))
