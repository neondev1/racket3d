;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname racket3d) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)
(require 2htdp/image)
(require 2htdp/universe)

(@htdw Camera)

;;
;; CONSTANTS
;;

(define WIDTH 640)
(define HEIGHT 480)

(define BEZEL-X 6)
(define BEZEL-WIDTH (/ BEZEL-X 2))

;TODO: add constants for some of this
;!!!
(define EMP (empty-scene WIDTH HEIGHT))
(define MTS (overlay/xy (rectangle 630 445 "outline" "black")
                        (- (- BEZEL-WIDTH) 1) -30 EMP))

(define TPS 60)
(define INACTIVE-DELAY (* TPS 20))

(define APPROX (expt 10 -12))

;;
;; BASIC DEFINITIONS
;;

(@htdd Colour)
;; Colour is Color
;; interp. the Color primitive data type provided by 2htdp/image,
;;         except with a Canadian English name.
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

;(define (make-colour r g b) "black") ;stub

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
;; interp. the Euler angles representing an orientation
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
(define-struct r3d-triangle (v0 v1 v2 colour))
;; Triangle is (make-r3d-triangle Point Point Point Colour)
;; interp. the three vertices and fill colour of a triangle
;; CONSTRAINT: Triangle must be non-degenerate
(define TRIANGLE1 (make-r3d-triangle (make-point 0 1 1)
                                     (make-point 1 0 1)
                                     (make-point 0 0 1)
                                     "black"))          ;triangles for a
(define TRIANGLE2 (make-r3d-triangle (make-point 0 1 1) ;rectangular mesh
                                     (make-point 1 1 1)
                                     (make-point 1 0 1)
                                     "black"))
(define TRIANGLE3 (make-r3d-triangle (make-point -1 1 1)
                                     (make-point 1 1 2)
                                     (make-point 0 0 3)
                                     "red"))

(@dd-template-rules compound ;4 fields
                    ref      ;(r3d-triangle-v0 Triangle) is Point
                    ref      ;(r3d-triangle-v1 Triangle) is Point
                    ref)     ;(r3d-triangle-v2 Triangle) is Point

(define (fn-for-triangle t)
  (... (fn-for-point (r3d-triangle-v0 t)) 
       (fn-for-point (r3d-triangle-v1 t))
       (fn-for-point (r3d-triangle-v2 t))
       (r3d-triangle-colour t)))          ;Colour

;;
;; OBJECT DATA DEFINITIONS
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
;; VECTOR DATA DEFINITIONS
;; All definitions here are namespaced with the r3d-
;; prefix to prevent conflicts with the base language
;;

(@htdd Vector)
(define-struct vector (x y z))
;; Vector is (make-vector Number Number Number)
;; interp. the x, y and z components of a 3D vector
(define ZERO-VECTOR (make-vector 0 0 0)) ;zero vector
(define VECTOR1 (make-vector 0 0 1))     ;unit vector normal to xy plane
(define VECTOR2 (make-vector 1.3 3.5 5.7))
(define VECTOR3 (make-vector -1.3 -3.5 -5.7))

(@dd-template-rules compound) ;3 fields

(define (fn-for-vector v)
  (... (vector-x v)   ;Number
       (vector-y v)   ;Number
       (vector-z v))) ;Number


(@htdd Plane)
(define-struct r3d-plane (a b c d))
;; Plane is (make-r3d-plane Number Number Number Number)
;; interp. a plane in Cartesian form, i.e. in the form ax+by+cz=d
;; CONSTRAINT: at least one of a, b, c must be nonzero
(define PLANE-XY (make-r3d-plane 0 0 1 0))         ;z=0, xy plane
(define PLANE1 (make-r3d-plane 1 -2 -3 2))
(define PLANE2 (make-r3d-plane 2 -1 -1 1))
(define PLANE3 (make-r3d-plane -0.5 1.2 5.6 -2.4)) ;negative a and
;                                                  ;d are allowed

(@dd-template-rules compound) ;4 fields

(define (fn-for-plane c)
  (... (r3d-plane-a c)   ;Number
       (r3d-plane-b c)   ;Number
       (r3d-plane-c c)   ;Number
       (r3d-plane-d c))) ;Number


(@htdd Line)
(define-struct r3d-line (position direction))
;; Line is (make-r3d-line Vector Vector)
;; interp. a line in vector parametric form
;; CONSTRAINT: direction vector must be nonzero
(define LINE-X (make-r3d-line ZERO-VECTOR ;x-axis
                              (make-vector 1 0 0)))
(define LINE1 (make-r3d-line VECTOR2 VECTOR3))

(@dd-template-rules compound ;2 fields
                    ref      ;Vector
                    ref)     ;Vector

(define (fn-for-line l)
  (... (fn-for-vector (r3d-line-position l))
       (fn-for-vector (r3d-line-direction l))))

;;
;; VECTOR ARITHMETIC FUNCTIONS
;;

(@htdf add)
(@signature Vector Vector -> Vector)
;; produce sum of two vectors
(check-expect (add (make-vector 0 0 0) (make-vector 0 0 0))
              (make-vector 0 0 0))
(check-expect (add (make-vector 2 3 4) (make-vector 0 0 0))
              (make-vector 2 3 4))
(check-expect (add (make-vector 1 2 3) (make-vector 1.2 2.3 -3.4))
              (make-vector 2.2 4.3 -0.4))

;(define (add v0 v1) ZERO-VECTOR) ;stub

(@template-origin Vector)

(@template
 (define (add v0 v1)
   (... (vector-x v0)
        (vector-y v0)
        (vector-z v0)
        (vector-x v1)
        (vector-y v1)
        (vector-z v1))))

(define (add v0 v1)
  (make-vector (+ (vector-x v0) (vector-x v1))
               (+ (vector-y v0) (vector-y v1))
               (+ (vector-z v0) (vector-z v1))))


(@htdf sub)
(@signature Vector Vector -> Vector)
;; produce difference of two vectors
(check-expect (sub (make-vector 0 0 0) (make-vector 0 0 0))
              (make-vector 0 0 0))
(check-expect (sub (make-vector 0 0 0) (make-vector 2 -4 6))
              (make-vector -2 4 -6))
(check-expect (sub (make-vector 3 4 5) (make-vector 1.2 2.3 3.4))
              (make-vector 1.8 1.7 1.6))

;(define (sub v0 v1) ZERO-VECTOR) ;stub

(@template-origin Vector)

(@template
 (define (sub v0 v1)
   (... (vector-x v0)
        (vector-y v0)
        (vector-z v0)
        (vector-x v1)
        (vector-y v1)
        (vector-z v1))))

(define (sub v0 v1)
  (make-vector (- (vector-x v0) (vector-x v1))
               (- (vector-y v0) (vector-y v1))
               (- (vector-z v0) (vector-z v1))))


(@htdf scalar-multiply)
(@signature Vector Number -> Vector)
;; produce vector multiplied by a scalar
(check-expect (scalar-multiply ZERO-VECTOR 2) ZERO-VECTOR)
(check-expect (scalar-multiply (make-vector 1 2 3) 0) ZERO-VECTOR)
(check-expect (scalar-multiply (make-vector 1.2 3.4 -5.6) -3)
              (make-vector -3.6 -10.2 16.8))

;(define (scalar-multiply v s) ZERO-VECTOR) ;stub

(@template-origin Vector)

(@template
 (define (scalar-multiply v s)
   (... s
        (vector-x v)
        (vector-y v)
        (vector-z v))))

(define (scalar-multiply v s)
  (make-vector (* (vector-x v) s)
               (* (vector-y v) s)
               (* (vector-z v) s)))


(@htdf scalar-divide)
(@signature Vector Number -> Vector)
;; produce vector divided by a scalar
;; CONSTRAINT: scalar must be nonzero
(check-expect (scalar-divide ZERO-VECTOR 2) ZERO-VECTOR)
(check-expect (scalar-divide (make-vector 1.2 -4.5 7.8) 3)
              (make-vector 0.4 -1.5 2.6))

;(define (scalar-divide v s) ZERO-VECTOR) ;stub

(@template-origin Vector)

(@template
 (define (scalar-divide v s)
   (... s
        (vector-x v)
        (vector-y v)
        (vector-z v))))

(define (scalar-divide v s)
  (make-vector (/ (vector-x v) s)
               (/ (vector-y v) s)
               (/ (vector-z v) s)))


(@htdf vector-magnitude)
(@signature Vector -> Number)
;; produce magnitude of given vector
(check-expect (vector-magnitude ZERO-VECTOR) 0)
(check-within (vector-magnitude (make-vector 1 1 1)) (sqrt 3) APPROX)
(check-expect (vector-magnitude (make-vector -2 3 -6)) 7)

;(define (vector-magnitude v) 0) ;stub

(@template-origin Vector)

(@template
 (define (vector-magnitude v)
   (... (vector-x v)
        (vector-y v)
        (vector-z v))))

(define (vector-magnitude v)
  (sqrt (+ (sqr (vector-x v))
           (sqr (vector-y v))
           (sqr (vector-z v)))))


(@htdf vector->point)
(@signature Vector -> Point)
;; produce point given position vector
(check-expect (vector->point ZERO-VECTOR) ORIGIN)
(check-expect (vector->point (make-vector 14.7 25.8 36.9))
              (make-point 14.7 25.8 36.9))

(@template-origin Vector)

(@template
 (define (vector->point p)
   (... (vector-x p)
        (vector-y p)
        (vector-z p))))

(define (vector->point p)
  (make-point (vector-x p)
              (vector-y p)
              (vector-z p)))


(@htdf point->vector)
(@signature Point -> Vector)
;; produce position vector of given point
(check-expect (point->vector ORIGIN) ZERO-VECTOR)
(check-expect (point->vector (make-point 14.7 25.8 36.9))
              (make-vector 14.7 25.8 36.9))

(@template-origin Point)

(@template
 (define (point->vector p)
   (... (point-x p)
        (point-y p)
        (point-z p))))

(define (point->vector p)
  (make-vector (point-x p)
               (point-y p)
               (point-z p)))


(@htdf points->vector)
(@signature Point Point -> Vector)
;; produce vector from first to second point
(check-expect (points->vector (make-point 0 0 0)
                              (make-point 0 0 0))
              (make-vector 0 0 0))
(check-expect (points->vector (make-point 0 0 0)
                              (make-point 1 -2 3))
              (make-vector 1 -2 3))
(check-expect (points->vector (make-point 1.2 -3.4 5.6)
                              (make-point 1.3 5.7 -9.1))
              (make-vector 0.1 9.1 -14.7))

;(define (points->vector p0 p1) ZERO-VECTOR) ;stub

(@template-origin Point)

(@template
 (define (points->vector p0 p1)
   (... (point-x p0)
        (point-y p0)
        (point-z p0)
        (point-x p1)
        (point-y p1)
        (point-z p1))))

(define (points->vector p0 p1)
  (make-vector (- (point-x p1) (point-x p0))
               (- (point-y p1) (point-y p0))
               (- (point-z p1) (point-z p0))))


(@htdf cross-product)
(@signature Vector Vector -> Vector)
;; produce cross product of given vectors
(check-expect (cross-product (make-vector 2 0 0)
                             (make-vector 0 2 0))
              (make-vector 0 0 4))
(check-expect (cross-product (make-vector 0 2 0)
                             (make-vector 2 0 0))
              (make-vector 0 0 -4))

;(define (cross-product v0 v1) ZERO-VECTOR) ;stub

(@template-origin Vector)

(@template
 (define (cross-product v0 v1)
   (... (vector-x v0)
        (vector-y v0)
        (vector-z v0)
        (vector-x v1)
        (vector-y v1)
        (vector-z v1))))

(define (cross-product v0 v1)
  (make-vector (- (* (vector-y v0) (vector-z v1))
                  (* (vector-y v1) (vector-z v0)))
               (- (* (vector-z v0) (vector-x v1))
                  (* (vector-z v1) (vector-x v0)))
               (- (* (vector-x v0) (vector-y v1))
                  (* (vector-x v1) (vector-y v0)))))


(@htdf dot-product)
(@signature Vector Vector -> Number)
;; produce dot product of given vectors
(check-expect (dot-product (make-vector 2 0 0)
                           (make-vector 0 2 2))
              0)
(check-expect (dot-product (make-vector 0 2 0)
                           (make-vector 0 3 0))
              6)
(check-expect (dot-product (make-vector 1.2 3.4 5.6)
                           (make-vector 9.8 -7.6 5.4))
              16.16)

;(define (dot-product v0 v1) 0) ;stub

(@template-origin Vector)

(@template
 (define (dot-product v0 v1)
   (... (vector-x v0)
        (vector-y v0)
        (vector-z v0)
        (vector-x v1)
        (vector-y v1)
        (vector-z v1))))

(define (dot-product v0 v1)
  (+ (* (vector-x v0) (vector-x v1))
     (* (vector-y v0) (vector-y v1))
     (* (vector-z v0) (vector-z v1))))


(@htdf normal)
(@signature Triangle -> Vector)
;; produce a vector normal to given triangle with unspecified magnitude
(check-expect (normal (make-r3d-triangle (make-point 0 0 0)
                                         (make-point 2 0 0)
                                         (make-point 0 2 0)
                                         "black"))
              (make-vector 0 0 4))
(check-expect (normal (make-r3d-triangle (make-point 0 0 0)
                                         (make-point 0 2 0)
                                         (make-point 2 0 0)
                                         "black"))
              (make-vector 0 0 -4))

;(define (normal t) ZERO-VECTOR) ;stub

(@template-origin Triangle)

(@template
 (define (normal t)
   (... (r3d-triangle-v0 t)
        (r3d-triangle-v1 t)
        (r3d-triangle-v2 t)
        (r3d-triangle-colour t))))

(define (normal t)
  (cross-product (points->vector (r3d-triangle-v0 t) (r3d-triangle-v1 t))
                 (points->vector (r3d-triangle-v0 t) (r3d-triangle-v2 t))))


(@htdf vector-angle)
(@signature Vector Vector -> Number)
;; produce angle between two given vectors in radians
;; CONSTRAINT: both vectors must be nonzero
(check-expect (vector-angle (make-vector 1 0 0)
                            (make-vector 1 0 0))
              0)
(check-within (vector-angle (make-vector 1 0 0)
                            (make-vector 0 2 3))
              (/ pi 2) APPROX)
(check-within (vector-angle (make-vector 1 0 0)
                            (make-vector -1 0 0))
              pi APPROX)

;(define (vector-angle v0 v1) 0) ;stub

(@template-origin fn-composition)

(@template
 (define (vector-angle v0 v1)
   (acos (/ (dot-product v0 v1)
            (* (vector-magnitude v0) (vector-magnitude v1))))))

(define (vector-angle v0 v1)
  (acos (/ (dot-product v0 v1)
           (* (vector-magnitude v0) (vector-magnitude v1)))))


(@htdf normal->plane)
(@signature Vector Vector -> Plane)
;; produce Cartesian form of plane given normal and a position vector on plane
;!!! examples

;(define (normal->plane n p) PLANE-XY) ;stub

(@template-origin Vector)

(@template
 (define (normal->plane n p)
   (... (vector-x n)
        (vector-y n)
        (vector-z n)
        (vector-x p)
        (vector-y p)
        (vector-z p))))

(define (normal->plane n p)
  (make-r3d-plane (vector-x n)
                  (vector-y n)
                  (vector-z n)
                  (dot-product n p)))


(@htdf triangle->plane)
(@signature Triangle -> Plane)
;; produce Cartesian form of plane containing triangle
(check-expect (triangle->plane (make-r3d-triangle (make-point 0 0 0)
                                                  (make-point 2 0 0)
                                                  (make-point 0 2 0)
                                                  "black"))
              (make-r3d-plane 0 0 4 0))
;!!! more examples

;(define (triangle->plane t) PLANE-XY) ;stub

(@template-origin fn-composition)

(@template
 (define (triangle->plane t)
   (normal->plane (normal t) (point->vector (r3d-triangle-v0 t)))))

(define (triangle->plane t)
  (normal->plane (normal t) (point->vector (r3d-triangle-v0 t))))


(@htdf plane-parallel?)
(@signature Plane Plane -> Boolean)
;; produce true if given planes are parallel, otherwise false
(check-expect (plane-parallel? (make-r3d-plane 0 2 3 9)
                               (make-r3d-plane -1 4 2 7)) false)
(check-expect (plane-parallel? (make-r3d-plane 1 -2 4 5)
                               (make-r3d-plane 1 -2 4 9)) true)
(check-expect (plane-parallel? (make-r3d-plane 2 3 4 -5)
                               (make-r3d-plane 2 3 4 -5)) true)
(check-expect (plane-parallel? (make-r3d-plane 1 -4 5 7)
                               (make-r3d-plane 1 -4 -5 7)) false)

;(define (plane-parallel? p0 p1) false) ;stub

(@template-origin Plane)

(@template
 (define (plane-parallel? p0 p1)
   (... (r3d-plane-a p0)
        (r3d-plane-b p0)
        (r3d-plane-c p0)
        (r3d-plane-d p0)
        (r3d-plane-a p1)
        (r3d-plane-b p1)
        (r3d-plane-c p1)
        (r3d-plane-d p1))))

(define (plane-parallel? p0 p1)
  (= (/ (r3d-plane-a p0) (r3d-plane-a p1))
     (/ (r3d-plane-b p0) (r3d-plane-b p1))
     (/ (r3d-plane-c p0) (r3d-plane-c p1))))


(@htdf plane-intersect)
(@signature Plane Plane -> Line)
;; produce parametric line of intersection between two planes
;; CONSTRAINT: planes must be nonparallel
(check-expect (plane-intersect PLANE1 PLANE2)
              (make-r3d-line (make-vector 0 -1 0)
                             (make-vector -1/3 -5/3 1)))

;(define (plane-intersect p0 p1) LINE-X) ;stub

(@template-origin fn-composition)

(define (plane-intersect p0 p1)
  (intersect-other p0 p1
                   (intersect-first p0 p1
                                    (denominator-x p0 p1)
                                    (denominator-y p0 p1)
                                    (denominator-z p0 p1))))


;; NOTE: The following functions exist to prevent recomputation;
;;       local is not used as we decided to stick to strict BSL.


(define (denominator-x p0 p1) (- (* (r3d-plane-a p0) (r3d-plane-b p1))
                                 (* (r3d-plane-a p1) (r3d-plane-b p0))))
(define (denominator-y p0 p1) (- (* (r3d-plane-a p0) (r3d-plane-c p1))
                                 (* (r3d-plane-a p1) (r3d-plane-c p0))))
(define (denominator-z p0 p1) (- (* (r3d-plane-c p0) (r3d-plane-b p1))
                                 (* (r3d-plane-c p1) (r3d-plane-b p0))))


(@htdf intersect-first)
(@signature Plane Plane -> Line)
;; produce first component of incomplete line of intersection between two planes
;!!! examples

;(define (intersect-first p0 p1) LINE-X) ;stub

(@template-origin Plane)

(@template
 (define (intersect-first p0 p1)
   (... (r3d-plane-a p0)
        (r3d-plane-b p0)
        (r3d-plane-c p0)
        (r3d-plane-d p0)
        (r3d-plane-a p1)
        (r3d-plane-b p1)
        (r3d-plane-c p1)
        (r3d-plane-d p1))))

(define (intersect-first p0 p1 dx dy dz)
  (cond [(not (zero? dx))
         (make-r3d-line
          (make-vector (/ (- (* (r3d-plane-d p0) (r3d-plane-b p1))
                             (* (r3d-plane-d p1) (r3d-plane-b p0))) dx)
                       0 #i0)
          (make-vector (/ (- (* (r3d-plane-b p0) (r3d-plane-c p1))
                             (* (r3d-plane-b p1) (r3d-plane-c p0))) dx)
                       0 #i1))]
        [(not (zero? dy))
         (make-r3d-line
          (make-vector (/ (- (* (r3d-plane-d p0) (r3d-plane-c p1))
                             (* (r3d-plane-d p1) (r3d-plane-c p0))) dy)
                       #i0 0)
          (make-vector (/ (- (* (r3d-plane-c p0) (r3d-plane-b p1))
                             (* (r3d-plane-c p1) (r3d-plane-b p0))) dy)
                       #i1 0))]
        [else
         (make-r3d-line
          (make-vector #i0 0
                       (/ (- (* (r3d-plane-d p0) (r3d-plane-b p1))
                             (* (r3d-plane-d p1) (r3d-plane-b p0))) dz))
          (make-vector #i1 0
                       (/ (- (* (r3d-plane-b p0) (r3d-plane-a p1))
                             (* (r3d-plane-b p1) (r3d-plane-a p0))) dz)))]))


(@htdf intersect-other)
(@signature Plane Plane Line -> Line)
;; produce other component of line of intersection from component and one plane
;!!! examples

;(define (intersect-other p0 p1 l) LINE-X) ;stub

(@template-origin Line)

(@template
 (define (intersect-other p0 p1 l)
   (... p0 p1
        (fn-for-vector (r3d-line-position l))
        (fn-for-vector (r3d-line-direction l)))))

(define (intersect-other p0 p1 l)
  (make-r3d-line (intersect-position p0 p1 (r3d-line-position l))
                 (intersect-direction p0 p1 (r3d-line-direction l))))


(@htdf intersect-position)
(@signature Plane Plane Vector -> Vector)
;; produce position vector of other component of line of intersection
;!!! examples

;(define (intersect-position p0 p1 pos) ZERO-VECTOR) ;stub

(@template-origin Plane Vector)

(@template
 (define (intersect-position p0 p1 pos)
   (... (r3d-plane-a p0)
        (r3d-plane-b p0)
        (r3d-plane-c p0)
        (r3d-plane-d p0)
        (r3d-plane-a p1)
        (r3d-plane-b p1)
        (r3d-plane-c p1)
        (r3d-plane-d p1)
        (vector-x pos)
        (vector-y pos)
        (vector-z pos))))

(define (intersect-position p0 p1 pos)
  (cond [(inexact? (vector-z pos))
         (if (zero? (r3d-plane-b p1))
             (make-vector (vector-x pos)
                          (/ (- (r3d-plane-d p0)
                                (* (r3d-plane-a p0) (vector-x pos)))
                             (r3d-plane-b p0))
                          0)
             (make-vector (vector-x pos)
                          (/ (- (r3d-plane-d p1)
                                (* (r3d-plane-a p1) (vector-x pos)))
                             (r3d-plane-b p1))
                          0))]
        [(inexact? (vector-y pos))
         (if (zero? (r3d-plane-b p1))
             (make-vector (vector-x pos)
                          0
                          (/ (- (r3d-plane-d p0)
                                (* (r3d-plane-a p0) (vector-x pos)))
                             (r3d-plane-c p0)))
             (make-vector (vector-x pos)
                          0
                          (/ (- (r3d-plane-d p1)
                                (* (r3d-plane-a p1) (vector-x pos)))
                             (r3d-plane-c p1))))]
        [else
         (if (zero? (r3d-plane-b p1))
             (make-vector 0
                          (vector-y pos)
                          (/ (- (r3d-plane-d p0)
                                (* (r3d-plane-b p0) (vector-y pos)))
                             (r3d-plane-c p0)))
             (make-vector 0
                          (vector-y pos)
                          (/ (- (r3d-plane-d p1)
                                (* (r3d-plane-b p1) (vector-y pos)))
                             (r3d-plane-c p1))))]))


(@htdf intersect-direction)
(@signature Plane Vector -> Vector)
;; produce direction vector of other component of line of intersection
;!!! examples

;(define (intersect-direction p dir) dir) ;stub
;                                         ;note that direction vector is nonzero

(@template-origin Plane Vector)

(@template
 (define (intersect-direction p dir)
   (... (r3d-plane-a p0)
        (r3d-plane-b p0)
        (r3d-plane-c p0)
        (r3d-plane-d p0)
        (r3d-plane-a p1)
        (r3d-plane-b p1)
        (r3d-plane-c p1)
        (r3d-plane-d p1)
        (vector-x dir)
        (vector-y dir)
        (vector-z dir))))

(define (intersect-direction p0 p1 dir)
  (cond [(inexact? (vector-z dir))
         (if (zero? (r3d-plane-b p1))
             (make-vector (vector-x dir)
                          (/ (- (- (r3d-plane-c p0))
                                (* (r3d-plane-a p0) (vector-x dir)))
                             (r3d-plane-b p0))
                          1)
             (make-vector (vector-x dir)
                          (/ (- (- (r3d-plane-c p1))
                                (* (r3d-plane-a p1) (vector-x dir)))
                             (r3d-plane-b p1))
                          1))]
        [(inexact? (vector-y dir))
         (if (zero? (r3d-plane-b p1))
             (make-vector (vector-x dir)
                          1
                          (/ (- (- (r3d-plane-b p0))
                                (* (r3d-plane-a p0) (vector-x dir)))
                             (r3d-plane-c p0)))
             (make-vector (vector-x dir)
                          1
                          (/ (- (- (r3d-plane-b p1))
                                (* (r3d-plane-a p1) (vector-x dir)))
                             (r3d-plane-c p1))))]
        [else
         (if (zero? (r3d-plane-b p1))
             (make-vector 1
                          (vector-y dir)
                          (/ (- (- (r3d-plane-a p0))
                                (* (r3d-plane-b p0) (vector-y dir)))
                             (r3d-plane-c p0)))
             (make-vector 1
                          (vector-y dir)
                          (/ (- (- (r3d-plane-a p1))
                                (* (r3d-plane-b p1) (vector-y dir)))
                             (r3d-plane-c p1))))]))

#|
TODO: Subdividing overlapping mesh faces
!!!

BASIC PROCEDURE

For each mesh face added to buffer, perform a comparison with each existing
element as follows:
1. Compute distance between centroids. If distance is greater than or equal to
   the sum of the greatest distances between the centroid and farthest vertex
   in both triangles, skip to next comparison.
2. Compute line of intersection between planes containing both triangles.
3. Check if computed line of intersection intersects both triangles.
   3a. Performing checks on two sides of each triangle is sufficient.
   3b. If either of the triangles have both intersection points very close
       (within the constant APPROX) to a vertex, return false.
4. If previous check returned false, skip to next comparison.
5. Subdivide both triangles along line of intersection.
   5a. For each subdivision:
   5b. Determine which two edges are intersected by the line.
   5c. Create a mesh face from the triangle created by cutting along the line.
   5d. Divide remaining quadrilateral into two triangular mesh faces.
6. The existing mesh face that has been subdivided is reinserted into the list
   without checks. The new mesh faces are inserted with this procedure.
   6a. If possible, skip checks for all polygons that have been checked before.

|#

;;
;; PROJECTION DATA DEFININTIONS
;;

(@htdd Element)
(define-struct element (face centroid))
;; Element is (make-z-element Triangle Point)
;; interp. a triangular mesh face and its centroid, used to construct buffer
(define ELEMENT1 (make-element TRIANGLE1 (make-point 1/3 1/3 1)))
(define ELEMENT2 (make-element TRIANGLE2 (make-point 1/3 1/3 1)))
(define ELEMENT3 (make-element TRIANGLE3 (make-point 0 2/3 1)))

(@dd-template-rules compound ;2 fields
                    ref      ;Triangle
                    ref)     ;Point

(define (fn-for-element e)
  (... (fn-for-triangle (element-face e))
       (fn-for-point (element-centroid e))))


(@htdd ElementBuffer)
;; ElementBuffer is one of:
;;  - empty
;;  - (cons Element ElementBuffer)
;; interp. a buffer, storing depth information of every mesh face to be
;;         rendered; elements are sorted by Euclidean distance from camera
;; CONSTRAINT: no two elements can intersect each other
(define EBUF1 empty)
(define EBUF2 (cons ELEMENT1 (cons ELEMENT2 (cons ELEMENT3 empty))))

(@dd-template-rules one-of          ;2 fields
                    atomic-distinct ;empty
                    compound        ;(cons ZIndex ZBuffer)
                    ref             ;(first ZBuffer) is ZIndex
                    self-ref)       ;(rest ZBuffer) is ZBuffer

(define (fn-for-ebuf ebuf)
  (cond [(empty? ebuf)
         (...)]
        [else
         (... (fn-for-element (first ebuf))
              (fn-for-ebuf (rest ebuf)))]))

;;
;; PROJECTION FUNCTIONS
;;

;;
;; WORLD
;;

(@htdd GUIState)
(define-struct gui-state (file help add selection))
;; GUIState is (make-gui-state Boolean Boolean Boolean Natural)
;; interp. GUI dropdown states for File and Help menus and object creation; 
;;         selection is 1-based index of selected object, 0 if no selection
;; CONSTRAINT: selection must be less than or equal to total object count
(define GUI1 (make-gui-state false false false 0))
(define GUI2 (make-gui-state true  false false 0))
(define GUI3 (make-gui-state false false true  2))

(@dd-template-rules compound) ;4 fields

(define (fn-for-gui-state gs)
  (... (gui-state-file gs)        ;Boolean
       (gui-state-help gs)        ;Boolean
       (gui-state-add gs)         ;Boolean
       (gui-state-selection gs))) ;Natural


(@htdd Camera)
(define-struct camera (gui objects position light time))
;; Camera is (make-camera GUIState ListOfObject Point Point Natural)
;; interp. GUI state, object list, position of camera/light, time (ticks) since
;;         last user interaction. The viewport camera always faces (0, 0, 0).
(define CAM1 (make-camera GUI1 empty (make-point 1 1 1) (make-point 2 2 2) 0))

(@dd-template-rules compound ;5 fields
                    ref      ;(camera-gui Camera) is GUIState
                    ref      ;(camera-objects Camera) is ListOfObject
                    ref      ;(camera-position Camera) is Point
                    ref)     ;(camera-light Camera) is Point

(define (fn-for-cam cam)
  (... (fn-for-gui-state (camera-gui-state cam))
       (fn-for-loo (camera-objects cam))
       (fn-for-point (camera-position cam))
       (fn-for-point (camera-light cam))
       (camera-time cam)))                  ;Natural


(@htdf main)
(@signature Camera -> Camera)
;; start the world with (main CAM1)

(@template-origin htdw-main)

(define (main cam)
  (big-bang cam                 ;Camera
    (on-tick    tick (/ 1 TPS)) ;Camera -> Camera
    (to-draw    render)         ;Camera -> Image
    (on-mouse   mouse-handler)  ;Camera Integer Integer MouseEvent -> Camera
    (on-key     key-handler)    ;Camera KeyEvent -> Camera
    (on-release key-release)))  ;Camera KeyEvent -> Camera

    
(@htdf tick)
(@signature Camera -> Camera)
;; increment ticks since last user interaction
;; !!!

(define (tick cam) cam) ;stub
  
    
(@htdf render)
(@signature Camera -> Image)
;; render ...
;; !!! purpose and examples
;(define (render cam) MTS) ;stub

(@template-origin Camera)

(@template
 (define (render cam)
   (... (fn-for-gui-state (camera-gui-state cam))
        (fn-for-loo (camera-objects cam))
        (fn-for-point (camera-position cam))
        (fn-for-point (camera-light cam))
        (camera-time cam))))

(define (render cam)
  (overlay (render-gui (camera-gui cam))
           (render-objects (camera-objects cam)
                           (camera-position cam)
                           (camera-light cam))))

(@htdf render-gui)
(@signature GUIState -> Image)
;; renders the gui and dropdowns
;; !!!
(check-expect (render-gui GUI1) MTS)

(define (render-gui gui) MTS) ;stub


(@htdf render-objects)
(@signature ListOfObject Point Point -> Image)
;; !!!

(define (render-objects obj pos light) MTS) ;stub


(@htdf mouse-handler)
(@signature Camera Integer Integer MouseEvent -> Camera)
;; on mouse click ...
;; !!!
(define (mouse-handler cam x y me) cam) ;stub


(@htdf key-handler)
(@signature Camera KeyEvent -> Camera)
;; on key press ...
;; !!!
(define (key-handler cam ke) cam) ;stub


(@htdf key-release)
(@signature Camera KeyEvent -> Camera)
;; on key release ...
;; !!!
(define (key-release cam ke) cam) ;stub
