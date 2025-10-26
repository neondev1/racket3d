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
(define-struct r3d-vector (x y z))
;; Vector is (make-r3d-vector Number Number Number)
;; interp. the x, y and z components of a 3D vector
(define ZERO-VECTOR (make-r3d-vector 0 0 0)) ;zero vector
(define VECTOR1 (make-r3d-vector 0 0 1))     ;unit vector normal to xy plane
(define VECTOR2 (make-r3d-vector 1.3 3.5 5.7))
(define VECTOR3 (make-r3d-vector -1.3 -3.5 -5.7))

(@dd-template-rules compound) ;3 fields

(define (fn-for-vector v)
  (... (r3d-vector-x v)   ;Number
       (r3d-vector-y v)   ;Number
       (r3d-vector-z v))) ;Number


(@htdd Plane)
(define-struct r3d-plane (a b c d))
;; Plane is (make-r3d-plane Number Number Number Number)
;; interp. a plane in Cartesian form, i.e. in the form ax+by+cz=d
(define PLANE0 (make-r3d-plane 0 0 1 0))           ;z=0, xy plane
(define PLANE1 (make-r3d-plane 1 2 -10 5))
(define PLANE2 (make-r3d-plane -0.5 1.2 5.6 -2.4)) ;negative a and
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
(define LINE1 (make-r3d-line ZERO-VECTOR ;x-axis example
                             (make-r3d-vector 1 0 0)))
(define LINE2 (make-r3d-line VECTOR2 VECTOR3))

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
(check-expect (add (make-r3d-vector 0 0 0) (make-r3d-vector 0 0 0))
              (make-r3d-vector 0 0 0))
(check-expect (add (make-r3d-vector 2 3 4) (make-r3d-vector 0 0 0))
              (make-r3d-vector 2 3 4))
(check-expect (add (make-r3d-vector 1 2 3) (make-r3d-vector 1.2 2.3 -3.4))
              (make-r3d-vector 2.2 4.3 -0.4))

;(define (add v0 v1) ZERO-VECTOR) ;stub

(@template-origin Vector)

(@template
 (define (add v0 v1)
   (... (r3d-vector-x v0)
        (r3d-vector-y v0)
        (r3d-vector-z v0)
        (r3d-vector-x v1)
        (r3d-vector-y v1)
        (r3d-vector-z v1))))

(define (add v0 v1)
  (make-r3d-vector (+ (r3d-vector-x v0) (r3d-vector-x v1))
                   (+ (r3d-vector-y v0) (r3d-vector-y v1))
                   (+ (r3d-vector-z v0) (r3d-vector-z v1))))


(@htdf sub)
(@signature Vector Vector -> Vector)
;; produce difference of two vectors
(check-expect (sub (make-r3d-vector 0 0 0) (make-r3d-vector 0 0 0))
              (make-r3d-vector 0 0 0))
(check-expect (sub (make-r3d-vector 0 0 0) (make-r3d-vector 2 -4 6))
              (make-r3d-vector -2 4 -6))
(check-expect (sub (make-r3d-vector 3 4 5) (make-r3d-vector 1.2 2.3 3.4))
              (make-r3d-vector 1.8 1.7 1.6))

;(define (sub v0 v1) ZERO-VECTOR) ;stub

(@template-origin Vector)

(@template
 (define (sub v0 v1)
   (... (r3d-vector-x v0)
        (r3d-vector-y v0)
        (r3d-vector-z v0)
        (r3d-vector-x v1)
        (r3d-vector-y v1)
        (r3d-vector-z v1))))

(define (sub v0 v1)
  (make-r3d-vector (- (r3d-vector-x v0) (r3d-vector-x v1))
                   (- (r3d-vector-y v0) (r3d-vector-y v1))
                   (- (r3d-vector-z v0) (r3d-vector-z v1))))


(@htdf scalar-multiply)
(@signature Vector Number -> Vector)
;; produce vector multiplied by a scalar
(check-expect (scalar-multiply ZERO-VECTOR 2) ZERO-VECTOR)
(check-expect (scalar-multiply (make-r3d-vector 1 2 3) 0) ZERO-VECTOR)
(check-expect (scalar-multiply (make-r3d-vector 1.2 3.4 -5.6) -3)
              (make-r3d-vector -3.6 -10.2 16.8))

;(define (scalar-multiply v s) ZERO-VECTOR) ;stub

(@template-origin Vector)

(@template
 (define (scalar-multiply v s)
   (... s
        (r3d-vector-x v)
        (r3d-vector-y v)
        (r3d-vector-z v))))

(define (scalar-multiply v s)
  (make-r3d-vector (* (r3d-vector-x v) s)
                   (* (r3d-vector-y v) s)
                   (* (r3d-vector-z v) s)))


(@htdf scalar-divide)
(@signature Vector Number -> Vector)
;; produce vector divided by a scalar
;; CONSTRAINT: scalar must be nonzero
(check-expect (scalar-divide ZERO-VECTOR 2) ZERO-VECTOR)
(check-expect (scalar-divide (make-r3d-vector 1.2 -4.5 7.8) 3)
              (make-r3d-vector 0.4 -1.5 2.6))

;(define (scalar-divide v s) ZERO-VECTOR) ;stub

(@template-origin Vector)

(@template
 (define (scalar-divide v s)
   (... s
        (r3d-vector-x v)
        (r3d-vector-y v)
        (r3d-vector-z v))))

(define (scalar-divide v s)
  (make-r3d-vector (/ (r3d-vector-x v) s)
                   (/ (r3d-vector-y v) s)
                   (/ (r3d-vector-z v) s)))


(@htdf vector-magnitude)
(@signature Vector -> Number)
;; produce magnitude of given vector
(check-expect (vector-magnitude ZERO-VECTOR) 0)
(check-within (vector-magnitude (make-r3d-vector 1 1 1)) (sqrt 3) APPROX)
(check-expect (vector-magnitude (make-r3d-vector -2 3 -6)) 7)

;(define (vector-magnitude v) 0) ;stub

(@template-origin Vector)

(@template
 (define (vector-magnitude v)
   (... (r3d-vector-x v)
        (r3d-vector-y v)
        (r3d-vector-z v))))

(define (vector-magnitude v)
  (sqrt (+ (sqr (r3d-vector-x v))
           (sqr (r3d-vector-y v))
           (sqr (r3d-vector-z v)))))


(@htdf vector->point)
(@signature Vector -> Point)
;; produce point given position vector
(check-expect (vector->point ZERO-VECTOR) ORIGIN)
(check-expect (vector->point (make-r3d-vector 14.7 25.8 36.9))
              (make-point 14.7 25.8 36.9))

(@template-origin Vector)

(@template
 (define (vector->point p)
   (... (r3d-vector-x p)
        (r3d-vector-y p)
        (r3d-vector-z p))))

(define (vector->point p)
  (make-point (r3d-vector-x p)
              (r3d-vector-y p)
              (r3d-vector-z p)))


(@htdf point->vector)
(@signature Point -> Vector)
;; produce position vector of given point
(check-expect (point->vector ORIGIN) ZERO-VECTOR)
(check-expect (point->vector (make-point 14.7 25.8 36.9))
              (make-r3d-vector 14.7 25.8 36.9))

(@template-origin Point)

(@template
 (define (point->vector p)
   (... (point-x p)
        (point-y p)
        (point-z p))))

(define (point->vector p)
  (make-r3d-vector (point-x p)
                   (point-y p)
                   (point-z p)))


(@htdf points->vector)
(@signature Point Point -> Vector)
;; produce vector from first to second point
(check-expect (points->vector (make-point 0 0 0)
                              (make-point 0 0 0))
              (make-r3d-vector 0 0 0))
(check-expect (points->vector (make-point 0 0 0)
                              (make-point 1 -2 3))
              (make-r3d-vector 1 -2 3))
(check-expect (points->vector (make-point 1.2 -3.4 5.6)
                              (make-point 1.3 5.7 -9.1))
              (make-r3d-vector 0.1 9.1 -14.7))

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
  (make-r3d-vector (- (point-x p1) (point-x p0))
                   (- (point-y p1) (point-y p0))
                   (- (point-z p1) (point-z p0))))


(@htdf cross-product)
(@signature Vector Vector -> Vector)
;; produce cross product of given vectors
(check-expect (cross-product (make-r3d-vector 2 0 0)
                             (make-r3d-vector 0 2 0))
              (make-r3d-vector 0 0 4))
(check-expect (cross-product (make-r3d-vector 0 2 0)
                             (make-r3d-vector 2 0 0))
              (make-r3d-vector 0 0 -4))

;(define (cross-product v0 v1) ZERO-VECTOR) ;stub

(@template-origin Vector)

(@template
 (define (cross-product v0 v1)
   (... (r3d-vector-x v0)
        (r3d-vector-y v0)
        (r3d-vector-z v0)
        (r3d-vector-x v1)
        (r3d-vector-y v1)
        (r3d-vector-z v1))))

(define (cross-product v0 v1)
  (make-r3d-vector (- (* (r3d-vector-y v0) (r3d-vector-z v1))
                      (* (r3d-vector-y v1) (r3d-vector-z v0)))
                   (- (* (r3d-vector-z v0) (r3d-vector-x v1))
                      (* (r3d-vector-z v1) (r3d-vector-x v0)))
                   (- (* (r3d-vector-x v0) (r3d-vector-y v1))
                      (* (r3d-vector-x v1) (r3d-vector-y v0)))))


(@htdf dot-product)
(@signature Vector Vector -> Number)
;; produce dot product of given vectors
(check-expect (dot-product (make-r3d-vector 2 0 0)
                           (make-r3d-vector 0 2 2))
              0)
(check-expect (dot-product (make-r3d-vector 0 2 0)
                           (make-r3d-vector 0 3 0))
              6)
(check-expect (dot-product (make-r3d-vector 1.2 3.4 5.6)
                           (make-r3d-vector 9.8 -7.6 5.4))
              16.16)

;(define (dot-product v0 v1) 0) ;stub

(@template-origin Vector)

(@template
 (define (dot-product v0 v1)
   (... (r3d-vector-x v0)
        (r3d-vector-y v0)
        (r3d-vector-z v0)
        (r3d-vector-x v1)
        (r3d-vector-y v1)
        (r3d-vector-z v1))))

(define (dot-product v0 v1)
  (+ (* (r3d-vector-x v0) (r3d-vector-x v1))
     (* (r3d-vector-y v0) (r3d-vector-y v1))
     (* (r3d-vector-z v0) (r3d-vector-z v1))))


(@htdf normal)
(@signature Triangle -> Vector)
;; produce a vector normal to given triangle with unspecified magnitude
(check-expect (normal (make-r3d-triangle (make-point 0 0 0)
                                         (make-point 2 0 0)
                                         (make-point 0 2 0)
                                         "black"))
              (make-r3d-vector 0 0 4))
(check-expect (normal (make-r3d-triangle (make-point 0 0 0)
                                         (make-point 0 2 0)
                                         (make-point 2 0 0)
                                         "black"))
              (make-r3d-vector 0 0 -4))

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
(check-expect (vector-angle (make-r3d-vector 1 0 0)
                            (make-r3d-vector 1 0 0))
              0)
(check-within (vector-angle (make-r3d-vector 1 0 0)
                            (make-r3d-vector 0 2 3))
              (/ pi 2) APPROX)
(check-within (vector-angle (make-r3d-vector 1 0 0)
                            (make-r3d-vector -1 0 0))
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

(@template-origin Vector)

(@template
 (define (normal->plane n p)
   (... (r3d-vector-x n)
        (r3d-vector-y n)
        (r3d-vector-z n)
        (r3d-vector-x p)
        (r3d-vector-y p)
        (r3d-vector-z p))))

(define (normal->plane n p)
  (make-r3d-plane (r3d-vector-x n)
                      (r3d-vector-y n)
                      (r3d-vector-z n)
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

(@template-origin fn-composition)

(@template
 (define (triangle->plane t)
   (normal->plane (normal t) (point->vector (r3d-triangle-v0 t)))))

(define (triangle->plane t)
   (normal->plane (normal t) (point->vector (r3d-triangle-v0 t))))

#|
TODO: Subdividing overlapping mesh faces
!!!

BASIC PROCEDURE

For each mesh face added to buffer, perform a comparison with each existing
element as follows:
1. Compute distance between centroids. If distance is too great (rigorous
   definition of "too great" TBD), end.
2. Compute line of intersection between planes containing both triangles.
3. Check if computed line of intersection intersects both triangles.
   3a. Performing 2D checks on two sides of each triangle is sufficient.
   3b. If either of the triangles have both intersection points very close
       (rigorous definition TBD) to a vertex, end.
4. If previous check returned false, end.
5. Subdivide both triangles along line of intersection.
   5a. For each subdivision:
   5b. Determine which two edges are intersected by the line.
   5c. Create a mesh face from the triangle created by cutting along the line.
   5d. Divide remaining quadrilateral into two triangular mesh faces.
6. The existing mesh face that has been subdivided is reinserted into the list
   without checks. The new mesh face is inserted normally (with this procedure).

Note: "End" refers to inserting the mesh face directly without any subdivision.
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
;; interp. GUI state, object list, position of camera/light, time in ticks since
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
