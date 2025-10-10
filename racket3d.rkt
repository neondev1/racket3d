;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname racket3d) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)
(require 2htdp/image)
(require 2htdp/universe)

(@htdw Camera)
;;
;; CONSTANTS
;;

(define WIDTH 640)
(define HEIGHT 480)

(define MTS (empty-scene WIDTH HEIGHT))

;;
;; BASIC DATA DEFINITIONS
;;

(@htdd Point)
(define-struct point (x y z))
;; Point is (make-point Number Number Number)
;; interp. the x, y and z coordinates of a point
(define P1 (make-point 0 0 0))
(define P2 (make-point 1 1 1))
(define P3 (make-point 0.1 1.2 2.3))

(@dd-template-rules compound) ;3 fields

(define (fn-for-point p)
  (... (point-x p)
       (point-y p)
       (point-z p)))


(@htdd Euler)
(define-struct euler (alpha beta gamma))
;; Euler is (make-euler Number Number Number)
;; interp. the Euler angles representing an orientation
(define EA1 (make-euler 0 0 0))
(define EA2 (make-euler 60 90 180))
(define EA3 (make-euler 12.3 45.6 78.9))

(@dd-template-rules compound) ;3 fields

(define (fn-for-euler e)
  (... (euler-alpha e)
       (euler-beta  e)
       (euler-gamma e)))


(@htdd Triangle)
(define-struct tri (v0 v1 v2))
;; Triangle is (make-tri Point Point Point)
;; interp. the three vertices of a triangle
;; CONSTRAINT: No two vertices should be equal
(define T1 (make-tri (make-point 0 1 1)
                     (make-point 1 0 1)
                     (make-point 0 0 1))) ;triangles for a
(define T2 (make-tri (make-point 0 1 1)   ;rectangular mesh
                     (make-point 1 1 1)
                     (make-point 1 0 1)))

(@dd-template-rules compound ;3 fields
                    ref      ;(tri-v0 Triangle) is Point
                    ref      ;(tri-v1 Triangle) is Point
                    ref)     ;(tri-v2 Triangle) is Point

(define (fn-for-triangle t)
  (... (fn-for-point (tri-v0 t))
       (fn-for-point (tri-v1 t))
       (fn-for-point (tri-v2 t))))

;;
;; INTERNAL DEFINITIONS
;;

(@htdd Cuboid)
(define-struct cuboid (pos rot w h d))
;; Cuboid is (make-cuboid Point Euler Number Number Number)
;; interp. the position, orientation, width, height and depth of a cuboid
(define C1 (make-cuboid (make-point 0 0 0)
                        (make-euler 0 0 0)
                        1 1 1)) ;Unit cube
(define C2 (make-cuboid (make-point 1 2 3)
                        (make-euler 0 0 0)
                        2 4 6))
(define C3 (make-cuboid (make-point 1 2 1)
                        (make-euler 45 45 45)
                        2 3 4))

(@dd-template-rules compound ;5 fields
                    ref      ;(cuboid-pos Cuboid) is Point
                    ref)     ;(cuboid-rot Cuboid) is Euler

(define (fn-for-cuboid c)
  (... (fn-for-point (cuboid-pos c))
       (fn-for-euler (cuboid-rot c))
       (cuboid-w c)
       (cuboid-h c)
       (cuboid-d c)))


(@htdd Ellipsoid)
(define-struct ellipsoid (pos rot xs ys zs))
;; Ellipsoid is (make-ellipsoid Point Euler Number Number Number)
;; interp. the position, orientation, and scales of an ellipsoid
(define E1 (make-ellipsoid (make-point 0 0 0)
                           (make-euler 0 0 0)
                           1 1 1)) ;sphere
(define E2 (make-ellipsoid (make-point 0 0 0)
                           (make-euler 23 37 79)
                           1 1 1)) ;rotating sphere has no effect
(define E3 (make-ellipsoid (make-point 1 3 5)
                           (make-euler 100 120 140)
                           3 4 5))

(@dd-template-rules compound ;5 fields
                    ref      ;(ellipsoid-pos Ellipsoid) is Point
                    ref)     ;(ellipsoid-rot Ellipsoid) is Euler

(define (fn-for-ellipsoid e)
  (... (fn-for-point (ellipsoid-pos e))
       (fn-for-euler (ellipsoid-rot e))
       (ellipsoid-xs e)
       (ellipsoid-ys e)
       (ellipsoid-zs e)))


(@htdd Mesh)
;; Mesh is one of:
;;  - empty
;;  - (cons Triangle Mesh)
(define M1 empty)
(define M2 (list (make-tri (make-point 2 0 0) ;Tetrahedron mesh
                           (make-point -1 -1 (/ (sqrt 13) 2))
                           (make-point -1 -1 (/ (sqrt 13) -2)))
                 (make-tri (make-point -1 2 0)
                           (make-point -1 -1 (/ (sqrt 13) 2))
                           (make-point -1 -1 (/ (sqrt 13) -2)))
                 (make-tri (make-point 2 0 0)
                           (make-point -1 2 0)
                           (make-point -1 -1 (/ (sqrt 13) 2)))
                 (make-tri (make-point 2 0 0)
                           (make-point -1 2 0)
                           (make-point -1 -1 (/ (sqrt 13) -2)))))

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

;;
;; PUBLIC DEFINITIONS
;;

(@htdd Object)
;; Object is one of:
;;  - Cuboid
;;  - Ellipsoid
;;  - Mesh
;; interp. the position, orientation and size info of an object
(define O1 C1)
(define O2 E1)
(define O3 M2)

(@dd-template-rules one-of   ;3 cases
                    compound ;Cuboid
                    ref
                    compound ;Ellipsoid
                    ref
                    compound ;Mesh
                    ref)

(define (fn-for-object o)
  (cond [(cuboid? o)
         (... (fn-for-cuboid o))]
        [(ellipsoid? o)
         (... (fn-for-ellipsoid o))]
        [else
         (... (fn-for-mesh o))]))


(@htdd ListOfObject)
;; ListOfObject is one of:
;;  - empty
;;  - (cons Object ListOfObject)
;; interp. a list of all objects to be rendered
(define LOO1 empty)
(define LOO2 (list O1 O2 O3))

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
;; LIGHTING DATA DEFINITIONS
;;

(@htdd Vector)
(define-struct vec (x y z))
;; Vector is (make-vector Number Number Number)
;; interp. the x, y and z components of a 3D vector
(define V1 (make-vec 0 0 0)) ;zero vector
(define V2 (make-vec 0 0 1)) ;unit vector normal to xy plane
(define V3 (make-vec 1.3 3.5 5.7))

(@dd-template-rules compound) ;3 fields

(define (fn-for-vector v)
  (... (vec-x v)
       (vec-y v)
       (vec-z v)))

;;
;; LIGHTING FUNCTIONS
;;

(@htdf mul)
(@signature Vector Number -> Vector)
;; produce vector multiplied by a scalar
(check-expect (mul (make-vec 0 0 0) 2) (make-vec 0 0 0))
(check-expect (mul (make-vec 1 2 3) 0) (make-vec 0 0 0))
(check-expect (mul (make-vec 1.2 3.4 -5.6) -3) (make-vec -3.6 -10.2 16.8))

;(define (mul v s) (make-vec 0 0 0)) ;stub

(@template-origin Vector)

(@template
 (define (mul v s)
   (... (vec-x v)
        (vec-y v)
        (vec-z v)
        s)))

(define (mul v s)
  (make-vec (* (vec-x v) s)
            (* (vec-y v) s)
            (* (vec-z v) s)))


(@htdf div)
(@signature Vector Number -> Vector)
;; produce vector divided by a scalar
;; CONSTRAINT: scalar must be nonzero
(check-expect (div (make-vec 0 0 0) 2) (make-vec 0 0 0))
(check-expect (div (make-vec 1.2 -4.5 7.8) 3) (make-vec 0.4 -1.5 2.6))

;(define (div v s) (make-vec 0 0 0)) ;stub

(@template-origin Vector)

(@template
 (define (div v s)
   (... (vec-x v)
        (vec-y v)
        (vec-z v)
        s)))

(define (div v s)
  (make-vec (/ (vec-x v) s)
            (/ (vec-y v) s)
            (/ (vec-z v) s)))


(@htdf cross)
(@signature Vector Vector -> Vector)
;; produce cross product of given vectors
(check-expect (cross (make-vec 2 0 0)
                     (make-vec 0 2 0))
              (make-vec 0 0 4))
(check-expect (cross (make-vec 0 2 0)
                     (make-vec 2 0 0))
              (make-vec 0 0 -4))

;(define (cross v0 v1) (make-vec 0 0 0)) ;stub

(@template-origin Vector)

(@template
 (define (cross v0 v1)
   (... (vec-x v0)
        (vec-y v0)
        (vec-z v0)
        (vec-x v1)
        (vec-y v1)
        (vec-z v1))))

(define (cross v0 v1)
  (make-vec (- (* (vec-y v0) (vec-z v1))
               (* (vec-y v1) (vec-z v0)))
            (- (* (vec-z v0) (vec-x v1))
               (* (vec-z v1) (vec-x v0)))
            (- (* (vec-x v0) (vec-y v1))
               (* (vec-x v1) (vec-y v0)))))


(@htdf mag)
(@signature Vector -> Number)
;; produce magnitude of given vector
(check-expect (mag (make-vec 0 0 0)) 0)
(check-within (mag (make-vec 1 1 1)) (sqrt 3) (expt 10 -10))
(check-expect (mag (make-vec -2 3 -6)) 7)

;(define (mag v) 0) ;stub

(@template-origin Vector)

(@template
 (define (mag v)
   (... (vec-x v)
        (vec-y v)
        (vec-z v))))

(define (mag v)
  (sqrt (+ (sqr (vec-x v)) (sqr (vec-y v)) (sqr (vec-z v)))))


(@htdf vec)
(@signature Point Point -> Vector)
;; produce vector from first to seconds point
(check-expect (to-vec (make-point 0 0 0)
                      (make-point 0 0 0))
              (make-vec 0 0 0))
(check-expect (to-vec (make-point 0 0 0)
                      (make-point 1 -2 3))
              (make-vec 1 -2 3))
(check-expect (to-vec (make-point 1.2 -3.4 5.6)
                      (make-point 1.3 5.7 -9.1))
              (make-vec 0.1 9.1 -14.7))

;(define (vec p0 p1) (make-vec 0 0 0)) ;stub

(@template-origin Point)

(@template
 (define (vec p0 p1)
   (... (point-x p0)
        (point-y p0)
        (point-z p0)
        (point-x p1)
        (point-y p1)
        (point-z p1))))

(define (to-vec p0 p1)
  (make-vec (- (point-x p1) (point-x p0))
            (- (point-y p1) (point-y p0))
            (- (point-z p1) (point-z p0))))


(@htdf normal)
(@signature Triangle -> Vector)
;; produce unit vector normal to given triangle
;; CONSTRAINT: given triangle must be non-degenerate
(check-expect (normal (make-tri (make-point 0 0 0)
                                (make-point 2 0 0)
                                (make-point 0 2 0)))
              (make-vec 0 0 1))
(check-expect (normal (make-tri (make-point 0 0 0)
                                (make-point 0 2 0)
                                (make-point 2 0 0)))
              (make-vec 0 0 -1))

;(define (normal t) (make-vec 0 0 0)) ;stub

(@template-origin fn-composition)

(define (normal t)
  (div (cross (to-vec (tri-v1 t) (tri-v0 t))
              (to-vec (tri-v2 t) (tri-v1 t)))
       (mag (cross (to-vec (tri-v1 t) (tri-v0 t))
                   (to-vec (tri-v2 t) (tri-v1 t))))))

;;
;; WORLD
;;

(@htdd Camera)

(@htdf main)
(@signature Camera -> Camera)
;; start the world with (main ...)

(@template-origin htdw-main)

(define (main cam)
  (big-bang cam                  ;Camera
    (on-tick    tick)            ;Camera -> Camera
    (to-draw    render)          ;Camera -> Image
    (on-mouse   mouse-handler)   ;Camera Integer Integer MouseEvent -> Camera
    (on-key     key-handler)     ;Camera KeyEvent -> Camera
    (on-release key-release)))   ;Camera KeyEvent -> Camera

    
(@htdf tick)
(@signature Camera -> Camera)
;; produce the next ...
;; !!!
(define (tick cam) cam) ;stub

    
(@htdf render)
(@signature Camera -> Image)
;; render ...
;; !!!
(define (render cam) MTS) ;stub


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
