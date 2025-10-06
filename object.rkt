;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname object) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)
(require 2htdp/image)
(require 2htdp/universe)

;; Basic types:

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
;; Triangle is (make-triangle Point Point Point)
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

;; Internal definitions:

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
(define M2 (cons (make-tri
                  (make-point 2 0 0)
                  (make-point -1 -1 (/ (sqrt 13) 2))
                  (make-point -1 -1 (/ (sqrt 13) -2)))
                 (cons (make-tri
                        (make-point -1 2 0)
                        (make-point -1 -1 (/ (sqrt 13) 2))
                        (make-point -1 -1 (/ (sqrt 13) -2)))
                       (cons (make-tri
                              (make-point 2 0 0)
                              (make-point -1 2 0)
                              (make-point -1 -1 (/ (sqrt 13) 2)))
                             (cons (make-tri
                                    (make-point 2 0 0)
                                    (make-point -1 2 0)
                                    (make-point -1 -1 (/ (sqrt 13) -2)))
                                   empty))))) ;Tetrahedron, simplest 3D mesh

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

;; External definitions:

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
