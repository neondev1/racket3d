;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname vector) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)
(require 2htdp/image)

(require "provide.rkt")
(provide (all-defined-out))

(require "common.rkt")
(@htdd Colour Point Euler Triangle)

;;
;; VECTOR.rkt
;;
;; Data definitions and functions for vector arithmetic
;;


;;
;; DATA DEFINITIONS
;; Most definitions here are namespaced with the r3d-
;; prefix to prevent conflicts with the base language;
;; others are so named for consistency, except vector
;; due to its heavy usage everywhere.
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
;; FUNCTIONS
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



(@htdf negate)
(@signature Vector -> Vector)
;; produce vector of equal magnitude and opposite direction
(check-expect (negate ZERO-VECTOR) ZERO-VECTOR)
(check-expect (negate (make-vector 1 2 3)) (make-vector -1 -2 -3))
(check-expect (negate (make-vector 1.2 3.4 -5.6)) (make-vector -1.2 -3.4 5.6))

;(define (negate v) ZERO-VECTOR) ;stub

(@template-origin Vector)

(@template
 (define (negate v)
   (... (vector-x v)
        (vector-y v)
        (vector-z v))))

(define (negate v)
  (make-vector (- (vector-x v))
               (- (vector-y v))
               (- (vector-z v))))



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
                                         (make-point 0 2 0)))
              (make-vector 0 0 4))
(check-expect (normal (make-r3d-triangle (make-point 0 0 0)
                                         (make-point 0 2 0)
                                         (make-point 2 0 0)))
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
                                                  (make-point 0 2 0)))
              (make-r3d-plane 0 0 4 0))
(check-expect (triangle->plane (make-r3d-triangle (make-point 1 -1 4)
                                                  (make-point -3 4 5)
                                                  (make-point -1 3 -1)))
              (make-r3d-plane -29 -22 -6 -31))

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


;; NOTE: EVERYTHING BEYOND THIS POINT IS BUGGY AND PROBABLY NEEDS REWRITING
;;       Maybe use Gaussian elimination instead of hard-coding the calculations


(@htdf plane-intersect)
(@signature Plane Plane -> Line)
;; produce parametric line of intersection between two planes
;; CONSTRAINT: planes must be nonparallel
#;
(check-expect (plane-intersect (make-r3d-plane 1 0 0 1)
                               (make-r3d-plane 0 0 1 1))
              (make-r3d-line (make-vector 1 0 1)
                             (make-vector 0 1 0)))
(check-expect (plane-intersect PLANE1 PLANE2)
              (make-r3d-line (make-vector 0 -1 0)
                             (make-vector -1/3 -5/3 1)))

;(define (plane-intersect p0 p1) LINE-X) ;stub

(@template-origin fn-composition)

(define (plane-intersect p0 p1)
  (intersect-other p0 p1 (intersect-first p0 p1 (intersect-denominator p0 p1))))


;; NOTE: The following functions exist to reduce recomputation;
;;       local is not used here as we are sticking to strict BSL.


(@htdf intersect-first)
(@signature Plane Plane Number Number -> Line)
;; produce first component of incomplete line of intersection between two planes
;!!! examples

;(define (intersect-first p0 p1 d) LINE-X) ;stub

(@template-origin Plane)

(@template
 (define (intersect-first p0 p1 d)
   (... (r3d-plane-a p0)
        (r3d-plane-b p0)
        (r3d-plane-c p0)
        (r3d-plane-d p0)
        (r3d-plane-a p1)
        (r3d-plane-b p1)
        (r3d-plane-c p1)
        (r3d-plane-d p1)
        d)))

(define (intersect-first p0 p1 d)
  (cond [(not (zero? d))
         (make-r3d-line
          (make-vector (/ (- (* (r3d-plane-d p0) (r3d-plane-b p1))
                             (* (r3d-plane-d p1) (r3d-plane-b p0))) d)
                       0 #i0)
          (make-vector (/ (- (* (r3d-plane-b p0) (r3d-plane-c p1))
                             (* (r3d-plane-b p1) (r3d-plane-c p0))) d)
                       0 #i1))]
        [(not (zero? (- (* (r3d-plane-a p0) (r3d-plane-c p1))
                        (* (r3d-plane-a p1) (r3d-plane-c p0)))))
         (make-r3d-line
          (make-vector (/ (- (* (r3d-plane-d p0) (r3d-plane-c p1))
                             (* (r3d-plane-d p1) (r3d-plane-c p0)))
                          (- (* (r3d-plane-a p0) (r3d-plane-c p1))
                             (* (r3d-plane-a p1) (r3d-plane-c p0))))
                       #i0 0)
          (make-vector (/ (- (* (r3d-plane-c p0) (r3d-plane-b p1))
                             (* (r3d-plane-c p1) (r3d-plane-b p0)))
                          (- (* (r3d-plane-a p0) (r3d-plane-c p1))
                             (* (r3d-plane-a p1) (r3d-plane-c p0))))
                       #i1 0))]
        [else
         (make-r3d-line
          (make-vector #i0 0
                       (/ (- (* (r3d-plane-d p0) (r3d-plane-b p1))
                             (* (r3d-plane-d p1) (r3d-plane-b p0)))
                          (- (* (r3d-plane-c p0) (r3d-plane-b p1))
                             (* (r3d-plane-c p1) (r3d-plane-b p0)))))
          (make-vector #i1 0
                       (/ (- (* (r3d-plane-b p0) (r3d-plane-a p1))
                             (* (r3d-plane-b p1) (r3d-plane-a p0)))
                          (- (* (r3d-plane-c p0) (r3d-plane-b p1))
                             (* (r3d-plane-c p1) (r3d-plane-b p0))))))]))



(@htdf intersect-denominator)
(@signature Plane Plane -> Number)
;; produce denominator of x component of line of intersection of given planes
;!!! examples

(@template-origin Plane)

(@template
 (define (intersect-denominator p0 p1)
   (... (r3d-plane-a p0)
        (r3d-plane-b p0)
        (r3d-plane-c p0)
        (r3d-plane-d p0)
        (r3d-plane-a p1)
        (r3d-plane-b p1)
        (r3d-plane-c p1)
        (r3d-plane-d p1))))

(define (intersect-denominator p0 p1)
  (- (* (r3d-plane-a p0) (r3d-plane-b p1))
     (* (r3d-plane-a p1) (r3d-plane-b p0))))



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
                          (/ (- (r3d-plane-d p0)
                                (* (r3d-plane-c p0) (vector-z pos)))
                             (r3d-plane-b p0))
                          (vector-z pos))
             (make-vector 0
                          (/ (- (r3d-plane-d p1)
                                (* (r3d-plane-c p1) (vector-z pos)))
                             (r3d-plane-b p1))
                          (vector-z pos)))]))



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
         (if (zero? (r3d-plane-c p1))
             (make-vector 1
                          (/ (- (- (r3d-plane-a p0))
                                (* (r3d-plane-c p0) (vector-z dir)))
                             (r3d-plane-b p0))
                          (vector-z dir))
             (make-vector 1
                          (/ (- (- (r3d-plane-a p1))
                                (* (r3d-plane-c p1) (vector-z dir)))
                             (r3d-plane-b p1))
                          (vector-z dir)))]))


#|
TODO: Subdividing overlapping mesh faces
!!!

BASIC PROCEDURE

For each mesh face added to buffer, perform a comparison with each existing
element as follows:
1. Compute distance between centroids. If distance is greater than or equal to
   the sum of the greatest distances between the centroid and farthest vertex
   in both triangles, skip the remainder of this comparison.
2. Compute line of intersection between planes containing both triangles.
3. Check if computed line of intersection intersects both triangles.
   3a. Performing checks on two sides of each triangle is sufficient.
   3b. If either of the triangles have both intersection points very close
       (within the constant APPROX) to a vertex, return false.
4. If previous check returned false, skip the remainder of this comparison.
5. Subdivide both triangles along line of intersection.
   5a. For each subdivision:
   5b. Determine which two edges are intersected by the line.
   5c. Create a mesh face from the triangle created by cutting along the line.
   5d. Divide remaining quadrilateral into two triangular mesh faces.
6. The existing mesh face that has been subdivided is reinserted into the list
   without checks. The new mesh faces are inserted with this procedure.
   6a. If possible, skip checks for all polygons that have been checked before.

|#
