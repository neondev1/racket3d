;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname vector) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)
(require 2htdp/image)

(require "provide.rkt")
(provide (all-defined-out))

(require "common.rkt")
(@htdd Colour Vector Euler Triangle)

;;
;; VECTOR.rkt
;;
;; Data definitions and functions for vector arithmetic
;;


;;
;; DATA DEFINITIONS
;;


(@htdd Plane)
(define-struct plane (a b c d))
;; Plane is (make-plane Number Number Number Number)
;; interp. a plane in Cartesian form, i.e. in the form ax+by+cz=d
;; CONSTRAINT: at least one of a, b, c must be nonzero
(define PLANE-XY (make-plane 0 0 1 0))         ;z=0, xy plane
(define PLANE1 (make-plane 1 -2 -3 2))
(define PLANE2 (make-plane 2 -1 -1 1))
(define PLANE3 (make-plane -0.5 1.2 5.6 -2.4)) ;negative a and
;                                                  ;d are allowed

(@dd-template-rules compound) ;4 fields

(define (fn-for-plane c)
  (... (plane-a c)   ;Number
       (plane-b c)   ;Number
       (plane-c c)   ;Number
       (plane-d c))) ;Number



(@htdd Line)
(define-struct parametric (position direction))
;; Line is (make-parametric Vector Vector)
;; interp. a line in vector parametric form
;; CONSTRAINT: direction vector must be nonzero
(define LINE-X (make-parametric ZERO-VECTOR ;x-axis
                                (make-vector 1 0 0)))
(define LINE1 (make-parametric VECTOR2 VECTOR3))

(@dd-template-rules compound ;2 fields
                    ref      ;Vector
                    ref)     ;Vector

(define (fn-for-line l)
  (... (fn-for-vector (parametric-position l))
       (fn-for-vector (parametric-direction l))))


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

(@template-origin Vector)

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

(@template-origin Vector)

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

(@template-origin Vector)

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

(@template-origin Vector)

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

(@template-origin Vector)

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

(@template-origin Vector)

(define (vector-magnitude v)
  (sqrt (+ (sqr (vector-x v))
           (sqr (vector-y v))
           (sqr (vector-z v)))))



(@htdf cross-product)
(@signature Vector Vector -> Vector)
;; produce cross product of given vectors
(check-expect (cross-product (make-vector 2 0 0)
                             (make-vector 0 2 0))
              (make-vector 0 0 4))
(check-expect (cross-product (make-vector 0 2 0)
                             (make-vector 2 0 0))
              (make-vector 0 0 -4))

(@template-origin Vector)

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

(@template-origin Vector)

(define (dot-product v0 v1)
  (+ (* (vector-x v0) (vector-x v1))
     (* (vector-y v0) (vector-y v1))
     (* (vector-z v0) (vector-z v1))))



(@htdf normal)
(@signature Triangle -> Vector)
;; produce a vector normal to given triangle with unspecified magnitude
(check-expect (normal (make-poly (make-vector 0 0 0)
                                 (make-vector 2 0 0)
                                 (make-vector 0 2 0)))
              (make-vector 0 0 4))
(check-expect (normal (make-poly (make-vector 0 0 0)
                                 (make-vector 0 2 0)
                                 (make-vector 2 0 0)))
              (make-vector 0 0 -4))

(@template-origin Triangle)

(define (normal t)
  (cross-product (sub (poly-v1 t) (poly-v0 t))
                 (sub (poly-v2 t) (poly-v0 t))))



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

(@template-origin fn-composition)

(define (vector-angle v0 v1)
  (acos (/ (dot-product v0 v1)
           (* (vector-magnitude v0) (vector-magnitude v1)))))



(@htdf normal->plane)
(@signature Vector Vector -> Plane)
;; produce Cartesian form of plane given normal and a position vector on plane
;!!! examples

(@template-origin Vector)

(define (normal->plane n p)
  (make-plane (vector-x n)
              (vector-y n)
              (vector-z n)
              (dot-product n p)))



(@htdf triangle->plane)
(@signature Triangle -> Plane)
;; produce Cartesian form of plane containing triangle
(check-expect (triangle->plane (make-poly (make-vector 0 0 0)
                                          (make-vector 2 0 0)
                                          (make-vector 0 2 0)))
              (make-plane 0 0 4 0))
(check-expect (triangle->plane (make-poly (make-vector 1 -1 4)
                                          (make-vector -3 4 5)
                                          (make-vector -1 3 -1)))
              (make-plane -29 -22 -6 -31))

(@template-origin fn-composition)

(define (triangle->plane t)
  (normal->plane (normal t) (poly-v0 t)))



(@htdf plane-parallel?)
(@signature Plane Plane -> Boolean)
;; produce true if given planes are parallel, otherwise false
(check-expect (plane-parallel? (make-plane 0 2 3 9)
                               (make-plane -1 4 2 7)) false)
(check-expect (plane-parallel? (make-plane 1 -2 4 5)
                               (make-plane 1 -2 4 9)) true)
(check-expect (plane-parallel? (make-plane 2 3 4 -5)
                               (make-plane 2 3 4 -5)) true)
(check-expect (plane-parallel? (make-plane 1 -4 5 7)
                               (make-plane 1 -4 -5 7)) false)

(@template-origin Plane)

(define (plane-parallel? p0 p1)
  (= (/ (plane-a p0) (plane-a p1))
     (/ (plane-b p0) (plane-b p1))
     (/ (plane-c p0) (plane-c p1))))


;; NOTE: EVERYTHING BEYOND THIS POINT IS BUGGY AND PROBABLY NEEDS REWRITING
;;       Maybe use Gaussian elimination instead of hard-coding the calculations

#|

(@htdf plane-intersect)
(@signature Plane Plane -> Line)
;; produce parametric line of intersection between two planes
;; CONSTRAINT: planes must be nonparallel
#;
(check-expect (plane-intersect (make-plane 1 0 0 1)
                               (make-plane 0 0 1 1))
              (make-parametric (make-vector 1 0 1)
                             (make-vector 0 1 0)))
(check-expect (plane-intersect PLANE1 PLANE2)
              (make-parametric (make-vector 0 -1 0)
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
   (... (plane-a p0)
        (plane-b p0)
        (plane-c p0)
        (plane-d p0)
        (plane-a p1)
        (plane-b p1)
        (plane-c p1)
        (plane-d p1)
        d)))

(define (intersect-first p0 p1 d)
  (cond [(not (zero? d))
         (make-parametric
          (make-vector (/ (- (* (plane-d p0) (plane-b p1))
                             (* (plane-d p1) (plane-b p0))) d)
                       0 #i0)
          (make-vector (/ (- (* (plane-b p0) (plane-c p1))
                             (* (plane-b p1) (plane-c p0))) d)
                       0 #i1))]
        [(not (zero? (- (* (plane-a p0) (plane-c p1))
                        (* (plane-a p1) (plane-c p0)))))
         (make-parametric
          (make-vector (/ (- (* (plane-d p0) (plane-c p1))
                             (* (plane-d p1) (plane-c p0)))
                          (- (* (plane-a p0) (plane-c p1))
                             (* (plane-a p1) (plane-c p0))))
                       #i0 0)
          (make-vector (/ (- (* (plane-c p0) (plane-b p1))
                             (* (plane-c p1) (plane-b p0)))
                          (- (* (plane-a p0) (plane-c p1))
                             (* (plane-a p1) (plane-c p0))))
                       #i1 0))]
        [else
         (make-parametric
          (make-vector #i0 0
                       (/ (- (* (plane-d p0) (plane-b p1))
                             (* (plane-d p1) (plane-b p0)))
                          (- (* (plane-c p0) (plane-b p1))
                             (* (plane-c p1) (plane-b p0)))))
          (make-vector #i1 0
                       (/ (- (* (plane-b p0) (plane-a p1))
                             (* (plane-b p1) (plane-a p0)))
                          (- (* (plane-c p0) (plane-b p1))
                             (* (plane-c p1) (plane-b p0))))))]))



(@htdf intersect-denominator)
(@signature Plane Plane -> Number)
;; produce denominator of x component of line of intersection of given planes
;!!! examples

(@template-origin Plane)

(@template
 (define (intersect-denominator p0 p1)
   (... (plane-a p0)
        (plane-b p0)
        (plane-c p0)
        (plane-d p0)
        (plane-a p1)
        (plane-b p1)
        (plane-c p1)
        (plane-d p1))))

(define (intersect-denominator p0 p1)
  (- (* (plane-a p0) (plane-b p1))
     (* (plane-a p1) (plane-b p0))))



(@htdf intersect-other)
(@signature Plane Plane Line -> Line)
;; produce other component of line of intersection from component and one plane
;!!! examples

;(define (intersect-other p0 p1 l) LINE-X) ;stub

(@template-origin Line)

(@template
 (define (intersect-other p0 p1 l)
   (... p0 p1
        (fn-for-vector (parametric-position l))
        (fn-for-vector (parametric-direction l)))))

(define (intersect-other p0 p1 l)
  (make-parametric (intersect-position p0 p1 (parametric-position l))
                 (intersect-direction p0 p1 (parametric-direction l))))



(@htdf intersect-position)
(@signature Plane Plane Vector -> Vector)
;; produce position vector of other component of line of intersection
;!!! examples

;(define (intersect-position p0 p1 pos) ZERO-VECTOR) ;stub

(@template-origin Plane Vector)

(@template
 (define (intersect-position p0 p1 pos)
   (... (plane-a p0)
        (plane-b p0)
        (plane-c p0)
        (plane-d p0)
        (plane-a p1)
        (plane-b p1)
        (plane-c p1)
        (plane-d p1)
        (vector-x pos)
        (vector-y pos)
        (vector-z pos))))

(define (intersect-position p0 p1 pos)
  (cond [(inexact? (vector-z pos))
         (if (zero? (plane-b p1))
             (make-vector (vector-x pos)
                          (/ (- (plane-d p0)
                                (* (plane-a p0) (vector-x pos)))
                             (plane-b p0))
                          0)
             (make-vector (vector-x pos)
                          (/ (- (plane-d p1)
                                (* (plane-a p1) (vector-x pos)))
                             (plane-b p1))
                          0))]
        [(inexact? (vector-y pos))
         (if (zero? (plane-b p1))
             (make-vector (vector-x pos)
                          0
                          (/ (- (plane-d p0)
                                (* (plane-a p0) (vector-x pos)))
                             (plane-c p0)))
             (make-vector (vector-x pos)
                          0
                          (/ (- (plane-d p1)
                                (* (plane-a p1) (vector-x pos)))
                             (plane-c p1))))]
        [else
         (if (zero? (plane-b p1))
             (make-vector 0
                          (/ (- (plane-d p0)
                                (* (plane-c p0) (vector-z pos)))
                             (plane-b p0))
                          (vector-z pos))
             (make-vector 0
                          (/ (- (plane-d p1)
                                (* (plane-c p1) (vector-z pos)))
                             (plane-b p1))
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
   (... (plane-a p0)
        (plane-b p0)
        (plane-c p0)
        (plane-d p0)
        (plane-a p1)
        (plane-b p1)
        (plane-c p1)
        (plane-d p1)
        (vector-x dir)
        (vector-y dir)
        (vector-z dir))))

(define (intersect-direction p0 p1 dir)
  (cond [(inexact? (vector-z dir))
         (if (zero? (plane-b p1))
             (make-vector (vector-x dir)
                          (/ (- (- (plane-c p0))
                                (* (plane-a p0) (vector-x dir)))
                             (plane-b p0))
                          1)
             (make-vector (vector-x dir)
                          (/ (- (- (plane-c p1))
                                (* (plane-a p1) (vector-x dir)))
                             (plane-b p1))
                          1))]
        [(inexact? (vector-y dir))
         (if (zero? (plane-b p1))
             (make-vector (vector-x dir)
                          1
                          (/ (- (- (plane-b p0))
                                (* (plane-a p0) (vector-x dir)))
                             (plane-c p0)))
             (make-vector (vector-x dir)
                          1
                          (/ (- (- (plane-b p1))
                                (* (plane-a p1) (vector-x dir)))
                             (plane-c p1))))]
        [else
         (if (zero? (plane-c p1))
             (make-vector 1
                          (/ (- (- (plane-a p0))
                                (* (plane-c p0) (vector-z dir)))
                             (plane-b p0))
                          (vector-z dir))
             (make-vector 1
                          (/ (- (- (plane-a p1))
                                (* (plane-c p1) (vector-z dir)))
                             (plane-b p1))
                          (vector-z dir)))]))

|#

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
