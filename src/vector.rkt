;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname vector) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)
(require 2htdp/image)

(require "provide.rkt")
(provide (matching-identifiers-out #rx"^((?!--).)*$" (all-defined-out)))

(require "common.rkt")
(@htdd Colour Vector Euler Triangle)

(require "matrix.rkt")
(@htdd Matrix)

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
;; interp. a plane in Cartesian form, i.e. ax+by+cz=d
;;         or the augmented matrix [a b c | d]
;; CONSTRAINT: at least one of a, b, c must be nonzero
(define PLANE-XY (make-plane 0 0 1 0))         ;z=0, xy plane
(define PLANE1 (make-plane 1 -2 -3 2))
(define PLANE2 (make-plane 2 -1 -1 1))
(define PLANE3 (make-plane -0.5 1.2 5.6 -2.4)) ;negative a and
;                                              ;d are allowed

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
(define LINE-Z (make-parametric ZERO-VECTOR ;z-axis
                                (make-vector 0 0 1)))
(define LINE1 (make-parametric VECTOR2 VECTOR3))

(@dd-template-rules compound ;2 fields
                    ref      ;Vector
                    ref)     ;Vector

(define (fn-for-line l)
  (... (fn-for-vector (parametric-position l))
       (fn-for-vector (parametric-direction l))))



(@htdd Row)
;; Row is one of:
;;  - empty
;;  - (cons Number Row)
;; interp. a row of a 2x4 augmented matrix, filled from right to left,
;;         with all zero entries to the left of the pivot omitted
;; CONSTRAINT: (length Row) must be at most 4
(define ROW0 (list        )) ;no pivots
(define ROW1 (list       1)) ;inconsistent
(define ROW2 (list 1 3 1 2)) ;[1 3 1 | 2]
(define ROW3 (list     1 2)) ;[0 0 1 | 2]

(@dd-template-rules one-of          ;2 cases
                    atomic-distinct ;empty
                    compound        ;(cons Number Row)
                    self-ref)       ;(rest Row) is Row

(define (fn-for-row r)
  (cond [(empty? r)
         (...)]
        [else
         (... (first r)
              (fn-for-row (rest r)))]))


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
(check-within (vector-magnitude (make-vector 1 1 1)) (sqrt 3) DELTA)
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
              (/ pi 2) DELTA)
(check-within (vector-angle (make-vector 1 0 0)
                            (make-vector -1 0 0))
              pi DELTA)

(@template-origin fn-composition)

(define (vector-angle v0 v1)
  (acos (/ (dot-product v0 v1)
           (* (vector-magnitude v0) (vector-magnitude v1)))))



(@htdf vectors->line)
(@signature Vector Vector -> Line)
;; produce a line containing both vectors
;!!! tests

(@template-origin fn-composition)

(define (vectors->line v0 v1)
  (make-parametric v0 (sub v1 v0)))



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



(@htdf plane-intersect plane-intersect--acc)
(@signature Plane Plane -> Line)
;; produce parametric line of intersection between two planes
;; CONSTRAINT: planes must be nonparallel
;!!! tests

(@template-origin accumulator)

(define (plane-intersect p0 p1)
  (if (>= (length (plane->row p0)) (length (plane->row p1)))
      (plane-intersect--acc (plane->row p0) (plane->row p1) 2)
      (plane-intersect--acc (plane->row p1) (plane->row p0) 2)))

(@template-origin Natural accumulator)

(define (plane-intersect--acc r0 r1 iters)
  (cond [(zero? iters)
         (rows->line (normalize r0) (normalize r1))]
        [else
         (if (= (length r0) (length r1))
             (plane-intersect--acc r0 (eliminate r0 r1) (sub1 iters))
             (plane-intersect--acc (eliminate r0 r1) r1 (sub1 iters)))]))



(@htdf plane->row)
(@signature Plane -> Row)
;; produce row of augmented matrix representing given plane in R^3
;!!! tests

(@template-origin Plane)

(define (plane->row p)
  (trim-zeros (list (plane-a p) (plane-b p) (plane-c p) (plane-d p))))



(@htdf rows->line)
(@signature Row Row -> Line)
;; produce parametric line given a 2x4 augmented matrix (split into two rows)
;; CONSTRAINT: the matrix with given rows must be in reduced row echelon form
;!!! tests

(@template-origin Row) ;treating Row as compound data here
;                      ;impossible to implement this otherwise

(define (rows->line r0 r1)
  (cond [(= (length r0) 3) ;first column is nonpivot (x free)
         (make-parametric (make-vector 0 (third r0) (second r1))
                          (make-vector 1 0 0))]
        [(= (length r1) 2) ;second column is nonpivot (y free)
         (make-parametric (make-vector (fourth r0) 0 (second r1))
                          (make-vector 0 1 0))]
        [else              ;third column is nonpivot (z free)
         (make-parametric (make-vector (fourth r0) (third r1) 0)
                          (make-vector (- (third r0)) (- (second r1)) 1))]))



(@htdf trim-zeros)
(@signature (listof Number) -> Row)
;; converts a list of numbers to a row by trimming all leading zeros
;; CONSTRAINT: the list must contain at least one nonzero number
;!!! tests

(@template-origin (listof Number))

(define (trim-zeros lon)
  (cond [(empty? lon)
         (error "Invalid plane given")]
        [else
         (if (zero? (first lon))
             (trim-zeros (rest lon))
             lon)]))



(@htdf normalize normalize--acc)
(@signature Row -> Row)
;; produce row divided by its first nonzero element
;; CONSTRAINT: the row must contain at least one nonzero number
;!!! tests

(@template-origin accumulator)

(define (normalize r)
  (normalize--acc (rest r) (first r) (list 1)))

(@template-origin Row accumulator)

;; div is Number
;; INVARIANT: the number by which the entire row is divided
;;
;; rsf is (listof Number)
;; INVARIANT: all numbers normalized so far, in reverse row order
(define (normalize--acc r div rsf)
  (cond [(empty? r)
         (reverse rsf)]
        [else
         (normalize--acc (rest r) div (cons (/ (first r) div) rsf))]))



(@htdf eliminate eliminate--acc)
(@signature Row Row -> Row)
;; produce the first row minus the second row
;; CONSTRAINT: both rows must represent valid planes in R^3;
;;             the first row must be at least as long as the second
(check-expect (eliminate (list 2 4 5 2) (list 1 2 3 -4)) (list -1 10))
;!!! more tests

(@template-origin accumulator)

(define (eliminate r0 r1)
  (trim-zeros (append (take r0 (- (length r0) (length r1)))
                      (eliminate--acc
                       (drop r0 (- (length r0) (length r1))) r1
                       (/ (first (drop r0 (- (length r0) (length r1))))
                          (first r1))
                       empty))))

(@template-origin Row accumulator)

;; ratio is (listof Number)
;; INVARIANT: the ratio of the first component of the first row to the
;;            first component of the second row
;;
;; rsf is (listof Number)
;; INVARIANT: all components of the difference computed so far, in reverse order
(define (eliminate--acc r0 r1 ratio rsf)
  (cond [(empty? r0)
         (reverse rsf)]
        [else
         (eliminate--acc (rest r0) (rest r1) ratio
                         (cons (- (first r0) (* ratio (first r1))) rsf))]))



(@htdf change-of-basis)
(@signature Vector Vector -> Matrix)
;; given a basis B={v0, v1}, produce the matrix from v to [v]_B for v in Span(B)
;; CONSTRAINT: {v0, v1} must form a basis for a subspace of R^3 of dimension 2
;!!! tests

(@template-origin fn-composition)

(define (change-of-basis v0 v1)
  (change-of-basis-r3 v0 v1 (cross-product v0 v1)))



(@htdf change-of-basis-r3)
(@signature Vector Vector Vector -> Matrix)
;; given a basis B={v0, v1, v2}, produce the matrix from v to [v]_B for v in R^3
;; CONSTRAINT: {v0, v1, v2} must span R^3
;!!! tests

(@template-origin fn-composition)

(define (change-of-basis-r3 v0 v1 v2)
  (invert (basis->standard-r3 v0 v1 v2)))



(@htdf basis->standard)
(@signature Vector Vector -> Matrix)
;; given a basis B={v0, v1}, produce the matrix from B to the standard basis
;; CONSTRAINT: {v0, v1} must form a basis for a subspace of R^3 of dimension 2
;!!! tests

(@template-origin fn-composition)

(define (basis->standard v0 v1)
  (basis->standard-r3 v0 v1 ZERO-VECTOR))



(@htdf basis->standard-r3)
(@signature Vector Vector Vector -> Matrix)
;; given a basis B={v0, v1, v2}, produce the matrix from B to the standard basis
;; CONSTRAINT: {v0, v1, v2} must span R^3
;!!! tests

(@template-origin Vector)

(define (basis->standard-r3 v0 v1 v2)
  (make-matrix (vector-x v0) (vector-x v1) (vector-x v2)
               (vector-y v0) (vector-y v1) (vector-y v2)
               (vector-z v0) (vector-z v1) (vector-z v2)))
