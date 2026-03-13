;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname render) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)

(require "provide.rkt")
(provide (matching-identifiers-out #rx"^((?!--).)*$" (all-defined-out)))

(require "common.rkt")
(@htdd Colour Vector Euler Triangle)

(require "matrix.rkt")
(@htdd Matrix)

(require "vector.rkt")
(@htdd Plane Line)

;;
;; RENDER.rkt
;;
;; Main racket3d rendering pipeline
;;


#|

WIP RACKET3D RENDERING PIPELINE

Phase 1 - merging element and vertex buffers; for each mesh face added to the
          buffer, perform a comparison with each existing element as follows:

1. Compute distance between centroids. If distance is greater than or equal to
   the sum of the greatest distances between the centroid and farthest vertex
   in both triangles, proceed to the next comparison.
2. Check if the two triangles intersect.
   2a. Transform one triangle into a basis B formed by two edges of the other
       triangle as well as their cross product, with the origin at its v0.
   2b. Consider each edge of the first triangle as a parametric line; call a
       point on the line v. Compute v such that the z-component of [v]_B is 0.
   2c. Check if the sum of the x/y components of [v]_B are in (DELTA, 1-DELTA).
   2d. If no intersection is found, repeat this for the other triangle.
   2e. If still no intersection is found, proceed to the next comparison.
3. Subdivide both triangles along their line of intersection.
   3a. Find the line of intersection by projecting a (non-perpendicular) edge
       of one triangle onto the plane of the other triangle (using the basis).
   3b. For each subdivision:
   3c. Determine which two edges are intersected by the line.
   3d. Create a mesh face from the triangle created by cutting along the line.
   3e. Divide the remaining quadrilateral into two triangular mesh faces.
4. The existing mesh face that has been subdivided is reinserted into the list
   without checks. The new mesh faces are inserted with this procedure.
   4a. If possible, skip checks for all polygons that have been checked before.

Phase 2 - camera matrix transformation + BST construction using vertex buffer,
          reconstructing polygons from element buffer, depth testing

|#


;;
;; DATA DEFINITIONS
;;


(@htdd Surface)
(define-struct surface (position basis))
;; Surface is (make-surface Vector Matrix)
;; interp. a triangular surface, defined by its position vector and a matrix
;;         transform from the standard basis to the basis defined by its edges
;; CONSTRAINT: basis must be an invertible matrix
(define SURFACE1
  (make-surface ORIGIN IDENTITY))
(define SURFACE2
  (make-surface (poly-v0 TRIANGLE3)
                (change-of-basis
                 (sub (poly-v1 TRIANGLE3) (poly-v0 TRIANGLE3))
                 (sub (poly-v2 TRIANGLE3) (poly-v0 TRIANGLE3)))))

(@dd-template-rules compound ;2 fields
                    ref      ;(surface-pos Surface) is Vector
                    ref)     ;(surface-basis Surface) is Matrix

(define (fn-for-surface s)
  (... (fn-for-vector (surface-pos s))
       (fn-for-matrix (surface-basis s))))


;;
;; FUNCTIONS
;;


(@htdf triangle-intersect)
(@signature Triangle Triangle -> Line or false)
;; produce the line of intersection of the triangles, false if it does not exist
;!!! tests

(@template-origin fn-composition)

(define (triangle-intersect t0 t1)
  (triangle-intersect/surface t0 t1
                              (triangle->surface t0) (triangle->surface t1)))



(@htdf triangle-intersect/surface)
(@signature Triangle Triangle Surface Surface -> Line or false)
;; produce the line of intersection of the triangles given their surfaces
;!!! tests

(@template-origin Surface)

(define (triangle-intersect/surface t0 t1 s0 s1)
  (triangle-intersect/lines
   t0 t1 s0 s1
   (triangle->lines (transform-triangle
                     (translate-triangle t0 (negate (surface-position s1)))
                     s1))
   (triangle->lines (transform-triangle
                     (translate-triangle t1 (negate (surface-position s0)))
                     s0))))



(@htdf triangle-intersect/lines)
(@signature Triangle Triangle Surface Surface (listof Line) (listof Line)
            -> Line or false)
;; produce the line of intersection of the triangles given their surfaces/edges
;!!! tests

(@template-origin Triangle)

(define (triangle-intersect/lines t0 t1 s0 s1 e0 e1)
  (cond [(edges-intersect? e0)
         (transform-line (project-line-xy (find-first-nonperpendicular e0)) s1
                         (basis->standard (sub (poly-v1 t1) (poly-v0 t1))
                                          (sub (poly-v2 t1) (poly-v0 t1))))]
        [(edges-intersect? e1)
         (transform-line (project-line-xy (find-first-nonperpendicular e1)) s0
                         (basis->standard (sub (poly-v1 t0) (poly-v0 t0))
                                          (sub (poly-v2 t0) (poly-v0 t0))))]
        [else
         false]))



(@htdf find-first-nonperpendicular)
(@signature (listof Line) -> Line)
;; produce the first line with a direction vector with nonzero x or y component
;; CONSTRAINT: the given list must contain at least one such line
;!!! tests

(@template-origin (listof Line))

(define (find-first-nonperpendicular lines)
  (cond [(empty? lines)
         (error "Triangle is degenerate")]
        [else
         (if (surface-perpendicular? (first lines))
             (find-first-nonperpendicular (rest lines))
             (first lines))]))



(@htdf edges-intersect?)
(@signature (listof Line) -> Boolean)
;; produce true iff any edge passes through the point (x, y, 0), x+y in (Δ, 1-Δ)
;!!! tests

(@template-origin (listof Line)) ;treat as compound data

(define (edges-intersect? edges)
  (or (line-intersect? (first edges))
      (line-intersect? (second edges))
      (line-intersect? (third edges))))



(@htdf line-intersect?)
(@signature Line -> Boolean)
;; produce true iff the line passes through the point (x, y, 0), x+y in (Δ, 1-Δ)
;!!! tests

(@template-origin Line)

(define (line-intersect? l)
  (if (zero? (vector-z (parametric-direction l)))
      false
      (on-surface? (add (parametric-position l)
                        (scalar-multiply
                         (parametric-direction l)
                         (zero-parameter (parametric-position l)
                                         (parametric-direction l)))))))



(@htdf on-surface?)
(@signature Vector -> Boolean)
;; produce true iff the vector's components sum is in (DELTA, 1-DELTA)
;; CONSTRAINT: the z component of the vector must be zero
;!!! tests

(@template-origin Vector)

(define (on-surface? v)
  (< DELTA (+ (vector-x v) (vector-y v)) (- 1 DELTA)))



(@htdf zero-parameter)
(@signature Vector Vector -> Number)
;; produce the parameter for which z=0 given vectors defining a parametric line
;; CONSTRAINT: the line must not be parallel to z=0
;!!! tests

(@template-origin Vector)

(define (zero-parameter pos dir)
  (- (/ (vector-z pos) (vector-z dir))))



(@htdf surface-perpendicular?)
(@signature Vector -> Boolean)
;; produce true iff the given vector's x and y components are both zero
;!!! tests

(@template-origin Vector)

(define (surface-perpendicular? v)
  (and (zero? (vector-x v)) (zero? (vector-y v))))



(@htdf triangle->lines)
(@signature Triangle -> (listof Line))
;; produce the lines containing the three edges of the triangle
;!!! tests

(@template-origin fn-composition)

(define (triangle->lines t)
  (list (vectors->line (poly-v0 t) (poly-v1 t))
        (vectors->line (poly-v0 t) (poly-v2 t))
        (vectors->line (poly-v1 t) (poly-v2 t))))



(@htdf triangle->surface)
(@signature Triangle -> Surface)
;; produce the surface corresponding to the given triangle
(check-expect (triangle->surface (make-poly ORIGIN
                                            (make-vector 1 0 0)
                                            (make-vector 0 1 0)))
              SURFACE1)
(check-expect (triangle->surface TRIANGLE3) SURFACE2)

(@template-origin Triangle)

(define (triangle->surface t)
  (make-surface (poly-v0 t)
                (change-of-basis (sub (poly-v1 t) (poly-v0 t))
                                 (sub (poly-v2 t) (poly-v0 t)))))



(@htdf project-line-xy)
(@signature Line -> Line)
;; produce the given line projected onto the xy plane
;!!! tests

(@template-origin Line)

(define (project-line-xy l)
  (make-parametric (project-vector-xy (parametric-position l))
                   (project-vector-xy (parametric-direction l))))



(@htdf project-vector-xy)
(@signature Vector -> Vector)
;; produce the given vector projected onto the xy plane
;!!! tests

(@template-origin Vector)

(define (project-vector-xy v)
  (make-vector (vector-x v) (vector-y v) 0))



(@htdf transform-line)
(@signature Line Surface Matrix -> Line)
;; produce transformation of the line from the surface basis to standard basis
;!!! tests

(@template-origin Line)

(define (transform-line l s b)
  (make-parametric (add (transform b (parametric-position l))
                        (surface-position s))
                   (transform b (parametric-direction l))))



(@htdf transform-triangle)
(@signature Triangle Surface -> Triangle)
;; produce the triangle transformed into the basis of the given surface
;!!! tests

(@template-origin Surface)

(define (transform-triangle t s)
  (make-poly (transform (surface-basis s) (poly-v0 t))
             (transform (surface-basis s) (poly-v1 t))
             (transform (surface-basis s) (poly-v2 t))))



(@htdf translate-triangle)
(@signature Triangle Vector -> Triangle)
;; produce the translation of the given triangle by the given vector
;!!! tests

(@template-origin Triangle)

(define (translate-triangle t v)
  (make-poly (add v (poly-v0 t))
             (add v (poly-v1 t))
             (add v (poly-v2 t))))
