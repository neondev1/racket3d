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
   2c. Check if the x and y components of [v]_B are in (DELTA, 1-DELTA).
   2d. If no intersection is found, repeat this for the other triangle.
   2e. If still no intersection is found, proceed to the next comparison.
3. Subdivide both triangles along their line of intersection.
   3a. For each subdivision:
   3b. Find the line of intersection by projecting a (non-perpendicular) edge
       of the other triangle onto the plane of the current triangle.
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
(define-struct surface (pos v0 v1 basis))
;; Surface is (make-surface Vector Vector Vector Matrix)
;; interp. a triangular surface, defined by its position vector and two basis
;;         vectors corresponding to its edges, as well as a matrix transform
;;         from the standard basis to the basis {v0, v1}
;; CONSTRAINT: {v0, v1} must form a basis for a subspace of R^3 of dimension 2;
;;             basis must map coordinates from the standard basis to said basis
(define SURFACE1
  (make-surface ORIGIN (make-vector 1 0 0) (make-vector 0 1 0) IDENTITY))
(define SURFACE2
  (make-surface (poly-v0 TRIANGLE3)
                (sub (poly-v1 TRIANGLE3) (poly-v0 TRIANGLE3))
                (sub (poly-v2 TRIANGLE3) (poly-v0 TRIANGLE3))
                (change-of-basis
                 (sub (poly-v1 TRIANGLE3) (poly-v0 TRIANGLE3))
                 (sub (poly-v2 TRIANGLE3) (poly-v0 TRIANGLE3)))))

(@dd-template-rules compound ;4 fields
                    ref      ;(surface-pos Surface) is Vector
                    ref      ;(surface-v0 Surface) is Vector
                    ref      ;(surface-v1 Surface) is Vector
                    ref)     ;(surface-basis Surface) is Matrix

(define (fn-for-surface s)
  (... (fn-for-vector (surface-pos s))
       (fn-for-vector (surface-v0 s))
       (fn-for-vector (surface-v1 s))
       (fn-for-matrix (surface-basis s))))


;;
;; FUNCTIONS
;;


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
                (sub (poly-v1 t) (poly-v0 t)) (sub (poly-v2 t) (poly-v0 t))
                (change-of-basis (sub (poly-v1 t) (poly-v0 t))
                                 (sub (poly-v2 t) (poly-v0 t)))))
