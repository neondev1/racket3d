;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname render) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)

(require "provide.rkt")
(provide (matching-identifiers-out #rx"^((?!--).)*$" (all-defined-out)))

(require "common.rkt")
(@htdd Colour Vector Euler Triangle)

(require "vector.rkt")
(@htdd Plane Line)

(require "matrix.rkt")
(@htdd Matrix)

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
   in both triangles, skip the remainder of this comparison.
2. Check if the two triangles intersect.
   2a. Transform one triangle into a basis B formed by two edges of the other
       triangle as well as their cross product, with the origin at its v0.
   2b. Consider each edge of the first triangle as a parametric line; call one
       such line v. Compute the point at which the z-component of [v]_B is 0.
   2c. Check if the x and y components of [v]_B are in [DELTA, 1-DELTA].
3. Compute line of intersection between planes containing both triangles.
4. Subdivide both triangles along line of intersection.
   4a. For each subdivision:
   4b. Transform the intersection into the basis of the triangle per above.
   4c. Determine which two edges are intersected by the line.
   4d. Create a mesh face from the triangle created by cutting along the line.
   4e. Divide remaining quadrilateral into two triangular mesh faces.
5. The existing mesh face that has been subdivided is reinserted into the list
   without checks. The new mesh faces are inserted with this procedure.
   5a. If possible, skip checks for all polygons that have been checked before.

Phase 2 - camera matrix transformation + BST construction using vertex buffer,
          reconstructing polygons from element buffer, depth testing

|#

