;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname buffers) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)

(require "provide.rkt")
(provide (all-defined-out))

(require "common.rkt")
(@htdd Colour Vector Euler Triangle)

(require "bst.rkt")
(@htdd BST)

;;
;; BUFFERS.rkt
;;
;; Data definitions and functions for vertex buffers and element buffers
;;


;;
;; DATA DEFINITIONS
;;


(@htdd VertexBuffer)
;; VertexBuffer is one of:
;;  - empty
;;  - (cons Vector VertexBuffer)
;; interp. a list of (ideally unique) vertices of a triangular mesh,
;;         similar to a VBO in OpenGL
(define VBUF0 empty)
(define VBUF1 (list ORIGIN
                    (make-vector 1 0 0)
                    (make-vector 0 1 0)
                    (make-vector 0 0 1)))

(@dd-template-rules one-of          ;2 cases
                    atomic-distinct ;empty
                    compound        ;(cons Vector VertexBuffer)
                    ref             ;(first VertexBuffer) is Vector
                    self-ref)       ;(rest VertexBuffer) is VertexBuffer

(define (fn-for-vbuf vbuf)
  (cond [(empty? vbuf)
         (...)]
        [else
         (... (fn-for-vector (first vbuf))
              (fn-for-vbuf (rest vbuf)))]))



(@htdd Element)
(define-struct element (v0 v1 v2))
;; Element is (make-element Natural Natural Natural)
;; interp. the indices of the three vertices of a triangular mesh element
;;         in the VertexBuffer to which this element corresponds
;; CONSTRAINT: No two vertices can be the same, and the resulting
;;             triangular element must not be degenerate; v0, v1, v2 must
;;             be less than the length of the corresponding VertexBuffer
(define ELEMENT1 (make-element 0 1 2))
(define ELEMENT2 (make-element 2 1 0))

(@dd-template-rules compound) ;3 fields

(define (fn-for-element e)
  (... (element-v0 e)
       (element-v1 e)
       (element-v2 e)))



(@htdd ElementBuffer)
;; ElementBuffer is one of:
;;  - empty
;;  - (cons Element ElementBuffer)
;; interp. a list of VertexBuffer indices of vertices of triangular elements
;;         constituting a mesh, similar to an EBO in OpenGL
;; N.B. The purpose of this type is to reduce the number of (per-frame) matrix
;;      transformations required, unlike in OpenGL where EBOs are primarily
;;      used for optimizing memory consumption (which is not a focus here).
(define EBUF0 empty)
(define EBUF1 (list (make-element 0 1 2)
                    (make-element 0 2 3)
                    (make-element 0 3 1)
                    (make-element 1 3 2)))

(@dd-template-rules one-of          ;2 cases
                    atomic-distinct ;empty
                    compound        ;(cons Element ElementBuffer)
                    ref             ;(first ElementBuffer) is Element
                    self-ref)       ;(rest ElementBuffer) is ElementBuffer

(define (fn-for-ebuf ebuf)
  (cond [(empty? ebuf)
         (...)]
        [else
         (... (fn-for-element (first ebuf))
              (fn-for-ebuf (rest ebuf)))]))



(@htdd Mesh)
(define-struct mesh (vertices elements))
;; Mesh is (make-mesh VertexBuffer ElementBuffer)
;; interp. the unique vertices and triangular elements of a mesh
(define MESH0 (make-mesh empty empty))
(define MESH1 (make-mesh VBUF1 EBUF1))

(@dd-template-rules compound ;2 fields
                    ref      ;(mesh-vertices Mesh) is VertexBuffer
                    ref)     ;(mesh-elements Mesh) is ElementBuffer

(define (fn-for-mesh m)
  (... (fn-for-vbuf (mesh-vertices m))
       (fn-for-ebuf (mesh-elements m))))


;;
;; FUNCTIONS
;;


(@htdf element->triangle)
(@signature VertexBuffer Element -> Triangle)
;; produce the actual positions of the vertices of the given element
(check-expect (element->triangle VBUF1 ELEMENT1)
              (make-poly ORIGIN
                         (make-vector 1 0 0)
                         (make-vector 0 1 0)))
(check-expect (element->triangle VBUF1 ELEMENT2)
              (make-poly (make-vector 0 1 0)
                         (make-vector 1 0 0)
                         ORIGIN))

(@template-origin Element)

(define (element->triangle vbuf e)
  (make-poly (list-ref vbuf (element-v0 e))
             (list-ref vbuf (element-v1 e))
             (list-ref vbuf (element-v2 e))))



(@htdf element->triangle/bst)
(@signature BST Element -> Triangle)
;; produce the actual positions of the vertices of the given element given a BST
;; CONSTRAINT: the set of BST values must consist only of vectors
(check-expect (element->triangle/bst (construct-bst VBUF1 (length VBUF1))
                                     ELEMENT1)
              (make-poly ORIGIN
                         (make-vector 1 0 0)
                         (make-vector 0 1 0)))
(check-expect (element->triangle/bst (construct-bst VBUF1 (length VBUF1))
                                     ELEMENT2)
              (make-poly (make-vector 0 1 0)
                         (make-vector 1 0 0)
                         ORIGIN))

(@template-origin Element)

(define (element->triangle/bst vbst e)
  (make-poly (lookup vbst (element-v0 e))
             (lookup vbst (element-v1 e))
             (lookup vbst (element-v2 e))))
