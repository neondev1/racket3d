;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname projection) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)
(require 2htdp/image)

(require "provide.rkt")
(provide (all-defined-out))

(require "common.rkt")
(@htdd Colour Point Euler Triangle)

(require "vector.rkt")
(@htdd Vector Plane Line)

;;
;; PROJECTION.rkt
;;
;; Data definitions and functions for render queuing
;; and projecting 3D meshes to screen coordinates
;;


;;
;; DATA DEFININTIONS
;;


(@htdd Element)
(define-struct element (face centroid))
;; Element is (make-element Triangle Point)
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
;; FUNCTIONS
;;

;!!!
