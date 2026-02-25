;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname matrix) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)
(require 2htdp/image)

(require "provide.rkt")
(provide (all-defined-out))

(require "common.rkt")
(@htdd Colour Vector Euler Triangle)

;;
;; MATRIX.rkt
;;
;; 3x3 matrix data definition and basic operations
;;


;;
;; DATA DEFINITIONS
;;


(@htdd Matrix)
(define-struct matrix (m11 m12 m13 m21 m22 m23 m31 m32 m33))
;; Matrix is (make-matrix Number Number Number
;;                        Number Number Number
;;                        Number Number Number)
;; interp. a 3x3 matrix
(define IDENTITY (make-matrix 1 0 0
                              0 1 0
                              0 0 1))
(define ZERO-MATRIX (make-matrix 0 0 0
                                 0 0 0
                                 0 0 0))
(define MATRIX1 (make-matrix 2 1 0
                             0 2 0
                             0 0 4))

(@dd-template-rules compound) ;9 fields

(define (fn-for-matrix m)
  (... (matrix-m11 m) (matrix-m12 m) (matrix-m13 m)   ;Number Number Number
       (matrix-m21 m) (matrix-m22 m) (matrix-m23 m)   ;Number Number Number
       (matrix-m31 m) (matrix-m32 m) (matrix-m33 m))) ;Number Number Number


;;
;; FUNCTIONS
;;


(@htdf matrix-multiply)
(@signature Matrix Matrix -> Matrix)
;; produce the product of two 3x3 matrices
;!!! tests

(@template-origin Matrix)

(define (matrix-multiply m0 m1)
  (make-matrix (+ (* (matrix-m11 m0) (matrix-m11 m1))
                  (* (matrix-m12 m0) (matrix-m21 m1))
                  (* (matrix-m13 m0) (matrix-m31 m1)))
               (+ (* (matrix-m11 m0) (matrix-m12 m1))
                  (* (matrix-m12 m0) (matrix-m22 m1))
                  (* (matrix-m13 m0) (matrix-m32 m1)))
               (+ (* (matrix-m11 m0) (matrix-m13 m1))
                  (* (matrix-m12 m0) (matrix-m23 m1))
                  (* (matrix-m13 m0) (matrix-m33 m1)))
               (+ (* (matrix-m21 m0) (matrix-m11 m1))
                  (* (matrix-m22 m0) (matrix-m21 m1))
                  (* (matrix-m23 m0) (matrix-m31 m1)))
               (+ (* (matrix-m21 m0) (matrix-m12 m1))
                  (* (matrix-m22 m0) (matrix-m22 m1))
                  (* (matrix-m23 m0) (matrix-m32 m1)))
               (+ (* (matrix-m21 m0) (matrix-m13 m1))
                  (* (matrix-m22 m0) (matrix-m23 m1))
                  (* (matrix-m23 m0) (matrix-m33 m1)))
               (+ (* (matrix-m31 m0) (matrix-m11 m1))
                  (* (matrix-m32 m0) (matrix-m21 m1))
                  (* (matrix-m33 m0) (matrix-m31 m1)))
               (+ (* (matrix-m31 m0) (matrix-m12 m1))
                  (* (matrix-m32 m0) (matrix-m22 m1))
                  (* (matrix-m33 m0) (matrix-m32 m1)))
               (+ (* (matrix-m31 m0) (matrix-m13 m1))
                  (* (matrix-m32 m0) (matrix-m23 m1))
                  (* (matrix-m33 m0) (matrix-m33 m1)))))



(@htdf transform)
(@signature Matrix Vector -> Vector)
;; produce the product of the given 3x3 matrix and vector in R^3
(check-expect (transform MATRIX1 ZERO-VECTOR) ZERO-VECTOR)
(check-expect (transform ZERO-MATRIX VECTOR2) ZERO-VECTOR)
(check-expect (transform IDENTITY VECTOR2) VECTOR2)
;!!! more tests

(@template-origin Matrix Vector)

(define (transform m v)
  (make-vector (+ (* (matrix-m11 m) (vector-x v))
                  (* (matrix-m12 m) (vector-y v))
                  (* (matrix-m13 m) (vector-z v)))
               (+ (* (matrix-m21 m) (vector-x v))
                  (* (matrix-m22 m) (vector-y v))
                  (* (matrix-m23 m) (vector-z v)))
               (+ (* (matrix-m31 m) (vector-x v))
                  (* (matrix-m32 m) (vector-y v))
                  (* (matrix-m33 m) (vector-z v)))))



(@htdf det)
(@signature Matrix -> Number)
;; produce the determinant of the given 3x3 matrix
(check-expect (det IDENTITY) 1)
(check-expect (det ZERO-MATRIX) 0)
;!!! more tests

(@template-origin Matrix)

(define (det m)
  (- (+ (* (matrix-m11 m) (matrix-m22 m) (matrix-m33 m))
        (* (matrix-m12 m) (matrix-m23 m) (matrix-m31 m))
        (* (matrix-m13 m) (matrix-m21 m) (matrix-m32 m)))
     (+ (* (matrix-m13 m) (matrix-m22 m) (matrix-m31 m))
        (* (matrix-m12 m) (matrix-m21 m) (matrix-m33 m))
        (* (matrix-m11 m) (matrix-m23 m) (matrix-m32 m)))))



(@htdf adj)
(@signature Matrix -> Matrix)
;; produce the adjugate of the given 3x3 matrix
(check-expect (adj IDENTITY) IDENTITY)
(check-expect (adj ZERO-MATRIX) ZERO-MATRIX)
;!!! more tests

(@template-origin Matrix)

(define (adj m)
  (make-matrix (- (* (matrix-m22 m) (matrix-m33 m))
                  (* (matrix-m23 m) (matrix-m32 m)))
               (- (* (matrix-m32 m) (matrix-m13 m))
                  (* (matrix-m33 m) (matrix-m12 m)))
               (- (* (matrix-m12 m) (matrix-m23 m))
                  (* (matrix-m13 m) (matrix-m22 m)))
               (- (* (matrix-m23 m) (matrix-m31 m))
                  (* (matrix-m21 m) (matrix-m33 m)))
               (- (* (matrix-m33 m) (matrix-m11 m))
                  (* (matrix-m31 m) (matrix-m13 m)))
               (- (* (matrix-m13 m) (matrix-m21 m))
                  (* (matrix-m11 m) (matrix-m23 m)))
               (- (* (matrix-m21 m) (matrix-m32 m))
                  (* (matrix-m22 m) (matrix-m31 m)))
               (- (* (matrix-m31 m) (matrix-m12 m))
                  (* (matrix-m32 m) (matrix-m11 m)))
               (- (* (matrix-m11 m) (matrix-m22 m))
                  (* (matrix-m12 m) (matrix-m21 m)))))



(@htdf invert)
(@signature Matrix -> Matrix)
;; produce the inverse of the given 3x3 matrix
;; CONSTRAINT: the given matrix must be invertible, i.e. (det m) is nonzero
(check-expect (invert IDENTITY) IDENTITY)
;!!! more tests

(@template-origin fn-composition)

(define (invert m)
  (adjugate->inverse (adj m) (det m)))



(@htdf adjugate->inverse)
(@signature Matrix Number -> Matrix)
;; produce the inverse of some matrix given its adjugate and determinant
;; CONSTRAINT: the determinant must be nonzero
;!!! tests

(@template-origin Matrix)

(define (adjugate->inverse a d)
  (make-matrix (/ (matrix-m11 a) d) (/ (matrix-m12 a) d) (/ (matrix-m13 a) d)
               (/ (matrix-m21 a) d) (/ (matrix-m22 a) d) (/ (matrix-m23 a) d)
               (/ (matrix-m31 a) d) (/ (matrix-m32 a) d) (/ (matrix-m33 a) d)))
