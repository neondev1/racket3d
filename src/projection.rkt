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
;; Data definitions and functions for matrix arithmetic
;; and projection of 3D meshes to screen coordinates
;;


(define VIEW-WIDTH  480)
(define VIEW-HEIGHT 480)


;;
;; DATA DEFININTIONS
;;


(@htdd Spherical)
(define-struct spherical (r pitch yaw))
;; Spherical is (make-spherical Number Number Number)
;; interp. spherical coordinates for camera position, with angles in degrees;
;;         note that the definition used by Racket3D is atypical.
;;         pitch is the angle of the position vector from the xz-plane,
;;         with elevation being positive and depression being negative, while
;;         yaw is the (counterclockwise) azimuthal angle measured from
;;         the positive x-axis. This convention is due to the vertical y-axis.
;; CONSTRAINT: r must be positive and pitch must be in [-90, 90]
;!!! examples

(@dd-template-rules compound) ;3 fields

(define (fn-for-spherical s)
  (... (spherical-r s)
       (spherical-pitch s)
       (spherical-yaw s)))



(@htdd Homogeneous)
(define-struct homogeneous (x y z w))
;; Homogeneous is (make-homo Number Number Number Number)
;; interp. homogeneous coordinates for matrix transformations
(define HOMO0 (make-homogeneous 0 0 0 1))
(define HOMO1 (make-homogeneous (point-x POINT1)
                                (point-y POINT1)
                                (point-z POINT1) 1))

(@dd-template-rules compound) ;4 fields

(define (fn-for-homo h)
  (... (homogeneous-x h)
       (homogeneous-y h)
       (homogeneous-z h)
       (homogeneous-w h)))



(@htdd Matrix)
(define-struct matrix
  (m11 m12 m13 m14 m21 m22 m23 m24 m31 m32 m33 m34 m41 m42 m43 m44))
;; Matrix is (make-matrix Number Number Number Number
;;                        Number Number Number Number
;;                        Number Number Number Number
;;                        Number Number Number Number)
;; interp. a 4x4 matrix
(define IDENTITY (make-matrix 1 0 0 0
                              0 1 0 0
                              0 0 1 0
                              0 0 0 1))
(define MATRIX1 (make-matrix 2 1 0 0
                             0 2 0 0
                             0 0 4 1
                             0 0 0 4))

(@dd-template-rules compound) ;16 fields

(define (fn-for-matrix m)
  (... (matrix-m11 m) (matrix-m12 m) (matrix-m13 m) (matrix-m14 m)
       (matrix-m21 m) (matrix-m22 m) (matrix-m23 m) (matrix-m24 m)
       (matrix-m31 m) (matrix-m32 m) (matrix-m33 m) (matrix-m34 m)
       (matrix-m41 m) (matrix-m42 m) (matrix-m43 m) (matrix-m44 m)))


;;
;; FUNCTIONS
;;


(@htdf camera-matrix)
(@signature Spherical -> Matrix)
;; produce 3D projection matrix from coordinates of camera
;!!! examples

(@template-origin Spherical)

(define (camera-matrix s)
  (matrix-multiply
   (projection-matrix (/ 2 VIEW-HEIGHT))
   (matrix-multiply (rotation-matrix (spherical-yaw s)
                                     (spherical-pitch s) 0)
                    (translation-matrix (negate (spherical->vector s))))))



(@htdf spherical->vector)
(@signature Spherical -> Vector)
;; convert spherical coordinates into a position vector
;!!! examples

(@template-origin Spherical)

(define (spherical->vector s)
  (make-vector (* (spherical-r s)
                  (cos (spherical-pitch s))
                  (cos (spherical-yaw s)))
               (* (spherical-r s)
                  (sin (spherical-pitch s)))
               (* (spherical-r s)
                  (cos (spherical-pitch s))
                  (sin (spherical-yaw s)))))



(@htdf transform)
(@signature Matrix Vector -> Vector)
;; apply transformation matrix to vector
;!!! examples

(@template-origin fn-composition)

(define (transform m v)
  (homogeneous->vector (transform-homogeneous m (vector->homogeneous v))))



(@htdf vector->homogeneous)
(@signature Vector -> Homogeneous)
;; convert vector to homogeneous coordinates
;!!! examples

(@template-origin Vector)

(define (vector->homogeneous v)
  (make-homogeneous (vector-x v)
                    (vector-y v)
                    (vector-z v)
                    1))



(@htdf homogeneous->vector)
(@signature Homogeneous -> Vector)
;; convert homogeneous coordinates to vector
;!!! examples

(@template-origin Homogeneous)

(define (homogeneous->vector h)
  (if (zero? (homogeneous-w h))
      ZERO-VECTOR
      (make-vector (/ (homogeneous-x h) (homogeneous-w h))
                   (/ (homogeneous-y h) (homogeneous-w h))
                   (homogeneous-z h)))) ;leave z untouched (yes this is cursed)



(@htdf translation-matrix)
(@signature Vector -> Matrix)
;; produce translation matrix from given vector

(@template-origin Vector)

(define (translation-matrix v)
  (make-matrix 1 0 0 (vector-x v)
               0 1 0 (vector-y v)
               0 0 1 (vector-z v)
               0 0 0 1))



(@htdf rotation-matrix)
(@signature Number Number -> Matrix)
;; produce rotation matrix from given yaw, pitch and roll

(@template-origin Number)

(define (rotation-matrix yaw pitch roll)
  (matrix-multiply
   (matrix-multiply
    (make-matrix 1  0          0              0
                 0  (cos roll) (- (sin roll)) 0
                 0  (sin roll) (cos roll)     0
                 0  0          0              1)    ;finally roll
    (make-matrix (cos pitch)     0  (sin pitch) 0
                 0               1  0           0
                 (- (sin pitch)) 0  (cos pitch) 0
                 0               0  0           1)) ;then pitch
   (make-matrix (cos yaw) (- (sin yaw)) 0  0
                (sin yaw) (cos yaw)     0  0
                0         0             1  0
                0         0             0  1)))     ;yaw first



(@htdf projection-matrix)
(@signature Number -> Matrix)
;; produce projection matrix onto plane z=1/r
;!!! examples

(@template-origin Number)

(define (projection-matrix r)
  (make-matrix 1 0 0 0
               0 1 0 0
               0 0 1 0
               0 0 r 0))



(@htdf matrix-multiply)
(@signature Matrix Matrix -> Matrix)
;; produce product of two 4x4 matrices, m0 * m1
;!!! examples

(@template-origin Matrix)

(define (matrix-multiply m0 m1)
  (make-matrix (+ (* (matrix-m11 m0) (matrix-m11 m1))
                  (* (matrix-m12 m0) (matrix-m21 m1))
                  (* (matrix-m13 m0) (matrix-m31 m1))
                  (* (matrix-m14 m0) (matrix-m41 m1)))
               (+ (* (matrix-m11 m0) (matrix-m12 m1))
                  (* (matrix-m12 m0) (matrix-m22 m1))
                  (* (matrix-m13 m0) (matrix-m32 m1))
                  (* (matrix-m14 m0) (matrix-m42 m1)))
               (+ (* (matrix-m11 m0) (matrix-m13 m1))
                  (* (matrix-m12 m0) (matrix-m23 m1))
                  (* (matrix-m13 m0) (matrix-m33 m1))
                  (* (matrix-m14 m0) (matrix-m43 m1)))
               (+ (* (matrix-m11 m0) (matrix-m14 m1))
                  (* (matrix-m12 m0) (matrix-m24 m1))
                  (* (matrix-m13 m0) (matrix-m34 m1))
                  (* (matrix-m14 m0) (matrix-m44 m1)))
               (+ (* (matrix-m21 m0) (matrix-m11 m1))
                  (* (matrix-m22 m0) (matrix-m21 m1))
                  (* (matrix-m23 m0) (matrix-m31 m1))
                  (* (matrix-m24 m0) (matrix-m41 m1)))
               (+ (* (matrix-m21 m0) (matrix-m12 m1))
                  (* (matrix-m22 m0) (matrix-m22 m1))
                  (* (matrix-m23 m0) (matrix-m32 m1))
                  (* (matrix-m24 m0) (matrix-m42 m1)))
               (+ (* (matrix-m21 m0) (matrix-m13 m1))
                  (* (matrix-m22 m0) (matrix-m23 m1))
                  (* (matrix-m23 m0) (matrix-m33 m1))
                  (* (matrix-m24 m0) (matrix-m43 m1)))
               (+ (* (matrix-m21 m0) (matrix-m14 m1))
                  (* (matrix-m22 m0) (matrix-m24 m1))
                  (* (matrix-m23 m0) (matrix-m34 m1))
                  (* (matrix-m24 m0) (matrix-m44 m1)))
               (+ (* (matrix-m31 m0) (matrix-m11 m1))
                  (* (matrix-m32 m0) (matrix-m21 m1))
                  (* (matrix-m33 m0) (matrix-m31 m1))
                  (* (matrix-m34 m0) (matrix-m41 m1)))
               (+ (* (matrix-m31 m0) (matrix-m12 m1))
                  (* (matrix-m32 m0) (matrix-m22 m1))
                  (* (matrix-m33 m0) (matrix-m32 m1))
                  (* (matrix-m34 m0) (matrix-m42 m1)))
               (+ (* (matrix-m31 m0) (matrix-m13 m1))
                  (* (matrix-m32 m0) (matrix-m23 m1))
                  (* (matrix-m33 m0) (matrix-m33 m1))
                  (* (matrix-m34 m0) (matrix-m43 m1)))
               (+ (* (matrix-m31 m0) (matrix-m14 m1))
                  (* (matrix-m32 m0) (matrix-m24 m1))
                  (* (matrix-m33 m0) (matrix-m34 m1))
                  (* (matrix-m34 m0) (matrix-m44 m1)))
               (+ (* (matrix-m41 m0) (matrix-m11 m1))
                  (* (matrix-m42 m0) (matrix-m21 m1))
                  (* (matrix-m43 m0) (matrix-m31 m1))
                  (* (matrix-m44 m0) (matrix-m41 m1)))
               (+ (* (matrix-m41 m0) (matrix-m12 m1))
                  (* (matrix-m42 m0) (matrix-m22 m1))
                  (* (matrix-m43 m0) (matrix-m32 m1))
                  (* (matrix-m44 m0) (matrix-m42 m1)))
               (+ (* (matrix-m41 m0) (matrix-m13 m1))
                  (* (matrix-m42 m0) (matrix-m23 m1))
                  (* (matrix-m43 m0) (matrix-m33 m1))
                  (* (matrix-m44 m0) (matrix-m43 m1)))
               (+ (* (matrix-m41 m0) (matrix-m14 m1))
                  (* (matrix-m42 m0) (matrix-m24 m1))
                  (* (matrix-m43 m0) (matrix-m34 m1))
                  (* (matrix-m44 m0) (matrix-m44 m1)))))



(@htdf transform-homogeneous)
(@signature Matrix Homogeneous -> Homogeneous)
;; apply transformation matrix to homogeneous coordinates, i.e. compute m * h
;!!! examples

(@template-origin Matrix Homogeneous)

(define (transform-homogeneous m h)
  (make-homogeneous (+ (* (matrix-m11 m) (homogeneous-x h))
                       (* (matrix-m12 m) (homogeneous-y h))
                       (* (matrix-m13 m) (homogeneous-z h))
                       (* (matrix-m14 m) (homogeneous-w h)))
                    (+ (* (matrix-m21 m) (homogeneous-x h))
                       (* (matrix-m22 m) (homogeneous-y h))
                       (* (matrix-m23 m) (homogeneous-z h))
                       (* (matrix-m24 m) (homogeneous-w h)))
                    (+ (* (matrix-m31 m) (homogeneous-x h))
                       (* (matrix-m32 m) (homogeneous-y h))
                       (* (matrix-m33 m) (homogeneous-z h))
                       (* (matrix-m34 m) (homogeneous-w h)))
                    (+ (* (matrix-m41 m) (homogeneous-x h))
                       (* (matrix-m42 m) (homogeneous-y h))
                       (* (matrix-m43 m) (homogeneous-z h))
                       (* (matrix-m44 m) (homogeneous-w h)))))
