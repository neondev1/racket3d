;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname projection) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)
(require 2htdp/image)

(require "provide.rkt")
(provide (all-defined-out))

(require "common.rkt")
(@htdd Colour Vector Euler Triangle)

(require "vector.rkt")
(@htdd Plane Line)

(require "matrix.rkt")
(@htdd Matrix)

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


;;
;; FUNCTIONS
;;


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



(@htdf rotation-matrix)
(@signature Number Number Number -> Matrix)
;; produce rotation matrix from given yaw, pitch and roll
;!!! tests

(@template-origin Number)

(define (rotation-matrix yaw pitch roll)
  (matrix-multiply
   (matrix-multiply
    (make-matrix 1  0          0
                 0  (cos roll) (- (sin roll))
                 0  (sin roll) (cos roll))        ;finally roll
    (make-matrix (cos pitch)     0  (sin pitch)
                 0               1  0
                 (- (sin pitch)) 0  (cos pitch))) ;then pitch
   (make-matrix (cos yaw) (- (sin yaw)) 0
                (sin yaw) (cos yaw)     0
                0         0             1)))      ;yaw first
