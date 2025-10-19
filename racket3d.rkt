;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname racket3d) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require asdfghjkl) ;for testing workflow

(require spd/tags)
(require 2htdp/image)
(require 2htdp/universe)

(@htdw Camera)

;;
;; CONSTANTS
;;

(define WIDTH 640)
(define HEIGHT 480)

(define BEZEL-X 6)
(define BEZEL-WIDTH (/ BEZEL-X 2))

;TODO: add constants for some of this
;!!!
(define EMP (empty-scene WIDTH HEIGHT))
(define MTS (overlay/xy (rectangle 630 445 "outline" "black")
                        (- (- BEZEL-WIDTH) 1) -30 EMP))

(define TPS 60)
(define INACTIVE-DELAY (* TPS 20))

(define APPROX (expt 10 -12))

;;
;; BASIC DATA DEFINITIONS
;;

(@htdd Point)
(define-struct point (x y z))
;; Point is (make-point Number Number Number)
;; interp. the x, y and z coordinates of a point
(define POINT1 (make-point 0 0 0))
(define POINT2 (make-point 1 1 1))
(define POINT3 (make-point 0.1 1.2 2.3))
(define POINT4 (make-point -0.1 0 -1))

(@dd-template-rules compound) ;3 fields

(define (fn-for-point p)
  (... (point-x p)   ;Number
       (point-y p)   ;Number
       (point-z p))) ;Number


(@htdd Euler)
(define-struct euler (alpha beta gamma))
;; Euler is (make-euler Number Number Number)
;; interp. the Euler angles representing an orientation
(define EULER1 (make-euler 0 0 0))
(define EULER2 (make-euler 60 90 180))
(define EULER3 (make-euler 12.3 45.6 78.9))
(define EULER4 (make-euler -60 -45.6 0))

(@dd-template-rules compound) ;3 fields

(define (fn-for-euler e)
  (... (euler-alpha e)   ;Number
       (euler-beta  e)   ;Number
       (euler-gamma e))) ;Number


(@htdd Triangle)
(define-struct tri (v0 v1 v2 color))
;; Triangle is (make-tri Point Point Point Color)
;; interp. the three vertices and fill color of a triangle
;; CONSTRAINT: No two vertices should be equal
(define TRIANGLE1 (make-tri (make-point 0 1 1)
                            (make-point 1 0 1)
                            (make-point 0 0 1)
                            "black"))          ;triangles for a
(define TRIANGLE2 (make-tri (make-point 0 1 1) ;rectangular mesh
                            (make-point 1 1 1)
                            (make-point 1 0 1)
                            "black"))
(define TRIANGLE3 (make-tri (make-point -1 1 1)
                            (make-point 1 1 1)
                            (make-point 0 0 1)
                            "red"))

(@dd-template-rules compound ;4 fields
                    ref      ;(tri-v0 Triangle) is Point
                    ref      ;(tri-v1 Triangle) is Point
                    ref)     ;(tri-v2 Triangle) is Point

(define (fn-for-triangle t)
  (... (fn-for-point (tri-v0 t)) 
       (fn-for-point (tri-v1 t))
       (fn-for-point (tri-v2 t))
       (tri-color t)))           ;Color

;;
;; INTERNAL DEFINITIONS
;;

(@htdd Cuboid)
(define-struct cuboid (position rotation x-scale y-scale z-scale color))
;; Cuboid is (make-cuboid Point Euler Number Number Number Color)
;; interp. the position, orientation, x, y, z scales and colour of a cuboid
(define CUBOID1 (make-cuboid (make-point 0 0 0) ;Unit cube
                             (make-euler 0 0 0)
                             1 1 1 "black")) 
(define CUBOID2 (make-cuboid (make-point 1 2 3)
                             (make-euler 0 0 0)
                             2 4 6 "black"))
(define CUBOID3 (make-cuboid (make-point 1 2 1)
                             (make-euler 45 45 45)
                             2 3 4 "black"))
(define CUBOID4 (make-cuboid (make-point -1 -2 -3)
                             (make-euler -50 -30 -15)
                             -5 0 -4 "red"))

(@dd-template-rules compound ;6 fields
                    ref      ;(cuboid-position Cuboid) is Point
                    ref)     ;(cuboid-rotation Cuboid) is Euler

(define (fn-for-cuboid c)
  (... (fn-for-point (cuboid-position c))
       (fn-for-euler (cuboid-rotation c))
       (cuboid-x-scale c)                 ;Number
       (cuboid-y-scale c)                 ;Number
       (cuboid-z-scale c)                 ;Number
       (cuboid-color c)))                 ;Color


(@htdd Icosphere)
(define-struct icosphere (position rotation x-scale y-scale z-scale color))
;; Icosphere is (make-icosphere Point Euler Number Number Number Color)
;; interp. the position, orientation, x, y, z scales and colour of an icosphere
(define ICOSPHERE1 (make-icosphere (make-point 0 0 0) ;sphere
                                   (make-euler 0 0 0)
                                   1 1 1 "black")) 
(define ICOSPHERE2 (make-icosphere (make-point 0 0 0)    ;rotated sphere is
                                   (make-euler 23 37 79) ;nearly identical
                                   1 1 1 "black")) 
(define ICOSPHERE3 (make-icosphere (make-point 1 3 5)
                                   (make-euler 100 120 140)
                                   3 4 5 "black"))
(define ICOSPHERE4 (make-icosphere (make-point -1 -3 -5)
                                   (make-euler -100 -120 140)
                                   -3 -4 -5 "red"))

(@dd-template-rules compound ;6 fields
                    ref      ;(icosphere-position Icosphere) is Point
                    ref)     ;(icosphere-rotation Icosphere) is Euler

(define (fn-for-icosphere i)
  (... (fn-for-point (icosphere-position i))
       (fn-for-euler (icosphere-rotation i))
       (icosphere-x-scale i)                 ;Number
       (icosphere-y-scale i)                 ;Number
       (icosphere-z-scale i)                 ;Number
       (icosphere-color i)))                 ;Color


(@htdd Mesh)
;; Mesh is one of:
;;  - empty
;;  - (cons Triangle Mesh)
;; interp. a mesh composed of triangular faces
(define MESH1 empty)
(define MESH2 (cons (make-tri (make-point 2 0 0) ;Tetrahedron mesh
                              (make-point -1 -1 (/ (sqrt 13) 2))
                              (make-point -1 -1 (/ (sqrt 13) -2))
                              "black")
                    (cons (make-tri (make-point -1 2 0)
                                    (make-point -1 -1 (/ (sqrt 13) 2))
                                    (make-point -1 -1 (/ (sqrt 13) -2))
                                    "black")
                          (cons (make-tri (make-point 2 0 0)
                                          (make-point -1 2 0)
                                          (make-point -1 -1 (/ (sqrt 13) 2))
                                          "black")
                                (cons (make-tri (make-point 2 0 0)
                                                (make-point -1 2 0)
                                                (make-point -1 -1
                                                            (/ (sqrt 13) -2))
                                                "black")
                                      empty)))))

(@dd-template-rules one-of          ;2 cases
                    atomic-distinct ;empty
                    compound        ;(cons Triangle Mesh)
                    ref             ;(first Mesh) is Triangle
                    self-ref)       ;(rest Mesh) is Mesh

(define (fn-for-mesh m)
  (cond [(empty? m)
         (...)]
        [else
         (... (fn-for-triangle (first m))
              (fn-for-mesh (rest m)))]))

;;
;; PUBLIC DEFINITIONS
;;

(@htdd Object)
;; Object is one of:
;;  - Cuboid
;;  - Icosphere
;;  - Mesh
;; interp. the position, orientation and size info of an object
(define OBJECT1 CUBOID1)
(define OBJECT2 ICOSPHERE1)
(define OBJECT3 MESH2)

(@dd-template-rules one-of   ;3 cases
                    compound ;Cuboid
                    ref      ;Cuboid
                    compound ;Icosphere
                    ref      ;Icosphere
                    compound ;Mesh
                    ref)     ;Mesh

(define (fn-for-object o)
  (cond [(cuboid? o)
         (... (fn-for-cuboid o))]
        [(icosphere? o)
         (... (fn-for-icosphere o))]
        [else
         (... (fn-for-mesh o))]))


(@htdd ListOfObject)
;; ListOfObject is one of:
;;  - empty
;;  - (cons Object ListOfObject)
;; interp. a list of all objects to be rendered
(define LOO1 empty)
(define LOO2 (cons OBJECT1
                   (cons OBJECT2
                         (cons OBJECT3
                               empty))))

(@dd-template-rules one-of          ;2 cases
                    atomic-distinct ;empty
                    compound        ;(cons Object ListOfObject)
                    ref             ;(first ListOfObject) is Object
                    self-ref)       ;(rest ListOfObject) is ListOfObject

(define (fn-for-loo loo)
  (cond [(empty? loo)
         (...)]
        [else
         (... (fn-for-object (first loo))
              (fn-for-loo (rest loo)))]))

;;
;; LIGHTING DATA DEFINITIONS
;;

(@htdd Vector)
(define-struct vector (x y z))
;; Vector is (make-vector Number Number Number)
;; interp. the x, y and z components of a 3D vector
(define VECTOR1 (make-vector 0 0 0)) ;zero vector
(define VECTOR2 (make-vector 0 0 1)) ;unit vector normal to xy plane
(define VECTOR3 (make-vector 1.3 3.5 5.7))
(define VECTOR4 (make-vector -1.3 -3.5 -5.7))

(@dd-template-rules compound) ;3 fields

(define (fn-for-vector v)
  (... (vector-x v)   ;Number
       (vector-y v)   ;Number
       (vector-z v))) ;Number

;;
;; LIGHTING FUNCTIONS
;;

(@htdf scalar-multiply)
(@signature Vector Number -> Vector)
;; produce vector multiplied by a scalar
(check-expect (scalar-multiply (make-vector 0 0 0) 2) (make-vector 0 0 0))
(check-expect (scalar-multiply (make-vector 1 2 3) 0) (make-vector 0 0 0))
(check-expect (scalar-multiply (make-vector 1.2 3.4 -5.6) -3)
              (make-vector -3.6 -10.2 16.8))

;(define (scalar-multiply v s) (make-vector 0 0 0)) ;stub

(@template-origin Vector)

(@template
 (define (scalar-multiply v s)
   (... s
        (vector-x v)
        (vector-y v)
        (vector-z v))))

(define (scalar-multiply v s)
  (make-vector (* (vector-x v) s)
               (* (vector-y v) s)
               (* (vector-z v) s)))


(@htdf scalar-divide)
(@signature Vector Number -> Vector)
;; produce vector divided by a scalar
;; CONSTRAINT: scalar must be nonzero
(check-expect (scalar-divide (make-vector 0 0 0) 2)
              (make-vector 0 0 0))
(check-expect (scalar-divide (make-vector 1.2 -4.5 7.8) 3)
              (make-vector 0.4 -1.5 2.6))

;(define (scalar-divide v s) (make-vector 0 0 0)) ;stub

(@template-origin Vector)

(@template
 (define (scalar-divide v s)
   (... s
        (vector-x v)
        (vector-y v)
        (vector-z v))))

(define (scalar-divide v s)
  (make-vector (/ (vector-x v) s)
               (/ (vector-y v) s)
               (/ (vector-z v) s)))


(@htdf cross-product)
(@signature Vector Vector -> Vector)
;; produce cross product of given vectors
(check-expect (cross-product (make-vector 2 0 0)
                             (make-vector 0 2 0))
              (make-vector 0 0 4))
(check-expect (cross-product (make-vector 0 2 0)
                             (make-vector 2 0 0))
              (make-vector 0 0 -4))

;(define (cross-product v0 v1) (make-vector 0 0 0)) ;stub

(@template-origin Vector)

(@template
 (define (cross-product v0 v1)
   (... (vector-x v0)
        (vector-y v0)
        (vector-z v0)
        (vector-x v1)
        (vector-y v1)
        (vector-z v1))))

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

;(define (dot-product v0 v1) 0) ;stub

(@template-origin Vector)

(@template
 (define (dot-product v0 v1)
   (... (vector-x v0)
        (vector-y v0)
        (vector-z v0)
        (vector-x v1)
        (vector-y v1)
        (vector-z v1))))

(define (dot-product v0 v1)
  (+ (* (vector-x v0) (vector-x v1))
     (* (vector-y v0) (vector-y v1))
     (* (vector-z v0) (vector-z v1))))


(@htdf mag)
(@signature Vector -> Number)
;; produce magnitude of given vector
(check-expect (mag (make-vector 0 0 0)) 0)
(check-within (mag (make-vector 1 1 1)) (sqrt 3) APPROX)
(check-expect (mag (make-vector -2 3 -6)) 7)

;(define (mag v) 0) ;stub

(@template-origin Vector)

(@template
 (define (mag v)
   (... (vector-x v)
        (vector-y v)
        (vector-z v))))

(define (mag v)
  (sqrt (+ (sqr (vector-x v))
           (sqr (vector-y v))
           (sqr (vector-z v)))))


(@htdf points->vector)
(@signature Point Point -> Vector)
;; produce vector from first to second point
(check-expect (points->vector (make-point 0 0 0)
                              (make-point 0 0 0))
              (make-vector 0 0 0))
(check-expect (points->vector (make-point 0 0 0)
                              (make-point 1 -2 3))
              (make-vector 1 -2 3))
(check-expect (points->vector (make-point 1.2 -3.4 5.6)
                              (make-point 1.3 5.7 -9.1))
              (make-vector 0.1 9.1 -14.7))

;(define (points->vector p0 p1) (make-vector 0 0 0)) ;stub

(@template-origin Point)

(@template
 (define (points->vector p0 p1)
   (... (point-x p0)
        (point-y p0)
        (point-z p0)
        (point-x p1)
        (point-y p1)
        (point-z p1))))

(define (points->vector p0 p1)
  (make-vector (- (point-x p1) (point-x p0))
               (- (point-y p1) (point-y p0))
               (- (point-z p1) (point-z p0))))


(@htdf normal)
(@signature Triangle -> Vector)
;; produce vector normal to given triangle
;; CONSTRAINT: given triangle must be non-degenerate
(check-expect (normal (make-tri (make-point 0 0 0)
                                (make-point 2 0 0)
                                (make-point 0 2 0)
                                "black"))
              (make-vector 0 0 4))
(check-expect (normal (make-tri (make-point 0 0 0)
                                (make-point 0 2 0)
                                (make-point 2 0 0)
                                "black"))
              (make-vector 0 0 -4))

;(define (normal t) (make-vector 0 0 0)) ;stub

(@template-origin fn-composition)

(@template
 (define (normal t)
   (cross-product (points->vector (tri-v1 t) (tri-v0 t))
                  (points->vector (tri-v2 t) (tri-v1 t)))))

(define (normal t)
  (cross-product (points->vector (tri-v1 t) (tri-v0 t))
                 (points->vector (tri-v2 t) (tri-v1 t))))


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

;(define (vector-angle v0 v1) 0) ;stub

(@template-origin fn-composition)

(@template
 (define (vector-angle v0 v1)
   (acos (/ (dot-product v0 v1)
            (* (mag v0) (mag v1))))))

(define (vector-angle v0 v1)
  (acos (/ (dot-product v0 v1)
           (* (mag v0) (mag v1)))))

;;
;; WORLD
;;

(@htdd GUIState)
(define-struct gui-state (file help add selection))
;; GUIState is (make-gui-state Boolean Boolean Boolean Natural)
;; interp. GUI dropdown states for File and Help menus and object creation; 
;;         selection is 1-based index of selected object, 0 if no selection
;; CONSTRAINT: selection must be less than or equal to total object count
(define GUI1 (make-gui-state false false false 0))
(define GUI2 (make-gui-state true  false false 0))
(define GUI3 (make-gui-state false false true  2))

(@dd-template-rules compound) ;4 fields

(define (fn-for-gui-state gs)
  (... (gui-state-file gs)        ;Boolean
       (gui-state-help gs)        ;Boolean
       (gui-state-add gs)         ;Boolean
       (gui-state-selection gs))) ;Natural


(@htdd Camera)
(define-struct camera (gui objects position light time))
;; Camera is (make-camera GUIState ListOfObject Point Point Natural)
;; interp. GUI state, object list, position of camera/light, time in ticks since
;;         last user interaction. The viewport camera always faces (0, 0, 0).
(define CAM1 (make-camera GUI1 empty (make-point 1 1 1) (make-point 2 2 2) 0))

(@dd-template-rules compound ;5 fields
                    ref      ;(camera-gui Camera) is GUIState
                    ref      ;(camera-objects Camera) is ListOfObject
                    ref      ;(camera-position Camera) is Point
                    ref)     ;(camera-light Camera) is Point

(define (fn-for-cam cam)
  (... (fn-for-gui-state (camera-gui-state cam))
       (fn-for-loo (camera-objects cam))
       (fn-for-point (camera-position cam))
       (fn-for-point (camera-light cam))
       (camera-time cam)))                       ;Natural


(@htdf main)
(@signature Camera -> Camera)
;; start the world with (main CAM1)

(@template-origin htdw-main)

(define (main cam)
  (big-bang cam                 ;Camera
    (on-tick    tick (/ 1 TPS)) ;Camera -> Camera
    (to-draw    render)         ;Camera -> Image
    (on-mouse   mouse-handler)  ;Camera Integer Integer MouseEvent -> Camera
    (on-key     key-handler)    ;Camera KeyEvent -> Camera
    (on-release key-release)))  ;Camera KeyEvent -> Camera

    
(@htdf tick)
(@signature Camera -> Camera)
;; increment ticks since last user interaction
;; !!!

(define (tick cam) cam) ;stub
  
    
(@htdf render)
(@signature Camera -> Image)
;; render ...
;; !!! purpose and examples
;(define (render cam) MTS) ;stub

(@template-origin Camera)

(@template
 (define (render cam)
   (... (fn-for-gui-state (camera-gui-state cam))
        (fn-for-loo (camera-objects cam))
        (fn-for-point (camera-position cam))
        (fn-for-point (camera-light cam))
        (camera-time cam))))

(define (render cam)
  (overlay (render-gui (camera-gui cam))
           (render-objects (camera-objects cam)
                           (camera-position cam)
                           (camera-light cam))))

(@htdf render-gui)
(@signature GUIState -> Image)
;; renders the gui and dropdowns
;; !!!
(check-expect (render-gui GUI1) MTS)

(define (render-gui gui) MTS) ;stub


(@htdf render-objects)
(@signature ListOfObject Point Point -> Image)
;; !!!

(define (render-objects obj pos light) MTS) ;stub


(@htdf mouse-handler)
(@signature Camera Integer Integer MouseEvent -> Camera)
;; on mouse click ...
;; !!!
(define (mouse-handler cam x y me) cam) ;stub


(@htdf key-handler)
(@signature Camera KeyEvent -> Camera)
;; on key press ...
;; !!!
(define (key-handler cam ke) cam) ;stub


(@htdf key-release)
(@signature Camera KeyEvent -> Camera)
;; on key release ...
;; !!!
(define (key-release cam ke) cam) ;stub
