;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname racket3d) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
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

(define EMP (empty-scene WIDTH HEIGHT))
(define MTS (overlay/xy (rectangle 630 445 "outline" "black")
                       (- (- BEZEL-WIDTH) 1) -30 EMP))

;;
;; BASIC DATA DEFINITIONS
;;

(@htdd Point)
(define-struct point (x y z))
;; Point is (make-point Number Number Number)
;; interp. the x, y and z coordinates of a point
(define P1 (make-point 0 0 0))
(define P2 (make-point 1 1 1))
(define P3 (make-point 0.1 1.2 2.3))

(@dd-template-rules compound) ;3 fields

(define (fn-for-point p)
  (... (point-x p)
       (point-y p)
       (point-z p)))


(@htdd Euler)
(define-struct euler (alpha beta gamma))
;; Euler is (make-euler Number Number Number)
;; interp. the Euler angles representing an orientation
(define EA1 (make-euler 0 0 0))
(define EA2 (make-euler 60 90 180))
(define EA3 (make-euler 12.3 45.6 78.9))

(@dd-template-rules compound) ;3 fields

(define (fn-for-euler e)
  (... (euler-alpha e)
       (euler-beta  e)
       (euler-gamma e)))

(@htdd Triangle)
(define-struct tri (v0 v1 v2 col))
;; Triangle is (make-tri Point Point Point Color)
;; interp. the three vertices and fill colour of a triangle
;; CONSTRAINT: No two vertices should be equal
(define T1 (make-tri (make-point 0 1 1)
                     (make-point 1 0 1)
                     (make-point 0 0 1)
                     "black"))            ;triangles for a
(define T2 (make-tri (make-point 0 1 1)   ;rectangular mesh
                     (make-point 1 1 1)
                     (make-point 1 0 1)
                     "black"))

(@dd-template-rules compound ;4 fields
                    ref      ;(tri-v0 Triangle) is Point
                    ref      ;(tri-v1 Triangle) is Point
                    ref)     ;(tri-v2 Triangle) is Point

(define (fn-for-triangle t)
  (... (fn-for-point (tri-v0 t))
       (fn-for-point (tri-v1 t))
       (fn-for-point (tri-v2 t))
       (tri-col t)))

;;
;; INTERNAL DEFINITIONS
;;

(@htdd Cuboid)
(define-struct cuboid (pos rot xs ys zs col))
;; Cuboid is (make-cuboid Point Euler Number Number Number Color)
;; interp. the position, orientation, x, y, z scales and colour of a cuboid
(define C1 (make-cuboid (make-point 0 0 0)
                        (make-euler 0 0 0)
                        1 1 1 "black")) ;Unit cube
(define C2 (make-cuboid (make-point 1 2 3)
                        (make-euler 0 0 0)
                        2 4 6 "black"))
(define C3 (make-cuboid (make-point 1 2 1)
                        (make-euler 45 45 45)
                        2 3 4 "black"))

(@dd-template-rules compound ;6 fields
                    ref      ;(cuboid-pos Cuboid) is Point
                    ref)     ;(cuboid-rot Cuboid) is Euler

(define (fn-for-cuboid c)
  (... (fn-for-point (cuboid-pos c))
       (fn-for-euler (cuboid-rot c))
       (cuboid-xs c)
       (cuboid-ys c)
       (cuboid-zs c)
       (cuboid-col c)))


(@htdd Icosphere)
(define-struct icosphere (pos rot xs ys zs col))
;; Icosphere is (make-icosphere Point Euler Number Number Number Color)
;; interp. the position, orientation, x, y, z scales and colour of an icosphere
(define E1 (make-icosphere (make-point 0 0 0)
                           (make-euler 0 0 0)
                           1 1 1 "black")) ;sphere
(define E2 (make-icosphere (make-point 0 0 0)
                           (make-euler 23 37 79)
                           1 1 1 "black")) ;rotated sphere is nearly identical
(define E3 (make-icosphere (make-point 1 3 5)
                           (make-euler 100 120 140)
                           3 4 5 "black"))

(@dd-template-rules compound ;6 fields
                    ref      ;(icosphere-pos Icosphere) is Point
                    ref)     ;(icosphere-rot Icosphere) is Euler

(define (fn-for-icosphere i)
  (... (fn-for-point (icosphere-pos i))
       (fn-for-euler (icosphere-rot i))
       (icosphere-xs i)
       (icosphere-ys i)
       (icosphere-zs i)
       (icosphere-col i)))


(@htdd Mesh)
;; Mesh is one of:
;;  - empty
;;  - (cons Triangle Mesh)
;; interp. a mesh composed of triangular faces
(define M1 empty)
(define M2 (list (make-tri (make-point 2 0 0) ;Tetrahedron mesh
                           (make-point -1 -1 (/ (sqrt 13) 2))
                           (make-point -1 -1 (/ (sqrt 13) -2))
                           "black")
                 (make-tri (make-point -1 2 0)
                           (make-point -1 -1 (/ (sqrt 13) 2))
                           (make-point -1 -1 (/ (sqrt 13) -2))
                           "black")
                 (make-tri (make-point 2 0 0)
                           (make-point -1 2 0)
                           (make-point -1 -1 (/ (sqrt 13) 2))
                           "black")
                 (make-tri (make-point 2 0 0)
                           (make-point -1 2 0)
                           (make-point -1 -1 (/ (sqrt 13) -2))
                           "black")))

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
(define O1 C1)
(define O2 E1)
(define O3 M2)

(@dd-template-rules one-of   ;3 cases
                    compound ;Cuboid
                    ref
                    compound ;Icosphere
                    ref
                    compound ;Mesh
                    ref)

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
(define LOO2 (list O1 O2 O3))

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
(define-struct vec (x y z))
;; Vector is (make-vector Number Number Number)
;; interp. the x, y and z components of a 3D vector
(define V1 (make-vec 0 0 0)) ;zero vector
(define V2 (make-vec 0 0 1)) ;unit vector normal to xy plane
(define V3 (make-vec 1.3 3.5 5.7))

(@dd-template-rules compound) ;3 fields

(define (fn-for-vector v)
  (... (vec-x v)
       (vec-y v)
       (vec-z v)))

;;
;; LIGHTING FUNCTIONS
;;

(@htdf mul)
(@signature Vector Number -> Vector)
;; produce vector multiplied by a scalar
(check-expect (mul (make-vec 0 0 0) 2) (make-vec 0 0 0))
(check-expect (mul (make-vec 1 2 3) 0) (make-vec 0 0 0))
(check-expect (mul (make-vec 1.2 3.4 -5.6) -3) (make-vec -3.6 -10.2 16.8))

;(define (mul v s) (make-vec 0 0 0)) ;stub

(@template-origin Vector)

(@template
 (define (mul v s)
   (... (vec-x v)
        (vec-y v)
        (vec-z v)
        s)))

(define (mul v s)
  (make-vec (* (vec-x v) s)
            (* (vec-y v) s)
            (* (vec-z v) s)))


(@htdf div)
(@signature Vector Number -> Vector)
;; produce vector divided by a scalar
;; CONSTRAINT: scalar must be nonzero
(check-expect (div (make-vec 0 0 0) 2) (make-vec 0 0 0))
(check-expect (div (make-vec 1.2 -4.5 7.8) 3) (make-vec 0.4 -1.5 2.6))

;(define (div v s) (make-vec 0 0 0)) ;stub

(@template-origin Vector)

(@template
 (define (div v s)
   (... (vec-x v)
        (vec-y v)
        (vec-z v)
        s)))

(define (div v s)
  (make-vec (/ (vec-x v) s)
            (/ (vec-y v) s)
            (/ (vec-z v) s)))


(@htdf cross)
(@signature Vector Vector -> Vector)
;; produce cross product of given vectors
(check-expect (cross (make-vec 2 0 0)
                     (make-vec 0 2 0))
              (make-vec 0 0 4))
(check-expect (cross (make-vec 0 2 0)
                     (make-vec 2 0 0))
              (make-vec 0 0 -4))

;(define (cross v0 v1) (make-vec 0 0 0)) ;stub

(@template-origin Vector)

(@template
 (define (cross v0 v1)
   (... (vec-x v0)
        (vec-y v0)
        (vec-z v0)
        (vec-x v1)
        (vec-y v1)
        (vec-z v1))))

(define (cross v0 v1)
  (make-vec (- (* (vec-y v0) (vec-z v1))
               (* (vec-y v1) (vec-z v0)))
            (- (* (vec-z v0) (vec-x v1))
               (* (vec-z v1) (vec-x v0)))
            (- (* (vec-x v0) (vec-y v1))
               (* (vec-x v1) (vec-y v0)))))


(@htdf dot)
(@signature Vector Vector -> Number)
;; produce dot product of given vectors
(check-expect (dot (make-vec 2 0 0)
                   (make-vec 0 2 2))
              0)
(check-expect (dot (make-vec 0 2 0)
                   (make-vec 0 3 0))
              6)
(check-expect (dot (make-vec 1.2 3.4 5.6)
                   (make-vec 9.8 -7.6 5.4))
              16.16)

;(define (dot v0 v1) 0) ;stub

(@template-origin Vector)

(@template
 (define (dot v0 v1)
   (... (vec-x v0)
        (vec-y v0)
        (vec-z v0)
        (vec-x v1)
        (vec-y v1)
        (vec-z v1))))

(define (dot v0 v1)
  (+ (* (vec-x v0) (vec-x v1))
     (* (vec-y v0) (vec-y v1))
     (* (vec-z v0) (vec-z v1))))


(@htdf mag)
(@signature Vector -> Number)
;; produce magnitude of given vector
(check-expect (mag (make-vec 0 0 0)) 0)
(check-within (mag (make-vec 1 1 1)) (sqrt 3) (expt 10 -10))
(check-expect (mag (make-vec -2 3 -6)) 7)

;(define (mag v) 0) ;stub

(@template-origin Vector)

(@template
 (define (mag v)
   (... (vec-x v)
        (vec-y v)
        (vec-z v))))

(define (mag v)
  (sqrt (+ (sqr (vec-x v)) (sqr (vec-y v)) (sqr (vec-z v)))))


(@htdf vec)
(@signature Point Point -> Vector)
;; produce vector from first to seconds point
(check-expect (to-vec (make-point 0 0 0)
                      (make-point 0 0 0))
              (make-vec 0 0 0))
(check-expect (to-vec (make-point 0 0 0)
                      (make-point 1 -2 3))
              (make-vec 1 -2 3))
(check-expect (to-vec (make-point 1.2 -3.4 5.6)
                      (make-point 1.3 5.7 -9.1))
              (make-vec 0.1 9.1 -14.7))

;(define (vec p0 p1) (make-vec 0 0 0)) ;stub

(@template-origin Point)

(@template
 (define (vec p0 p1)
   (... (point-x p0)
        (point-y p0)
        (point-z p0)
        (point-x p1)
        (point-y p1)
        (point-z p1))))

(define (to-vec p0 p1)
  (make-vec (- (point-x p1) (point-x p0))
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
              (make-vec 0 0 4))
(check-expect (normal (make-tri (make-point 0 0 0)
                                (make-point 0 2 0)
                                (make-point 2 0 0)
                                "black"))
              (make-vec 0 0 -4))

;(define (normal t) (make-vec 0 0 0)) ;stub

(@template-origin fn-composition)

(define (normal t)
  (cross (to-vec (tri-v1 t) (tri-v0 t))
         (to-vec (tri-v2 t) (tri-v1 t))))

;;
;; WORLD
;;
(@htdd GuiState)
(define-struct gui (file help side sel))
;; GuiState is (make-gui MenuState Sidebar Properties)
;; interp. all gui dropdown states. sel is 0 or index in list selected
(define GUI1 (make-gui false false false 0))
(define GUI2 (make-gui true false false 0))
(define GUI3 (make-gui false false true 2))

(@dd-template-rules compound) ;5 fields

(define (GuiState gui)
  (... (gui-file gui)
       (gui-help gui)
       (gui-side gui)
       (gui-sel gui)))


(@htdd Camera)
(define-struct camera (gui objs pos light))
;; Camera is (make-camera GuiState ListOfObject Point Point
;; interp. gui state, list of objects, position of camera and light.
(define CAM1 (make-camera GUI1 empty (make-point 0 0 0) (make-point 0 0 0)))

(@dd-template-rules compound)

(define (fn-for-cam cam)
  (... (camera-gui cam)
       (camera-objs cam)
       (camera-pos cam)
       (camera-light cam)))
       



(@htdf main)
(@signature Camera -> Camera)
;; start the world with (main ...)

(@template-origin htdw-main)

(define (main cam)
  (big-bang cam                  ;Camera
    (on-tick    tick)            ;Camera -> Camera
    (to-draw    render)          ;Camera -> Image
    (on-mouse   mouse-handler)   ;Camera Integer Integer MouseEvent -> Camera
    (on-key     key-handler)     ;Camera KeyEvent -> Camera
    (on-release key-release)))   ;Camera KeyEvent -> Camera

    
(@htdf tick)
(@signature Camera -> Camera)
;; produce the next camera state
;; !!!
(define (tick cam) cam) ;stub
  
    
(@htdf render)
(@signature Camera -> Image)
;; render ...
;; !!!
;(define (render cam) MTS) ;stub

(@template-origin Camera)

(@template
 (define (render cam)
  (... (camera-gui cam)
       (camera-objs cam)
       (camera-pos cam)
       (camera-light cam))))

(define (render cam)
  (overlay (render-gui (camera-gui cam))
           (render-objs (camera-objs cam)
                        (camera-pos cam)
                        (camera-light cam))))

(@htdf render-gui)
(@signature GuiState -> Image)
;; renders the gui and dropdowns
;; !!!
;(define (render-gui g) MTS) ;stub
(check-expect (render-gui GUI1) MTS)

(define (render-gui gui) MTS)
(define (render-objs objs pos light) MTS)


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
