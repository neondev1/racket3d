;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname main) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)
(require 2htdp/image)
(require 2htdp/universe)

(require "common.rkt")
(@htdd Colour Point Euler Triangle)

(require "vector.rkt")
(@htdd Vector Plane Line)

(require "object.rkt")
(@htdd Cuboid Icosphere Mesh VertexBuffer Element ElementBuffer Object)

(require "bst.rkt") ;temporary (for running tests)

;;
;; MAIN.rkt
;;
;; Entry point and graphical user interface of Racket3D
;;


;!!! needs refactoring
;also perhaps creating another file for defining specific GUI controls/widgets
(@htdw Camera)


;;
;; CONSTANTS
;;


(define WIDTH  640)
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
;; Camera is (make-camera GUIState (listof Object) Point Point Natural)
;; interp. GUI state, object list, position of camera/light, time (ticks) since
;;         last user interaction. The viewport camera always faces (0, 0, 0).
(define CAM1 (make-camera GUI1 empty (make-point 1 1 1) (make-point 2 2 2) 0))

(@dd-template-rules compound ;5 fields
                    ref      ;(camera-gui Camera) is GUIState
                    ref      ;(camera-objects Camera) is (listof Object)
                    ref      ;(camera-position Camera) is Point
                    ref)     ;(camera-light Camera) is Point

(define (fn-for-cam cam)
  (... (fn-for-gui-state (camera-gui-state cam))
       (fn-for-loo (camera-objects cam))
       (fn-for-point (camera-position cam))
       (fn-for-point (camera-light cam))
       (camera-time cam)))                  ;Natural



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
(@signature (listof Object) Point Point -> Image)
;; !!!

(define (render-objects objs pos light) MTS) ;stub



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
