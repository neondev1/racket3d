;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname main) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)
(require 2htdp/image)
(require 2htdp/universe)
(require "object.rkt")

(@htdw Camera)
;; =================
;; Constants:

(define WIDTH 1280)
(define HEIGHT 720)

(define MTS (empty-scene WIDTH HEIGHT))

;; =================
;; Data definitions:
(@htdd Camera)

;; =================
;; Functions:

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
;; produce the next ...
;; !!!
(define (tick cam) cam) ;stub

    
(@htdf render)
(@signature Camera -> Image)
;; render ...
;; !!!
(define (render cam) MTS) ;stub


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
