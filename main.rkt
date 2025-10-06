;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname main) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)
(require 2htdp/image)
(require 2htdp/universe)


;; My world program (make this more specific)
(@htdw WS)
;; =================
;; Constants:



;; =================
;; Data definitions:


(@htdd WS)
;; WS is ... (give WS a better name)




;; =================
;; Functions:

(@htdf main)
(@signature WS -> WS)
;; start the world with (main ...)
;; no tests for main function

(@template-origin htdw-main)

(define (main ws)
 (big-bang ws ;WS
 (on-tick tock) ;WS -> WS
 (to-draw render) ;WS -> Image
 (on-mouse ...) ;WS Integer Integer MouseEvent -> WS
 (on-key ...))) ;WS KeyEvent -> WS

(@htdf tock)
(@signature WS -> WS)
;; produce the next ...
;; !!!
(define (tock ws) ws)

(@htdf render)
(@signature WS -> Image)
;; render ...
;; !!!
(define (render ws) empty-image)