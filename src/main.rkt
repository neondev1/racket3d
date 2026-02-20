;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname main) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)
(require 2htdp/image)
(require 2htdp/universe)

(require "common.rkt")
(@htdd Colour Vector Euler Triangle)

(require "object.rkt")
(@htdd Cuboid Icosphere Mesh VertexBuffer Element ElementBuffer Object)

;;
;; MAIN.rkt
;;
;; Entry point and graphical user interface of Racket3D
;;
