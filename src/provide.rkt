#lang racket

;;
;; PROVIDE.rkt
;;
;; Exports the provide and (all-defined-out) primitives
;; to allow the project to be split into multiple files
;;


(provide provide)
(provide all-defined-out) ;NOT A TYPO
