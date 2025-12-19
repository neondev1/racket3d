;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname bst) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)

(require "provide.rkt")
(provide (all-defined-out))

(require "common.rkt")
(@htdd Colour Point Euler Triangle)

(require "vector.rkt")
(@htdd Vector Plane Line)

;;
;; BST.rkt
;;
;; BST data definition and utility functions
;;

;;
;; DATA DEFINITIONS
;;

(@htdd BST)
(define-struct node (key value left right))
;; BST is one of:
;;  - false
;;  - (make-node Natural Vector BST BST)
;; interp. a node in a binary search tree, with its key/value and children
;; CONSTRAINT: all keys in left must be less than key;
;;             all keys in right must be greater than key;
;;             all keys must be unique
(define BST0 false)
(define BST1 (make-node 4 ZERO-VECTOR
                        (make-node 2 VECTOR1
                                   (make-node 1 VECTOR2 false false)
                                   (make-node 3 VECTOR3 false false))
                        false))

(@dd-template-rules one-of          ;2 cases
                    atomic-distinct ;false
                    compound        ;(make-node Natural Vector BST BST)
                    ref             ;(node-value BST) is Vector
                    self-ref        ;(node-left BST) is BST
                    self-ref)       ;(node-right BST) is BST

(define (fn-for-bst bst)
  (cond [(false? bst)
         (...)]
        [else
         (... (node-key bst)
              (fn-for-vector (node-value bst))
              (fn-for-bst (node-left bst))
              (fn-for-bst (node-right bst)))]))


;;
;; FUNCTIONS
;;


(@htdf construct-bst construct-bst--acc)
(@signature (listof Vector) -> BST)
;; construct binary search tree from given list of vectors
;!!! examples

;(define (construct-bst lov) false) ;stub

(@template-origin accumulator)

(@template
 (define (construct-bst lov)
   (... (construct-bst--acc (... lov) (... lov) (... lov) (... lov)
                            (... lov) (... lov) (... lov) (... lov)))))

(define (construct-bst lov)
  (cond [(empty? lov)
         false]
        [(empty? (rest lov))
         (make-node 0 (first lov) false false)]
        [else
         (construct-bst--acc (rest (rest lov))
                             (rest (rest (bst-pattern (length lov))))
                             1 1 2 (list 1)
                             empty
                             (list (make-node 1 (second lov)
                                              (make-node 0 (first lov)
                                                         false false)
                                              false)))]))

(@template-origin (listof Vector) accumulator)

(@template
 (define (construct-bst--acc lov depths last-depth eff-depth
                             index fold-counts perfect imperfect)
   (cond [(empty? lov)
          (... depths last-depth eff-depth index fold-counts perfect imperfect)]
         [else
          (... (fn-for-vector (first lov))
               depths last-depth eff-depth index fold-counts perfect imperfect
               (construct-bst--acc (rest lov) (... depths)
                                   (... last-depth) (... eff-depth)
                                   (... index) (... fold-counts)
                                   (... perfect) (... imperfect)))])))

;; !!! invariants
(define (construct-bst--acc lov depths last-depth eff-depth
                            index fold-counts perfect imperfect)
  (cond [(empty? lov)
         (if (empty? imperfect)
             perfect
             (fold perfect imperfect
                   (first (reverse fold-counts))))]
        [(zero? (first depths))
         (if (= last-depth 1)
             (construct-bst--acc (rest lov) (rest depths) 0 eff-depth
                                 (add1 index) (rest fold-counts)
                                 (fold (make-node index (first lov) false false)
                                       imperfect (first fold-counts))
                                 (trim imperfect (first fold-counts)))
             (construct-bst--acc (rest lov) (rest depths) 0 eff-depth
                                 (add1 index) fold-counts
                                 (make-node index (first lov) false false)
                                 imperfect))]
        [(= (first depths) (sub1 eff-depth))
         (construct-bst--acc (rest lov) (rest depths)
                             (first depths) (first depths)
                             (add1 index) fold-counts
                             false
                             (cons (make-node index (first lov) perfect false)
                                   imperfect))]
        [(> (first depths) eff-depth)
         (construct-bst--acc (rest lov) (rest depths)
                             (first depths) (first depths)
                             (add1 index) (cons (first depths) fold-counts)
                             false
                             (cons (make-node index (first lov) perfect false)
                                   imperfect))]
        [else
         (construct-bst--acc (rest lov) (rest depths)
                             (first depths) eff-depth
                             (add1 index) (cons (first depths) fold-counts)
                             false
                             (cons (make-node index (first lov) perfect false)
                                   imperfect))]))



(@htdf fold)
(@signature (listof BST) (listof BST) Natural -> (listof BST))
;; recursively attach perfect to (first imperfect) until count = 0 or list empty
;!!!

;(define (fold perfect imperfect count) empty) ;stub

(@template-origin 2-one-of accumulator)

(@template
 (define (fold perfect imperfect count)
   (cond [(or (zero? count) (empty? imperfect))
          (... perfect)]
         [else
          (... perfect (first imperfect) count
               (fold (... perfect)
                     (rest imperfect)
                     (sub1 count)))])))

#|
!!!
|#

;; !!! invariants
(define (fold perfect imperfect count)
  (cond [(or (zero? count) (empty? imperfect)) ;[0]
         perfect]
        [else                                  ;[1]
         (fold (set-right (first imperfect) perfect)
               (rest imperfect)
               (sub1 count))]))



(@htdf set-right)
(@signature BST BST -> BST)
;; produce first BST with right child set to second BST
;!!!

;(define (set-right bst right) false) ;stub

(@template-origin BST)

(@template
 (define (set-right bst right)
   (... (node-key bst)
        (node-value bst)
        (node-left bst)
        (node-right bst)
        right)))

(define (set-right bst right)
  (make-node (node-key bst)
             (node-value bst)
             (node-left bst)
             right))



(@htdf bst-pattern)
(@signature Natural -> (listof Natural))
;; produce list of BST node depths for a list with count elements; 0 is deepest
;; CONSTRAINT: count must be nonzero
(check-expect (bst-pattern 1)  (list 0))
(check-expect (bst-pattern 3)  (list 0 1 0))
(check-expect (bst-pattern 10) (list 0 1 0 2 0 1 0 3 0 1))
(check-expect (bst-pattern 15) (list 0 1 0 2 0 1 0 3 0 1 0 2 0 1 0))
;; This really just generates the ruler sequence (OEIS: A007814)

;(define (bst-pattern count) empty) ;stub

(@template-origin fn-composition)

(define (bst-pattern count)
  (depth->bst-pattern (floor (log2 count)) count))



(@htdf depth->bst-pattern depth->bst-pattern--acc)
(@signature Natural -> (listof Natural))
;; produce list of BST node depths for a BST with given depth, trimmed to count
(check-expect (depth->bst-pattern 1 1) (list 0))
(check-expect (depth->bst-pattern 2 3) (list 0 1 0))
(check-expect (depth->bst-pattern 4 10) (list 0 1 0 2 0 1 0 3 0 1))
(check-expect (depth->bst-pattern 4 15) (list 0 1 0 2 0 1 0 3 0 1 0 2 0 1 0))

;(define (depth->bst-pattern depth count) empty) ;stub

(@template-origin accumulator)

(@template
 (define (depth->bst-pattern depth count)
   (... (depth->bst-pattern--acc (... depth count)
                                 (... depth count)
                                 (... depth count)))))

(define (depth->bst-pattern depth count)
  (reverse (trim (depth->bst-pattern--acc depth 0 empty)
                 (- (sub1 (expt 2 (add1 depth))) count))))

(@template-origin genrec accumulator)

(@template
 (define (depth->bst-pattern--acc target depth last)
   (if (... target depth last)
       (... target depth last)
       (... target depth last
            (depth->bst-pattern--acc target (... depth) (... last))))))

;; depth is Natural
;; INVARIANT: depth of the current part of the BST pattern being generated
;;
;; last is (listof Natural)
;; INVARIANT: previously generated part of the pattern, up to and not including
;;            the current depth; represents deeper parts of the BST
;;
;; Trivial case:   (= depth target)
;; Reduction step: (add1 depth)
;; Termination argument:
;;   For natural number target >= 0 and depth starting at 0,
;;   adding 1 to depth will cause it to eventually reach target
(define (depth->bst-pattern--acc target depth last)
  (if (= depth target)
      (cons-pattern depth last)
      (depth->bst-pattern--acc target (add1 depth) (cons-pattern depth last))))



(@htdf cons-pattern)
(@signature Natural (listof Natural) -> (listof Natural))
;; produce next pattern by prepending and appending list to given number
(check-expect (cons-pattern 0 empty) (list 0))
(check-expect (cons-pattern 4 (list 1 2 3)) (list 1 2 3 4 1 2 3))
(check-expect (cons-pattern 3 (list 0 1 0 2 0 1 0))
              (list 0 1 0 2 0 1 0 3 0 1 0 2 0 1 0))

;(define (cons-pattern n lon) empty) ;stub

(@template-origin Natural)

(@template
 (define (cons-pattern n lon)
   (... n lon)))

(define (cons-pattern n lon)
  (append lon (list n) lon))



(@htdf trim)
(@signature (listof Natural) Natural -> (listof Natural))
;; remove given number of elements from beginning of list
;; CONSTRAINT: list must have at least that many elements
;!!!

;(define (trim lon count) empty) ;stub

(@template-origin Natural)

(@template
 (define (trim lon count)
   (cond [(zero? count)
          (... lon)]
         [else
          (... lon count)])))

(define (trim lon count)
  (cond [(zero? count)
         lon]
        [else
         (trim (rest lon) (sub1 count))]))



(@htdf log2)
(@signature Number -> Number)
;; produce log base 2 of a number
;; CONSTRAINT: the given number must be positive
(check-expect (log2 0.5) -1)
(check-expect (log2 1) 0)
(check-expect (log2 16) 4)
(check-expect (log2 4294967296) 32)
(check-within (log2 10000) 13.28771237954945 APPROX)

(@template-origin Number)

(@template
 (define (log2 x)
   (... x)))

(define (log2 x)
  (inexact->exact (/ (log x) (log 2))))
