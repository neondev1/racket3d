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
;; Binary search tree data definitions and utility functions
;; since BSL doesn't have a collection with faster than O(n) lookup
;;

;;
;; DATA DEFINITIONS
;;

(@htdd BST)
(define-struct node (key value left right))
;; BST is one of:
;;  - false
;;  - (make-node Natural X BST BST)
;; interp. a node in a binary search tree, with its key/value and children
;; CONSTRAINT: all keys in left must be less than key;
;;             all keys in right must be greater than key;
;;             all keys must be unique
(define BST0 false)
(define BST1 (make-node 3 ZERO-VECTOR
                        (make-node 1 VECTOR1
                                   (make-node 0 VECTOR2 false false)
                                   (make-node 2 VECTOR3 false false))
                        false))

(@dd-template-rules one-of          ;2 cases
                    atomic-distinct ;false
                    compound        ;(make-node Natural X BST BST)
                    ref             ;(node-value BST) is X
                    self-ref        ;(node-left BST) is BST
                    self-ref)       ;(node-right BST) is BST

(define (fn-for-bst bst)
  (cond [(false? bst)
         (...)]
        [else
         (... (node-key bst)
              (node-value bst)
              (fn-for-bst (node-left bst))
              (fn-for-bst (node-right bst)))]))


;;
;; FUNCTIONS
;;


(@htdf lookup)
(@signature BST Natural -> X or false)
;; produce value of node in tree with given key, or false if it does not exist
;!!! examples

(@template-origin BST)

(@template
 (define (lookup tree key)
   (cond [(false? tree)
          (... key)]
         [else
          (... key
               (node-key tree)
               (node-value tree)
               (lookup (node-left tree) key)
               (lookup (node-right tree) key))])))

(define (lookup tree key)
  (cond [(false? tree)
         false]
        [else
         (cond [(= key (node-key tree))
                (node-value tree)]
               [(< key (node-key tree))
                (lookup (node-left tree) key)]
               [else
                (lookup (node-right tree) key)])]))



(@htdf construct-bst construct-bst--acc)
(@signature (listof X) -> BST)
;; construct binary search tree from given list
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
        [else
         (construct-bst--acc (rest lov)
                             (rest (bst-pattern (length lov))) 0
                             empty 1 empty
                             (make-node 0 (first lov) false false)
                             empty)]))

(@template-origin (listof X) accumulator)

(@template
 (define (construct-bst--acc lov depths last-depth working-depths
                             index fold-counts perfect imperfect)
   (cond [(empty? lov)
          (... depths last-depth working-depths
               index fold-counts perfect imperfect)]
         [else
          (... (first lov)
               depths last-depth working-depths
               index fold-counts perfect imperfect
               (construct-bst--acc (rest lov) (... depths)
                                   (... last-depth) (... eff-depth)
                                   (... index) (... fold-counts)
                                   (... perfect) (... imperfect)))])))

;; depths is (listof Natural)
;; INVARIANT: the depth of every node to be created from the corresponding
;;            element in lov; 0 is deepest and greater values are less deep
;; last-depth is Natural
;; INVARIANT: the depth of the last element of lov seen
;; working-depths is (listof Natural)
;; INVARIANT: the maximum node depth of every BST in imperfect
;; index is Natural
;; INVARIANT: the zero-based index of the current element of lov
;; fold-counts is (listof Natural)
;; INVARIANT: the sequence of numbers of fold operations required to construct
;;            a complete binary search tree from perfect and trees in imperfect
;; perfect is BST
;; INVARIANT: the last perfect binary search tree constructed that has not
;;            been added to a larger tree; false if it does not exist
;; imperfect is (listof BST)
;; INVARIANT: the list of all imperfect binary search trees constructed,
;;            in reverse chronological order
(define (construct-bst--acc lov depths last-depth working-depths
                            index fold-counts perfect imperfect)
  (cond [(empty? lov)
         (if (empty? imperfect)
             perfect
             (fold perfect imperfect
                   (first (reverse fold-counts))))]
        [else
         (cond [(zero? (first depths))
                (if (= last-depth 1)
                    (construct-bst--acc (rest lov) (rest depths) 0
                                        (trim working-depths
                                              (first fold-counts))
                                        (add1 index) (rest fold-counts)
                                        (fold (make-node index (first lov)
                                                         false false)
                                              imperfect (first fold-counts))
                                        (trim imperfect (first fold-counts)))
                    (construct-bst--acc (rest lov) (rest depths) 0
                                        working-depths
                                        (add1 index) fold-counts
                                        (make-node index (first lov)
                                                   false false)
                                        imperfect))]
               [(or (empty? imperfect)
                    (not (= (first depths) (sub1 (first working-depths)))))
                (construct-bst--acc (rest lov) (rest depths) (first depths)
                                    (cons (first depths) working-depths)
                                    (add1 index) (cons (first depths)
                                                       fold-counts)
                                    false
                                    (cons (make-node index (first lov)
                                                     perfect false)
                                          imperfect))]
               [else
                (construct-bst--acc (rest lov) (rest depths) (first depths)
                                    (cons (first depths) working-depths)
                                    (add1 index) fold-counts
                                    false
                                    (cons (make-node index (first lov)
                                                     perfect false)
                                          imperfect))])]))



(@htdf fold)
(@signature (listof BST) (listof BST) Natural -> (listof BST))
;; recursively attach perfect to (first imperfect) until empty or count is 0
;!!!

;(define (fold perfect imperfect depths) empty) ;stub

(@template-origin 2-one-of accumulator)

(@template
 (define (fold perfect imperfect count)
   (cond [(and (zero? count) (empty? imperfect))
          (... perfect)]
         [(zero? count)
          (... perfect)]
         [(empty? imperfect)
          (... perfect)]
         [else
          (... perfect (first imperfect) count
               (fold--acc (... perfect)
                          (rest imperfect) (sub1 count)))])))

#|
             count    0              (add1 Natural)
imperfect

empty                 perfect [0]    perfect [0]

(cons BST             perfect [0]    (fold (set-right (first imperfect) perfect)
      (listof BST))                        (rest imperfect) (sub1 count)) [1]
|#

;; perfect is BST
;; INVARIANT: the last perfect binary search tree constructed
(define (fold perfect imperfect count)
  (cond [(or (zero? count) (empty? imperfect)) ;[0]
         perfect]
        [else                                  ;[1]
         (fold (set-right (first imperfect) perfect)
               (rest imperfect) (sub1 count))]))



(@htdf set-right)
(@signature BST BST -> BST)
;; produce first BST with right child set to second BST
;; CONSTRAINT: first BST must be nonempty
;!!!

;(define (set-right bst right) false) ;stub

(@template-origin BST)

(@template
 (define (set-right bst right)
   (cond [(false? bst)
          (...)]
         [else
          (... (node-key bst)
               (node-value bst)
               (fn-for-bst (node-left bst))
               (fn-for-bst (node-right bst))
               right)])))

(define (set-right bst right)
  (cond [(false? bst)
         (error "First BST must be nonempty")]
        [else
         (make-node (node-key bst)
                    (node-value bst)
                    (node-left bst)
                    right)]))



(@htdf bst-pattern)
(@signature Natural -> (listof Natural))
;; produce list of BST node depths for a list with count elements; 0 is deepest
;; CONSTRAINT: count must be nonzero
(check-expect (bst-pattern 1)  (list 0))
(check-expect (bst-pattern 3)  (list 0 1 0))
(check-expect (bst-pattern 10) (list 0 1 0 2 0 1 0 3 0 1))
(check-expect (bst-pattern 15) (list 0 1 0 2 0 1 0 3 0 1 0 2 0 1 0))
;; This really just generates the zero-based ruler sequence (OEIS: A007814)

;(define (bst-pattern count) empty) ;stub

(@template-origin fn-composition)

(define (bst-pattern count)
  (depth->bst-pattern (floor (log2 count)) count))



(@htdf depth->bst-pattern depth->bst-pattern--acc)
(@signature Natural -> (listof Natural))
;; produce list of BST node depths for a BST with given depth, trimmed to count
(check-expect (depth->bst-pattern 1 1)  (list 0))
(check-expect (depth->bst-pattern 2 3)  (list 0 1 0))
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
(check-expect (log2 10000) 13)

;(define (log2 x) 0) ;stub

(@template-origin Number)

(@template
 (define (log2 x)
   (... x)))

(define (log2 x)
  (inexact->exact (round (/ (log x) (log 2)))))
