#lang scheme

(define reachability-table
  (lambda (l size)
    (let ((table (make-vector size 0)))
      (for-each (lambda (address)
                  (if (and (number? address) 
                           (< address size)
                           (>= address 0))
                      (vector-set! table address 1)
                      (error 'reachability-table
                             "not valid address"
                             address)))
                l)
      table)))
;;:marks 0
;;---

(define make-memory
  (lambda (l)
    (apply vector (reverse l))))
;;:marks 0          
;;---


(begin
  (define size 0)
  (define mem '())
  (define mini-null "null")  ;;; reserved symbol for end of list
  (define mini-cons (lambda (x y)
                      (let ((address size))
                        (set! mem (cons (cons  x y) mem))
                        (set! size (+ size 1))
                        address)))
  (define mini-quote (lambda (x)
                       (let ((address size))
                         (set! mem (cons x mem))
                         (set! size (+ size 1))
                         address)))
  (define deref (lambda (x)
                  (list-ref mem (- (- size 1) x))))
  (define mini-null? (lambda (x)
                       (eq? (deref x) mini-null)))
  (define mini-car (lambda (x)
                     (car (deref x))))
  (define mini-cdr (lambda (x)
                     (cdr (deref x))))
  (define mini-append (lambda (l1 l2)
                        (if (mini-null? l1)
                            l2
                            (mini-cons (mini-car l1) (mini-append (mini-cdr l1) l2))))))
;;:marks 0

(define convert-to-memory-representation
  (lambda (sexp memory mLength)
    (if (and (sexp-valid? sexp) (memory-valid? memory))
        (let ([newMemory (allocate-sexp-to-memory-iter sexp memory mLength)])
          (list (- (get-memLength newMemory) 1) (get-mem newMemory) (get-memLength newMemory)))
        '()
    )
  )
)



(define allocate-sexp-to-memory-iter
  (lambda (sexp memory mLength)
    (cond [(atom? sexp) (list mLength (cons sexp memory) (+ mLength 1))]
          [(pair? sexp) (let ([left-address (allocate-sexp-to-memory-iter (car sexp) memory mLength)])
                              (let ([right-address (allocate-sexp-to-memory-iter (cdr sexp) (get-mem left-address) (get-memLength left-address))])                          
                                (list (get-memLength right-address)
                                      (cons (cons (get-sexp left-address) (get-sexp right-address)) (get-mem right-address))
                                      (+ (get-memLength right-address) 1)
                                )
                              )
                        )
          ]          
    )                   
  )
)

(define get-sexp
  (lambda (memList)
    (list-ref memList 0)
  )
)

(define get-mem
  (lambda (memList)
    (list-ref memList 1)
  )
)

(define get-memLength
  (lambda (memList)
    (list-ref memList 2)
  )
)

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define sexp-valid?
  (lambda (sexp)
    (cond [(atom? sexp) (string? sexp)]
          [(pair? sexp) (begin (sexp-valid? (car sexp)) (sexp-valid? (cdr sexp)))]
          [else #f]
    )
  )
)

(define memory-valid?
  (lambda (memory)
    (cond [(null? memory) #t]
          [(pair? memory) (begin (memory-contents? (car memory)) (memory-valid? (cdr memory)))]
          [else #f]
    )
  )
)

(define memory-contents?
  (lambda (contents)
    (cond [(string? contents) #t]
          [(pair? contents) (and (number? (car contents)) (number? (cdr contents)))]
          [else #f]
    )
  )
)

;;Checks that the length of the list of roots is less than or equal to the size of the memory
;;Then calls a function to return the full list of reachable address
(define tracer
  (lambda (roots memVector)
    (let ([memSize (vector-length memVector)])
      (if (<= (length roots) memSize)
          (trace-roots roots memVector '() memSize  0)
          '()
      )
    )
  )
)

;;Checks that the current index being look at is within the bounds of the memory, if the accumulator has reached the end of the memory, return the list of rachable addresses
;;Otherwise, check if the current address at the accumulator index is in the lsit of roots, and hasn't already been marked as reachable
;;If so, then add the current address (and all sub-addresses if it's a pair) to the reachabile address list
;;Otherwise, call this function again, increasing the accumulator by 1
(define trace-roots
  (lambda (roots memVector reachableList memSize acc)
    (if (>= acc memSize)
        reachableList
        (if (and (memq acc roots) (not (memq acc reachableList)))
            (trace-roots roots memVector (return-address memVector acc reachableList '()) memSize (+ acc 1))
            (trace-roots roots memVector reachableList memSize (+ acc 1))
        )
    )
  )
)

;;If the memory element at the address is a string, then add the address of that element to the reachable list
;;Otherwise, it is a pair, so call this function again with the left and right addresses of the pair
(define return-address
  (lambda (memVector index reachableList seen)
          (let ([currentElement (vector-ref memVector index)])
          (if (not (memq index seen))
              (if (string? currentElement)
                  (cons index reachableList)              
                  (let ([left-pair-address (left-address memVector index)]
                        [right-pair-address (right-address memVector index)]
                       )
                       (let ([left-cycle (or (= left-pair-address index) (memq left-pair-address reachableList))]
                             [right-cycle  (or (= right-pair-address index) (memq right-pair-address reachableList))]
                             )                                
                             (cond [(or (and left-cycle right-cycle) (and (memq left-pair-address seen) (memq right-pair-address seen)))  (cons index reachableList)]
                                   [(or (and left-cycle (not right-cycle)) (memq left-pair-address seen) ) (cons index (return-address memVector right-pair-address reachableList (cons index seen)))]
                                   [(or (and right-cycle (not left-cycle)) (memq right-pair-address seen) ) (cons index (return-address memVector left-pair-address reachableList (cons index seen)))]
                                   [else (if (= left-pair-address right-pair-address)
                                     (cons index (return-address memVector left-pair-address reachableList (cons index seen)))                                     
                                     (cons index (return-address memVector left-pair-address (return-address memVector right-pair-address reachableList (cons index seen)) (cons index seen)))
                                     )]
                    )
                   )
              )
          )
          reachableList
          )
          )
  )
)

;;Returns the left address of a pair
(define left-address
  (lambda (memVector index)
    (let ([element (vector-ref memVector index)])
      (car element)
    )
  )
)

;;Returns the right address of a pair
(define right-address
  (lambda (memVector index)
    (let ([element (vector-ref memVector index)])
      (cdr element)
    )
  )
)



(define memVector-valid?
  (lambda (memVector)
    (let ([validityVector (vector-map sexp-valid? memVector)])
     (only-contains-#t? (vector->list validityVector))
    )
  )
)

(define (boolean-true? x) (eq? x #t))

(define (only-contains-#t? l)
  (andmap boolean-true? l))

(define convert-from-memory
  (lambda (address memory-vector)
    (let ([memLength (vector-length memory-vector)])
      (if (number? address)
      (if (and (<= address memLength) (>= address 0))
          (find-sexp address memory-vector memLength '())
          '()
      )
      '()
      )
    )
  )
)

(define find-sexp
  (lambda (address memory-vector memLength seenElements)
    (let ([requestedPosition address])
      (let ([requestedElement (vector-ref memory-vector requestedPosition)])
        (cond [(string? requestedElement) requestedElement]
              [(pair? requestedElement)
                                              (if (memq requestedElement seenElements)
                                                  requestedElement
                                                  (cons (find-sexp (car requestedElement) memory-vector memLength (cons requestedElement seenElements))
                                                        (find-sexp (cdr requestedElement) memory-vector memLength (cons requestedElement seenElements)))
                                              )]
              [else '()]
        )
      )
    )
  )
)


(define copy-memory
  (lambda (roots memVector)
    (let ([reachableList (tracer roots memVector)]
             ;;[rootVector (list->vector roots)]
             ;;[rootElements (elements-at-reachable-addresses rootVector memVector (vector-length rootVector) 0)]
             ;;[reachableElements (elements-at-reachable-addresses reachable memVector (vector-length reachableVector) 0)]))
)
             (elements-at-reachable-addresses (list->vector reachableList) memVector (length reachableList) 0)
    )
  )
)

(define elements-at-reachable-addresses
  (lambda (reachableVector memVector reachableAddsLength acc)
    (if (>= acc reachableAddsLength)
        '()
        (let ([reachableElement (vector-ref memVector (vector-ref reachableVector acc))])
              (cons reachableElement (elements-at-reachable-addresses reachableVector memVector reachableAddsLength (+ acc 1)))
              (cons (convert-from-memory (vector-ref reachableVector acc) memVector)
                    (elements-at-reachable-addresses reachableVector memVector reachableAddsLength (+ acc 1)))
        )
    )
  )
)


;;Takes a vector of the root elements, and iterates through it, adding each root element (all all other elements it points to) to a new list of memory
;;It returns a list, first argument is the new root addresses, second argument is the new memory list, third is the length of the new memory
(define make-new-memory
  (lambda (newRoots newMemory rootElements rootElementsLength acc seenElements)
    (if (>= acc rootElementsLength)
        (list newRoots newMemory (length newMemory))
            (let (;;[rootElement (vector-ref rootElements acc)]
                     [addedElementRepresentation (convert-to-memory-representation (vector-ref rootElements acc) '() (length newMemory))]
                 )
              (if (or (memq (vector-ref rootElements acc) seenElements) (null? addedElementRepresentation))
                  (make-new-memory newRoots newMemory rootElements rootElementsLength (+ acc 1) seenElements)
                  (make-new-memory (cons (get-sexp addedElementRepresentation) newRoots) (append (get-mem addedElementRepresentation) newMemory)
                             rootElements rootElementsLength (+ acc 1) (cons (vector-ref rootElements acc) seenElements))
              )
            )
    )
  )
)

(define assign-new-mem-position
  (lambda (element memory)
    (convert-to-memory-representation element memory (length memory))
   )
)
    





    
(copy-memory '(6) (make-memory '((2 . 5) (3 . 4) "b" "a" (0 . 1) "y" "x")))
;:marks 0
;((0) ("b" "a" (5 . 6) "y" "x" (2 . 3) (1 . 4)) 7) ;;; this solution is dependent on the order of traversal, so we don't give marks

(copy-memory '(5) (make-memory '((2 . 5) (5 . 5) "b" "a" (0 . 1) "y" "x")))
;$test-copy-cycle
;((0) ((0 . 0)) 1)


(copy-memory '(6) (make-memory '((6 . 5) (5 . 6) "b" "a" (0 . 1) "y" "x")))
;$test-copy-cycle2
;(or ((0) ((1 . 0) (0 . 1)) 2)
;    ((1) ((1 . 0) (0 . 1)) 2))

(copy-memory '(6) (make-memory '((2 . 2) (3 . 4) "b" "a" (0 . 1) "y" "x")))
;;$test-copy-sharing ;;; this solution is dependent on the order of traversal
;(or ((0) ("y" "x" (2 . 3) (1 . 1)) 4) 
;    ((0) ("x" "y" (3 . 2) (1 . 1)) 4)
 ;   ((0) ("x" (3 . 1) "y" (2 . 2)) 4)
  ;  ((0) ("y" (1 . 3) "x" (2 . 2)) 4)
  ;  ((0) ((1 . 2) "y" "x" (3 . 3)) 4)
  ;  ((0) ((2 . 1) "x" "y" (3 . 3)) 4)

;    ((1) ("y" "x" (0 . 0) (2 . 3)) 4) 
;    ((1) ("x" "y" (0 . 0) (3 . 2)) 4)
 ;   ((1) ("x" (3 . 0) (2 . 2) "y") 4)
  ;  ((1) ("y" (0 . 3) (2 . 2) "x") 4)
   ; ((1) ((0 . 2) "y" (3 . 3) "x") 4)
    ;((1) ((2 . 0) "x" (3 . 3) "y") 4)
    
;    ((2) ("y" (0 . 0) "x" (1 . 3)) 4) 
 ;   ((2) ("x" (0 . 0) "y" (3 . 1)) 4)
  ;  ((2) ("x" (1 . 1) (3 . 0) "y") 4)
   ; ((2) ("y" (1 . 1) (0 . 3) "x") 4)
    ;((2) ((0 . 1) (3 . 3) "y" "x") 4)
    ;((2) ((1 . 0) (3 . 3) "x" "y") 4)
    
;    ((3) ((0 . 0) "y" "x" (1 . 2)) 4) 
 ;   ((3) ((0 . 0) "x" "y" (2 . 1)) 4)
  ;  ((3) ((1 . 1) "x" (2 . 0) "y") 4)
   ; ((3) ((1 . 1) "y" (0 . 2) "x") 4)
    ;((3) ((2 . 2) (0 . 1) "y" "x") 4)
    ;((3) ((2 . 2) (1 . 0) "x" "y") 4)
    
;)
    
