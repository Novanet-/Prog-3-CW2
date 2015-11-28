#lang scheme

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
      (if (and (<= (length roots) memSize) (memVector-valid? memVector))
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
          (find-sexp address memory-vector memLength)
          '()
      )
      '()
      )
    )
  )
)

(define find-sexp
  (lambda (address memory-vector memLength)
    (let ([requestedPosition address])
      (let ([requestedElement (vector-ref memory-vector requestedPosition)])
        (cond [(string? requestedElement) requestedElement]
              [(pair? requestedElement) (cons (find-sexp (car requestedElement) memory-vector memLength)
                                              (find-sexp (cdr requestedElement) memory-vector memLength))]
              [else '()]
        )
      )
    )
  )
)

(define make-memory
  (lambda (l)
    (apply vector (reverse l))))

(define copy
  (lambda (roots memVector)
    (letrec ([reachableVector (list->vector (tracer roots memVector))]
             [rootVector (list->vector roots)]
             [rootElements (elements-at-reachable-addresses rootVector memVector (vector-length rootVector) 0)]
             [reachableElements (elements-at-reachable-addresses reachableVector memVector (vector-length reachableVector) 0)])
          rootElements
    )
  )
)

(define elements-at-reachable-addresses
  (lambda (reachableVector memVector reachableAddsLength acc)
    (if (>= acc reachableAddsLength)
        '()
        (let ([reachableElement (vector-ref memVector (vector-ref reachableVector acc))])
          ;;(if (pair? reachableElement)
              ;;(cons (vector-ref memVector (vector-ref reachableVector acc)) (elements-at-reachable-addresses reachableVector memVector reachableAddsLength (+ acc 1)))
              (cons (convert-from-memory (vector-ref reachableVector acc) memVector) (elements-at-reachable-addresses reachableVector memVector reachableAddsLength (+ acc 1)))
          ;;)
        )
    )
  )
)

(define make-new-memory
  (lambda (newMemory rootElements)
    (if (null? rootElements)
        '()
        (letrec ([rootElement (car rootelements)]
                 [addedElementRepresentation (convert-to-memory-representation element memory (length memory))]
        )
        (map cons 
    )
    )
  )

(define assign-new-mem-position
  (lambda (element memory)
    (convert-to-memory-representation element memory (length memory))
   )
)
    


;;(copy '(0 1) (make-memory '((0 . 1) "y" "x")))
;;(copy '(2) (make-memory '("b" "a" (0 . 1) "y" "x")))
  