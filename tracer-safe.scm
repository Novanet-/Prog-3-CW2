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

(define make-memory
  (lambda (l)
    (apply vector (reverse l))))

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


(define sexp-valid?
  (lambda (sexp)
    (cond [(atom? sexp) (string? sexp)]
          [(pair? sexp) (begin (number? (car sexp)) (number? (cdr sexp)))]
          [else #f]
    )
  )
)

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

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
    
    










(tracer '(0) (make-memory '("a")))
;(0)

(tracer '(2) (make-memory '("a" "x" "y")))
;(2)

(tracer '(1) (make-memory '("a" "x" "y")))
;(1)

(tracer '(1 1) (make-memory '("a" "x" "y")))
;(1)

(reachability-table (tracer '(1 2) (make-memory '("a" "x" "y"))) 3)
;#(0 1 1)  ;;; this solution is dependent on the order of traversal

(tracer '(1 2) (make-memory '("a" "x" "y")))
  ;;; this solution is dependent on the order of traversal, so give all possible answers
;(or (1 2)
;    (2 1))


(tracer '(3) (make-memory '("w" "x" (1 . 5) "z" "y" "a")))
  ;;; this solution is dependent on the order of traversal, so give all possible answers
;;;(or (1 3 5)
;    (3 1 5)
;    (3 5 1)     
;    (1 5 3)
;    (5 3 1)
;    (5 1 3))

(let ((memory-list '((2 . 5) (3 . 4) "b" "a" (0 . 1) "y" "x")))
  (reachability-table (tracer '(6) (make-memory memory-list)) (length memory-list)))
;#(1 1 1 1 1 1 1)

(let ((memory-list '("hi" "foo" (2 . 5) (3 . 4) "b" "a" (0 . 1) "y" "x")))
  (reachability-table (tracer '(6) (make-memory memory-list)) (length memory-list)))
;;#(1 1 1 1 1 1 1 0 0)

(let ((memory-list '("z" (6 . 6) (3 . 4) "b" "a" (0 . 1) "y" "x")))
       ;;; at address 6, we find a pair with car and cdr pointing to address 6
  (reachability-table (tracer '(6) (make-memory memory-list)) (length memory-list)))
  ;;; the pair at address 6 has a car and cdr pointing to itself
;;$test-cycle1
;;:marks 3
;;#(0 0 0 0 0 0 1 0)


(let ((memory-list '("c" (1 . 6) (3 . 4) "b" "a" (0 . 1) (6 . 1) "x")))
       ;;; at address 6, we find a pair with car and cdr pointing to address 6
  (reachability-table (tracer '(6) (make-memory memory-list)) (length memory-list)))
  ;;; the pair at address 6 has a car and cdr pointing to itself
;;$test-cycle2
;;#(0 1 0 0 0 0 1 0)

(let ((memory-list '("c" (1 . 6) (3 . 4) "b" "a" (0 . 1) (6 . 1) "x")))
       ;;; at address 6, we find a pair with car and cdr pointing to address 6
  (tracer '(6) (make-memory memory-list)))
  ;;; the pair at address 6 has a car and cdr pointing to itself
  ;;; result is order dependent
;;(or (1 6)
;;    (6 1))


(let ((memory-list '("hi" (1 . 1) (3 . 4) "b" "a" (0 . 1) "y" "x")))
  (tracer '(6) (make-memory memory-list)))
  ;;; the pair at address 6 has a car and cdr pointing address 1
;;$test-sharing1
;;; this solution may be dependent on the order of traversal
;;(or (1 6)
;;    (6 1))

(let ((memory-list '((5 . 5) (3 . 4) "b" "a" (0 . 1) "y" "x")))
   ;;; at address 6, we find a pair with car and cdr pointing to address 5
  (reachability-table (tracer '(6) (make-memory memory-list)) (length memory-list)))
;;$test-sharing2
;;#(0 0 0 1 1 1 1)

(let ((memory-list '((1 . 2) (3 . 4) "b" "a" (0 . 1) "y" "x")))
  (reachability-table (tracer '(6) (make-memory memory-list)) (length memory-list)))
;;#(1 1 1 0 0 0  1)

;;; wrong memory structure    
(tracer '(0) (make-memory '(a)))
;;***
(tracer '(0) (make-memory '(1)))
;;***
;;; wrong address
(tracer '(1) (make-memory '(1)))
;;***


