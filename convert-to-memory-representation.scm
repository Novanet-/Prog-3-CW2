

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

(define convert-to-memory-representation*
  (lambda (mini-Sexp memory)
    (convert-to-memory-representation mini-Sexp memory (length memory))))

(convert-to-memory-representation* "a" '())
;(0 ("a") 1)

(convert-to-memory-representation* "a" '("x" "y"))
;(2 ("a" "x" "y") 3)

(convert-to-memory-representation* "x" '("x" "y"))
;(2 ("x" "x" "y") 3)

(convert-to-memory-representation* (cons "a" "b") '())
;(2 ((0 . 1) "b" "a") 3)

(convert-to-memory-representation* (cons (cons "x" "y") (cons "a" "b")) '())
;(6 ((2 . 5) (3 . 4) "b" "a" (0 . 1) "y" "x") 7)

        
