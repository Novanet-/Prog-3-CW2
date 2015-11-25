#lang racket

(define convert-to-memory-representation
  (lambda (sexp memory mLength)
    (if (and (sexp-valid? sexp) (memory-valid? memory))
        (let ([newMemory (allocate-sexp-to-memory-iter sexp memory mLength)])
          (list (- (list-ref newMemory 2) 1) (list-ref newMemory 1) (list-ref newMemory 2)))
        '()
    )
  )
)

(define allocate-sexp-to-memory-iter
  (lambda (sexp memory mLength)
    (cond [(atom? sexp) (list mLength (cons sexp memory) (+ mLength 1))]
          [(pair? sexp) (let ([left-address (allocate-sexp-to-memory-iter (car sexp) memory mLength)])
                              (let ([right-address (allocate-sexp-to-memory-iter (cdr sexp) (list-ref left-address 1) (list-ref left-address 2))])                          
                                (list (cons (list-ref left-address 0) (list-ref right-address 0))
                                      (cons (cons (list-ref left-address 0) (list-ref right-address 0)) (list-ref right-address 1))
                                      (+ (list-ref right-address 2) (list-ref left-address 2))
                                )
                              )
                        )
          ]          
    )                   
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

        
