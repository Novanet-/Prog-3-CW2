
(define make-memory
  (lambda (l)
    (apply vector (reverse l))))


(define convert-from-memory
  (lambda (address memory-vector)
    (let ([memLength (vector-length memory-vector)])
      (if (<= address memLength)
          (find-sexp address memory-vector memLength)
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
              [else #f]
        )
      )
    )
  )
)

(convert-from-memory 0 (make-memory '("a")))
;"a"

(convert-from-memory 0 (make-memory '(a)))
;***

(convert-from-memory 0 (make-memory '(1234)))
;***

(convert-from-memory 0 (make-memory '(())))
;***

(convert-from-memory 2 (make-memory '("a" "x" "y")))
;"a"

(convert-from-memory 1 (make-memory '("a" "x" "y")))
;"x"

(convert-from-memory 2 (make-memory '((0 . 1) "b" "a")))
;("a" . "b")

(convert-from-memory 6 (make-memory '((2 . 5) (3 . 4) "b" "a" (0 . 1) "y" "x")))
;(("x" . "y") . ("a" . "b"))

(convert-from-memory 6 (make-memory '((2 . 2) (3 . 4) "b" "a" (0 . 1) "y" "x")))
;(("x" . "y") . ("x" . "y"))
              