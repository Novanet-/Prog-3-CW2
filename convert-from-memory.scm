#lang scheme

(define make-memory
  (lambda (l)
    (apply vector (reverse l))))


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

(convert-from-memory 7 (make-memory '("m" "n" (9 . 8) (0 . 1) (3 . 4) "b" "a" (6 . 7) "y" "x")))
;;; in this example, Sexp children  may be in higher addresses
;("m" . "n")

(convert-from-memory 2 (make-memory '("m" "n" (9 . 8) (0 . 1) (3 . 4) "b" "a" (6 . 7) "y" "x")))
;;; in this example, Sexp children  may be in lower addresses or higher addresses
;(("x" . "y") . ("m" . "n"))


(let ((Sexp (convert-from-memory 6 (make-memory '((2 . 2) (3 . 4) "b" "a" (0 . 1) "y" "x")))))
  (eq? (car Sexp) (cdr Sexp)))
    ;;; the pair at location 6 has its car and cdr fields pointing to the same address 2
;$test-memory-sharing-is-ignored
;:marks 3
;#f

;(convert-from-memory 6 (make-memory '((6 . 6) (3 . 4) "b" "a" (0 . 1) "y" "x")))
    ;;; the pair at address 6 points to itself
;$test-memory-cycle-does-not-terminate
;:marks 3
;oo


;;; out of range, and incorrect addresses

(convert-from-memory 10 (make-memory '("a")))
;***

(convert-from-memory -10 (make-memory '("a")))
;***

(convert-from-memory 'a (make-memory '("a")))
;***

;;; memory content not compatible with <memory> grammar are not accepted

(convert-from-memory 0 (make-memory '(1234)))
;***

(convert-from-memory 0 (make-memory '(cons a b)))
;***

(map (lambda (x)
       (equal? x '(("x" . "y") . ( "x" . "y"))))
     (list   (convert-from-memory 0 (make-memory '("y" "x" (2 . 3) (1 . 1))))
             (convert-from-memory 0 (make-memory '("x" "y" (3 . 2) (1 . 1))))
             (convert-from-memory 0 (make-memory '("x" (3 . 1) "y" (2 . 2))))
             (convert-from-memory 0 (make-memory '("y" (1 . 3) "x" (2 . 2))))
             (convert-from-memory 0 (make-memory '((1 . 2) "y" "x" (3 . 3))))
             (convert-from-memory 0 (make-memory '((2 . 1) "x" "y" (3 . 3))))
        
             (convert-from-memory 1 (make-memory '("y" "x" (0 . 0) (2 . 3))))
             (convert-from-memory 1 (make-memory '("x" "y" (0 . 0) (3 . 2))))
             (convert-from-memory 1 (make-memory '("x" (3 . 0) (2 . 2) "y")))
             (convert-from-memory 1 (make-memory '("y" (0 . 3) (2 . 2) "x")))
             (convert-from-memory 1 (make-memory '((0 . 2) "y" (3 . 3) "x")))
             (convert-from-memory 1 (make-memory '((2 . 0) "x" (3 . 3) "y")))

             (convert-from-memory 2 (make-memory '("y" (0 . 0) "x" (1 . 3))))
             (convert-from-memory 2 (make-memory '("x" (0 . 0) "y" (3 . 1))))
             (convert-from-memory 2 (make-memory '("x" (1 . 1) (3 . 0) "y")))
             (convert-from-memory 2 (make-memory '("y" (1 . 1) (0 . 3) "x")))
             (convert-from-memory 2 (make-memory '((0 . 1) (3 . 3) "y" "x")))
             (convert-from-memory 2 (make-memory '((1 . 0) (3 . 3) "x" "y")))

             (convert-from-memory 3 (make-memory '((0 . 0) "y" "x" (1 . 2))))
             (convert-from-memory 3 (make-memory '((0 . 0) "x" "y" (2 . 1))))
             (convert-from-memory 3 (make-memory '((1 . 1) "x" (2 . 0) "y")))
             (convert-from-memory 3 (make-memory '((1 . 1) "y" (0 . 2) "x")))
             (convert-from-memory 3 (make-memory '((2 . 2) (0 . 1) "y" "x")))
             (convert-from-memory 3 (make-memory '((2 . 2) (1 . 0) "x" "y")))))
;(#t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t)

              