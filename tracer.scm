#lang scheme

(define tracer
  (lambda (roots memVector)
    (let ([memSize (length memVector)])
      (if (<= (length roots) memsize)
          (trace-roots roots memVector '() memSize  0)
          '()
      )
    )
  )
)

(define trace-roots
  (lambda (roots memVector fullRootList memSize acc)
    (if (>= acc memSize)
        fullRootList
        (if (memq? (vector-ref memVector acc) roots)
            (cons (return-element memVector acc) fullRootList)
            (trace-roots roots memVector fullRootList memSize (+ acc 1))
        )
    )
  )
)

(define return-element
  (lambda (memVector index)
    (let ([element (vector-ref memVector index)])
          (if (string? element)
              (cons element '())
              (cons (return-element memVector (car element)) (return-element memVector (cdr element)))
          )
    )
  )
)