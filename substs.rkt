#lang racket
(define (subsets s)
  (if (null? s)
      (list null)
      (let ((rest (subsets (cdr s))))
        (append rest (map list rest)))))
(subsets (list 1 2 3))