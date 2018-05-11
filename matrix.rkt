#lang racket

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      null
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-times-vector m v)
  (map (lambda (row) (dot-product row v)) m))

(define matrix (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9) (list 5 8 9 2)))
(define vector (list 1 2 3 4))

;(matrix-times-vector matrix vector)

(define (transpose mat)
  (accumulate-n cons null mat))

;(transpose matrix)

(define (matrix-times-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (matrix-times-vector cols row)) m)))

(matrix-times-matrix matrix matrix)