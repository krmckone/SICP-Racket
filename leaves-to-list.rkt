#lang racket

(define (fringe tree-list)
  (define (fringe-help tree)
    (cond ((null? tree) null)
          ((not (pair? tree)) (list tree))
          (else (append (fringe-help (car tree))
                        (fringe-help (cdr tree))))))
  (fringe-help tree-list))

(define x (list (list 1 2) (list 3 4)))

(fringe (list x x))