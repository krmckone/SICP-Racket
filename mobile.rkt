#lang racket

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car(cdr branch)))

(define (total-weight mobile)
  (cond ((null? mobile) 0)
        ((not (pair? mobile)) mobile)
        (else (+ (total-weight(branch-structure(left-branch mobile)))
                 (total-weight(branch-structure(right-branch mobile)))))))

(define x (make-mobile (make-branch 5 5) (make-branch 10 2)))

(total-weight x)

