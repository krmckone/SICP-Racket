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

(define (torque branch)
  (if (not (pair? (branch-structure branch)))
      (* (branch-length branch)
         (branch-structure branch))
      (torque (branch-structure branch))))

(define (balanced? mobile)
  (= (torque (left-branch mobile))
     (torque (right-branch mobile))))

(balanced? (make-mobile (make-branch 4 (make-mobile (make-branch 2 2) (make-branch 2 4))) (make-branch 2 10)))

