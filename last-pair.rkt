#lang racket

(define (last-pair list)
  (if (null? (cdr list))
      (car list)
      (last-pair (cdr list))))

(define (reverse rev_list)
  (define (rev-help rev-list index new-list)
    (if (< index 0)
        new-list
        (rev-help rev-list (- index 1) (append new-list (list (list-ref rev-list index))))))
  (rev-help rev_list (- (length rev_list) 1) (list)))
