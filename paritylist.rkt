#lang racket

(define (same-parity x . y)
  (define (parity-helper x y-list new-list)
    (cond ((null? y-list) new-list)
          ((and (even? x) (even? (car y-list))) (parity-helper x (cdr y-list) (append new-list (list(car y-list)))))
          ((and (odd? x) (odd? (car y-list))) (parity-helper x (cdr y-list) (append new-list (list(car y-list)))))
          (else (parity-helper x (cdr y-list) new-list))))
  (parity-helper x y (list x)))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)
