#lang racket

(define (square-list items)
  (if (null? items)
      null
      (cons ((lambda (x) (* x x)) car items)
            (square-list cdr items))))

(define (square-list-map items)
  (map (lambda (x) (* x x)) items))

(define (square-list-iter items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons ((lambda (x) (* x x)) (car things))
                    answer))))
  (iter items null))

(define (for-each procedure list)
  (if (null? list) true
      (procedure (car list)))
  (if (null? (cdr list)) true
      (for-each procedure (cdr list))))

(for-each (lambda (x)
            (display x)
            (newline))
          (list 57 321 88))

