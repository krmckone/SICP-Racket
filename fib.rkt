#lang racket

(define (fib n a b)
  (cond((= n a) 1)
       ((= n b) 0)
       (else (+ (fib (- n 1) 1 0) (fib (- n 2) 1 0)))))

(fib 10 1 0)