#lang racket

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (rev-fold-left sequence)
  (fold-left (lambda (x y) (append (list y) (list x))) null sequence))

(define (enumerate-interval low high)
  (if (> low high)
      null
      (cons low (enumerate-interval (+ low 1) high))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))


(define (flatmap proc seq)
  (accumulate append null (map proc seq)))

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (unique-triples n)
  (flatmap (lambda (i)
             (flatmap (lambda (j)
                    (map (lambda (k) (list i j k))
                         (enumerate-interval 1 n)))
                  (enumerate-interval 1 n)))
           (enumerate-interval 1 n)))

(define (make-triple-sum triple)
  (list (car triple) (car (cdr triple)) (car (cdr (cdr triple))) (+ (car triple) (car (cdr triple)) (car (cdr (cdr triple))))))

(define (triples-sum-to-s n s)
  (define (triple-sum-to-s? triple)
    (= (+
        (car triple)
        (car (cdr triple))
        (car (cdr (cdr triple))))
       s))
  (map make-triple-sum
       (filter triple-sum-to-s? (unique-triples n)))) 

(triples-sum-to-s 5 10)