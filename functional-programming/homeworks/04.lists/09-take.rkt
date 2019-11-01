#lang racket
(require rackunit)
(require rackunit/text-ui)

(define (append xs ys)
  (cond
    ((null? xs) ys)
    (else (cons (car xs) (append (cdr xs) ys)))))

(define (reverse xs)
  (if (null? xs)
      xs
      (append (reverse (cdr xs)) (list (car xs))))
)
; Търсим функция, която връща списък от първите n елемента на даден такъв.

(define (take n xs)
  (cond
    ((= n 0) '())
    ((null? xs) xs)
    (else (append (list (car xs)) (take (- n 1) (cdr xs)))))
)

(define tests
  (test-suite "Take tests"
     (check-equal? (take 2 '(1 2 3)) '(1 2))
     (check-equal? (take 0 '(2 9 2)) '())
     (check-equal? (take 2134 '(9 7 2 3)) '(9 7 2 3))
  )
)

(run-tests tests 'verbose)
