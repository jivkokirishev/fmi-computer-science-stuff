#lang racket
(require rackunit)
(require rackunit/text-ui)

(define (append xs ys)
  (cond
    ((null? xs) ys)
    (else (cons (car xs) (append (cdr xs) ys)))))

; Търсим функция, която обръща даден списък
(define (reverse xs)
  (if (null? xs)
      xs
      (append (reverse (cdr xs)) (list (car xs))))
)

; И нейн итеративен вариант
(define (reverse-iter xs)
  (define (reverse xs new-xs)
  (if (null? xs)
      new-xs
      (reverse (cdr xs) (cons (car xs) new-xs))))

  (reverse xs '())
)

(define tests
  (test-suite "Reverse tests"
      (check-equal? (reverse-iter '(1 2 3)) (reverse '(1 2 3)))
      (check-equal? (reverse '()) '())
      (check-equal? (reverse '(1)) '(1))
      (check-equal? (reverse '(1 5)) '(5 1))
  )
)

(run-tests tests 'verbose)