#lang racket
(require rackunit)
(require rackunit/text-ui)

; Търсим функция, която конкатенира два списъка

(define (last xs)
  (if (null? (cdr xs))
      (car xs)
      (last (cdr xs)))
)

(define (remove-last xs)
  (define (remove xs new-xs)
  (if (null? (cdr xs))
      new-xs
      (remove (cdr xs) (cons (car xs) new-xs))))

  (define (reverse xs new-xs)
  (if (null? xs)
      new-xs
      (reverse (cdr xs) (cons (car xs) new-xs))))

  (reverse (remove xs '()) '())
)

(define (append-iter xs ys)
  (cond
    ((null? xs) ys)
    ((null? ys) xs)
    (else (append-iter (remove-last xs) (cons (last xs) ys))))
)

(define (append xs ys)
  (cond
    ((null? xs) ys)
    (else (cons (car xs) (append (cdr xs) ys)))))

(define tests
  (test-suite "append tests"
    (check-equal? (append '(5 9 2) '(1)) '(5 9 2 1))
    (check-equal? (append '() '(2 3)) '(2 3))
    (check-equal? (append '(2 3) '()) '(2 3))
    (check-equal? (append '(1 8 6 2 3) '(2 3)) '(1 8 6 2 3 2 3))
  )
)

(run-tests tests 'verbose)
