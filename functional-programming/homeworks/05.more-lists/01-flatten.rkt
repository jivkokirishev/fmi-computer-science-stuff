#lang racket
(require rackunit)
(require rackunit/text-ui)

; Искаме да направим списък от всички стойности в даден такъв.
; Искаме нивата на влагане да изчезнат.


(define (flatten xs)
  (cond
    ((not (list? xs)) (list xs))
    ((null? xs) xs)
    ((list? xs) (append (flatten (car xs)) (flatten (cdr xs)))))
)

(define tests
  (test-suite "Flatten"
    (check-equal? (flatten '(1 3 ("wow" ("nesting") ("overload" 38 91)))) '(1 3 "wow" "nesting" "overload" 38 91))
    (check-equal? (flatten '(1 2 3 4 5)) '(1 2 3 4 5))
    (check-equal? (flatten '(((3)))) '(3))
  )
)

(run-tests tests 'verbose)
