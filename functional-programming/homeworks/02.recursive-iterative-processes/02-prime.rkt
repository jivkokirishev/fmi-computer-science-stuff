#lang racket
(require rackunit)
(require rackunit/text-ui)

; Искаме да проверим дали число е просто.

(define (prime? number)
  (define (prime-iter? number divisor)
    (if (< divisor (sqrt number))
        (if (= (remainder number divisor) 0)
            #f
            (prime-iter? number (+ divisor 1)))
        #t))
  (prime-iter? number 2)
)

(define tests
  (test-suite "prime? tests"
    ;(check-false (prime? 1))
    (check-true (prime? 5))
    (check-false (prime? 1729))
    (check-false (prime? 41041))
  )
)

(run-tests tests 'verbose)
