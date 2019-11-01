#lang racket
(require rackunit)
(require rackunit/text-ui)

; Обръщаме число в двоична бройна система

(define (to-binary number)
  (define (to-binary-iter number power result)
    (cond
      ((= number 0) result)
      (else (to-binary-iter (quotient number 2) (* power 10) (+ (* (remainder number 2) power) result)))))
  (to-binary-iter number 1 0)
)

(define tests
  (test-suite "to-binary tests"
    (check-equal? (to-binary 0) 0)
    (check-equal? (to-binary 1) 1)
    (check-equal? (to-binary 2) 10)
    (check-equal? (to-binary 3) 11)
    (check-equal? (to-binary 4) 100)
    (check-equal? (to-binary 5) 101)
    (check-equal? (to-binary 6) 110)
    (check-equal? (to-binary 7) 111)
    (check-equal? (to-binary 8) 1000)
    (check-equal? (to-binary 9) 1001)
    (check-equal? (to-binary 10) 1010)
    (check-equal? (to-binary 11) 1011)
    (check-equal? (to-binary 12) 1100)
    (check-equal? (to-binary 13) 1101)
    (check-equal? (to-binary 14) 1110)
    (check-equal? (to-binary 15) 1111)
    (check-equal? (to-binary 16) 10000)
    (check-equal? (to-binary 17) 10001)
    (check-equal? (to-binary 18) 10010)
    (check-equal? (to-binary 19) 10011)
    (check-equal? (to-binary 20) 10100)
  )
)

(run-tests tests 'verbose)
