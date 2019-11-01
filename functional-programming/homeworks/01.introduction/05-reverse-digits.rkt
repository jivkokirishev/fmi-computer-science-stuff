#lang racket
(require rackunit)
(require rackunit/text-ui)
; 1.6 - Съчинете процедура, която обръща цифрите на дадено число.
; Трябва да работи и за отрицателни числа.

(define (reverse-algo new-number old-number)
  (if (not (= old-number 0))
      (reverse-algo (+  (* 10 new-number) (modulo old-number 10)) (floor (/ old-number 10)))
      new-number
      )
  )

(define (reverse-digits number)
  (if (< number 0)
      (* -1 (reverse-algo 0 (abs number)))
      (reverse-algo 0 number)
  )
)

(define tests
  (test-suite
  "Reverse digits tests"

    (test-case "Should reverse correctly"
      (check-equal? (reverse-digits 2134) 4312)
    )

    (test-case "Should work alright with digits"
      (let ((k (random 10)))
        (check-equal? (reverse-digits k) k))
    )

    (test-case "Should work with negative numbers"
      (check-equal? (reverse-digits -298245) -542892)
    )
  )
)

(run-tests tests)
