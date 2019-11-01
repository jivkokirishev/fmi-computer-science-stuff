#lang racket
(require rackunit)
(require rackunit/text-ui)


; Обръщаме число от двоична в десетична бройна система
(define (to-decimal number)
  (define (to-decimal-iter number power result)
    (if (= number 0)
        result
        (to-decimal-iter (quotient number 10) (+ power 1) (+ result (* (remainder number 10) (expt 2 power))))))
  (to-decimal-iter number 0 0)
)

(define tests
  (test-suite "to-decimal tests"
    (check-equal? (to-decimal 11001) 25)
    (check-equal? (to-decimal 1100011) 99)
  )
)

(run-tests tests 'verbose)
