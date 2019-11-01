#lang racket
(require rackunit)
(require rackunit/text-ui)


; Търсим сумата от цифрите на дадено число.
; Процедурата да генерира итеративен процес.
(define (sum-digits number)
  (define (sum-digits-iter number result)
    (if (= 0 number)
        result
        (sum-digits-iter (quotient number 10) (+ result (remainder number 10)))))
  (sum-digits-iter number 0)
)

(define tests
  (test-suite "to-decimal tests"
    (check-equal? (sum-digits 11001) 3)
    (check-equal? (sum-digits 804357) 27)
    (check-equal? (sum-digits 981) 18)
  )
)

(run-tests tests 'verbose)
