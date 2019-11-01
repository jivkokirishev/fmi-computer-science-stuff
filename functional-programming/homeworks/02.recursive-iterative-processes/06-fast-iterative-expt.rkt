#lang racket
(require rackunit)
(require rackunit/text-ui)

; Стъпвайки на дефиницията за бързо повдигане на степен,
; търсим такава, която генерира итеративен процес
(define (expt x n)
  (define (fast-pow-iter tmp-num power result)
    (cond
      ((= 0 power) 1)
      ((= 1 power) result)
      (else (if (odd? power)
                (fast-pow-iter (* tmp-num tmp-num) (quotient power 2) (* result tmp-num tmp-num))
                (fast-pow-iter (* tmp-num tmp-num) (quotient power 2) (* result tmp-num))))))
  (fast-pow-iter x n x))

(define tests
  (test-suite "expt tests"
    (check-equal? (expt 4 4) 256)
    (check-equal? (expt 29139123 0) 1)
    (check-equal? (expt 3 4) 81)
    (check-equal? (expt 2 1) 2)
  )
)

(run-tests tests 'verbose)
