#lang racket
(require rackunit)
(require rackunit/text-ui)

; Искаме да дефинираме следните имена: one, two, three, ..., nine, plus, minus, times, div,
; така че извиквания от типа на (one (plus (three))) (операция с точно две операнди) да връщат легитимни числови стойности (в този случай - 4)
; Още малко примери:
; (three (times (five))) -> 15
; (nine (div (three))) -> 3
; (eight (minus (four))) -> 4
;

(define (operation oper)
  (lambda (x) (lambda (y) (oper y x))))

(define (digit num)
  (lambda args (if (= (length args) 0)
                           num
                           ((car args) num))))


(define plus (operation +))
(define minus (operation -))
(define times (operation *))
(define div (operation /))

(define zero (digit 0))
(define one (digit 1))
(define two (digit 2))
(define three (digit 3))
(define four (digit 4))
(define five (digit 5))
(define six (digit 6))
(define seven (digit 7))
(define eight (digit 8))
(define nine (digit 9))

(define digits-tests
  (test-suite "digit results"
    (check-equal? (zero) 0)
    (check-equal? (one) 1)
    (check-equal? (two) 2)
    (check-equal? (five) 5)
    (check-equal? (nine) 9)
  )
)

(define +tests
  (test-suite "words operations with +"
    (check-equal? (three (plus (five))) 8)
    (check-equal? (five (plus (three))) 8)
    (check-equal? (seven (plus (zero))) 7)
  )
)

(define -tests
  (test-suite "word operations with -"
    (check-equal? (eight (minus (four))) 4)
    (check-equal? (four (minus (eight))) -4)
  )
)

(define *tests
  (test-suite "word operations with *"
    (check-equal? (five (times (five))) 25)
    (check-equal? (one (times (nine))) 9)
    (check-equal? (zero (times (four))) 0)
  )
)

(define /tests
  (test-suite "word operations with /"
    (check-equal? (five (div (five))) 1)
    (check-equal? (six (div (three))) 2)
    (check-equal? (seven (div (two))) 7/2)
  )
)


(run-tests digits-tests 'verbose)
(run-tests +tests 'verbose)
(run-tests -tests 'verbose)
(run-tests *tests 'verbose)
(run-tests /tests 'verbose)