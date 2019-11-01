#lang racket
(require rackunit)
(require rackunit/text-ui)

; Искаме функция, която приема списък и две числа и връща
; списък, състоящ се от елементите на списъка, които се намират на индекси от първото число до второто.

(define (slice xs start end)
  (define (selector list counter)
    (cond
      ((null? list) '())
      ((< counter start) (selector (cdr list) (+ counter 1)))
      ((and (>= counter start) (<= counter end)) (cons (car list) (selector (cdr list) (+ counter 1))))
      ((> counter end) '())))
  (selector xs 0)
)

(define tests
 (test-suite "Slice tests"
     (check-equal? (slice '(1 9 8 2) 1 2) '(9 8))
     (check-equal? (slice '(1 9 2 8 3) 2 10) '(2 8 3))
     (check-equal? (slice '(9 7 2 3) 0 2) '(9 7 2)) 
  )
)

(run-tests tests 'verbose)
