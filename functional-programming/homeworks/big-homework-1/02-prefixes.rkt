#lang racket
(require rackunit)
(require rackunit/text-ui)

; Искаме да намерим всички префикси на даден списък
; Например за '(1 2 3), това са '(), '(1), '(1 2) и '(1 2 3)

(define (remove-last list)
  (if (null? (cdr list))
      '()
      (cons (car list) (remove-last (cdr list)))))

(define (prefixes xs)
  (define (prefix-iterator xs result)
    (if (null? xs)
         (cons '() result)
        (prefix-iterator (remove-last xs) (cons xs result))))
  (prefix-iterator xs '()))

(define tests
  (test-suite "prefixes edge cases"
    (check-equal?  (prefixes '()) '(()))
    (check-equal?  (prefixes '(1)) '(() (1)))
    (check-equal?  (prefixes '(1 2)) '(() (1) (1 2)))
    (check-equal? (prefixes '(1 2 3 43 53 3 2 2)) '(() (1) (1 2) (1 2 3) (1 2 3 43) (1 2 3 43 53) (1 2 3 43 53 3) (1 2 3 43 53 3 2) (1 2 3 43 53 3 2 2)))
  )
)

(run-tests tests 'verbose)