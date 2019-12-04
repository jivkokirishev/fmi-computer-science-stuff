#lang racket
(require rackunit)
(require rackunit/text-ui)

(define (make-date day month year)
  (list day month year))

(define (day date)
  (car date))

(define (month date)
  (cadr date))

(define (year date)
  (caddr date))

(define (leapYear? date)
  (cond
    ((= (remainder (year date) 400) 0) #t)
    ((= (remainder (year date) 100) 0) #f)
    ((= (remainder (year date) 4) 0) #t)
    (else #f)))

(define (partOutside? date part low up)
  (or (< (part date) low) (> (part date) up)))

(define (date? date)
  (cond
    ((not (= (length date) 3)) #f)
    ((not (number? (day date))) #f)
    ((not (number? (month date))) #f)
    ((not (number? (year date))) #f)
    ((partOutside? date month 1 12) #f)
    ((and (= (month date) 2) (leapYear? date) (partOutside? date day 1 29)) #f)
    ((and (= (month date) 2) (not (leapYear? date)) (partOutside? date day 1 28)) #f)
    ((and (= (month date) 1) (partOutside? date day 1 31)) #f)
    ((and (= (month date) 3) (partOutside? date day 1 31)) #f)
    ((and (= (month date) 5) (partOutside? date day 1 31)) #f)
    ((and (= (month date) 7) (partOutside? date day 1 31)) #f)
    ((and (= (month date) 8) (partOutside? date day 1 31)) #f)
    ((and (= (month date) 10) (partOutside? date day 1 31)) #f)
    ((and (= (month date) 12) (partOutside? date day 1 31)) #f)
    ((and (= (month date) 4) (partOutside? date day 1 30)) #f)
    ((and (= (month date) 6) (partOutside? date day 1 30)) #f)
    ((and (= (month date) 9) (partOutside? date day 1 30)) #f)
    ((and (= (month date) 11) (partOutside? date day 1 30)) #f)
    (else #t)))

(define (date->string date)
  (string-append (number->string (day date)) "." (number->string (month date)) "." (number->string (year date))))

(define (next-day date)
  (cond
    ((date? (make-date (+ (day date) 1) (month date) (year date))) (make-date (+ (day date) 1) (month date) (year date)))
    ((date? (make-date 1 (+ (month date) 1) (year date))) (make-date 1 (+ (month date) 1) (year date)))
    (else (make-date 1 1 (+ (year date) 1)))))

(define (date< date1 date2)
  (cond
    ((< (year date1) (year date2)) #t)
    ((and (= (year date1) (year date2)) (< (month date1) (month date2))) #t)
    ((and (= (year date1) (year date2)) (= (month date1) (month date2)) (< (day date1) (day date2))) #t)
    (else #f)))

(define (calcJDN date)
  (define D (day date))
  (define M (month date))
  (define Y (year date))
  (+ (quotient (* 1461 (+ Y 4800 (quotient (- M 14) 12))) 4)
            (quotient (* 367 (- M 2 (* 12 (quotient (- M 14) 12)))) 12)
            (- 0 (quotient (* 3 (quotient (+ Y 4900 (quotient (- M 14) 12)) 100)) 4))
            D
            (- 0 32075)))

(define (weekday date)
  (let ([JD (+ (remainder (calcJDN date) 7) 1)])
        (cond
          ((= JD 1) 'Monday)
          ((= JD 2) 'Tuesday)
          ((= JD 3) 'Wednesday)
          ((= JD 4) 'Thursday)
          ((= JD 5) 'Friday)
          ((= JD 6) 'Saturday)
          ((= JD 7) 'Sunday)
          (else 'Error))))


(define (next-weekday day date)
  (if (equal? (weekday date) day)
      date
      (next-weekday day (next-day date))))

(define (events-for-day date events)
  (filter (lambda (event) (equal? date (car event))) events))

(define (any? pred? list)
  (cond
    ((null? list) #f)
    ((pred? (car list)) #t)
    (else (any? pred? (cdr list)))))

(define (calendar events)

  (define (eventsOnThisDate? calendar date)
    (any? (lambda (date-events) (equal? date (car date-events))) calendar))
  
  (define (addDate calendar date)
    (cons (list date) calendar))
  
  (define (addEvent calendar event)
    (if (equal? (caar calendar) (car event))
         (cons (append (car calendar) (list (cdr event))) (cdr calendar))
         (cons (car calendar) (addEvent (cdr calendar) event))))

  (define (calendarFiller calendar events)
    (cond
      ((null? events) calendar)
      ((eventsOnThisDate? calendar (caar events)) (calendarFiller (addEvent calendar (car events)) (cdr events)))
      (else (calendarFiller (addEvent (addDate calendar (caar events)) (car events)) (cdr events)))))

  (define (eventsSortPred? events1 events2)
    (date< (car events1) (car events2)))
  
  (sort (calendarFiller '() events) eventsSortPred?))


(define make-date-tests
  (test-suite "make-date tests"
    (check-equal? (make-date 1 12 2019) '(1 12 2019))
    (check-equal? (make-date 11 6 -230) '(11 6 -230))
    (check-equal? (make-date 25 3 2158) '(25 3 2158))
  )
)

(define day-month-year-tests
  (test-suite "day month year tests"
    (check-equal? (day (make-date 21 11 2019)) 21)
    (check-equal? (month (make-date 21 11 2019)) 11)
    (check-equal? (year (make-date 21 11 2019)) 2019)
  )
)

(define date?-tests
  (test-suite "date? tests"
    (check-true (date? (make-date 21 11 2019)))
    (check-true (date? (make-date 21 11 -2019)))
    (check-false (date? (make-date 51 11 2019)))
    (check-false (date? (make-date 21 13 2019)))
    (check-false (date? '(21 13 2019 424)))
    (check-false (date? '(21 'Jan 2019)))
    (check-false (date? '(21 13)))
    (check-true (date? '(10 9 2009)))
    (check-false (date? (make-date 29 2 2017)))
    (check-true (date? (make-date 29 2 2016)))
    (check-true (date? (make-date 31 12 2019)))
    (check-true (date? (make-date 30 11 2019)))
    (check-false (date? (make-date 31 11 2019)))
    (check-false (date? (make-date -2 11 2019)))
    (check-false (date? (make-date 2 -11 2019)))
    
  )
)

(define date->string-tests
  (test-suite "date->string tests"
    (check-equal? (date->string (make-date 21 11 2019)) "21.11.2019")
    (check-equal? (date->string (make-date 31 12 -2019)) "31.12.-2019")
    (check-equal? (date->string (make-date 1 1 1)) "1.1.1")
  )
)
                  
(define next-day-tests
  (test-suite "next-day tests"
    (check-equal?  (next-day (make-date 21 11 2019)) '(22 11 2019))
    (check-equal? (next-day (make-date 30 11 2019)) '(1 12 2019))
    (check-equal? (next-day (make-date 31 12 2019)) '(1 1 2020))
    (check-equal? (next-day (make-date 28 2 2016)) '(29 2 2016))
    (check-equal? (next-day (make-date 29 2 2016)) '(1 3 2016))
    (check-equal? (next-day (make-date 28 2 2015)) '(1 3 2015))
  )
)

(define date<-tests
  (test-suite "date< tests"
    (check-true (date< (make-date 21 11 2019) (make-date 1 1 2020)))
    (check-false (date< (make-date 21 11 2019) (make-date 1 1 2019)))
    (check-true (date< (make-date 21 11 2019) (make-date 22 11 2019)))
    (check-true (date< (make-date 21 11 2019) (make-date 21 12 2019)))
    (check-false (date< (make-date 29 2 2016) (make-date 29 2 2016)))
    (check-false (date< (make-date 29 2 2016) (make-date 28 2 2016)))
  )
)

(define weekday-tests
  (test-suite "weekday tests"
    (check-equal? (weekday (make-date 21 11 2019)) 'Thursday)
    (check-equal? (weekday (make-date 22 11 2019)) 'Friday)
    (check-equal? (weekday (make-date 17 3 2018)) 'Saturday)
    (check-equal? (weekday (make-date 13 4 2020)) 'Monday)
  )
)

(define next-weekday-tests
  (test-suite "next-weekday tests"
    (check-equal? (next-weekday 'Thursday (make-date 21 11 2019)) '(21 11 2019))
    (check-equal? (next-weekday 'Tuesday (make-date 21 11 2019)) '(26 11 2019))
    (check-equal? (next-weekday 'Monday (make-date 10 4 2020)) '(13 4 2020))
    (check-equal? (next-weekday 'Wednesday (make-date 28 5 2020)) '(3 6 2020))
  )
)

(define any?-tests
  (test-suite "any? tests"
    (check-true (any? odd? '(2 4 4 2 8 9 2 0)))
    (check-true (any? (lambda (x) (> (length x) 3)) '((1 2 3) (3 4 4 2) (2 1))))
    (check-false (any? (lambda (x) (> x 2)) (map (lambda (x) (remainder x 3)) (range 1 100))))
  )
)


(define events-for-day-tests
  (test-suite "events-for-day tests"
    (check-equal? (events-for-day (make-date 27 11 2019)
                                                       (list (cons (make-date 27 11 2019) "Първа лекция за Хаскел")
                                                             (cons (make-date 27 11 2019) "Спират водата в Младост")
                                                             (cons (make-date 28 11 2019) "Спират водата в Лозенец")))
                                       '(((27 11 2019) . "Първа лекция за Хаскел") ((27 11 2019) . "Спират водата в Младост")))
    (check-equal? (events-for-day (make-date 28 11 2019)
                                                       (list (cons (make-date 27 11 2019) "Първа лекция за Хаскел")
                                                             (cons (make-date 27 11 2019) "Спират водата в Младост")
                                                             (cons (make-date 28 11 2019) "Спират водата в Лозенец")))
                                       '(((28 11 2019) . "Спират водата в Лозенец")))
  )
)



(define calendar-tests
  (test-suite "calendar tests"
    (check-equal? (calendar (list (cons (make-date 27 11 2019) "Първа лекция за Хаскел")
                                  (cons (make-date 25 12 2019) "Коледа")
                                  (cons (make-date 27 11 2019) "Спират водата в Младост")
                                  (cons (make-date 23 3 2018) "Концерт на Лепа Брена")))
                                       '(((23 3 2018) "Концерт на Лепа Брена")
                                         ((27 11 2019) "Първа лекция за Хаскел" "Спират водата в Младост")
                                         ((25 12 2019) "Коледа")))
    (check-equal? (calendar (list (cons (make-date 27 11 2019) "Спират водата в Студентски")
                                  (cons (make-date 27 11 2019) "Първа лекция за Хаскел")
                                  (cons (make-date 25 12 2019) "Коледа")
                                  (cons (make-date 27 11 2019) "Спират водата в Младост")
                                  (cons (make-date 23 3 2018) "Концерт на Лепа Брена")))
                                       '(((23 3 2018) "Концерт на Лепа Брена")
                                         ((27 11 2019) "Спират водата в Студентски" "Първа лекция за Хаскел" "Спират водата в Младост")
                                         ((25 12 2019) "Коледа")))
   (check-equal? (calendar (list (cons (make-date 27 11 2019) "Спират водата в Студентски")
                                 (cons (make-date 26 11 2019) "Първа лекция за Хаскел")
                                 (cons (make-date 25 12 2019) "Коледа")
                                 (cons (make-date 21 11 2019) "Спират водата в Младост")
                                 (cons (make-date 29 3 2018) "Концерт на Лепа Брена")))
                                       '(((29 3 2018) "Концерт на Лепа Брена")
                                         ((21 11 2019) "Спират водата в Младост")
                                         ((26 11 2019) "Първа лекция за Хаскел")
                                         ((27 11 2019) "Спират водата в Студентски")
                                         ((25 12 2019) "Коледа")))
  )
)



(run-tests make-date-tests 'verbose)
(run-tests day-month-year-tests 'verbose)
(run-tests date?-tests 'verbose)
(run-tests date->string-tests 'verbose)
(run-tests next-day-tests 'verbose)
(run-tests date<-tests 'verbose)
(run-tests weekday-tests 'verbose)
(run-tests next-weekday-tests 'verbose)

(run-tests events-for-day-tests 'verbose)
(run-tests any?-tests 'verbose)
(run-tests calendar-tests 'verbose)