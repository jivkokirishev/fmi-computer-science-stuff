#lang racket

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
    ((= (remainder (year date) 400)) #t)
    ((= (remainder (year date) 100)) #f)
    ((= (remainder (year date) 4)) #t)
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
  (define M (month date))
  (define Y (year date))
  (floor (+ (/ (* 1461 (+ Y 4800 (/ (- M 14) 12))) 4)
            (/ (* 367 (- M 2 (* 12 (/ (- M 14) 12)))) 12)
            (- (/ (* 3 (/ (+ (year date) 4900 (/ (- (month date) 14) 12)) 100)) 4))
            (day date)
            (- 32075))))

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