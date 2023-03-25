;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/time.scm - time library
;;;  
;;;   Copyright (c) 2010-2017  Takashi Kato  <ktakashi@ymail.com>
;;;   
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;   
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;  
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;  
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;  

(library (sagittarius time)
    (export make-time
	    time?
	    time-type
	    time-nanosecond
	    time-second
	    set-time-type!
	    set-time-nanosecond!
	    set-time-second!
	    copy-time
	    current-time

	    ;; symbols
	    time-duration
	    time-monotonic
	    time-tai
	    time-utc
	    time-thread
	    time-process

	    ;; time convertion
	    time->seconds seconds->time

	    ;; compiration
	    time<=? time<?
	    time=?
	    time>=? time>?

	    ;; calculation
	    time-difference time-difference!
	    add-duration add-duration!
	    subtract-duration subtract-duration!
	    
	    ;; current date and resolution
	    current-date current-julian-day
	    current-modified-julian-day
	    time-resolution

	    ;; date
	    make-date
	    date? date-nanosecond
	    date-second date-minute date-hour
	    date-day date-month date-year
	    date-zone-offset
	    date-year-day
	    date-week-day
	    date-week-number

	    ;; converter
	    ;; time-monotonic->*
	    time-monotonic->date
	    time-monotonic->julian-day
	    time-monotonic->modified-julian-day
	    time-monotonic->time-tai
	    time-monotonic->time-tai!
	    time-monotonic->time-utc
	    time-monotonic->time-utc!
	    ;; time-tai->*
	    time-tai->date
	    time-tai->julian-day
	    time-tai->modified-julian-day
	    time-tai->time-monotonic
	    time-tai->time-monotonic!
	    time-tai->time-utc
	    time-tai->time-utc!
	    ;; time-utc->*
	    time-utc->date
	    time-utc->julian-day
	    time-utc->modified-julian-day
	    time-utc->time-monotonic
	    time-utc->time-monotonic!
	    time-utc->time-tai
	    time-utc->time-tai!

	    ;; date->*
	    date->julian-day
	    date->modified-julian-day
	    date->time-monotonic
	    date->time-tai
	    date->time-utc
	    
	    ;; julian-day->*
	    julian-day->date
	    julian-day->time-monotonic
	    julian-day->time-tai
	    julian-day->time-utc

	    ;; modified-julian-day->*
	    modified-julian-day->date
	    modified-julian-day->time-utc
	    modified-julian-day->time-tai
	    modified-julian-day->time-monotonic

	    ;; converte to string
	    date->string string->date
	    ;; clos
	    <time> <date>
	    ;; other
	    get-time-of-day
	    
	    ;; this can be useful since the <date> treats
	    ;; date of gregorian calendar (more or less)
	    <local-time> local-time? make-local-time
	    local-time-hour local-time-minute
	    local-time-second local-time-nanosecond
	    <local-date> local-date? make-local-date
	    local-date-day local-date-month local-date-year
	    local-date=? local-time=?

	    &time-error time-error? time-error-type 
	    )
    (import (core)
	    (core base)
	    (core io)
	    (core errors)
	    (sagittarius)
	    (rename (sagittarius time-private)
		    (leap-second-delta tm:leap-second-delta)
		    (+leap-second-table+ tm:leap-second-table))
	    (sagittarius time-util)
	    (sagittarius timezone)
	    (clos user)
	    (sagittarius mop validator)
	    (sagittarius calendar locals))

  (define open-input-string open-string-input-port)
  (define (local-tz-offset) (timezone-offset (local-timezone)))
  
  (define (check-integer o v)
    (or (and (exact? v) (integer? v) v)
	(error o "integer required" v)))
  (define-class <date> (<validator-mixin>)
    ((nanosecond :init-keyword :nanosecond :reader date-nanosecond
		 :writer set-date-nanosecond!
		 :validator check-integer)
     (second     :init-keyword :second     :reader date-second
		 :writer set-date-second!
		 :validator check-integer)
     (minute     :init-keyword :minute     :reader date-minute
		 :writer set-date-minute!
		 :validator check-integer)
     (hour       :init-keyword :hour       :reader date-hour
		 :writer set-date-hour!
		 :validator check-integer)
     (day        :init-keyword :day        :reader date-day
		 :writer set-date-day!
		 :validator check-integer)
     (month      :init-keyword :month      :reader date-month
		 :writer set-date-month!
		 :validator check-integer)
     (year       :init-keyword :year       :reader date-year
		 :writer set-date-year!
		 :validator check-integer)
     (zone-offset :init-keyword :zone-offset :reader date-zone-offset
		  :writer set-date-zone-offset!
		  :validator check-integer)))

  (define-method write-object ((d <date>) out)
    (format out "#<date ~d/~2,'0d/~2,'0d ~2,'0d:~2,'0d:~2,'0d.~9,'0d (~a)>"
	    (date-year d) (date-month d) (date-day d)
	    (date-hour d) (date-minute d) (date-second d)
	    (date-nanosecond d) (date-zone-offset d)))



  (define (date? o) (is-a? o <date>))
  (define (make-date nano sec min hour day month year offset)
    (make <date> :nanosecond nano :second sec :minute min
	  :hour hour :day day :month month :year year :zone-offset offset))

  (define (tm:time-error caller type value)
    (let ((m (if (member type tm:time-error-types)
		 (if value
		     (format "TIME-ERROR type ~s: ~s" type value)
		     (format "TIME-ERROR type ~s" type))
		 (format "TIME-ERROR unsupported error type ~s" type))))
      (raise (condition (make-time-error type)
			(make-who-condition caller)
			(make-message-condition m)
			(make-irritants-condition value)))))

  ;; from srfi-19 reference implementation
  (define tm:time-error-types
    '(invalid-clock-type
      unsupported-clock-type
      incompatible-time-types
      not-duration
      dates-are-immutable
      bad-date-format-string
      bad-date-template-string
      invalid-month-specification
      ))
  (define tm:locale-number-separator ".")

  (define tm:locale-abbr-weekday-vector (vector "Sun" "Mon" "Tue" "Wed"
                                                "Thu" "Fri" "Sat"))
  (define tm:locale-long-weekday-vector (vector "Sunday" "Monday"
                                                "Tuesday" "Wednesday"
                                                "Thursday" "Friday"
                                                "Saturday"))
  ;; note empty string in 0th place.
  (define tm:locale-abbr-month-vector   (vector "" "Jan" "Feb" "Mar"
                                                "Apr" "May" "Jun" "Jul"
                                                "Aug" "Sep" "Oct" "Nov"
                                                "Dec"))
  (define tm:locale-long-month-vector   (vector "" "January" "February"
                                                "March" "April" "May"
                                                "June" "July" "August"
                                                "September" "October"
                                                "November" "December"))

  (define-constant tm:locale-pm "PM")
  (define-constant tm:locale-am "AM")

  ;; See date->string
  (define-constant tm:locale-date-time-format "~a ~b ~d ~H:~M:~S~z ~Y")
  (define-constant tm:locale-short-date-format "~m/~d/~y")
  (define-constant tm:locale-time-format "~H:~M:~S")
  (define-constant tm:iso-8601-date-time-format "~Y-~m-~dT~H:~M:~S~z")

  ;; going from tai seconds to utc seconds ...
  (define (tm:leap-second-neg-delta tai-seconds)
    (letrec ((lsd (lambda (table)
		    (cond ((null? table) 0)
			  ((<= (cdar table) (- tai-seconds (caar table)))
			   (cdar table))
			  (else (lsd (cdr table)))))))
      (if (< tai-seconds  (* (- 1972 1970) 365 tm:sid)) 0
          (lsd  tm:leap-second-table))))

  ;; time-*->time-*  
  (define (tm:time-tai->time-utc! time-in time-out caller)
    (if (not (eq? (time-type time-in) time-tai))
        (tm:time-error caller 'incompatible-time-types time-in))
    (set-time-type! time-out time-utc)
    (set-time-nanosecond! time-out (time-nanosecond time-in))
    (set-time-second!     time-out (- (time-second time-in)
                                      (tm:leap-second-neg-delta
                                       (time-second time-in))))
    time-out)

  (define (time-tai->time-utc time-in)
    (tm:time-tai->time-utc! time-in (make-time 'dummy 0 0) 'time-tai->time-utc))

  (define (time-tai->time-utc! time-in)
    (tm:time-tai->time-utc! time-in time-in 'time-tai->time-utc!))

  (define (tm:time-utc->time-tai! time-in time-out caller)
    (if (not (eq? (time-type time-in) time-utc))
        (tm:time-error caller 'incompatible-time-types time-in))
    (set-time-type! time-out time-tai)
    (set-time-nanosecond! time-out (time-nanosecond time-in))
    (set-time-second!     time-out (+ (time-second time-in)
                                      (tm:leap-second-delta
                                       (time-second time-in))))
    time-out)

  (define (time-utc->time-tai time-in)
    (tm:time-utc->time-tai! time-in (make-time 'dummy 0 0) 'time-utc->time-tai))

  (define (time-utc->time-tai! time-in)
    (tm:time-utc->time-tai! time-in time-in 'time-utc->time-tai!))

  ;; -- these depend on time-monotonic having the same definition as time-tai!
  (define (time-monotonic->time-utc time-in)
    (if (not (eq? (time-type time-in) time-monotonic))
        (tm:time-error 'time-monotoinc->time-utc 'incompatible-time-types time-in))
    (let ((ntime (copy-time time-in)))
      (set-time-type! ntime time-tai)
      (tm:time-tai->time-utc! ntime ntime 'time-monotonic->time-utc)))

  (define (time-monotonic->time-utc! time-in)
    (if (not (eq? (time-type time-in) time-monotonic))
        (tm:time-error 'time-monotonic->time-utc! 'incompatible-time-types time-in))
    (set-time-type! time-in time-tai)
    (tm:time-tai->time-utc! time-in time-in 'time-monotonic->time-utc))

  (define (time-monotonic->time-tai time-in)
    (if (not (eq? (time-type time-in) time-monotonic))
        (tm:time-error 'time-monotonic->time-tai 'incompatible-time-types time-in))
    (let ((ntime (copy-time time-in)))
      (set-time-type! ntime time-tai)
      ntime))

  (define (time-monotonic->time-tai! time-in)
    (if (not (eq? (time-type time-in) time-monotonic))
        (tm:time-error 'time-monotonic->time-tai! 'incompatible-time-types time-in))
    (set-time-type! time-in time-tai)
    time-in)

  (define (time-utc->time-monotonic time-in)
    (if (not (eq? (time-type time-in) time-utc))
        (tm:time-error 'time-utc->time-monotonic 'incompatible-time-types time-in))
    (let ((ntime (tm:time-utc->time-tai! time-in (make-time 'dummy 0 0)
                                         'time-utc->time-monotonic)))
      (set-time-type! ntime time-monotonic)
      ntime))

  (define (time-utc->time-monotonic! time-in)
    (if (not (eq? (time-type time-in) time-utc))
        (tm:time-error 'time-utc->time-montonic! 'incompatible-time-types time-in))
    (let ((ntime (tm:time-utc->time-tai! time-in time-in
                                         'time-utc->time-monotonic!)))
      (set-time-type! ntime time-monotonic)
      ntime))

  (define (time-tai->time-monotonic time-in)
    (if (not (eq? (time-type time-in) time-tai))
        (tm:time-error 'time-tai->time-monotonic 'incompatible-time-types time-in))
    (let ((ntime (copy-time time-in)))
      (set-time-type! ntime time-monotonic)
      ntime))

  (define (time-tai->time-monotonic! time-in)
    (if (not (eq? (time-type time-in) time-tai))
        (tm:time-error 'time-tai->time-monotonic!  'incompatible-time-types time-in))
    (set-time-type! time-in time-monotonic)
    time-in)

  (define (tm:time->date time tz-offset ttype)
    (if (not (eq? (time-type time) ttype))
        (tm:time-error 'time->date 'incompatible-time-types  time))
    (let* ((offset (if (null? tz-offset) (local-tz-offset) (car tz-offset))))
      (receive (secs date month year)
          (tm:decode-julian-day-number
           (tm:time->julian-day-number (time-second time) offset))
        (let* ((hours    (quotient secs (* 60 60)))
               (rem      (remainder secs (* 60 60)))
               (minutes  (quotient rem 60))
               (seconds  (remainder rem 60)))
          (make-date (time-nanosecond time)
		     seconds
                     minutes
                     hours
                     date
                     month
                     year
                     offset)))))

  (define (time-tai->date time . tz-offset)
    (if (tm:tai-before-leap-second? (time-second time))
        ;; if it's *right* before the leap, we need to pretend to 
	;; subtract a second ...
        (let ((d (tm:time->date (subtract-duration! 
				 (time-tai->time-utc time)
				 (make-time time-duration 0 1))
				tz-offset time-utc)))
          (set-date-second! d 60)
          d)
        (tm:time->date (time-tai->time-utc time) tz-offset time-utc)))

  (define (time-tai->julian-day time)
    (if (not (eq? (time-type time) time-tai))
	(tm:time-error 'time-tai->julian-day 'incompatible-time-types  time))
    (+ (/ (+ (- (time-second time) 
		(tm:leap-second-delta (time-second time)))
	     (/ (time-nanosecond time) tm:nano))
	  tm:sid)
       tm:tai-epoch-in-jd))

  (define (time-tai->modified-julian-day time)
    (- (time-tai->julian-day time)
       (/ 4800001 2)))

  (define (time-utc->date time . tz-offset)
    (tm:time->date time tz-offset time-utc))

  (define (time-utc->julian-day time)
    (if (not (eq? (time-type time) time-utc))
	(tm:time-error 'time-utc->julian-day 'incompatible-time-types  time))
    (+ (/ (+ (time-second time) (/ (time-nanosecond time) tm:nano))
	  tm:sid)
       tm:tai-epoch-in-jd))

  (define (time-utc->modified-julian-day time)
    (- (time-utc->julian-day time)
       (/ 4800001 2)))

  ;; again, time-monotonic is the same as time tai
  (define (time-monotonic->date time . tz-offset)
    (tm:time->date time tz-offset time-monotonic))

  (define (time-monotonic->julian-day time)
    (if (not (eq? (time-type time) time-monotonic))
	(tm:time-error 'time-monotonic->julian-day 'incompatible-time-types time))
    (+ (/ (+ (- (time-second time) 
		(tm:leap-second-delta (time-second time)))
	     (/ (time-nanosecond time) tm:nano))
	  tm:sid)
       tm:tai-epoch-in-jd))

  (define (time-monotonic->modified-julian-day time)
    (- (time-monotonic->julian-day time)
       (/ 4800001 2)))

  (define (tm:char-pos char str index len)
    (cond
     ((>= index len) #f)
     ((char=? (string-ref str index) char)
      index)
     (else
      (tm:char-pos char str (+ index 1) len))))

  (define (tm:fractional-part r)
    (if (integer? r) "0"
	(let ((str (number->string (inexact r))))
	  (let ((ppos (tm:char-pos #\. str 0 (string-length str))))
	    (substring str  (+ ppos 1) (string-length str))))))


  (define (tm:find proc l)
    (if (null? l)
        #f
        (if (proc (car l))
            #t
            (tm:find proc (cdr l)))))

  (define (tm:tai-before-leap-second? second)
    (tm:find (lambda (x)
               (= second (- (+ (car x) (cdr x)) 1)))
             tm:leap-second-table))

  (define (tm:natural-year n)
    (let* ( (current-year (date-year (current-date)))
	    (current-century (* (quotient current-year 100) 100)) )
      (cond
       ((>= n 100) n)
       ((<  n 0) n)
       ((<=  (- (+ current-century n) current-year) 50)
	(+ current-century n))
       (else
	(+ (- current-century 100) n)))))

  (define (date->julian-day date)
    (let ((nanosecond (date-nanosecond date))
          (second (date-second date))
          (minute (date-minute date))
          (hour (date-hour date))
          (day (date-day date))
          (month (date-month date))
          (year (date-year date))
          (offset (date-zone-offset date)))
      (+ (tm:encode-julian-day-number day month year)
         (- 1/2)
         (+ (/ (+ (* hour 60 60)
                  (* minute 60)
                  second
                  (/ nanosecond tm:nano)
                  (- offset))
               tm:sid)))))

  (define (date->modified-julian-day date)
    (- (date->julian-day date)
       4800001/2))

  (define (date->time-monotonic date)
    (time-utc->time-monotonic! (date->time-utc date)))

  (define (date->time-utc date)
    (let ( (nanosecond (date-nanosecond date))
           (second (date-second date))
           (minute (date-minute date))
           (hour (date-hour date))
           (day (date-day date))
           (month (date-month date))
           (year (date-year date))
           (offset (date-zone-offset date)) )
      (let ( (jdays (- (tm:encode-julian-day-number day month year)
                       tm:tai-epoch-in-jd)) )
        (make-time
         time-utc
         nanosecond
         (+ (* (- jdays 1/2) 24 60 60)
            (* hour 60 60)
            (* minute 60)
            second
            (- offset))
         ))))

  (define (date->time-tai d)
    (if (= (date-second d) 60)
        (subtract-duration! (time-utc->time-tai! (date->time-utc d)) (make-time time-duration 0 1))
        (time-utc->time-tai! (date->time-utc d))))

  (define (tm:leap-year? year)
    (or (= (modulo year 400) 0)
	(and (= (modulo year 4) 0) (not (= (modulo year 100) 0)))))

  (define (leap-year? date)
    (tm:leap-year? (date-year date)))

  ;; tm:year-day fixed: adding wrong number of days.
  (define  tm:month-assoc '((0 . 0) (1 . 31)  (2 . 59)   (3 . 90)   (4 . 120)
                            (5 . 151) (6 . 181)  (7 . 212)  (8 . 243)
                            (9 . 273) (10 . 304) (11 . 334)))

  (define (tm:year-day day month year)
    (let ((days-pr (assoc (- month 1) tm:month-assoc)))
      (if (not days-pr)
          (tm:time-error 'date-year-day 'invalid-month-specification month))
      (if (and (tm:leap-year? year) (> month 2))
          (+ day (cdr days-pr) 1)
          (+ day (cdr days-pr)))))

  (define (date-year-day date)
    (tm:year-day (date-day date) (date-month date) (date-year date)))

  (define (tm:week-day day month year)
    (let* ((a (quotient (- 14 month) 12))
           (y (- year a))
           (m (+ month (* 12 a) -2)))
      (modulo (+ day y (quotient y 4) (- (quotient y 100))
                 (quotient y 400) (quotient (* 31 m) 12))
              7)))

  (define (date-week-day date)
    (tm:week-day (date-day date) (date-month date) (date-year date)))

  (define (tm:days-before-first-week date day-of-week-starting-week)
    (let* ( (first-day (make-date 0 0 0 0
                                  1
                                  1
                                  (date-year date)
                                  -1))
            (fdweek-day (date-week-day first-day))  )
      (modulo (- day-of-week-starting-week fdweek-day)
              7)))

  (define (date-week-number date day-of-week-starting-week)
    (quotient (- (date-year-day date)
                 (tm:days-before-first-week  date day-of-week-starting-week))
              7))

  (define (current-date . tz-offset)
    (time-utc->date (current-time time-utc)
                    (if (null? tz-offset) (local-tz-offset) (car tz-offset))))


  (define (julian-day->time-utc jdn)
    (let ( (nanosecs (* tm:nano tm:sid (- jdn tm:tai-epoch-in-jd))) )
      (make-time time-utc
		 ;; FIX using mod instead of remainder
		 ;; to avoid Windows and Cygwin issue.
		 ;; not sure why this doesn't happen on Linux...
                 (mod nanosecs tm:nano)
                 (floor (/ nanosecs tm:nano)))))

  (define (julian-day->time-tai jdn)
    (time-utc->time-tai! (julian-day->time-utc jdn)))

  (define (julian-day->time-monotonic jdn)
    (time-utc->time-monotonic! (julian-day->time-utc jdn)))

  (define (julian-day->date jdn . tz-offset)
    (let ((offset (if (null? tz-offset) (local-tz-offset) (car tz-offset))))
      (time-utc->date (julian-day->time-utc jdn) offset)))

  (define (modified-julian-day->date jdn . tz-offset)
    (let ((offset (if (null? tz-offset) (local-tz-offset) (car tz-offset))))
      (julian-day->date (+ jdn 4800001/2) offset)))

  (define (modified-julian-day->time-utc jdn)
    (julian-day->time-utc (+ jdn 4800001/2)))

  (define (modified-julian-day->time-tai jdn)
    (julian-day->time-tai (+ jdn 4800001/2)))

  (define (modified-julian-day->time-monotonic jdn)
    (julian-day->time-monotonic (+ jdn 4800001/2)))

  (define (current-julian-day)
    (time-utc->julian-day (current-time time-utc)))

  (define (current-modified-julian-day)
    (time-utc->modified-julian-day (current-time time-utc)))
;; returns a string rep. of number N, of minimum LENGTH,
;; padded with character PAD-WITH. If PAD-WITH if #f,
;; no padding is done, and it's as if number->string was used.
;; if string is longer than LENGTH, it's as if number->string was used.

  (define (tm:padding n pad-with length)
    (let* ( (str (number->string n))
            (str-len (string-length str)) )
      (if (or (> str-len length)
              (not pad-with))
          str
          (let* ( (new-str (make-string length pad-with))
                  (new-str-offset (- (string-length new-str)
                                     str-len)) )
            (do ((i 0 (+ i 1)))
              ((>= i (string-length str)))
              (string-set! new-str (+ new-str-offset i)
                           (string-ref str i)))
            new-str))))

  (define (tm:last-n-digits i n)
    (abs (remainder i (expt 10 n))))

  (define (tm:locale-abbr-weekday n)
    (vector-ref tm:locale-abbr-weekday-vector n))

  (define (tm:locale-long-weekday n)
    (vector-ref tm:locale-long-weekday-vector n))

  (define (tm:locale-abbr-month n)
    (vector-ref tm:locale-abbr-month-vector n))

  (define (tm:locale-long-month n)
    (vector-ref tm:locale-long-month-vector n))

  (define (tm:vector-find needle haystack comparator)
    (let ((len (vector-length haystack)))
      (define (tm:vector-find-int index)
        (cond
         ((>= index len) #f)
         ((comparator needle (vector-ref haystack index)) index)
         (else (tm:vector-find-int (+ index 1)))))
      (tm:vector-find-int 0)))

  (define (tm:locale-abbr-weekday->index string)
    (tm:vector-find string tm:locale-abbr-weekday-vector string=?))

  (define (tm:locale-long-weekday->index string)
    (tm:vector-find string tm:locale-long-weekday-vector string=?))

  (define (tm:locale-abbr-month->index string)
    (tm:vector-find string tm:locale-abbr-month-vector string=?))

  (define (tm:locale-long-month->index string)
    (tm:vector-find string tm:locale-long-month-vector string=?))

;; do nothing.
;; Your implementation might want to do something...
;;
  (define (tm:locale-print-time-zone date port)
    (values))

;; Again, locale specific.
  (define (tm:locale-am/pm hr)
    (if (> hr 11) tm:locale-pm tm:locale-am))

  (define (tm:tz-printer offset port :optional (sep #f))
    (cond
     ((= offset 0) (display "Z" port))
     ((negative? offset) (display "-" port))
     (else (display "+" port)))
    (if (not (= offset 0))
        (let ( (hours   (abs (quotient offset (* 60 60))))
               (minutes (abs (quotient (remainder offset (* 60 60)) 60))) )
          (display (tm:padding hours #\0 2) port)
	  (when sep (display sep port))
          (display (tm:padding minutes #\0 2) port))))

;; A table of output formatting directives.
;; the first time is the format char.
;; the second is a procedure that takes the date, a padding character
;; (which might be #f), and the output port.
;;
  (define tm:directives
    (list
     (cons #\~ (lambda (date pad-with port) (display #\~ port)))

     (cons #\a (lambda (date pad-with port)
                 (display (tm:locale-abbr-weekday (date-week-day date))
                          port)))
     (cons #\A (lambda (date pad-with port)
                 (display (tm:locale-long-weekday (date-week-day date))
                          port)))
     (cons #\b (lambda (date pad-with port)
                 (display (tm:locale-abbr-month (date-month date))
                          port)))
     (cons #\B (lambda (date pad-with port)
                 (display (tm:locale-long-month (date-month date))
                          port)))
     (cons #\c (lambda (date pad-with port)
                 (display (date->string date tm:locale-date-time-format) port)))
     (cons #\d (lambda (date pad-with port)
                 (display (tm:padding (date-day date)
                                      #\0 2)
                          port)))
     (cons #\D (lambda (date pad-with port)
                 (display (date->string date "~m/~d/~y") port)))
     (cons #\e (lambda (date pad-with port)
                 (display (tm:padding (date-day date)
                                      #\space 2)
                          port)))
     (cons #\f (lambda (date pad-with port)
                 (if (> (date-nanosecond date)
                        tm:nano)
                     (display (tm:padding (+ (date-second date) 1)
                                          pad-with 2)
                              port)
                     (display (tm:padding (date-second date)
                                          pad-with 2)
                              port))
                 (let* ((ns (tm:fractional-part (/
                                                 (date-nanosecond date)
                                                 tm:nano 1.0)))
                        (le (string-length ns)))
                   (if (> le 2)
                       (begin
                         (display tm:locale-number-separator port)
                         (display (substring ns 2 le) port))))))
     (cons #\h (lambda (date pad-with port)
                 (display (date->string date "~b") port)))
     (cons #\H (lambda (date pad-with port)
                 (display (tm:padding (date-hour date)
                                      pad-with 2)
                          port)))
     (cons #\I (lambda (date pad-with port)
                 (let ((hr (date-hour date)))
                   (if (> hr 12)
                       (display (tm:padding (- hr 12)
                                            pad-with 2)
                                port)
                       (display (tm:padding hr
                                            pad-with 2)
                                port)))))
     (cons #\j (lambda (date pad-with port)
                 (display (tm:padding (date-year-day date)
                                      pad-with 3)
                          port)))
     (cons #\k (lambda (date pad-with port)
                 (display (tm:padding (date-hour date)
                                      #\space 2)
                          port)))
     (cons #\l (lambda (date pad-with port)
                 (let ((hr (if (> (date-hour date) 12)
                               (- (date-hour date) 12) (date-hour date))))
                   (display (tm:padding hr  #\space 2)
                            port))))
     (cons #\m (lambda (date pad-with port)
                 (display (tm:padding (date-month date)
                                      pad-with 2)
                          port)))
     (cons #\M (lambda (date pad-with port)
                 (display (tm:padding (date-minute date)
                                      pad-with 2)
                          port)))
     (cons #\n (lambda (date pad-with port)
                 (newline port)))
     (cons #\N (lambda (date pad-with port)
                 (display (tm:padding (date-nanosecond date)
                                      pad-with 9)
                          port)))
     (cons #\p (lambda (date pad-with port)
                 (display (tm:locale-am/pm (date-hour date)) port)))
     (cons #\r (lambda (date pad-with port)
                 (display (date->string date "~I:~M:~S ~p") port)))
     (cons #\s (lambda (date pad-with port)
                 (display (time-second (date->time-utc date)) port)))
     (cons #\S (lambda (date pad-with port)
                 (if (> (date-nanosecond date)
                        tm:nano)
                     (display (tm:padding (+ (date-second date) 1)
                                          pad-with 2)
                              port)
                     (display (tm:padding (date-second date)
                                          pad-with 2)
                              port))))
     (cons #\t (lambda (date pad-with port)
                 (display (integer->char 9) port)))
     (cons #\T (lambda (date pad-with port)
                 (display (date->string date "~H:~M:~S") port)))
     (cons #\U (lambda (date pad-with port)
                 (if (> (tm:days-before-first-week date 0) 0)
                     (display (tm:padding (+ (date-week-number date 0) 1)
                                          #\0 2) port)
                     (display (tm:padding (date-week-number date 0)
                                          #\0 2) port))))
     (cons #\V (lambda (date pad-with port)
                 (display (tm:padding (date-week-number date 1)
                                      #\0 2) port)))
     (cons #\w (lambda (date pad-with port)
                 (display (date-week-day date) port)))
     (cons #\x (lambda (date pad-with port)
                 (display (date->string date tm:locale-short-date-format) port)))
     (cons #\X (lambda (date pad-with port)
                 (display (date->string date tm:locale-time-format) port)))
     (cons #\W (lambda (date pad-with port)
                 (if (> (tm:days-before-first-week date 1) 0)
                     (display (tm:padding (+ (date-week-number date 1) 1)
                                          #\0 2) port)
                     (display (tm:padding (date-week-number date 1)
                                          #\0 2) port))))
     (cons #\y (lambda (date pad-with port)
                 (display (tm:padding (tm:last-n-digits
                                       (date-year date) 2)
                                      pad-with
                                      2)
                          port)))
     (cons #\Y (lambda (date pad-with port)
                 (display (date-year date) port)))
     (cons #\z (lambda (date pad-with port)
                 (tm:tz-printer (date-zone-offset date) port)))
     (cons #\Z (lambda (date pad-with port)
                 (tm:locale-print-time-zone date port)))
     (cons #\1 (lambda (date pad-with port)
                 (display (date->string date "~Y-~m-~d") port)))
     (cons #\2 (lambda (date pad-with port)
                 (display (date->string date "~H:~M:~S~z") port)))
     (cons #\3 (lambda (date pad-with port)
                 (display (date->string date "~H:~M:~S") port)))
     (cons #\4 (lambda (date pad-with port)
                 (display (date->string date "~Y-~m-~dT~H:~M:~S~z") port)))
     (cons #\5 (lambda (date pad-with port)
                 (display (date->string date "~Y-~m-~dT~H:~M:~S") port)))
     ;; extension for Sagittarius
     ;; XSD dateTime
     ;; I want something that can specify timezone offset printer
     ;; other than #\z but almost all chars are occupied...
     (cons #\6 (lambda (date pad-with port)
                 (display (date->string date "~Y-~m-~dT~H:~M:~S") port)
		 (tm:tz-printer (date-zone-offset date) port #\:)))
     ))

  (define (tm:get-formatter char)
    (let ( (associated (assoc char tm:directives)) )
      (if associated (cdr associated) #f)))

  (define (tm:date-printer date index format-string str-len port)
    (if (>= index str-len)
        (values)
        (let ( (current-char (string-ref format-string index)) )
          (if (not (char=? current-char #\~))
              (begin
                (display current-char port)
                (tm:date-printer date (+ index 1) format-string str-len port))

              (if (= (+ index 1) str-len) ; bad format string.
                  (tm:time-error 'tm:date-printer 'bad-date-format-string
                                 format-string)
                  (let ( (pad-char? (string-ref format-string (+ index 1))) )
                    (cond
                     ((char=? pad-char? #\-)
                      (if (= (+ index 2) str-len) ; bad format string.
                          (tm:time-error 'tm:date-printer 'bad-date-format-string
                                         format-string)
                          (let ( (formatter (tm:get-formatter
                                             (string-ref format-string
                                                         (+ index 2)))) )
                            (if (not formatter)
                                (tm:time-error 'tm:date-printer 'bad-date-format-string
                                               format-string)
                                (begin
                                  (formatter date #f port)
                                  (tm:date-printer date (+ index 3)
                                                   format-string str-len port))))))

                     ((char=? pad-char? #\_)
                      (if (= (+ index 2) str-len) ; bad format string.
                          (tm:time-error 'tm:date-printer 'bad-date-format-string
                                         format-string)
                          (let ( (formatter (tm:get-formatter
                                             (string-ref format-string
                                                         (+ index 2)))) )
                            (if (not formatter)
                                (tm:time-error 'tm:date-printer 'bad-date-format-string
                                               format-string)
                                (begin
                                  (formatter date #\space port)
                                  (tm:date-printer date (+ index 3)
                                                   format-string str-len port))))))
                     (else
                      (let ( (formatter (tm:get-formatter
                                         (string-ref format-string
                                                     (+ index 1)))) )
                        (if (not formatter)
                            (tm:time-error 'tm:date-printer 'bad-date-format-string
                                           format-string)
                            (begin
                              (formatter date #\0 port)
                              (tm:date-printer date (+ index 2)
                                               format-string str-len port))))))))))))

  (define (date->string date .  format-string)
    (let ((str-port (open-output-string))
          (fmt-str (if (null? format-string) "~c" (car format-string))))
      (tm:date-printer date 0 fmt-str (string-length fmt-str) str-port)
      (get-output-string str-port)))

  (define (tm:char->int ch)
    (cond
     ((char=? ch #\0) 0)
     ((char=? ch #\1) 1)
     ((char=? ch #\2) 2)
     ((char=? ch #\3) 3)
     ((char=? ch #\4) 4)
     ((char=? ch #\5) 5)
     ((char=? ch #\6) 6)
     ((char=? ch #\7) 7)
     ((char=? ch #\8) 8)
     ((char=? ch #\9) 9)
     (else (tm:time-error 'string->date 'bad-date-template-string
                          (list "Non-integer character" ch )))))

;; read an integer upto n characters long on port; upto -> #f if any length
  (define (tm:integer-reader upto port)
    (define (accum-int port accum nchars)
      (let ((ch (peek-char port)))
        (if (or (eof-object? ch)
                (not (char-numeric? ch))
                (and upto (>= nchars  upto )))
            accum
            (accum-int port (+ (* accum 10) (tm:char->int (read-char
                                                           port))) (+
                                                                    nchars 1)))))
    (accum-int port 0 0))

  (define (tm:make-integer-reader upto)
    (lambda (port)
      (tm:integer-reader upto port)))

;; read an fractional integer upto n characters long on port; upto -> #f if any length
;;
;; The return value is normalized to upto decimal places. For example, if upto is 9 and
;; the string read is "123", the return value is 123000000.
  (define (tm:fractional-integer-reader upto port)
    (define (accum-int port accum nchars)
      (let ((ch (peek-char port)))
        (if (or (eof-object? ch)
                (not (char-numeric? ch))
                (and upto (>= nchars  upto )))
            (* accum (expt 10 (- upto nchars)))
            (accum-int port (+ (* accum 10) (tm:char->int (read-char port))) (+ nchars 1)))))
    (accum-int port 0 0))

  (define (tm:make-fractional-integer-reader upto)
    (lambda (port)
      (tm:fractional-integer-reader upto port)))

;; read *exactly* n characters and convert to integer; could be padded
  (define (tm:integer-reader-exact n port)
    (let ( (padding-ok #t) )
      (define (accum-int port accum nchars)
        (let ((ch (peek-char port)))
          (cond
           ((>= nchars n) accum)
           ((eof-object? ch)
            (tm:time-error 'string->date 'bad-date-template-string
                           "Premature ending to integer read."))
           ((char-numeric? ch)
            (set! padding-ok #f)
            (accum-int port (+ (* accum 10) (tm:char->int (read-char
                                                           port)))
                       (+ nchars 1)))
           (padding-ok
            (read-char port) ; consume padding
            (accum-int port accum (+ nchars 1)))
           (else ; padding where it shouldn't be
            (tm:time-error 'string->date 'bad-date-template-string
                           "Non-numeric characters in integer read.")))))
      (accum-int port 0 0)))

  (define (tm:make-integer-exact-reader n)
    (lambda (port)
      (tm:integer-reader-exact n port)))

  (define (tm:zone-reader port)
    (let ( (offset 0)
           (positive? #f) )
      (let ( (ch (read-char port)) )
        (if (eof-object? ch)
            (tm:time-error 'string->date 'bad-date-template-string
                           (list "Invalid time zone +/-" ch)))
        (if (or (char=? ch #\Z) (char=? ch #\z))
            0
            (begin
              (cond
               ((char=? ch #\+) (set! positive? #t))
               ((char=? ch #\-) (set! positive? #f))
               (else
                (tm:time-error 'string->date 'bad-date-template-string
                               (list "Invalid time zone +/-" ch))))
              (let ((ch (read-char port)))
                (if (eof-object? ch)
                    (tm:time-error 'string->date 'bad-date-template-string
                                   (list "Invalid time zone number" ch)))
                (set! offset (* (tm:char->int ch)
                                10 60 60)))
              (let ((ch (read-char port)))
                (if (eof-object? ch)
                    (tm:time-error 'string->date 'bad-date-template-string
                                   (list "Invalid time zone number" ch)))
                (set! offset (+ offset (* (tm:char->int ch)
                                          60 60))))
	      ;; accept colon as well (ISO8601 style HH:MM)
	      (let ((ch (peek-char port)))
                (if (eof-object? ch)
                    (tm:time-error 'string->date 'bad-date-template-string
                                   (list "Invalid time zone number" ch)))
                (when (char=? ch #\:) (read-char port)))
              (let ((ch (read-char port)))
                (if (eof-object? ch)
                    (tm:time-error 'string->date 'bad-date-template-string
                                   (list "Invalid time zone number" ch)))
                (set! offset (+ offset (* (tm:char->int ch)
                                          10 60))))
              (let ((ch (read-char port)))
                (if (eof-object? ch)
                    (tm:time-error 'string->date 'bad-date-template-string
                                   (list "Invalid time zone number" ch)))
                (set! offset (+ offset (* (tm:char->int ch)
                                          60))))
              (if positive? offset (- offset)))))))

;; looking at a char, read the char string, run thru indexer, return index
  (define (tm:locale-reader port indexer)
    (let ( (string-port (open-output-string)) )
      (define (read-char-string)
        (let ((ch (peek-char port)))
          (if (char-alphabetic? ch)
              (begin (write-char (read-char port) string-port)
                (read-char-string))
              (get-output-string string-port))))
      (let* ( (str (read-char-string))
              (index (indexer str)) )
        (if index index (tm:time-error 'string->date
                                       'bad-date-template-string
                                       (list "Invalid string for " indexer))))))

  (define (tm:make-locale-reader indexer)
    (lambda (port)
      (tm:locale-reader port indexer)))

  (define (tm:make-char-id-reader char)
    (lambda (port)
      (if (char=? char (read-char port))
          char
          (tm:time-error 'string->date
                         'bad-date-template-string
                         "Invalid character match."))))

;; A List of formatted read directives.
;; Each entry is a list.
;; 1. the character directive;
;; a procedure, which takes a character as input & returns
;; 2. #t as soon as a character on the input port is acceptable
;; for input,
;; 3. a port reader procedure that knows how to read the current port
;; for a value. Its one parameter is the port.
;; 4. a action procedure, that takes the value (from 3.) and some
;; object (here, always the date) and (probably) side-effects it.
;; In some cases (e.g., ~A) the action is to do nothing

  (define (make-flags) (make-vector 8 #f))
  (define (nanosecod-read? f) (vector-ref f 0))
  (define (second-read? f) (vector-ref f 1))
  (define (minute-read? f) (vector-ref f 2))
  (define (hour-read? f) (vector-ref f 3))
  (define (day-read? f) (vector-ref f 4))
  (define (month-read? f) (vector-ref f 5))
  (define (year-read? f) (vector-ref f 6))
  (define (offset-read? f) (vector-ref f 7))
  
  (define (nanosecod-read! f) (vector-set! f 0 #t))
  (define (second-read! f) (vector-set! f 1 #t))
  (define (minute-read! f) (vector-set! f 2 #t))
  (define (hour-read! f) (vector-set! f 3 #t))
  (define (day-read! f) (vector-set! f 4 #t))
  (define (month-read! f) (vector-set! f 5 #t))
  (define (year-read! f) (vector-set! f 6 #t))
  (define (offset-read! f) (vector-set! f 7 #t))
  
  (define tm:read-directives
    (let ( (ireader4 (tm:make-integer-reader 4))
           (ireader2 (tm:make-integer-reader 2))
           (fireader9 (tm:make-fractional-integer-reader 9))
           (ireaderf (tm:make-integer-reader #f))
           (eireader2 (tm:make-integer-exact-reader 2))
           (eireader4 (tm:make-integer-exact-reader 4))
           (locale-reader-abbr-weekday (tm:make-locale-reader
                                        tm:locale-abbr-weekday->index))
           (locale-reader-long-weekday (tm:make-locale-reader
                                        tm:locale-long-weekday->index))
           (locale-reader-abbr-month   (tm:make-locale-reader
                                        tm:locale-abbr-month->index))
           (locale-reader-long-month   (tm:make-locale-reader
                                        tm:locale-long-month->index))
           (char-fail (lambda (ch) #t))
           (do-nothing (lambda (val object flags) (values)))
           )

      (list
       (list #\~ char-fail (tm:make-char-id-reader #\~) do-nothing)
       (list #\a char-alphabetic? locale-reader-abbr-weekday do-nothing)
       (list #\A char-alphabetic? locale-reader-long-weekday do-nothing)
       (list #\b char-alphabetic? locale-reader-abbr-month
             (lambda (val object flags)
	       (month-read! flags)
               (set-date-month! object val)))
       (list #\B char-alphabetic? locale-reader-long-month
             (lambda (val object flags)
	       (month-read! flags)
               (set-date-month! object val)))
       (list #\d char-numeric? ireader2 (lambda (val object flags)
					  (day-read! flags)
                                          (set-date-day!
                                           object val)))
       (list #\e char-fail eireader2 (lambda (val object flags)
				       (day-read! flags)
                                       (set-date-day! object val)))
       (list #\h char-alphabetic? locale-reader-abbr-month
             (lambda (val object flags)
	       (month-read! flags)
               (set-date-month! object val)))
       (list #\H char-numeric? ireader2 (lambda (val object flags)
					  (hour-read! flags)
                                          (set-date-hour! object val)))
       (list #\k char-fail eireader2 (lambda (val object flags)
				       (hour-read! flags)
                                       (set-date-hour! object val)))
       (list #\m char-numeric? ireader2 (lambda (val object flags)
					  (month-read! flags)
                                          (set-date-month! object val)))
       (list #\M char-numeric? ireader2 (lambda (val object flags)
					  (minute-read! flags)
                                          (set-date-minute!
                                           object val)))
       (list #\N char-numeric? fireader9 (lambda (val object flags)
					   (nanosecod-read! flags)
                                           (set-date-nanosecond! object val)))
       (list #\S char-numeric? ireader2 (lambda (val object flags)
					  (second-read! flags)
                                          (set-date-second! object val)))
       (list #\y char-fail eireader2
             (lambda (val object flags)
	       (year-read! flags)
               (set-date-year! object (tm:natural-year val))))
       (list #\Y char-numeric? ireader4 (lambda (val object flags)
					  (year-read! flags)
                                          (set-date-year! object val)))
       (list #\z (lambda (c)
                   (or (char=? c #\Z)
                       (char=? c #\z)
                       (char=? c #\+)
                       (char=? c #\-)))
             tm:zone-reader (lambda (val object flags)
			      (offset-read! flags)
                              (set-date-zone-offset! object val)))
       )))

  (define (tm:string->date date flags index format-string str-len port template-string)
    (define (skip-until port skipper)
      (let ((ch (peek-char port)))
        (if (eof-object? ch)
            (tm:time-error 'string->date 'bad-date-format-string template-string)
            (if (not (skipper ch))
                (begin (read-char port) (skip-until port skipper))))))
    (if (>= index str-len)
	(values)
        (let ( (current-char (string-ref format-string index)) )
          (if (not (char=? current-char #\~))
              (let ((port-char (read-char port)))
                (if (or (eof-object? port-char)
                        (not (char=? current-char port-char)))
                    (tm:time-error 'string->date 'bad-date-format-string template-string))
                (tm:string->date date flags (+ index 1) format-string str-len port template-string))
              ;; otherwise, it's an escape, we hope
              (if (> (+ index 1) str-len)
                  (tm:time-error 'string->date 'bad-date-format-string template-string)
                  (let* ( (format-char (string-ref format-string (+ index 1)))
                          (format-info (assoc format-char tm:read-directives)) )
                    (if (not format-info)
                        (tm:time-error 'string->date 'bad-date-format-string template-string)
                        (begin
                          (let ((skipper (cadr format-info))
                                (reader  (caddr format-info))
                                (actor   (cadddr format-info)))
                            (skip-until port skipper)
                            (let ((val (reader port)))
                              (if (eof-object? val)
                                  (tm:time-error 'string->date 'bad-date-format-string template-string)
                                  (actor val date flags)))
                            (tm:string->date date flags (+ index 2) format-string  str-len port template-string))))))))))

  (define (string->date input-string template-string)
    (define (tm:date-ok? date flags)
      (unless (date-zone-offset date)
	;; rather incomplete but better than nothing
	(set-date-zone-offset! date 0)
	(let ((off (if (and (year-read? flags) (month-read? flags) (day-read? flags))
		       (timezone-offset (local-timezone) (date->time-utc date))
		       (timezone-offset (local-timezone)))))
	  (set-date-zone-offset! date off)))
      (and (date-nanosecond date)
           (date-second date)
           (date-minute date)
           (date-hour date)
           (date-day date)
           (date-month date)
           (date-year date)
           (date-zone-offset date)))
    (let ((newdate (make-date 0 0 0 0 0 0 0 #f))
	  (flags (make-flags)))
      (tm:string->date newdate flags
                       0
                       template-string
                       (string-length template-string)
                       (open-input-string input-string)
                       template-string)
      (if (tm:date-ok? newdate flags)
          newdate
          (tm:time-error 'string->date 'bad-date-format-string (list "Incomplete date read. " newdate template-string)))))

)
