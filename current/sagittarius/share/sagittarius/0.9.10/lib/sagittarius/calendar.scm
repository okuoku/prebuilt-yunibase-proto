;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/calendar.scm - calender
;;;  
;;;   Copyright (c) 2018  Takashi Kato  <ktakashi@ymail.com>
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

#!nounbound
(library (sagittarius calendar)
    (export (rename (calendar <calender>)) calendar?
	    calendar:gregorian
	    calendar:iso
	    (rename (calendar:gregorian calendar:system))
	    calendar-name

	    (rename (calendar-date <calendar-date>))
	    make-calendar-date calendar-date? 
	    make-gregorian-calendar-date make-iso-calendar-date
	    time-utc->calendar-date calendar-date->time-utc current-calendar-date
	    calendar-date-calendar
	    (rename (calendar-date-absolute-date calendar-date->gregorian-day))
	    calendar-date-timezone

	    calendar-date->julian-day
	    
	    calendar-date-add  calendar-date-subtract
	    calendar-date=?
	    calendar-date<?
	    calendar-date<=?
	    calendar-date>?
	    calendar-date>=?
	    +calendar-unit:nanosecond+
	    +calendar-unit:second+
	    +calendar-unit:minute+
	    +calendar-unit:hour+
	    +calendar-unit:day+
	    +calendar-unit:week+
	    +calendar-unit:month+
	    +calendar-unit:year+

	    ;; partial-time and date
	    <partial-time> <partial-date> partial-time? partial-date?
	    ;; this format is so common so export them.
	    <local-time> local-time? make-local-time
	    local-time-hour local-time-minute
	    local-time-second local-time-nanosecond
	    <local-date> local-date? make-local-date
	    local-date-day local-date-month local-date-year)
    (import (rnrs)
	    (rnrs r5rs)
	    (clos user)
	    (sagittarius)
	    (sagittarius time-private)
	    (sagittarius time-util)
	    (sagittarius timezone)
	    (sagittarius calendar constants)
	    (sagittarius calendar gregorian)
	    (sagittarius calendar iso)
	    (sagittarius calendar locals))

(define-record-type calendar
  (fields name
	  components->absolute
	  absolute->components
	  add-unit))

(define calendar:gregorian (make-calendar "Gregorian"
					  gregorian-components->absolute
					  absolute->gregorian-components
					  gregorian-calendar-add-unit))
(define calendar:iso (make-calendar "ISO"
				    iso-components->absolute
				    absolute->iso-components
				    iso-calendar-add-unit))
;; TBD
;; (define calendar:julian (make-calendar "Julian" #f #f))

;; time-utc -> absolute date
(define (time-utc->absolute time timezone)
  (let ((sec (+ (time-second time) (timezone-offset timezone)
		+epoch-in-utc-second+)))
    (+ (nanosecond->day (+ (* sec tm:nano) (time-nanosecond time))) 1)))

(define (absolute->time-utc absolute timezone)
  (let ((nsec (day->nanosecond (- absolute 1))))
    (make-time time-utc
	       (mod nsec tm:nano)
	       (- (floor (/ nsec tm:nano))
		  +epoch-in-utc-second+
		  (timezone-offset timezone)))))

(define-record-type calendar-date
  ;; absent = #f
  (fields absolute-date ;; 1 = 1/1/1 12:00:00
	  timezone ;; timezone
	  calendar ;; calendar-type
	  >time-utc ;; cache
	  )
  (protocol (lambda (p)
	      (lambda (absolute-date timezone calendar)
		(p absolute-date timezone calendar
		   (absolute->time-utc absolute-date timezone))))))

(define (%make-calendar-date calendar . components)
  (let-values (((absolute tz)
		(apply (calendar-components->absolute calendar)
		       components)))
    (make-calendar-date absolute tz calendar)))
(define (make-gregorian-calendar-date n s m h d M y
				      :optional (timezone (local-timezone)))
  (%make-calendar-date calendar:gregorian n s m h d M y timezone))
(define (make-iso-calendar-date n s m h d w y
				:optional (timezone (local-timezone)))
  (%make-calendar-date calendar:iso n s m h d w y timezone))


(define (time-utc->calendar-date time :optional (timezone (local-timezone))
				 (calendar calendar:gregorian))
  (unless (eq? (time-type time) 'time-utc)
    (assertion-violation 'time-utc->calendar "invalid time type" time))
  (let ((absolute (time-utc->absolute time timezone)))
    (make-calendar-date absolute timezone calendar)))

(define (current-calendar-date :optional (calendar calendar:gregorian))
  (time-utc->calendar-date (current-time) (local-timezone) calendar))

(define (calendar-date->julian-day cd)
  (+ (calendar-date-absolute-date cd) +julian-day-offset+))

(define (calendar-date=? cd1 cd2)
  (time=? (calendar-date->time-utc cd1) (calendar-date->time-utc cd2)))
(define (calendar-date<? cd1 cd2)
  (time<? (calendar-date->time-utc cd1) (calendar-date->time-utc cd2)))
(define (calendar-date<=? cd1 cd2)
  (time<=? (calendar-date->time-utc cd1) (calendar-date->time-utc cd2)))
(define (calendar-date>? cd1 cd2)
  (time>? (calendar-date->time-utc cd1) (calendar-date->time-utc cd2)))
(define (calendar-date>=? cd1 cd2)
  (time>=? (calendar-date->time-utc cd1) (calendar-date->time-utc cd2)))

;; API
(define (calendar-date-add calendar-date unit amount)
  (define calendar (calendar-date-calendar calendar-date))
  (define absolute (calendar-date-absolute-date calendar-date))
  (unless (and (exact? amount) (integer? amount))
    (assertion-violation 'calendar-date-add "Amount must be an exact integer"
			 amount))
  (let ((new-absolute ((calendar-add-unit calendar) absolute unit amount)))
    (make-calendar-date new-absolute (calendar-date-timezone calendar-date)
			calendar)))

;; API
(define (calendar-date-subtract calendar-date unit amount)
  (calendar-date-add calendar-date unit (- amount)))

;; print
(define-method write-object ((c calendar) out)
  (format out "#<calendar ~a>" (calendar-name c)))
(define-method write-object ((cd calendar-date) out)
  (format out "#<calendar-date ~a ~d (~a)>"
	  (calendar-date-calendar cd)
	  (calendar-date-absolute-date cd)
	  (calendar-date-timezone cd)))
)
