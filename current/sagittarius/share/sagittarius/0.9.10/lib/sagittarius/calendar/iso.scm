;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/calendar/iso.scm - ISO calendar calculation
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

(library (sagittarius calendar iso)
    (export iso->absolute absolute->iso
	    make-iso-local-date iso-local-date?
	    iso-local-date-day iso-local-date-week iso-local-date-year

	    +iso-calendar-unit+ iso-calendar-add-unit
	    ;; aux APIs
	    iso-components->absolute absolute->iso-components
	    )
    (import (rnrs)
	    (sagittarius)
	    (sagittarius timezone)
	    (sagittarius time-util)
	    (sagittarius calendar gregorian)
	    (sagittarius calendar constants)
	    (sagittarius calendar locals))

;; like 2018-W52-7
;; day  = 7
;; week = 52
;; year = 2018
(define-record-type iso-local-date
  (parent <partial-date>)
  (fields day week year))

;;; Aux APIs
(define (absolute->day-of-week date) (mod date 7))
(define (kday-on-or-before k date) (- date (absolute->day-of-week (- date k))))
(define (kday-on-or-after k date) (kday-on-or-before k (+ date 6)))
(define (kday-before k date) (kday-on-or-before k (- date 1)))
(define (kday-after k date) (kday-on-or-after k (+ date 7)))
(define (nth-kday n k d m y tz)
  ;; do we need tz or can be GMT?
  (let ((d (gregorian-components->absolute 0 0 0 12 d m y tz)))
    (if (> n 0)
	(+ (* n 7) (kday-before k d))
	(+ (* n 7) (kday-after k d)))))

;;; Aux API
(define (iso-components->absolute n s m h d w y . maybe-tz)
  (define tz (if (null? maybe-tz) (local-timezone) (car maybe-tz)))
  (let ((hid (time-components->absolute n s m h tz)))
    (values (+ (nth-kday w +sunday+ 28 12 (- y 1) tz) d hid) tz)))

;;; API
(define (iso->absolute local-time iso-date . maybe-tz)
  (define tz (if (null? maybe-tz) (local-timezone) (car maybe-tz)))
  (iso-components->absolute (local-time-nanosecond local-time)
			   (local-time-second local-time)
			   (local-time-minute local-time)
			   (local-time-hour local-time)
			   (iso-local-date-day iso-date)
			   (iso-local-date-week iso-date)
			   (iso-local-date-year iso-date)
			   tz))

(define (iso-long-year? year tz)
  (let ((jan1 (absolute->day-of-week (gregorian-new-year year tz)))
	(dec31 (absolute->day-of-week (gregorian-end-of-year year tz))))
    (or (= jan1 +thursday+)
	(= dec31 +thursday+))))

;;; API
(define (absolute->iso-components odate . maybe-tz)
  (define tz (if (null? maybe-tz) (local-timezone) (car maybe-tz)))
  (define (nsec->day nsec)
    (/ nsec tm:nano ;; -> sec
       60 ;; -> min
       60 ;; -> hour
       24 ;; -> day
       ))
  (define (->day date)
    (let ((d (mod date 7)))
      (if (zero? d)
	  (+ d 7)
	  d)))
  (define (fixup n s m h d0 w y tz)
    (define (compute-w&y w y)
      (cond ((<= w 52) (values w y))
	    ((and (= w 53) (iso-long-year? y tz)) (values w y))
	    (else
	     (let ((y0 (+ y 1)))
	       (if (iso-long-year? y tz)
		   (values (- w 53) y0)
		   (values (- w 52) y0))))))
    (if (<= d0 7)
	(values n s m h d0 w y)
	(let ((d (->day d0))
	      (w0 (+ w 1)))
	  (let-values (((w y) (compute-w&y w0 y)))
	    (values n s m h d w y)))))
  (let* ((date (exact (floor odate)))
	 (approx (absolute->gregorian-year (- date 3) tz))
	 (tmp (iso-components->absolute 0 0 0 12 1 1 (+ approx 1) tz))
	 (year (if (>= date tmp) (+ approx 1) approx))
	 (week (+ (div (- date (iso-components->absolute 0 0 0 12 1 1 year tz))
		       7)
		  1))
	 (day (->day date)))
    (let*-values (((d nsec) (absolute->day&nanosecond odate))
		  ((n s m h c) (absolute->time-components (nsec->day nsec) tz)))
	(if (zero? c)
	    (values n s m h day week year)
	    ;; at this moment, carry must be 1
	    (fixup n s m h (+ day c) week year tz)))))

(define (absolute->iso odate . maybe-tz)
  (define tz (if (null? maybe-tz) (local-timezone) (car maybe-tz)))
  (let-values (((n s m h d w y) (absolute->iso-components odate tz)))
    (values (make-local-time n s m h)
	    (make-iso-local-date d w y)
	    tz)))

(define +iso-calendar-unit+
  (calendar-unit-set nanosecond second minute hour week month year))

(define (iso-calendar-add-unit absolute unit amount)
  (define gmt (timezone "GMT"))
  (unless (enum-set-member? unit +iso-calendar-unit+)
    (assertion-violation 'iso-calendar-add-unit "unsupported unit" unit))
  (if (enum-set-member? unit +common-time-calendar-unit-set+)
      (add-common-calendar-unit absolute unit amount)
      (let-values (((n s m h d w y)
		    (absolute->iso-components absolute *timezone/gmt*)))
	(case unit
	  ((week) (iso-components->absolute n s m h d (+ w amount) y gmt))
	  ((year) (iso-components->absolute n s m h d w (+ y amount) gmt))))))
)
