;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; text/markdown/parser/source.scm - Source info of input markdown
;;;  
;;;   Copyright (c) 2022  Takashi Kato  <ktakashi@ymail.com>
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
(library (text markdown parser source)
    (export source-line:of
	    source-line?
	    source-line-content
	    source-line-location
	    source-line:substring
	    source-line:char-at
	    source-line:length
	    source-line:prefix?
	    source-line:letter?
	    source-line:whitespace?
	    source-line:regexp-search
	    source-line:index
	    source-line:index-right

	    source-location:of
	    source-location?
	    source-location-line
	    source-location-column
	    source-location-length

	    source-locations?
	    source-locations:empty
	    source-locations:add!
	    source-locations:add-all!
	    source-locations:locations

	    source-lines:of source-lines?
	    source-lines:empty source-lines:empty?
	    source-lines:size
	    source-lines:first
	    source-lines:last
	    source-lines:content
	    source-lines:add-line!
	    source-lines->vector ;; for scanner
	    source-lines:source-loactions
	    source-lines:for-each
	    )
    (import (rnrs)
	    (core misc) ;; for define-vector-type
	    (srfi :1 lists)
	    (srfi :13 strings)
	    (srfi :14 char-sets)
	    (srfi :115 regexp)
	    (srfi :117 list-queues)
	    (util flexible-vector))

(define-vector-type source-line (source-line:of content location) source-line?
  (content  source-line-content)
  (location source-line-location))

(define source-line:substring
  (case-lambda
   ((sl start) (source-line:substring sl start (source-line:length sl)))
   ((sl start end)
    (define content (source-line-content sl))
    (define loc (source-line-location sl))
    (define (compute-loc loc start end)
      (and loc
	   (not (= start end))
	   (source-location:of (source-location-line loc)
			       (+ (source-location-column loc)  start)
			       (- end start))))
    (let ((c (substring content start end)))
      (source-line:of c (compute-loc loc start end))))))

(define (source-line:char-at sl index)
  (let ((content (source-line-content sl)))
    ;; we use string-ref here as it's O(1) anyway
    (and (< index (string-length content))
	 (string-ref content index))))
(define (source-line:length sl)
  (string-length (source-line-content sl)))
(define (source-line:prefix? sl s . start&end)
  (apply string-prefix? s (source-line-content sl) 0 (string-length s)
	 start&end))
(define (source-line:letter? sl index)
  (cond ((source-line:char-at sl index) =>
	 (lambda (c) (char-set-contains? char-set:letter c)))
	(else #f)))
(define (source-line:whitespace? sl index)
  (cond ((source-line:char-at sl index) => char-whitespace?)
	(else #f)))
(define (source-line:regexp-search sl rx . start&end)
  (apply regexp-search rx (source-line-content sl) start&end))
(define (source-line:index sl char . start&end)
  (apply string-index (source-line-content sl) char start&end))
(define (source-line:index-right sl char . start&end)
  (apply string-index-right (source-line-content sl) char start&end))


(define-vector-type source-location (source-location:of line column length)
  source-location?
  (line   source-location-line)
  (column source-location-column)
  (length source-location-length))

(define-vector-type source-locations (make-source-locations locations)
  source-locations?
  (locations source-locations-locations))
(define (source-locations:empty) (make-source-locations (flexible-vector)))
(define (source-locations:locations locs)
  (make-list-queue (flexible-vector->list (source-locations-locations locs))))
(define (source-locations:add-all! locs loc*)
  (define (add! loc) (source-locations:add! locs loc))
  (for-each add! loc*))
(define (source-locations:add! locs loc*)
  (define fv (source-locations-locations locs))
  (cond ((list-queue-empty? loc*))
	((flexible-vector-empty? fv)
	 (apply flexible-vector-insert-back! fv (list-queue-list loc*)))
	(else
	 (let* ((last-index (- (flexible-vector-size fv) 1))
		(loc-list (list-queue-list loc*))
		(a (flexible-vector-ref fv last-index))
		(b (car loc-list)))
	   (if (and (= (source-location-line a) (source-location-line b))
		    (= (+ (source-location-column a) (source-location-length a))
		       (source-location-column b)))
	       (let ((sl (source-location:of (source-location-line a)
					     (source-location-column a)
					     (+ (source-location-length a)
						(source-location-length b)))))
		 (flexible-vector-set! fv last-index sl)
		 (apply flexible-vector-insert-back! fv (cdr loc-list)))
	       (apply flexible-vector-insert-back! fv loc-list))))))

(define-vector-type source-lines (make-source-lines lines) source-lines?
  (lines source-lines-lines))
(define (source-lines:of line)
  (cond ((list-queue? line) (make-source-lines line))
	((source-line? line) (make-source-lines (list-queue line)))
	(else (assertion-violation 'source-lines:of "Unsupported value" line))))
(define (source-lines:empty) (make-source-lines (list-queue)))
(define (source-lines:empty? sl*)
  (list-queue-empty? (source-lines-lines sl*)))
(define (source-lines:size sl*)
  (list-queue-length (source-lines-lines sl*)))
(define (source-lines:first sl*)
  (list-queue-front (source-lines-lines sl*)))
(define (source-lines:last sl*)
  (list-queue-back (source-lines-lines sl*)))
(define (source-lines->vector sl*)
  (list->vector (list-queue-list (source-lines-lines sl*))))
(define (source-lines:content sl*)
  (define lines (source-lines-lines sl*))
  (string-join (map source-line-content (list-queue-list lines)) "\n"))

(define (source-lines:add-line! sl* line)
  (list-queue-add-back! (source-lines-lines sl*) line)
  sl*)

(define (source-lines:source-loactions sl*)
  (filter-map source-line-location (list-queue-list (source-lines-lines sl*))))

(define (source-lines:for-each proc sl*)
  (list-queue-for-each proc (source-lines-lines sl*)))
)
