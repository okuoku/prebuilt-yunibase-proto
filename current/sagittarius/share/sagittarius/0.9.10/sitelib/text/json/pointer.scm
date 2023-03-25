;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; text/json/pointer.scm - JSON Pointer
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

;; reference:
;; RFC 6901: https://tools.ietf.org/html/rfc6901
;; For now, we only provide get (and it's enough for most of the time)
;; So '-' is more or less useless
#!nounbound
(library (text json pointer)
    (export json-pointer json-pointer-not-found?
	    parse-json-pointer)
    (import (rnrs)
	    (peg)
	    (text json)
	    (text json mutable)
	    (sagittarius generators)
	    (srfi :127 lseqs))

(define unescaped
  ($do (c ($satisfy (lambda (c) (not (or (eqv? #\/ c) (eqv? #\~ c))))))
       ($return c)))
(define escaped
  ($do (($satisfy (lambda (c) (eqv? #\~ c))))
       (c ($satisfy (lambda (c) (or (eqv? #\0 c) (eqv? #\1 c)))))
       ($return (case c ((#\0) #\~) ((#\1) #\/)))))
(define reference-token
  ($do (chars ($many ($or unescaped escaped)))
       ($return (list->string chars))))
(define root
  ($do (jp ($many ($seq ($satisfy (lambda (c) (eqv? c #\/)))
			reference-token)))
       ($return jp)))

;; more for testing...
(define (parse-json-pointer p)
  (define (->lseq p)
    (if (port? p)
	(generator->lseq (port->char-generator p))
	(string->list p)))
  (let-values (((s v nl) (root (->lseq p))))
    (if (and (parse-success? s) (null? nl))
	v
	(error 'json-pointer "Failed to parse JSON pointer"))))

(define-record-type not-found)
(define +json-pointer-not-found+ (make-not-found))
(define (json-pointer-not-found? v) (eq? +json-pointer-not-found+ v))

;; API
(define (json-pointer p . maybe-parent)
  (define parent (if (null? maybe-parent) values (car maybe-parent)))
  (define tokens (parse-json-pointer p))
  (case (*json-map-type*)
    ((vector) (make-vector-json-pointer tokens parent))
    ((alist)  (make-alist-json-pointer tokens parent))))

(define (->array-index p)
  (let ((n (string->number p)))
    (and n
	 (string=? p (number->string n))
	 n)))

(define (handle-mutable-json json p)
  (cond ((mutable-json-object? json)
	 (mutable-json-object-ref json p +json-pointer-not-found+))
	((and (mutable-json-array? json) (->array-index p)) =>
	 (lambda (n)
	   (and (< n (mutable-json-array-size json))
		(mutable-json-array-ref json n))))
	(else #f)))
(define (make-vector-json-pointer tokens parent)
  (define (find-vector-map json p)
    (define len (vector-length json))
    (do ((i 0 (+ i 1)))
	((or (= i len) (equal? (car (vector-ref json i)) p))
	 (if (= i len)
	     +json-pointer-not-found+
	     (cdr (vector-ref json i))))))
  (define (find-list-array json p)
    (do ((i 0 (+ i 1)) (j json (cdr j)))
	((or (= i p) (null? j))
	 (if (null? j)
	     +json-pointer-not-found+
	     (car j)))))
  (fold-left (lambda (pointer p)
	       (lambda (json)
		 (let ((json (pointer json)))
		   (cond ((vector? json) (find-vector-map json p))
			 ((and (pair? json) (->array-index p)) =>
			  (lambda (n) (find-list-array json n)))
			 ((handle-mutable-json json p))
			 (else +json-pointer-not-found+)))))
	     (lambda (json) (parent json)) tokens))

(define (make-alist-json-pointer tokens parent)
  (fold-left (lambda (pointer p)
	       (lambda (json)
		 (let ((json (pointer json)))
		   (cond ((and (pair? json) (assoc p json)) => cdr)
			 ((and (vector? json) (->array-index p)) =>
			  (lambda (n)
			    (if (< n (vector-length json))
				(vector-ref json n)
				+json-pointer-not-found+)))
			 ((handle-mutable-json json p))
			 (else +json-pointer-not-found+)))))
	     (lambda (json) (parent json)) tokens))
)
