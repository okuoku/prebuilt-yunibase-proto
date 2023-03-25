;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sitelib/%3a4/numeric-vectors.scm - Homogeneous numeric vector datatypes.
;;;  
;;;   Copyright (c) 2010-2019  Takashi Kato  <ktakashi@ymail.com>
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
(library (srfi :4 numeric-vectors)
    (export :export-reader-macro
	    <s8vector>
	    make-s8vector s8vector s8vector? s8vector-length s8vector-ref
	    s8vector-set! s8vector->list list->s8vector
	    <u8vector>
	    make-u8vector u8vector u8vector? u8vector-length u8vector-ref
	    u8vector-set! u8vector->list list->u8vector
	    <s16vector>
	    make-s16vector s16vector s16vector? s16vector-length s16vector-ref
	    s16vector-set! s16vector->list list->s16vector
	    <u16vector>
	    make-u16vector u16vector u16vector? u16vector-length u16vector-ref
	    u16vector-set! u16vector->list list->u16vector
	    <s32vector>
	    make-s32vector s32vector s32vector? s32vector-length s32vector-ref
	    s32vector-set! s32vector->list list->s32vector
	    <u32vector>
	    make-u32vector u32vector u32vector? u32vector-length u32vector-ref
	    u32vector-set! u32vector->list list->u32vector
	    <s64vector>
	    make-s64vector s64vector s64vector? s64vector-length s64vector-ref
	    s64vector-set! s64vector->list list->s64vector
	    <u64vector>
	    make-u64vector u64vector u64vector? u64vector-length u64vector-ref
	    u64vector-set! u64vector->list list->u64vector
	    <f32vector>
	    make-f32vector f32vector f32vector? f32vector-length f32vector-ref
	    f32vector-set! f32vector->list list->f32vector
	    <f64vector>
	    make-f64vector f64vector f64vector? f64vector-length f64vector-ref
	    f64vector-set! f64vector->list list->f64vector

	    ;; for SRFI-160 ...
	    define-tagged-vector
	    )
    (import (rnrs)
	    (sagittarius reader)
	    (sagittarius)
	    (util list)
	    (clos user))

  (define (write-vector bv prefix bytevector-length offset getter port)
    (display #\# port)
    (display prefix port)
    (display #\( port)
    (do ((limit (bytevector-length bv))
	 (i 0 (+ i offset)))
	((= i limit))
      (unless (zero? i)
	(display " " port))
      (write (getter bv i) port))
    (display #\) port))

  (define (generate-reader ctr)
    (lambda (in ctx)
      (let* ((bv (read-cache-object in ctx))
	     (r (ctr (bytevector-length bv))))
	(slot-set! r 'value bv)
	r)))
  (define (cache-writer o out ctx)
    (write-object-cache (slot-ref o 'value) out ctx))

  (define-syntax define-tagged-vector
    (lambda (x)
      (syntax-case x ()
	((k tag offset make-bytevector bytevector-length bytevector=?
	    getter setter)
	 (let ((name (format "~avector" (syntax->datum #'tag)))
	       (formats (lambda (f name)
			  (string->symbol (format f name)))))
	   (with-syntax ((meta (datum->syntax #'k (formats "<~a-meta>" name)))
			 (class (datum->syntax #'k (formats "<~a>" name)))
			 (ctr   (datum->syntax #'k (formats "make-~a" name)))
			 (ctr2  (datum->syntax #'k (formats "~a" name)))
			 (pred  (datum->syntax #'k (formats "~a?" name)))
			 (len  (datum->syntax #'k (formats "~a-length" name)))
			 (ref  (datum->syntax #'k (formats "~a-ref" name)))
			 (set  (datum->syntax #'k (formats "~a-set!" name)))
			 (->list (datum->syntax #'k
						(formats "~a->list" name)))
			 (list-> (datum->syntax #'k
						(formats "list->~a" name))))
	     #'(begin
		 (define-class meta (<class>) ())
		 ;; ctr is used in initialize, so it must be here
		 (define (ctr n :optional (value 0))
		   (let* ((len (* n offset))
			  (v (make-bytevector len)))
		     (do ((i 0 (+ i offset)))
			 ((= i len) (make class :value v))
		       (setter v i value))))

		 (define (ctr2 . args)
		   (let* ((len (* (length args) offset))
			  (bv (make-bytevector len)))
		     (do ((i 0 (+ i offset)) (v args (cdr v)))
			 ((= i len) (make class :value bv))
		       (setter bv i (car v)))))

		 (define-method initialize ((klass meta) initargs)
		   (call-next-method)
		   ;; we don't need scanner
		   (slot-set! klass 'cache-reader (generate-reader ctr))
		   (slot-set! klass 'cache-writer cache-writer))

		 (define-class class (<sequence>)
		   ((value :init-keyword :value))
		   :metaclass meta)

		 (define-method write-object ((o class) (p <port>))
		   (write-vector (slot-ref o 'value) tag bytevector-length
				 offset getter p))
		 (define-method object-equal? ((a class) (b class))
		   (bytevector=? (slot-ref a 'value) (slot-ref b 'value)))
		 
		 (define (pred o) (is-a? o class))
		 (define (len bv)
		   (unless (pred bv)
		     (assertion-violation 'len
					  (format "~a required but got ~s"
						  class bv)))
		   (/ (bytevector-length (slot-ref bv 'value)) offset))
		 (define (ref bv i)
		   (unless (pred bv)
		     (assertion-violation 'ref
					  (format "~a required but got ~s"
						  class bv)))
		   (getter (slot-ref bv 'value) (* i offset)))
		 (define (set bv i o)
		   (unless (pred bv)
		     (assertion-violation 'set
					  (format "~a required but got ~s"
						  class bv)))
		   (setter (slot-ref bv 'value) (* i offset) o))
		 (define (->list bv :optional (start 0) (end (len bv)))
		   (unless (pred bv)
		     (assertion-violation '->list
					  (format "~a required but got ~s"
						  class bv)))
		   (do ((limit end)
			(i start (+ i 1))
			(r '() (cons (ref bv i) r)))
		       ((= i limit) (reverse! r))))
		 (define (list-> lst)
		   (define len (length lst))
		   (let ((r (ctr len)))
		     (do ((i 0 (+ i 1)) (lst lst (cdr lst)))
			 ((null? lst) r)
		       (set r i (car lst)))))))))
	((k tag offset getter setter)
	 #'(k tag offset make-bytevector bytevector-length bytevector=?
	      getter setter)))))

  (define-tagged-vector "s8" 1 bytevector-s8-ref bytevector-s8-set!)
  (define-tagged-vector "u8" 1 bytevector-u8-ref bytevector-u8-set!)
  (define-tagged-vector "s16" 2 bytevector-s16-native-ref
    bytevector-s16-native-set!)
  (define-tagged-vector "u16" 2 bytevector-u16-native-ref
    bytevector-u16-native-set!)
  (define-tagged-vector "s32" 4 bytevector-s32-native-ref
    bytevector-s32-native-set!)
  (define-tagged-vector "u32" 4 bytevector-u32-native-ref
    bytevector-u32-native-set!)
  (define-tagged-vector "s64" 8 bytevector-s64-native-ref
    bytevector-s64-native-set!)
  (define-tagged-vector "u64" 8 bytevector-u64-native-ref
    bytevector-u64-native-set!)
  (define-tagged-vector "f32" 4 bytevector-ieee-single-native-ref
    bytevector-ieee-single-native-set!)
  (define-tagged-vector "f64" 8 bytevector-ieee-double-native-ref
    bytevector-ieee-double-native-set!)

  (define-dispatch-macro |#u-reader| #\# #\u
    (lambda (port c param)
      (let ((n (read port)))
	(unless (integer? n)
	  (raise-i/o-read-error '|#s-reader| "invalid character for #u" port))
	(let ((lst (read port))) ;; must be a list
	  (unless (list? lst)
	    (raise-i/o-read-error '|#s-reader|
				  (format "list required, but got ~s" lst)
				  port))
	  (let ((ctr (case n
		       ((8)  u8vector)
		       ((16) u16vector)
		       ((32) u32vector)
		       ((64) u64vector)
		       (else
			(raise-i/o-read-error '|#u-reader|
			 (format "given number was not supported ~a" n)
			 port)))))
	    (apply ctr lst))))))

  (define-dispatch-macro |#s-reader| #\# #\s
    (lambda (port c param)
      (let ((n (read port)))
	(unless (integer? n)
	  (raise-i/o-read-error '|#s-reader| "invalid character for #s" port))
	(let ((lst (read port))) ;; must be a list
	  (unless (list? lst)
	    (raise-i/o-read-error '|#s-reader|
				  (format "list required, but got ~s" lst)
				  port))
	  (let ((ctr (case n
		       ((8)  s8vector)
		       ((16) s16vector)
		       ((32) s32vector)
		       ((64) s64vector)
		       (else
			(raise-i/o-read-error '|#s-reader|
			 (format "given number was not supported ~a" n)
			 port)))))
	    (apply ctr lst))))))

  (define-dispatch-macro |#f-reader| #\# #\f
    (lambda (port c param)
      (if (delimited-char? (lookahead-char port))
	  #f
	  (let ((n (read port)))
	    (cond 
	     ((eq? n 'alse) #f) ;; for R7RS support
	     (else 
	      (unless (integer? n)
		(raise-i/o-read-error '|#f-reader|
				      "invalid character for #f" port))
	      (let ((lst (read port))) ;; must be a list
		(unless (list? lst)
		  (raise-i/o-read-error '|#f-reader|
					(format "list required, but got ~s" lst)
					port))
		(let ((ctr (case n
			     ((32) f32vector)
			     ((64) f64vector)
			     (else
			      (raise-i/o-read-error '|#f-reader|
			       (format "given number was not supported ~a" n)
			       port)))))
		  (apply ctr lst)))))))))
  )
