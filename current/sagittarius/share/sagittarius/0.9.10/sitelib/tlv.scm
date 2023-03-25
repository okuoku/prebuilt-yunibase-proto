;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; tlv.scm - TLV parser
;;;
;;;   Copyright (c) 2010-2014  Takashi Kato  <ktakashi@ymail.com>
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
(library (tlv)
    (export make-tlv-parser make-emv-tlv-parser
	    EMV DGI
	    
	    tlv-builder
	    tlv-object?
	    tlv-tag (rename (%tlv-tag tlv-tag-as-bytevector))
	    tlv-data tlv-components
	    <tlv>
	    ;; utilities
	    dump-tlv
	    tlv->bytevector
	    ->tlv make-tlv-unit
	    read-tlv

	    ;; for extension
	    ;; it's actually BER length reader
	    (rename (read-length read-ber-length))

	    ;; for compatibility
	    (rename (write-emv-tlv write-tlv))
	    write-emv-tlv write-dgi-tlv
	    *tag-dictionary*

	    ;; this is LV (not TLV) but good to have here
	    make-lv-parser
	    read-lv
	    )
    (import (rnrs) 
	    (clos user)
	    (only (sagittarius)
		  format define-constant reverse! and-let*
		  bytevector->integer integer->bytevector)
	    (sagittarius control)
	    (binary pack)
	    (util bytevector)
	    (srfi :26 cut)
	    (srfi :39 parameters))

  ;; default TLV class
  (define-class <tlv> ()
    ((tag    :init-keyword :tag    :reader %tlv-tag)
     ;; bytevector
     (data   :init-keyword :data   :reader tlv-data :init-value #f)
     ;; constructed TLV
     (components :init-keyword :components :reader tlv-components
		 :init-value '())))

  (define (tlv-object? o) (is-a? o <tlv>))
  (define (tlv-tag tlv) (bytevector->integer (%tlv-tag tlv)))

  (define-method write-object ((o <tlv>) p)
    (if (tlv-data o)
	(format p "#<tlv :tag ~a :data ~a>" 
		(bytevector->hex-string (%tlv-tag o)) (tlv-data o))
	(format p "#<tlv :tag ~a :components ~a>"
		(bytevector->hex-string (%tlv-tag o)) (tlv-components o))))

  (define (make-tlv-unit tag data) 
    (let1 tag (cond ((integer? tag) (integer->bytevector tag))
		    ((bytevector? tag) tag)
		    (else (assertion-violation 
			   'make-tlv-unit 
			   "tag must be an integer or bytevector" tag)))
      (make <tlv> :tag tag :data data)))

  (define (make-generic-tlv-parser tag-reader length-reader object-builder)
    (define (parse-tlv-object-list in in-indefinite?)
      (let loop ((o (tlv-parser in)) (r '()))
	(cond (in-indefinite?
	       (let ((b (lookahead-u8 in)))
		 (when (eof-object? b)
		   (assertion-violation 'tlv-parser
					"Indefinite without termination"))
		 (cond ((and (zero? b) (get-u8 in))
			(let ((b2 (lookahead-u8 in)))
			  (when (eof-object? b2)
			    (assertion-violation 'tlv-parser
				 "Indefinite without termination"))
			  (if (zero? b2)
			      (begin (get-u8 in) (reverse! (cons o r)))
			      (loop (tlv-parser-rec b in) (cons o r)))))
		       (else (loop (tlv-parser in) (cons o r))))))
	      ((eof-object? o)
	       (when in-indefinite?
		 (assertion-violation 'tlv-parser
				      "Indefinite without termination"))
	       (reverse! r))
	      (else (loop (tlv-parser in) (cons o r))))))

    (define (handle-indefinite b tag in)
      (let ((v (parse-tlv-object-list in #t)))
	;; Is indefinite length value always constructed?
	(object-builder b tag v 'indefinite)))

    (define (tlv-parser-rec b in)
      ;; This is ASN.1 specific, not sure if we put it here
      ;; but who's using custom length reader et.al?
      (define (can-be-shorter? size data)
	(define data-len (bytevector-length data))
	(and (pair? size)
	     (let ((len (cond ((< data-len 127) 1)
			      (else (+ (get-emv-length data-len) 1)))))
	       (not (= (car size) len)))))
      (if (eof-object? b)
	  b
	  (let-values (((tag constructed?) (tag-reader in b))
		       ((len) (length-reader in)))
	    (cond (len
		   (let* ((l (if (pair? len) (cdr len) len))
			  (data (get-bytevector-n in l)))
		     (when (< (bytevector-length data) l)
		       (assertion-violation 'tlv-parser "corrupted data"))
		     (if constructed?
			 (object-builder b tag 
			   (parse-tlv-object-list 
			    (open-bytevector-input-port data) #f)
			   (if (can-be-shorter? len data)
			       'long-definite
			       'definite))
			 (object-builder b tag data
					 (can-be-shorter? len data)))))
		  ((not constructed?)
		   (assertion-violation 'tlv-parser
					"indefinite length found" tag))
		  (else
		   (handle-indefinite b tag in))))))
    ;; separator is null TLV object
    ;; = tag 0 length 0 data empty
    (define (tlv-parser in) (tlv-parser-rec (get-u8 in) in))
    tlv-parser)

  ;; DGI style tag and length reader
  (define (read-dgi-tag in b)
    ;; it's always 2 bytes
    (let1 r (make-bytevector 2 b)
      (bytevector-u8-set! r 1 (get-u8 in))
      (values r #f)))
  (define (read-dgi-length in)
    ;; length can be 1 or 3 bytes
    (let1 b (get-u8 in)
      (if (= b #xFF)
	  (+ (bitwise-arithmetic-shift b 16)
	     (bitwise-arithmetic-shift (get-u8 in) 8)
	     (get-u8 in))
	  b)))
  (define (make-dgi-tlv-parser :key (object-builder tlv-builder))
    (make-generic-tlv-parser read-dgi-tag read-dgi-length object-builder))

  ;; EMV(ASN.1) style TLV tag and length reader.
  (define (read-tag in b)
    (define (rec)
      (if (= (bitwise-and b #x1F) #x1F)
	  (call-with-bytevector-output-port
	   (lambda (out)
	     (put-u8 out b)
	     (let1 b2 (get-u8 in)
	       (when (zero? (bitwise-and b2 #x7F))
		 (assertion-violation 
		  'read-tag "corrupted stream - invalid high tag number found"
		  b2))
	       (do ((b3 b2 (get-u8 in)))
		   ((or (eof-object? b3) (zero? (bitwise-and b3 #x80)))
		    (when (eof-object? b3)
		      (assertion-violation 'read-tag
					   "EOF found inside tag value"))
		    (put-u8 out b3))
		 (put-u8 out b3)))))
	  (make-bytevector 1 b)))
    (let1 tag (rec)
      (values tag (not (zero? (bitwise-and #x20 b))))))

  (define (read-length in)
    (let1 len (get-u8 in)
      (when (eof-object? len)
	(assertion-violation 'read-length "EOF found when length expected"))
      (cond ((= len #x80) #f) ;; indefinite length. TODO correct?
	    ((zero? (bitwise-and len #x80)) len)
	    (else
	     (let1 size (bitwise-and len #x7F)
	       (let loop ((i 1) (rlen (get-u8 in)))
		 (if (= i size)
		     (begin 
		       (when (negative? rlen)
			 (assertion-violation 'read-length
			  "corrupted stream - negative length found"))
		       (cons (+ size 1) rlen))
		     (loop (+ i 1) (+ (bitwise-arithmetic-shift rlen 8)
				      (get-u8 in))))))))))
  
  (define (tlv-builder b tag data constructed?)
    (if constructed?
	(make <tlv> :tag tag :components data)
	(make <tlv> :tag tag :data data)))
  
  (define (make-emv-tlv-parser :key (object-builder tlv-builder))
    (make-generic-tlv-parser read-tag read-length object-builder))

  ;; marker
  (define EMV (list read-tag read-length))
  (define DGI (list read-dgi-tag read-dgi-length))

  ;; for convenience
  (define (make-tlv-parser format :optional (object-builder tlv-builder))
    (apply make-generic-tlv-parser `(,@format ,object-builder)))

  ;; alist of tag and name
  (define *tag-dictionary* (make-parameter #f))

  (define (dump-tlv tlv :optional (out (current-output-port)))
    (define dic (*tag-dictionary*))
    (define (print-indent indent)
      (dotimes (i indent)
	(display #\space out)))
    (define (print-tag tag)
      (format out "[Tag] ~a" (bytevector->hex-string tag))
      (and-let* (( (pair? dic) )
		 (name (assv tag dic)))
	(format out ": ~a" (cadr name))))

    (define (dump-data tlv indent)
      (define (dump-hex data)
	(dotimes (i (bytevector-length data))
	  (format out " ~2,'0X" (bytevector-u8-ref data i))))
      (let ((data (tlv-data tlv))
	    (type (and-let* ((tag (bytevector->integer (%tlv-tag tlv)))
			     ( (pair? dic) )
			     (slot (assv tag dic)))
		    (if (null? (cddr slot))
			'hex
			(caddr slot)))))
	(newline out)
	(print-indent (+ indent 2))
	(display "[Data]" out)
	(case type
	  ((number)
	   ;; converts to integer
	   (display #\space)
	   (display (bytevector->integer data) out))
	  ((text)
	   ;; assume UTF8
	   (display #\space)
	   (display (utf8->string data) out)
	   (display ":" out)
	   (dump-hex data))
	  (else 
	   ;; Default hex
	   (dump-hex data)))))
    (define (dump-components tlv indent)
      (unless (zero? indent) (newline out))
      (print-indent indent)
      (print-tag (%tlv-tag tlv))
      (let1 components (tlv-components tlv)
	(if (null? components)
	    (dump-data tlv indent)
	    (for-each (cut dump-components <> (+ indent 2)) components))))
    (dump-components tlv 0) (newline out))

  (define (tlv->bytevector tlv :key (writer write-emv-tlv))
    (call-with-bytevector-output-port (cut writer tlv <>)))

  (define (generic-tlv-writer tlv write-tag write-length
			      :optional (out (current-output-port)))
    (define (write-value bv) (put-bytevector out bv))
    (define (components-values components)
      (call-with-bytevector-output-port
       (lambda (out)
	 (for-each (cut generic-tlv-writer <> write-tag write-length out)
		   components))))

    (unless (binary-port? out)
      (assertion-violation 'write-tlv "binary port required" out))
    (let1 components (tlv-components tlv)
      (let1 value (if (null? components)
		      (tlv-data tlv)
		      (components-values components))
	(write-tag out (%tlv-tag tlv))
	(write-length out (bytevector-length value))
	(write-value value))))

  (define (write-dgi-tlv tlv . opts)
    (define (write-tag out tag) (put-bytevector out tag))
    (define (write-length out len) 
      ;; 1 or 3
      (if (> len #xFF)
	  (put-bytevector out (integer->bytevector len 3))
	  (put-u8 out len)))
    (apply generic-tlv-writer tlv write-tag write-length opts))

  (define (get-emv-length len)
    (define ashr bitwise-arithmetic-shift-right)
    (do ((r len (ashr r 8)) (i 0 (+ i 1)))
	((zero? r) i)))
  (define (write-emv-tlv tlv . opts)
    (define (write-tag out tag) (put-bytevector out tag))
    (define (write-length out len)
      (define ashr bitwise-arithmetic-shift-right)
      (cond ((< len 0))
	    ((<= len #x7F) (put-u8 out len))
	    (else
	     ;; NOTE: #xFFFF: length = 4
	     ;; EMV tlv accepts maximum 4 bytes length but we don't check it
	     (let1 length-bits (get-emv-length len)
	       (put-u8 out (+ #x80 length-bits))
	       (do ((i length-bits (- i 1)))
		   ((= i 0))
		 (put-u8 out (bitwise-and #xFF (ashr len (* (- i 1) 8)))))))))
    (apply generic-tlv-writer tlv write-tag write-length opts))

  (define (read-tlv in :optional (parser (make-emv-tlv-parser)))
    (do ((tlv (parser in) (parser in)) (r '() (cons tlv r)))
	((or (not tlv) (eof-object? tlv)) (reverse! r))))
  
  ;; returns list of TLV objects
  ;; the given list must be alist of TLV
  ;; ((tag . value))
  ;; tag must be integer
  ;; value can be list or bytevector
  (define-generic ->tlv)
  (define-method ->tlv ((o <list>))
    (define (convert e)
      (define (->valid-data data)
	(cond ((bytevector? data) (values #f data))
	      ((integer? data) (values #f (integer->bytevector data)))
	      ((and (list? data) (for-all integer? data))
	       (values #f (u8-list->bytevector data)))
	      (else
	       (values #t (->tlv data)))))
      (unless (pair? e)
	(assertion-violation '->tlv "invalid structured object is given" o))
      (unless (or (integer? (car e)) (bytevector? (car e)))
	(assertion-violation '->tlv "tag must be integer or bytevector" o))
      (let-values (((constructed? data) (->valid-data (cdr e))))
	(let1 tag (if (integer? (car e))
		      (integer->bytevector (car e))
		      (car e))
	  (tlv-builder -1 tag data constructed?))))
    (fold-right (^(e seed) (cons (convert e) seed)) '() o))


  ;; LV utilities
  ;; default length reader
  ;;   LL dddddd
  ;;   LL = one byte length
  ;;   dd = LL bytes data
  (define (make-lv-parser :optional (length-reader get-u8))
    (lambda (in)
      (let ((len (length-reader in)))
	(if (eof-object? len)
	    #f
	    (let ((r (get-bytevector-n in len)))
	      (cond ((eof-object? r) #f)
		    ((= (bytevector-length r) len) r)
		    (else (error 'lv-reader "not a valid LV structure"))))))))
  (define (read-lv in :optional (parser (make-lv-parser)))
    ;; lazy
    (read-tlv in parser))

)
