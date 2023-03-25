;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; uri.scm - parse and construct URIs 
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

;; Main reference:
;; RFC3986 URI Generic Syntax <https://tools.ietf.org/html/rfc3986>
#!nounbound
#!read-macro=sagittarius/regex
(library (rfc uri)
    (export uri-parse
	    uri-scheme&specific
	    uri-decompose-hierarchical
	    uri-decompose-authority

	    uri-decode
	    uri-decode-string
	    uri-encode
	    uri-encode-string

	    uri-compose
	    uri-merge
	    *rfc3986-unreserved-char-set*
	    *rfc2396-unreserved-char-set*)
    (import (rnrs)
	    (rfc uri regex)
	    (srfi :13 strings)
	    (srfi :14 char-sets)
	    (sagittarius)
	    (sagittarius io)
	    (encoding decoder)
	    (sagittarius control)
	    (sagittarius regex))

  ;; compose
  (define (uri-compose :key (scheme #f) (userinfo #f) (host #f) (port #f)
			    (authority #f) (path  #f) (path* #f) (query #f)
			    (fragment #f) (specific #f))
      (with-output-to-string
	(lambda ()
	  (when scheme (display scheme) (display ":"))
	  (if specific
	      (display specific)
	      (begin
		(display "//")
		(if authority
		    (begin (display authority))
		    (begin
		      (when userinfo (display userinfo) (display "@"))
		      (when host     (display host))
		      (when port     (display ":") (display port))))
		(if path*
		    (begin
		      (unless (string-prefix? "/" path*) (display "/"))
		      (display path*))
		    (begin
		      (if path
			  (begin (unless (string-prefix? "/" path) (display "/"))
				 (display path))
			  (display "/"))
		      (when query (display "?") (display query))
		      (when fragment (display "#") (display fragment))))
		))
	  )))

  ;; RFC 3986 section 5
  (define (uri-merge base-uri rel-uri . more-rels)
    (define (rec base-uri rel-uri more initial?)
      (if (null? more)
	  (uri-merge-1 base-uri rel-uri initial?)
	  (rec (uri-merge-1 base-uri rel-uri initial?)
	       (car more) (cdr more) #f)))
    (rec base-uri rel-uri more-rels #t))

  (define (uri-merge-1 base-uri rel-uri pre-norm?)
    (define (split uri)
      (let*-values (((scheme specific) (uri-scheme&specific uri))
		    ((authority path query fragment)
		     (uri-decompose-hierarchical specific)))
	(values scheme authority path query fragment)))
    ;; 5.2.3
    (define (merge base-path rel-path base-auth)
      (cond ((and base-auth (string-null? base-path))
	     (string-append "/" rel-path))
	    ((matches #/(.*)\/[^\/]*/ base-path)
	     => (^m (string-append (m 1) "/" rel-path)))
	    (else rel-path)))
    ;; 5.2.4
    (define (remove-dot-segment path)
      (let loop ((input path) (output '()))
	(cond ((not input) (string-concatenate (reverse! output)))
	      ;; 2.A
	      ((matches #/\.{1,2}\/(.*)/ input) => (^m (loop (m 1) output)))
	      ;; 2.B
	      ((matches #/(?:\/\.\/(.*)|\/\.)/ input) ;; for emacs |))
	       => (^m (loop (string-append "/" (or (m 1) "")) output)))
	      ;; 2.C
	      ((matches #/^(?:\/\.\.\/(.*)|\/\.\.)/ input) ;; for emacs |))
	       => (^m (loop (string-append "/" (or (m 1) ""))
			    (if (null? output) output (cdr output)))))
	      ;; 2.D
	      ((matches #/\.{1,2}/ input) (loop #f output))
	      ;; 2.E
	      ((matches #/(\/?[^\/]+)(\/.*)/ input) 
	       => (^m (loop (m 2) (cons (m 1) output))))
	      (else (loop #f (cons input output))))))
    ;; 5.3
    (define (recompose scheme auth path query frag)
      (call-with-string-output-port
	(lambda (out)
	  (when scheme (display scheme out) (display ":" out))
	  (when auth (display "//" out) (display auth out))
	  (display path out)
	  (when query (display "?" out) (display query out))
	  (when frag (display "#" out) (display frag out)))))

    (let-values (((b.scheme b.auth b.path b.query b.frag) (split base-uri))
		 ((r.scheme r.auth r.path r.query r.frag) (split rel-uri)))
      (when pre-norm? (set! b.path (remove-dot-segment b.path))) ; 5.2.1
      ;; 5.2.2
      (recompose (or r.scheme b.scheme)
		 (if r.scheme r.auth (or r.auth b.auth))
		 (cond ((or r.scheme r.auth) (remove-dot-segment r.path))
		       ((not r.path) b.path)
		       ((string-prefix? "/" r.path) (remove-dot-segment r.path))
		       (else (remove-dot-segment (merge b.path r.path b.auth))))
		 (if (or r.scheme r.auth r.path r.query) r.query b.query)
		 r.frag)))

  ;; encoding & decoding
  ;; This is for internal.
  ;; in and out must be binary-port
  (define (uri-decode in out :key (cgi-decode #f))
    (define (hex-char? n)
      (cond ((<= #x30 n #x39) ;; #\0 - #\9
	     (- n #x30))
	    ((<= #x61 n #x66) ;; #\a - #\f
	     (- n #x57))
	    ((<= #x41 n #x46) ;; #\A - #\F
	     (- n #x37))
	    (else #f)))
    (check-arg binary-port? in 'uri-decode)
    (let loop ((c (get-u8 in)))
      (cond ((eof-object? c))
	    ((= c #x25) ;; %
	     (let1 c1 (get-u8 in)
	       (cond ((eof-object? c1) (put-u8 out c))
		     ((hex-char? c1)
		      => (lambda (i1)
			   (let1 c2 (get-u8 in)
			     (cond ((eof-object? c2)
				    (put-u8 out c) (put-u8 out c1))
				   ((hex-char? c2)
				    => (lambda (i2)
					 (put-u8 out (+ (* i1 16) i2))
					 (loop (get-u8 in))))
				   (else (put-u8 out c) (put-u8 out c1)
					 (loop c2))))))
		     (else (put-u8 out c) (loop c1)))))
	    ((= c #x2b) ;; +
	     (if cgi-decode
		 (put-u8 out #x20) ;; #\space
		 (put-u8 out #x2b))
	     (loop (get-u8 in)))
	    (else (put-u8 out c) (loop (get-u8 in))))))

  (define (uri-decode-string string 
			     :key
			     (encoding 'utf-8)
			     (cgi-decode #f))
    ;; decoder is mere codec.
    (let ((decoder (lookup-decoder encoding)))
      (if decoder
	  (let ((bv (string->utf8 string)))
	    (bytevector->string
	     (call-with-bytevector-output-port
	      (lambda (out)
		(uri-decode (open-bytevector-input-port bv) out
			    :cgi-decode cgi-decode)))
	     (make-transcoder decoder)))
	  string)))

  ;; 2396 -_.!~*'() + [0-9a-zA-Z]
  (define letter+digit 
    (char-set-intersection char-set:ascii char-set:letter+digit))
  (define *rfc2396-unreserved-char-set*
    (char-set-union (string->char-set "-_.!~*'()") letter+digit))
  ;; 3986 -_.~ + [0-9a-zA-Z]
  (define *rfc3986-unreserved-char-set*
    (char-set-union (string->char-set "-_.~") letter+digit))

  (define (uri-encode in out
		      :key ((:noescape echars) *rfc3986-unreserved-char-set*)
			   (upper-case #t)
			   (cgi-encode #f))
    (define (hex->char-integer h)
      (cond ((<= 0 h 9) (+ h #x30))
	    ((<= #xa h #xf) (+ h (- (if upper-case #x41 #x61) 10)))
	    (else (assertion-violation 'hex->char-integer
				       "invalid hex number" h))))
    (let loop ((b (get-u8 in)))
      (unless (eof-object? b)
	(cond ((and (< b #x80) (char-set-contains? echars (integer->char b)))
	       (put-u8 out b))
	      ;; W3C.REC-html401-19991224, i think
	      ((and cgi-encode (= b #x20))
	       (put-u8 out #x2b)) ;; +
	      (else
	       (put-u8 out #x25) ;; %
	       (let ((hi (fxand (fxarithmetic-shift-right b 4) #xf))
		     (lo (fxand b #xf)))
		 (put-u8 out (hex->char-integer hi))
		 (put-u8 out (hex->char-integer lo)))))
	(loop (get-u8 in)))))

  (define (uri-encode-string string :key (encoding 'utf-8)
			     :allow-other-keys args)
    (let ((decoder (lookup-decoder encoding)))
      (if decoder
	  (let ((bv (string->bytevector string (make-transcoder decoder))))
	    (utf8->string
	     (call-with-bytevector-output-port
	      (lambda (out)
		(apply uri-encode (open-bytevector-input-port bv) out args)))))
	  string)))
)


;; Local Variables:
;; coding: utf-8-unix
;; End:
