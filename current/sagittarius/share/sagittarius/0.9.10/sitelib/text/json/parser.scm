;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; text/json/parser.scm - JSON parser
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

;; peg parser of JSON
;; reference:
;; https://tools.ietf.org/html/rfc8259
#!nounbound
(library (text json parser)
    (export json:parser
	    json-error?	      ;; for backward compatibility
	    json-parse-error?
	    *json:escape-required*
	    *json:raise-error?*

	    ;; for convenience ;)
	    *json:array-handler*
	    *json:object-handler*
	    *json:null-handler*
	    *json:boolean-handler*
	    *json:number-handler*
	    *json:string-handler*
	    
	    json:null
	    json:number
	    json:string
	    json:true
	    json:false
	    json:object
	    json:array
	    json:value)
    (import (rnrs)
	    (peg)
	    (peg chars)
	    (sagittarius generators)
	    (srfi :14 char-sets)
	    (srfi :39 parameters)
	    (srfi :127 lseqs)
	    (text json errors)
	    (text json parser-parameters))
#|
;; Ironically this is slower than PEG...
;; convert lazy sequence to input port.
(define (lseq->char-input-port lseq)
  (define current lseq)
  (define (read! s start count)
    (let loop ((i 0) (seq current))
      (cond ((= i count) (set! current seq) i)
	    ((null? seq) (set! current '()) i)
	    (else
	     (string-set! s (+ i start) (lseq-car seq))
	     (loop (+ i 1) (lseq-cdr seq))))))
  ;; we do sloppy way
  (define (get-position)
    (let loop ((i 0) (seq lseq))
      (cond ((eq? seq current) i)
	    ((null? seq) i)
	    (else (loop (+ i 1) (lseq-cdr seq))))))
  (define (set-position! n)
    (do ((i 0 (+ i 1)) (seq lseq (lseq-cdr seq)))
	((or (= i n) (null? seq)) (set! current seq))))
  (values (make-custom-textual-input-port "lseq-input-port" read!
					  get-position set-position! #f)
	  (lambda () current)))
(define (json:parser l)
  (let-values (((in nl) (lseq->char-input-port l)))
    (let ((r (json-read in)))
      (return-result r (nl)))))
|#
(define-condition-type &json-parse &json
  make-json-parse-error json-parse-error?)
;; If the JSON is embedded to somewhere (i.e. JMESPath) it may require
;; extra escape rule.
(define *json:escape-required* (make-parameter #f))
(define *json:raise-error?* (make-parameter #f))

(define (json-parse-error message . irr)
  (define c (condition (make-json-parse-error)
		       (make-who-condition 'parse-json)
		       (make-message-condition message)))
  (if (null? irr)
      (raise c)
      (raise (condition c (make-irritants-condition irr)))))

(define ($error message)
  (lambda (in)
    (if (*json:raise-error?*)
	(if (null? in)
	    (json-parse-error "Unexpected EOF")
	    (json-parse-error message (lseq-car in)))
	(return-expect message in))))

(define $cs $char-set-contains?)
;; read only one char 
(define $getc $any)
(define $peekc ($peek $getc))

(define ws ($many ($cs (char-set #\space #\tab #\newline #\return))))

(define begin-array ($seq ws ($eqv? #\[) ws))
(define begin-object ($seq ws ($eqv? #\{) ws))
(define end-array ($seq ws ($eqv? #\]) ws))
(define end-object ($seq ws ($eqv? #\}) ws))
(define name-separactor ($seq ws ($eqv? #\:) ws))
(define value-separactor ($seq ws ($eqv? #\,) ws))

(define (token s)
  ($or ($token s)
       ($error (string-append "Unexpected character for token " s))))

(define json:true  ($seq (token "true") ($return ((*json:boolean-handler*) #t))))
(define json:false ($seq (token "false") ($return ((*json:boolean-handler*) #f))))
(define json:null  ($seq (token "null") ($return ((*json:null-handler*)))))

(define num-set
  ($cs (char-set #\- #\+ #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\. #\e #\E)))
;; This doesn't make happy...
#;(define num-set
  ($satisfy
   (lambda (c)
     (memv c '(#\- #\+ #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\. #\e #\E)))))
(define json:number
  ($do (c* ($many num-set 1))
       ($return (let* ((s (list->string c*))
		       (v (string->number s)))
		  (or ((*json:number-handler*) v)
		      (json-parse-error "Invalid JSON number" s))))))

;;; This is more acculate and follows specification but slow
;;; (5% slower than above)
;; (define digit ($cs (string->char-set "0123456789")))
;; (define json:int
;;   ($or ($do (($eqv? #\0)) ($return "0"))
;;        ($do (c ($cs (string->char-set "123456789")))
;; 	    (c* ($many digit))
;; 	    ($return (apply string c c*)))))
;; (define json:frac
;;   ($do (($eqv? #\.))
;;        (c* ($many digit 1))
;;        ($return (list->string c*))))
;; (define json:exp
;;   ($do (e ($or ($eqv? #\e) ($eqv? #\E)))
;;        (s ($or ($eqv? #\-) ($eqv? #\+)))
;;        (c* ($many digit 1))
;;        ($return (apply string e s c*))))
;; 
;; (define json:number
;;   ($do (sig ($optional ($do (($eqv? #\-)) ($return "-")) "+"))
;;        (int json:int)
;;        (frac ($optional json:frac ""))
;;        (exp  ($optional json:exp ""))
;;        ($return (let* ((s (string-append sig int frac exp))
;; 		       (v (string->number s)))
;; 		  (or v
;; 		      (json-parse-error "Invalid JSON number" s))))))

(define json:unescaped
  ($cs (char-set-union
	(ucs-range->char-set #x20 (+ #x21 1))
	(ucs-range->char-set #x23 (+ #x5B 1))
	(ucs-range->char-set #x5D (+ #x10FFFF 1)))))

(define json:code-point
  ($do (c* ($repeat ($cs char-set:hex-digit) 4))
       ($return (string->number (list->string c*) 16))))
(define json:escaped
  ($seq ($eqv? #\\)
	;; The same as (text json parse)
	($or ;;($do (($eqv? #\")) ($return #\"))
	     ;;($do (($eqv? #\\)) ($return #\\))
	     ;;($do (($eqv? #\/)) ($return #\/))
	     ($do (($eqv? #\b)) ($return #\backspace))
	     ($do (($eqv? #\f)) ($return #\page))
	     ($do (($eqv? #\n)) ($return #\linefeed))
	     ($do (($eqv? #\r)) ($return #\return))
	     ($do (($eqv? #\t)) ($return #\tab))
	     ($do (($eqv? #\u))
		  (cp json:code-point)
		  ($if (<= #xd800 cp #xdbff)
		       ($do (($eqv? #\\)) (($eqv? #\u))
			    (cp2 json:code-point)
			    ($if (<= #xdc00 cp2 #xdfff)
				 ($return (integer->char
					   (+ #x10000
					      (* (- cp #xd800) #x400)
					      (- cp2 #xdc00))))
				 ($error "Invalid unicode code point")))
		       ($lazy ($return (integer->char cp)))))
	     $getc)))

(define (not-in-escape in) (($not (*json:escape-required*)) in))
(define json:char
  ;; This didn't make me happy at all...
  #;($do (c $peekc)
       ($cond ((eqv? c #\\) json:escaped)
	      ((eqv? c #\")
	       (lambda (l) (return-unexpect "end of JSON string" l)))
	      (else $getc)))
  ($or ($if (*json:escape-required*)
	    ($seq not-in-escape json:unescaped)
	    json:unescaped)
       json:escaped))
(define json:string
  ($do (($eqv? #\"))
       (c* ($many json:char))
       (($eqv? #\"))
       ($return ((*json:string-handler*) (list->string c*)))))

(define array-values
  ($optional ($do (($not ($eqv? #\])))
		  (v1 json:value)
		  (v* ($many ($seq value-separactor json:value)))
		  ($return (cons v1 v*)))
	     '()))
(define json:array
  ($do begin-array
       (v* array-values)
       (($or end-array ($error "Invalid JSON array")))
       ($return ((*json:array-handler*) v*))))

(define json:member
  ($do (k json:string) name-separactor (v json:value) ($return (cons k v))))
(define object-values
  ($optional ($do (($not ($eqv? #\})))
		  (v1 json:member)
		  (v* ($many ($seq value-separactor json:member)))
		  ($return (cons v1 v*)))
	     '()))
(define json:object
  ($do begin-object
       (v* object-values)
       (($or end-object ($error "Invalid JSON object")))
       ($return ((*json:object-handler*) v*))))

(define json:value
  ($lazy
   ;; This is better performance than listing with $or
   ($do (c $peekc)
	($cond ((eqv? #\{ c) json:object)
	       ((eqv? #\[ c) json:array)
	       ((eqv? #\" c) json:string)
	       ((memv c '(#\- #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
		json:number)
	       ((eqv? #\t c) json:true)
	       ((eqv? #\f c) json:false)
	       ((eqv? #\n c) json:null)
	       (else ($error "Invalid JSON character"))))))

(define json:parser
  ($parameterize ((*json:raise-error?* #t))
   ($do ws (v json:value) ws ($return v))))
)
