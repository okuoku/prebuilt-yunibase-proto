;;; -*- mode: scheme; coding: utf-8; -*-
;;;
;;; math/luhn.scm - Luhn checksum.
;;;  
;;;   Copyright (c) 2010-2015  Takashi Kato  <ktakashi@ymail.com>
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

;; Luhn algorithm
;;   https://en.wikipedia.org/wiki/Luhn_algorithm
(library (math luhn)
    (export luhn-valid?
	    luhn-calculate
	    *luhn-converter*)
    (import (rnrs)
	    (math modulus-check-digit)
	    (srfi :14 char-sets)
	    (srfi :39 parameters))

  (define identifier-char-set
    (string->char-set "0123456789ABCDEFGHIJKLMNOPQRSTUVYWXZ_"))
  (define *luhn-converter* 
    (make-parameter 
     ;; we accept identifiers as well like the following link
     ;; https://wiki.openmrs.org/display/docs/Check+Digit+Algorithm
     (lambda (s/i) 
       (map (lambda (c) 
	      (unless (char-set-contains? identifier-char-set c)
		(assertion-violation 'luhn-checksum "invalid character" c))
	      (- (char->integer c) 48))
	    (string->list 
	     (if (string? s/i) 
		 (string-upcase s/i)
		 (number->string s/i)))))))

  (define (luhn-weight n l r)
    (if (even? r) (- (* n 2) (* (div n 5) 9)) n))

  (define (luhn-conv code) ((*luhn-converter*) code))

  (define-check-digit luhn-valid? luhn-calculate 10 luhn-weight luhn-conv)

  )
