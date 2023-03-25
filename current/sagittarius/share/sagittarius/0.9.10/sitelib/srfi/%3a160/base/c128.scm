;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; srfi/%3a160/base/c128.scm - Homogeneous numeric vector datatypes (base)
;;;  
;;;   Copyright (c) 2020  Takashi Kato  <ktakashi@ymail.com>
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
(library (srfi :160 base c128)
    (export make-c128vector c128vector c128vector? <c128vector>

	    c128vector-length c128vector-ref c128vector-set!
	    c128vector->list list->c128vector)
    (import (rnrs)
	    (srfi :4 numeric-vectors))

(define-tagged-vector "c128" 4
  make-f64vector f64vector-length equal?
  f64vector-ref-f64-as-complex
  f64vector-set-complex-as-f64!)

(define (f64vector-ref-f64-as-complex vec index)
  (let ((real (f64vector-ref vec index))
	(imag (f64vector-ref vec (+ index 1))))
    (if (zero? imag)
	real
	(make-rectangular real imag))))

(define (f64vector-set-complex-as-f64! vec index value)
  (let ((real (real-part value))
	(imag (imag-part value)))
    (f64vector-set! vec index real)
    (f64vector-set! vec (+ index 1) imag)))

)
