;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; immutable.scm - metaclass to support immutable object
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
(library (sagittarius mop immutable)
    (export <immutable> <immutable-meta>)
    (import (rnrs)
	    (clos core)
	    (clos user))

(define-class <immutable-meta> (<class>) ())
(define-class <immutable> () () :metaclass <immutable-meta>)
;; copy&paste of <record-meta>
;; This is needed due to the record inspection, <record-meta>
;; is treated during record inspection and this would ignore object-equal?
;; Might be better to revisit the implementation to adjust
;; as this means, record can't be specialised with object-equal? method.
(define-method compute-getter-and-setter ((c <immutable-meta>) slot)
    (let ((mutability (slot-definition-option slot :mutable #f))
	  (accessors (call-next-method)))
      (if mutability
	  accessors
	  (list (car accessors)
		(lambda (o v)
		  (error 'slot-setter "field is immutable"
			 (slot-definition-name slot) o))
		(caddr accessors)))))
)
