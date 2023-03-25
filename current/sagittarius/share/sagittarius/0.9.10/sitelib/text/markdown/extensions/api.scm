;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; text/markdown/extensions/api.scm - Extension API
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
(library (text markdown extensions api)
    (export markdown-extension-builder
	    markdown-extension?
	    markdown-extension-block-factories
	    markdown-extension-inline-content-factories
	    markdown-extension-delimiter-processors
	    markdown-extension-reference-processors
	    markdown-extension-post-processors
	    
	    combine-markdown-extensions

	    define-markdown-converter
	    markdown-converter:merge
	    )
    (import (rnrs)
	    (record accessor)
	    (record builder)
	    (srfi :1 lists)
	    (text markdown converter api))

(define-record-type markdown-extension
  (fields block-factories
	  inline-content-factories
	  delimiter-processors
	  reference-processors
	  post-processors))
(define (make-check-list who)
  (lambda (v)
    (unless (or (null? v) (pair? v))
      (assertion-violation who "Must be a list" v))
    v))
(define-syntax markdown-extension-builder
  (make-record-builder markdown-extension
   ((block-factories '() (make-check-list 'block-factories))
    (inline-content-factories '() (make-check-list 'inline-content-factories))
    (delimiter-processors '() (make-check-list 'delimiter-processors))
    (reference-processors '() (make-check-list 'reference-processors))
    (post-processors '() (make-check-list 'post-processors)))))

(define empty-extension (markdown-extension-builder))

(define *markdown-extension-accessors*
  (record-type-all-field-accessors (record-type-descriptor markdown-extension)))
(define (combine-markdown-extensions . extension*)
  (define (combine-fields e*)
    (map (lambda (accessor) (append-map accessor e*))
	 *markdown-extension-accessors*))
  (if (null? extension*)
      empty-extension
      (apply make-markdown-extension (combine-fields extension*))))

)
    
