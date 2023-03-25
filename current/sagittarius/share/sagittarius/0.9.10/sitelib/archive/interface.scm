;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; archive/interface.scm - Generic archive interfaces.
;;;  
;;;   Copyright (c) 2010-2013  Takashi Kato  <ktakashi@ymail.com>
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

(library (archive interface)
    (export <archive-input>
	    <archive-output>
	    <archive-entry>

	    ;; for input
	    make-archive-input
	    archive-input?
	    next-entry!
	    extract-entry
	    set-file-attribute!
	    
	    ;; for output
	    make-archive-output
	    archive-output?
	    create-entry
	    append-entry!

	    archive-entry?
	    archive-entry-name
	    archive-entry-type
	    ;; common
	    finish!
	    )
    (import (rnrs)
	    (clos user)
	    (sagittarius))

  (define-class <archive-input> ()
    ((source :init-keyword :source)))
  (define (archive-input? o) (is-a? o <archive-input>))

  (define-class <archive-output> ()
    ((sink   :init-keyword :sink)))
  (define (archive-output? o) (is-a? o <archive-output>))

  (define-class <archive-entry> ()
    ((name   :init-keyword :name :reader archive-entry-name)
     ;; 'file or 'directory
     (type   :init-keyword :type :reader archive-entry-type
	     ;; output entry may not have to know
	     :init-value #f)))
  (define (archive-entry? o) (is-a? o <archive-entry>))

  (define-method write-object ((e <archive-entry>) out)
    (format out "#<archive-entry ~a ~a>" (archive-entry-name e)
	    (archive-entry-type e)))

  ;; constructors
  (define-generic make-archive-input)
  (define-generic make-archive-output)

  (define-generic next-entry!)
  (define-generic extract-entry)
  (define-generic set-file-attribute!)
  (define-generic create-entry)
  (define-generic append-entry!)
  (define-generic finish!)

  ;; for convenience
  (define-method create-entry ((out <archive-output>) file)
    (create-entry out file file))

  ;; default doesn't do anything
  (define-method finish! ((in <archive-input>)) #t)

  (define-method set-file-attribute! ((e <archive-entry>) file) #f)
  
)
