;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; archive.scm - Generic archive access
;;;  
;;;   Copyright (c) 2010-2018  Takashi Kato  <ktakashi@ymail.com>
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


(library (archive)
    (export <archive-input>
	    <archive-output>
	    <archive-entry>

	    ;; for input
	    make-input-archive
	    ;; backward compatibility
	    (rename (make-input-archive make-archive-input))
	    archive-input?
	    next-entry!
	    extract-entry
	    set-file-attribute!
	    
	    ;; for output
	    make-output-archive
	    ;; backward compatibility
	    (rename (make-output-archive make-archive-input))
	    archive-output?
	    create-entry
	    append-entry!

	    ;; common
	    finish!

	    archive-entry?
	    archive-entry-name
	    archive-entry-type

	    ;; utilities
	    call-with-input-archive
	    call-with-output-archive
	    call-with-input-archive-port
	    call-with-output-archive-port
	    call-with-input-archive-file
	    call-with-output-archive-file
	    ;; backward compatibility
	    (rename (call-with-input-archive-port call-with-archive-input)
		    (call-with-output-archive-port call-with-archive-output))

	    do-entry
	    extract-all-entries
	    )
    (import (rnrs)
	    (rnrs eval)
	    (archive interface)
	    (sagittarius)
	    (sagittarius control)
	    (util file)
	    (srfi :26 cut))

  ;; utilities
  (define (call-with-input-archive-file type file proc)
    (call-with-input-file file
      (cut call-with-input-archive-port type <> proc)
      :transcoder #f))

  (define (call-with-output-archive-file type file proc)
    (call-with-output-file file
      (cut call-with-output-archive-port type <> proc)
      :transcoder #f))

  (define (call-with-input-archive-port type source proc)
    (call-with-input-archive (make-input-archive type source) proc))

  (define (call-with-output-archive-port type sink proc)
    (call-with-output-archive (make-output-archive type sink) proc))

  (define (call-with-input-archive input proc)
    (let-values ((result (proc input)))
      (finish! input)
      (apply values result)))

  (define (call-with-output-archive output proc)
    (let-values ((result (proc output)))
      (finish! output)
      (apply values result)))

  ;; generic constructors
  (define (make-input-archive type source)
    (eval `(make-archive-input ',type ,source)
	  (environment '(rnrs) '(archive interface) `(archive ,type))))

  (define (make-output-archive type source)
    (eval `(make-archive-output ',type ,source)
	  (environment '(rnrs) '(archive interface) `(archive ,type))))

  (define-syntax do-entry
    (syntax-rules ()
      ((_ (e in) body ...)
       (do-entry (e in #t) body ...))
      ((_ (e in r) body ...)
       (do ((e (next-entry! in) (next-entry! in)))
	   ((not e) r)
	 body ...))))

  (define (extract-all-entries in :key (destinator archive-entry-name)
			       (overwrite #f))
    (do-entry (e in)
      (let ((dest (destinator e)))
	(cond ((not dest)) ;; ignore if destinator returned #f
	      ((eq? (archive-entry-type e) 'directory)
	       (create-directory* dest))
	      (else
	       (when (and overwrite (file-exists? dest)) (delete-file dest))
	       (call-with-output-file dest
		 (lambda (out) (extract-entry e out))
		 :transcoder #f)))
	(when dest (set-file-attribute! e dest)))))
)
