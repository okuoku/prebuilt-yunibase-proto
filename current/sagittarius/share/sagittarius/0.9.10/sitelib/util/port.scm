;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; port.scm - port utility
;;;  
;;;   Copyright (c) 2010-2012  Takashi Kato  <ktakashi@ymail.com>
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

;; The API's names are from Gauche
(library (util port)
    (export port->list
	    port->string
	    port->sexp-list
	    port->string-list
	    port-fold
	    port-fold-right
	    port-for-each
	    port-map
	    copy-binary-port
	    ;; lock port
	    lock-port-resource!
	    unlock-port-resource!
	    call-with-resource-locked-port

	    ;; port data
	    add-port-data!
	    remove-port-data!
	    get-port-data
	    )
    (import (rnrs)
	    (srfi :1)
	    (srfi :38)
	    (sagittarius))
  (define (port->list reader port)
    (let loop ((s (reader port))
	       (r '()))
      (if (eof-object? s)
	  (reverse! r)
	  (loop (reader port) (cons s r)))))

  (define (port->string port)
    (car (port->list get-string-all port)))

  (define (port->sexp-list port)
    (port->list read/ss port))

  (define (port->string-list port)
    (port->list get-line port))

  (define (port-fold fn knil reader)
    (let loop ((item (reader))
	       (r    knil))
      (if (eof-object? item)
	  r
	  (loop (reader) (fn item r)))))

  (define (port-fold-right fn knil reader)
    (let loop ((item (reader)))
      (if (eof-object? item)
	  knil
	  (fn item (loop (reader))))))

  (define (port-for-each fn reader)
    (let loop ((item (reader)))
      (unless (eof-object? item)
	(fn item)
	(loop (reader)))))

  (define (port-map fn reader)
    (let loop ((item (reader)) (r '()))
      (if (eof-object? item)
	  (reverse! r)
	  (let ((x (fn item)))
	    (loop (reader) (cons x r))))))

  ;; FIXME we are using reckless optional argument to read however
  ;;       this should go away since it doesn't work for all port
  ;;       types.
  (define (copy-binary-port dst src :key (size #f))
    (define bufsize 4096)
    (define valid-size? (and size (integer? size) (positive? size)))
    ;; if the specified size is smaller than buffer size then just
    ;; read and copy it
    (if (and valid-size? (< size bufsize))
	(let ((bv (get-bytevector-n src size #t)))
	  (put-bytevector dst bv)
	  ;; most of the case, it's the same as size but sometimes not.
	  (bytevector-length bv))
	(let ((buf (make-bytevector bufsize))
	      (read-size (if valid-size? size #f)))
	  (let loop ((r 0))
	    (define (rsize) (if (and read-size (< (- read-size r) bufsize))
				(- read-size r)
				bufsize))
	    (let ((n (get-bytevector-n! src buf 0 (rsize) #t)))
	      (cond ((eof-object? n) r)
		    ((< n bufsize)
		     (put-bytevector dst buf 0 n)
		     (+ n r))
		    (else
		     (put-bytevector dst buf 0 n)
		     (loop (+ r n)))))))))

  ;; lock file port
  (define (call-with-resource-locked-port port proc . opt)
    ;; the lock must be unlocked after proc no matter what
    (apply lock-port-resource! port opt)
    (dynamic-wind values
	(lambda () (proc port))
	(lambda () (unlock-port-resource! port)))
    )

  ;; make all port data alist
  (define (add-port-data! port key value :key (compare eq?))
    ;; default data is '()
    (let ((data (%port-data port)))
      (cond ((assoc key data compare) =>
	     (lambda (slot) (set! (cdr slot) value)))
	    (else
	     (set! (%port-data port) (acons key value data))))))

  (define (remove-port-data! port key :key (compare eq?))
    (let ((data (%port-data port)))
      (set! (%port-data port) (remp (lambda (s) (compare key (car s))) data))))

  (define (get-port-data port key :key (compare eq?))
    (let ((data (%port-data port)))
      (cond ((assoc key data compare) => cdr)
	    (else #f))))

)
