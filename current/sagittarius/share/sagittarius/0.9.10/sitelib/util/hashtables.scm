;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; hashtables.scm - hashtable utilities
;;;  
;;;   Copyright (c) 2010-2011  Takashi Kato  <ktakashi@ymail.com>
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

(library (util hashtables)
    (export hashtable-for-each
	    hashtable-map
	    hashtable-fold
	    hashtable->alist
	    make-hashtable/comparator
	    hashtable-adjoin!
	    hashtable-replace!
	    hashtable-seek
	    
	    alist->hashtable
	    hashtable-keys-list
	    hashtable-values-list)
    (import (rnrs)
	    (sagittarius control)
	    (only (core base) hashtable-for-each hashtable-map
		  hashtable->alist hashtable-fold)
	    (rename (only (sagittarius)
			  hashtable-keys-list hashtable-values-list
			  make-hashtable/comparator
			  %hashtable-iter)
		    (%hashtable-iter hashtable-iterator)))

(define (create-hashtable compare hasher)
  (cond ((eq? compare eq?) (make-eq-hashtable))
	((eq? compare eqv?) (make-eqv-hashtable))
	(else (make-hashtable hasher compare))))

(define (alist->hashtable alist :key (compare eq?) (hasher symbol-hash))
  (let ((ht (create-hashtable compare hasher)))
    (for-each (lambda (k/v) (hashtable-set! ht (car k/v) (cdr k/v))) alist)
    ht))

(define %unique (list #t))
(define (hashtable-adjoin! ht k v)
  (hashtable-update! ht k (lambda (old) (if (eq? old %unique) v old)) %unique))
(define (hashtable-replace! ht k v)
  (when (hashtable-contains? ht k) (hashtable-set! ht k v)))

(define (hashtable-seek ht pred succ fail)
  (let ((itr (hashtable-iterator ht)) (eof (cons #t #t)))
    (let loop ()
      (let-values (((k v) (itr eof)))
	(cond ((eq? k eof) (fail))
	      ((pred k v) => (lambda (r) (succ r k v)))
	      (else (loop)))))))  

)
