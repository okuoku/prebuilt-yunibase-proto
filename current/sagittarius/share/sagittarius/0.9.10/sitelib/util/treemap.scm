;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; treemap.scm - treemap utilities
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

(library (util treemap)
    (export make-rb-treemap
	    make-rb-treemap/comparator
	    treemap?
	    treemap-ref
	    treemap-set!
	    treemap-adjoin!
	    treemap-replace!
	    treemap-delete!
	    treemap-update!
	    treemap-clear!
	    treemap-copy
	    treemap-contains?
	    treemap-size
	    treemap-entries treemap-entries-list
	    treemap-keys    treemap-keys-list
	    treemap-values  treemap-values-list
	    treemap-higher-entry treemap-lower-entry
	    treemap-first-entry treemap-last-entry
	    treemap-pop-first-entry! treemap-pop-last-entry!
	    
	    treemap-for-each treemap-map treemap-fold
	    treemap-for-each-reverse treemap-map-reverse treemap-fold-reverse
	    treemap-find/index treemap-reverse-find/index
	    treemap-find treemap-reverse-find
	    treemap-seek
	    treemap->alist alist->treemap
	    )
    (import (rnrs)
	    (only (core base) wrong-type-argument-message)
	    (clos user)
	    (sagittarius)
	    (sagittarius treemap)
	    (sagittarius object))
;; should we move this to lib_treemap.stub?
(define %unique (list #t))
(define (treemap-adjoin! tree k v)
  (treemap-update! tree k (lambda (old) (if (eq? old %unique) v old)) %unique))
;; TODO not efficient
(define (treemap-replace! tree k v)
  (when (treemap-contains? tree k) (treemap-set! tree k v)))

;; from Gauche
(define (treemap-seek tm pred succ fail)
  (let ((itr (treemap-iterator tm)) (eof (cons #t #t)))
    (let loop ()
      (let-values (((k v) (itr eof)))
	(cond ((eq? k eof) (fail))
	      ((pred k v) => (lambda (r) (succ r k v)))
	      (else (loop)))))))
;; end

(define (treemap-pop-first-entry! tm)
  (let ((itr (treemap-iterator tm)) (eof (cons #t #t)))
    (let-values (((k v) (itr eof)))
      (and (not (eq? k eof))
	   (treemap-delete! tm k)
	   (cons k v)))))
(define (treemap-pop-last-entry! tm :optional (fallback #f))
  (let ((itr (treemap-reverse-iterator tm)) (eof (cons #t #t)))
    (let-values (((k v) (itr eof)))
      (and (not (eq? k eof))
	   (treemap-delete! tm k)
	   (cons k v)))))

;; for generic ref
(define-method ref ((tm <tree-map>) key)
  (treemap-ref tm key #f))
(define-method ref ((tm <tree-map>) key fallback)
  (treemap-ref tm key fallback))
(define-method (setter ref) ((tm <tree-map>) key value)
  (treemap-set! tm key value))

)
