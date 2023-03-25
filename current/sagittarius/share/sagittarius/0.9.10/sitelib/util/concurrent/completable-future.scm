;;; -*- mode:scheme;coding:utf-8 -*-
;;;
;;; util/concurrent/completable-future.scm - Completable future
;;;  
;;;   Copyright (c) 2021  Takashi Kato  <ktakashi@ymail.com>
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

;; not sure if we should make separate library for this...
#!nounbound
(library (util concurrent completable-future)
    (export thunk->future future-map future-flatmap
	    future-guard

	    *completable-future:default-executor*)
    (import (rnrs)
	    (srfi :39 parameters)
	    (util concurrent future)
	    (util concurrent executor)
	    (scheme lazy))

(define *completable-future:default-executor*
  ;; Let's not create an executor during library load
  (make-parameter
   (delay (make-fork-join-executor))
   (lambda (v)
     (cond ((promise? v) v)
	   ((executor? v) (delay v))
	   (else (assertion-violation '*completable-future:default-executor*
				      "Promise or executor required" v))))))

(define-record-type completable-future
  (parent <executor-future>)
  (fields executor)
  (protocol (lambda (n)
	      (lambda (thunk executor)
		((n thunk) executor)))))

(define thunk->future
  (case-lambda
   ((thunk)
    (thunk->future thunk (force (*completable-future:default-executor*))))
   ((thunk executor)
    (let ((future (make-completable-future thunk executor)))
      (execute-future! executor future)
      future))))

(define (search-executor future future*)
  (cond ((completable-future? future)
	 (completable-future-executor future))
	((filter completable-future? future*) =>
	 (lambda (f*) (completable-future-executor (car f*))))
	(else (force (*completable-future:default-executor*)))))
(define (future-map proc future . future*)
  (thunk->future (if (null? future*)
		     (lambda opt (proc (apply future-get future opt)))
		     (lambda opt
		       (apply proc
			      (cons (apply future-get future opt)
				    (map (lambda (f) (apply future-get f opt))
					 future*)))))
		 (search-executor future future*)))

;; For now very naive implementation...
(define (future-flatmap proc future . future*)
  (thunk->future
   (if (null? future*)
       (lambda opt
	 (let ((f (proc (apply future-get future opt))))
	   (apply future-get f opt)))
       (lambda opt
	 (let ((f (apply proc
			 (cons (apply future-get future opt)
			       (map (lambda (f) (apply future-get f opt))
				    future*)))))
	   (apply future-get f opt))))
   (search-executor future future*)))

(define (future-guard proc future)
  (thunk->future
   (lambda opt
     (guard (e (else (proc e)))
       (apply future-get future opt)))
   (if (completable-future? future)
       (completable-future-executor future)
       (force (*completable-future:default-executor*)))))
)
