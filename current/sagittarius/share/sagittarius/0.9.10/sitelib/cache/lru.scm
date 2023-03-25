;;; -*- mode:scheme; coding: utf-8; -*-
;;;
;;; cache/lru.scm - LRU cache
;;;
;;;   Copyright (c) 2015-2016  Takashi Kato  <ktakashi@ymail.com>
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

;; might be useful 
(library (cache lru)
    (export make-simple-lru-cache
	    <lru-cache>)
    (import (rnrs)
	    (clos user)
	    (cache apis)
	    (srfi :1)
	    (srfi :19)
	    (srfi :114 comparators)
	    (sagittarius vm)
	    (only (scheme base) vector-copy!))

  (define-class <lru-cache> (<cache>)
    ((queue :init-value '())
     equal?))

  (define-method initialize ((o <lru-cache>) initargs)
    (call-next-method)
    (slot-set! o 'equal? 
	       (hashtable-equivalence-function (slot-ref o 'storage))))

  (define (re-order o k)
    (let ((storage (slot-ref o 'storage))
	  (queue (slot-ref o 'queue))
	  (equal? (slot-ref o 'equal?)))
      ;; FIXME we should manage tail as well to make this O(1)
      (slot-set! o 'queue `(,@(remove (lambda (o) (equal? o k)) queue) ,k))))
  (define-method cache-access ((o <lru-cache>) on k v)
    ;; we need to re-order
    (re-order o k))

  (define-method cache-pop! ((o <lru-cache>))
    (let ((queue (slot-ref o 'queue))
	  (storage (slot-ref o 'storage)))
      (slot-set! o 'queue (cdr queue))
      (let ((v (hashtable-ref storage (car queue) #f)))
	(hashtable-delete! storage (car queue))
	v)))
  (define-method cache-evict! ((o <lru-cache>) k)
    (let ((queue (slot-ref o 'queue))
	  (equal? (slot-ref o 'equal?)))
      (remove! (lambda (o) (equal? o k)) queue)
      (call-next-method)))

  (define (make-simple-lru-cache size create 
				 :optional (comparator default-comparator))
    (define cache (make <lru-cache> :max-size size :comparator comparator))
    (define mark (list 'not-found))
    (lambda (name)
      (let ((r (cache-get cache name mark)))
	(if (eq? r mark)
	    (let ((o (create name)))
	      (cache-put! cache name o)
	      o)
	    r))))

  ;; Found in java/util/Scanner.java
  ;; it seems caches are created per name and comparison
  ;; is done with given name and actual object.
  ;; easy enough to implement :)
  ;;
  ;; NB: if we need to store with KV, then we need a cache class.
  ;;     this implementation assumes that values can be created
  ;;     from given names.
;;   (define (make-simple-lru-cache size create has-name?)
;;     ;; cache itself, we initialise this when needed
;;     (define buffer #f)
;;     ;; empty mark
;;     (define empty (list 'empty))
;;     (unless (and (fixnum? size) (positive? size))
;;       (assertion-violation 'make-simple-lru-cache
;; 			   "size must be positive fixnum" size))
;;     (lambda (name)
;;       ;; put the last object in the first of buffer
;;       ;; so it won't be removed for couple of iteration
;;       (define (move! vec index)
;; 	(let ((o (vector-ref vec index)))
;; 	  (vector-copy! vec 1 vec 0 index)
;; 	  (vector-set! vec 0 o)))
;; 
;;       (define (get-cache name)
;; 	(if buffer
;; 	    (let loop ((i 0))
;; 	      (if (= i size)
;; 		  empty
;; 		  (let ((o (vector-ref buffer i)))
;; 		    (cond ((eq? o empty) (loop (+ i 1)))
;; 			  ((has-name? o name)
;; 			   ;; refresh this expiration period
;; 			   (when (positive? i) (move! buffer i))
;; 			   o)
;; 			  (else (loop (+ i 1)))))))
;; 	    (begin
;; 	      (set! buffer (make-vector size empty))
;; 	      empty)))
;;       (let ((cache (get-cache name)))
;; 	(if (eq? cache empty)
;; 	    (let ((o (create name))
;; 		  (index (- size 1)))
;; 	      (vector-set! buffer index o)
;; 	      (move! buffer index)
;; 	      o)
;; 	    cache))))

)
