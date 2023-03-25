;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; text/json/object-builder.scm - JSON to Scheme object builder
;;;  
;;;   Copyright (c) 2017-2021  Takashi Kato  <ktakashi@ymail.com>
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
(library (text json object-builder)
    (export json-string->object read-object-from-json
	    json:builder? @ ? 
	    json-object-builder

	    object->json-string write-object-as-json
	    json:serializer? ->
	    json-object-serializer

	    *post-json-object-build*
	    *post-json-array-build*

	    make-hashtable-serializer ;; for convenience

	    ;; don't want to expose but no choice...
	    json->object
	    object->json
	    )
    (import (rnrs)
	    (text json parse)
	    (util hashtables)
	    (srfi :1)
	    (srfi :39))

 (define-record-type json:builder
   (fields build-object))
 
 (define-record-type json:builder-mapping
   (fields order key builder optional? default)
   (protocol (lambda (p)
	       (lambda (order key builder optional? default)
		 (unless (and (fixnum? order) (>= order 0))
		   (assertion-violation 'make-json:builder-mapping 
		     "order must be a non negative fixnum" order))
		 (unless (string? key)
		   (assertion-violation 'make-json:builder-mapping 
		     "key must be a string" key))
		 (unless (json:builder? builder)
		   (assertion-violation 'make-json:builder-mapping 
		     "builder must be a json:builder" builder))
		 (p order key builder optional? default)))))

 (define-record-type json:object-builder
   (fields constructor mappings)
   (parent json:builder)
   (protocol (lambda (p)
	       (lambda (ctr mappings)
		 (unless (procedure? ctr)
		   (assertion-violation 'make-json:object-builder
		     "constructor must be a procedure" ctr))
		 (unless (for-all json:builder-mapping? mappings)
		   (assertion-violation 'make-json:object-builder
		     "mappings must be a list of json:builder-mapping"
		     mappings))
		 ;; TODO check order duplication
		 ((p build-json-object) ctr
		  (list-sort (lambda (a b)
			       (< (json:builder-mapping-order a)
				  (json:builder-mapping-order b)))
			     mappings))))))
 
 (define-record-type json:array-builder
   (fields >array builder)
   (parent json:builder)
   (protocol (lambda (p)
	       (lambda (->array builder)
		 (unless (json:builder? builder)
		   (assertion-violation 'make-json:array-builder
		     "builder must be a json:builder" builder))
		 (unless (procedure? ->array)
		   (assertion-violation 'make-json:array-builder
		     "->array must be a procedure" ->array))
		 ((p build-json-array) ->array builder)))))

 (define (default-no-mapping-handler key value)
   (assertion-violation 'json-string->object "no mapping found for key"
			key value))

 (define *post-json-object-build* (make-parameter values))
 (define (build-json-object builder json handler)
   (define mappings (json:object-builder-mappings builder))
   (define mapping-length (length mappings))
   (define ctr (json:object-builder-constructor builder))
   (define len (vector-length json))

   (define (find-mapping key mappings)
     (find (lambda (m) (string=? key (json:builder-mapping-key m))) mappings))
   (define (check-existence v mappings)
     (do ((i 0 (+ i 1)) (mappings mappings (cdr mappings)))
	 ((null? mappings))
       (let ((m (car mappings))
	     (val (vector-ref v i)))
	 (when (eq? val ctr)
	   (if (json:builder-mapping-optional? m)
	       (vector-set! v i (json:builder-mapping-default m))
	       (error 'json-string->object "missing key"
		      (json:builder-mapping-key m)))))))

   (do ((i 0 (+ i 1)) (v (make-vector mapping-length ctr)))
       ((= i len)
	(check-existence v mappings)
	((*post-json-object-build*) (apply ctr (vector->list v))))
     (let* ((kv (vector-ref json i))
	    (mapping (find-mapping (car kv) mappings)))
       (if mapping
	   (vector-set! v (json:builder-mapping-order mapping)
			(json->object (cdr kv)
				      (json:builder-mapping-builder mapping)
				      handler))
	   (handler (car kv) (cdr kv))))))

 ;; array must contain the same object in sense of builder creates
 (define *post-json-array-build* (make-parameter values))
 (define (build-json-array builder json handler)
   (define ->array (json:array-builder->array builder))
   (define element-builder (json:array-builder-builder builder))
   ((*post-json-array-build*)
    (apply ->array
	   (map (lambda (j) (json->object j element-builder handler)) json))))
 
 ;; internal use only since we don't have THE representation for JSON.
 (define json->object
   (case-lambda
    ((json builder)
     ((json:builder-build-object builder)
      builder json default-no-mapping-handler))
    ((json builder handler)
     ((json:builder-build-object builder) builder json handler))))

 (define read-object-from-json
   (case-lambda
    ((builder)
     (read-object-from-json builder (current-input-port)))
    ((builder port)
     (json->object (json-read port) builder))
    ((builder port handler)
     (json->object (json-read port) builder handler))))
 
 (define json-string->object
   (case-lambda
    ((json-string builder)
     (json-string->object json-string builder default-no-mapping-handler))
    ((json-string builder handler)
     (read-object-from-json builder (open-string-input-port json-string)
			    handler))))

 (define (simple-build builder json handler) json)
 (define simple-json-builder (make-json:builder simple-build))

 (define-syntax @ (syntax-rules ()))
 (define-syntax ? (syntax-rules ()))
 
 (define-syntax json-object-object-builder
   (syntax-rules (?)
     ((_ "parse" ctr n (mapping ...) ((? key default spec) rest ...))
      (json-object-object-builder "parse" ctr (+ n 1)
        (mapping ... 
	 (make-json:builder-mapping n key 
				    (json-object-builder spec) #t default))
	(rest ...)))
     ((_ "parse" ctr n (mapping ...) ((? key default) rest ...))
      (json-object-object-builder "parse" ctr (+ n 1)
        (mapping ... 
	 (make-json:builder-mapping n key simple-json-builder #t default))
	(rest ...)))
     ((_ "parse" ctr n (mapping ...) ((key spec) rest ...))
      (json-object-object-builder "parse" ctr (+ n 1)
        (mapping ... 
	 (make-json:builder-mapping n key (json-object-builder spec) #f #f))
	(rest ...)))
     ((_ "parse" ctr n (mapping ...) (key rest ...))
      (let ((k key))
	(cond ((string? k)
	       (json-object-object-builder "parse" ctr (+ n 1)
		(mapping ... 
		 (make-json:builder-mapping n key simple-json-builder #f #f))
		(rest ...)))
	      ((json:object-builder? k)
	       (let ((m* (json:object-builder-mappings k)))
		 (json-object-object-builder "parse" ctr (+ n (length m*))
		   (mapping ... m*)
		   (rest ...))))
	      (else
	       (syntax-violation 'json-object-builder '(key rest ...)
		"Key must be string or json:object-builder")))))
     ((_ "parse" ctr n (mappings ...) ())
      (let ((m* (flatten mappings ...)))
	(make-json:object-builder ctr m*)))
     ((_ ctr spec ...)
      (json-object-object-builder "parse" ctr 0 () (spec ...)))))
 
 (define-syntax json-object-builder
   (syntax-rules (@)
     ;; top level array
     ((_ (@ ->array spec))
      (make-json:array-builder ->array (json-object-builder spec)))
     ((_ (@ ->array))
      (make-json:array-builder ->array simple-json-builder))
     ;; kv
     ((_ (ctr kb* ...))
      (json-object-object-builder ctr kb* ...))
     ;; top level string or number?
     ((_ ctr)
      (let ((t ctr))
	(if (json:builder? t)
	    t
	    (make-json:builder (lambda (builder json handler) (t json))))))))

;;; Serializer
 (define-record-type json:serializer
   (fields serialize-object))
 ;; container
 (define-record-type json:array-serializer
   (parent json:serializer)
   (fields serializer))
 (define-record-type json:sequential-access-array-serializer
   (parent json:array-serializer)
   (fields car cdr null?)
   (protocol (lambda (p)
	       (lambda (car cdr null? serializer)
		 ((p serialize-list-like-object serializer) car cdr null?)))))
 (define-record-type json:random-access-array-serializer
   (parent json:array-serializer)
   (fields ref length)
   (protocol (lambda (p)
	       (lambda (ref length serializer)
		 ((p serialize-vector-like-object serializer) ref length)))))

 (define-record-type json:serializer-mapping
   (fields name ref serializer optional? absent))
 (define-record-type json:object-serializer
   (parent json:serializer)
   (fields mappings)
   (protocol (lambda (p)
	       (lambda (mappings)
		 ((p serialize-object) mappings)))))

 (define (serialize-object serializer obj)
   (define mappings (json:object-serializer-mappings serializer))
   (define (serialize-element mapping obj)
     (define absent-value (json:serializer-mapping-absent mapping))
     (define optional? (json:serializer-mapping-optional? mapping))
     (let ((val ((json:serializer-mapping-ref mapping) obj)))
       (if (and optional?
		(or (and (procedure? absent-value) (absent-value val))
		    (equal? absent-value val)))
	   (values #t #f)
	   (let* ((serializer (json:serializer-mapping-serializer mapping))
		  (json (object->json val serializer)))
	     (values #f (cons (json:serializer-mapping-name mapping) json))))))
   (let loop ((mappings mappings) (r '()))
     (if (null? mappings)
	 (list->vector (reverse r))
	 (let ((m (car mappings)))
	   (cond ((json:serializer-mapping? m)
		  (let-values (((absent? json) (serialize-element m obj)))
		    (if absent?
			(loop (cdr mappings) r)
			(loop (cdr mappings) (cons json r)))))
		 ((json:serializer? m)
		  (loop (cdr mappings)
			(append! (vector->list (object->json obj m)) r)))
		 (else (assertion-violation 'serialize-object
					    "invalid serializer")))))))

 (define (serialize-list-like-object serializer obj)
   (define car (json:sequential-access-array-serializer-car serializer))
   (define cdr (json:sequential-access-array-serializer-cdr serializer))
   (define null? (json:sequential-access-array-serializer-null? serializer))
   (define element-serializer (json:array-serializer-serializer serializer))
   (let loop ((objs obj) (r '()))
     (if (null? objs)
	 (reverse r)
	 (let ((o (car objs)))
	   (loop (cdr objs) (cons (object->json o element-serializer) r))))))
 (define (serialize-vector-like-object serializer obj)
   (define ref (json:random-access-array-serializer-ref serializer))
   (define length (json:random-access-array-serializer-length serializer))
   (define element-serializer (json:array-serializer-serializer serializer))
   (define len (length obj))
   (let loop ((i 0) (r '()))
     (if (= i len)
	 (reverse r)
	 (let ((obj (ref obj i)))
	   (loop (+ i 1) (cons (object->json obj element-serializer) r))))))

 (define (make-hashtable-serializer getter)
   (define (serialize ser obj)
     (define hashtable (getter obj))
     (list->vector (hashtable-map cons hashtable)))
   (make-json:serializer serialize))
 
 (define (object->json obj serializer)
   ((json:serializer-serialize-object serializer) serializer obj))

 (define write-object-as-json
   (case-lambda
    ((obj serializer)
     (write-object-as-json obj serializer (current-output-port)))
    ((obj serializer out)
     (json-write (object->json obj serializer) out))))
 
 (define (object->json-string obj serializer)
   (let-values (((out extract) (open-string-output-port)))
     (write-object-as-json obj serializer out)
     (extract)))

 (define simple-json-serializer (make-json:serializer (lambda (_ obj) obj)))

 (define (serializer->mapping serializer)
   (if (json:object-serializer? serializer)
       (json:object-serializer-mappings serializer)
       serializer))
 
 (define (flatten . lis)
   (define (rec lis acc stk)
     (cond ((null? lis)
	    (if (null? stk)
		(reverse! acc)
		(rec (car stk) acc (cdr stk))))
	   ((pair? (car lis))
	    (rec (car lis) acc (cons (cdr lis) stk)))
	   (else (rec (cdr lis) (cons (car lis) acc) stk))))
   (rec lis '() '()))
    
 (define-syntax -> (syntax-rules ()))
 (define-syntax json-object-object-serializer
   (syntax-rules (? json-object-serializer)
     ((_ "parse" (mapping ...) ((? name absent ref spec) rest ...))
      (json-object-object-serializer "parse"
	(mapping ... (name ref (json-object-serializer spec) #t absent))
	(rest ...)))
     ((_ "parse" (mapping ...) ((? name absent ref) rest ...))
      (json-object-object-serializer "parse"
	(mapping ... (name ref simple-json-serializer #t absent)) (rest ...)))
     ((_ "parse" (mapping ...) ((name ref spec) rest ...))
      (json-object-object-serializer "parse"
	(mapping ... (name ref (json-object-serializer spec) #f #f))
	(rest ...)))
     ((_ "parse" (mapping ...) ((name ref) rest ...))
      (json-object-object-serializer "parse"
	(mapping ... (name ref simple-json-serializer #f #f)) (rest ...)))
     ((_ "parse" (mapping ...) (serializer rest ...))
      (json-object-object-serializer "parse"
	(mapping ... (json-object-serializer serializer)) (rest ...)))
     ((_ "parse" (results ...) ())
      (json-object-object-serializer "ctr" () (results ...)))
     ((_ "ctr" (mapping ...) ((name ref serializer optional? absent) rest ...))
      (json-object-object-serializer "ctr"
       (mapping ... (make-json:serializer-mapping 
		     name ref serializer optional? absent))
       (rest ...)))
     ((_ "ctr" (mapping ...) ((json-object-serializer serializer) rest ...))
      (json-object-object-serializer "ctr"
       (mapping ... (serializer->mapping serializer))
       (rest ...)))
     ((_ "ctr" (mapping ...) ())
      (make-json:object-serializer (flatten mapping ...)))
     ((_ mapping mapping* ...)
      (json-object-object-serializer "parse" () (mapping mapping* ...)))))
 
 (define-syntax json-object-serializer
   (syntax-rules (-> @)
     ;; sequential access container (e.g. list)
     ((_ (-> car cdr null? spec))
      (make-json:sequential-access-array-serializer
       car cdr null? (json-object-serializer spec)))
     ((_ (-> car cdr null?))
      (make-json:sequential-access-array-serializer
       car cdr null? simple-json-serializer))
     ;; for convenience
     ((_ (-> spec))
      (make-json:sequential-access-array-serializer
       car cdr null? (json-object-serializer spec)))
     ((_ (->))
      (make-json:sequential-access-array-serializer
       car cdr null? simple-json-serializer))
     ;; random access container (e.g. vector)
     ((_ (@ ref len spec))
      (make-json:random-access-serializer
       ref len (json-object-serializer spec)))
     ((_ (@ ref len))
      (make-json:random-access-array-serializer
       ref len simple-json-serializer))
     ;; for convenience
     ((_ (@ spec))
      (make-json:random-access-array-serializer
       vector-ref vector-length (json-object-serializer spec)))
     ((_ (@))
      (make-json:random-access-array-serializer
       vector-ref vector-length simple-json-serializer))
     ((_ (mapping mapping* ...))
      (json-object-object-serializer mapping mapping* ...))
     ((_ serializer)
      (let ((t serializer))
	(if (json:serializer? t)
	    t
	    (make-json:serializer (lambda (_ o) (t o))))))))
 )
