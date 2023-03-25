;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/crypto/asn1/modules.scm - ASN.1 module API
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

;; This library is basically separated from (sagittarius crypto asn1)
;; It only provides a macro which defines ASN.1 module, its reader and
;; encoder
#!nounbound
(library (sagittarius crypto asn1 modules)
    (export define-asn1-encodable of
	    asn1-object->asn1-encodable
	    bytevector->asn1-encodable
	    ;; For now only these two, maybe we want to add
	    asn1-choice
	    asn1-sequence
	    asn1-set

	    ;; For convenience, it's here
	    asn1-encodable-container? <asn1-encodable-container>
	    asn1-encodable-container-c)
    (import (rnrs)
	    (clos core)
	    (clos user)
	    (srfi :1 lists) ;; for concatenate!
	    (srfi :13 strings) ;; for string-trim...
	    (sagittarius)
	    (sagittarius crypto asn1 types)
	    (sagittarius crypto asn1 reader)
	    (sagittarius mop immutable)
	    (sagittarius mop allocation))

(define-class <asn1-encodable-container> (<immutable> <cached-allocation>)
  ((c :init-keyword :c :reader asn1-encodable-container-c)))
(define (asn1-encodable-container? o) (is-a? o <asn1-encodable-container>))

(define-generic asn1-object->asn1-encodable)
(define (asn1-encodable-class? class)
  (and (is-a? class <class>) (subtype? class <asn1-encodable>)))
(define (bytevector->asn1-encodable (class asn1-encodable-class?)
				    (bv bytevector?))
  (asn1-object->asn1-encodable class (bytevector->asn1-object bv)))

(define-class <asn1-choice-encodable> (<asn1-encodable>) ())
(define-syntax of (syntax-rules ()))
;; we want to define ASN.1 module like this
;; e.g.
;; AlgorithmIdentifier
;; (define-asn1-encodable <algorithm-identifier>
;;   (asn1-sequence
;;    ((algorithm :type <der-object-identifier>)
;;     (parameter :type <asn1-encodable> :optional #t))))
;; 
;; TBSRequest
;; (define-asn1-encodable <tbs-request>
;;   (asn1-sequence
;;    ((version :type <der-integer> :tag 0 :explicit #t :default v1)
;;     (requestor-name :type <general-name> :tag 1 :explicit #t :optional #t)
;;                         not sure if we want sequence-of, or set-of...
;;     (request-list :type (sequence-of <request>))
;;     (request-extensions :type <extensions> :tag 2 :optional #t))))
(define-syntax define-asn1-encodable
  (syntax-rules (of asn1-choice)
    ((_ (name parents ...) (base-type (of spec ...) opts ...))
     (define-asn1-encodable "emit-class" (name parents ...)
       (base-type ((of :multiple #t
		       spec ...
		       :init-keyword :elements ;; default
		       :init-value '())) opts ...)
       ((o p)
	(asn1-generic-write name
	 (asn1-object-list->string (slot-ref o 'of)) p))))
    ((_ name (base-type (of spec ...) opts ...))
     (define-asn1-encodable (name) (base-type (of spec ...) opts ...)))
    ((_ (name parents ...) (asn1-choice ((slot spec* ...) ...) opts ...))
     (define-asn1-encodable "emit-class" (name parents ...)
       (asn1-choice ((slot spec* ...) ...) opts ...)
       ((o p)
	(let-values (((out e) (open-string-output-port)))
	  (put-datum out (slot-ref o 'value))
	  (asn1-generic-write name (string-trim (e)) p)))))
    ((_ name (asn1-choice ((slot spec* ...) ...) opts ...))
     (define-asn1-encodable (name)
       (asn1-choice ((slot spec* ...) ...) opts ...)))
    ((_ (name parents ...) (base-type ((slot spec* ...) ...) opts ...))
     (define-asn1-encodable "emit-class" (name parents ...)
       (base-type ((slot spec* ...) ...) opts ...)
       ((o p)
	(define (slots->list v)
	  (filter-map (lambda (s) (slot-ref v (slot-definition-name s)))
		      (class-slots name)))
	(asn1-generic-write name
	 (asn1-object-list->string (slots->list o)) p))))
    ((_ name (base-type ((slot spec* ...) ...) opts ...))
     (define-asn1-encodable (name) (base-type ((slot spec* ...) ...) opts ...)))
    ((_ "emit-class" (name parents ...)
	(base-type ((slot spec* ...) ...) opts ...)
	((o p) object-writer ...))
     (begin 
       (base-type (name parents ...) (((slot spec* ...) ...) opts ...)
		  asn1-object->this
		  this->asn1-object)
       (define-method asn1-object->asn1-encodable ((m (eql name))
						   (ao <asn1-object>))
	 (asn1-object->this ao))
       (define-method asn1-encodable->asn1-object ((ao name) type)
	 (this->asn1-object ao type))
       (define-method write-object ((o name) p) object-writer ...)))))

;; <asn1-object> is always <asn1-encodable>, so this is fine
(define-method asn1-object->asn1-encodable (m (o <asn1-object>)) o)

;; dispatcher
(define-syntax base-collection
  (syntax-rules ()
    ((_ (name parents ...) (((slot spec* ...) ...) opts ...)
	(->this pred) (->asn1-object ctr))
     (begin 
       (define-class name (<asn1-encodable> parents ...)
	 ((slot spec* ...
		;; needs to be after
		:init-keyword (symbol->keyword 'slot)
		;; for optional
		:init-value #f) ...))
       (define ->this (make-asn1-object->asn1-encodable name pred))
       (define ->asn1-object (make-asn1-encodable->asn1-object name ctr))))))

(define-syntax asn1-sequence
  (syntax-rules ()
    ((_ name (((slot spec* ...) ...) opts ...) ->this ->asn1-object)
     (base-collection name (((slot spec* ...) ...) opts ...)
		      (->this ber-sequence?)
		      (->asn1-object
		       (lambda (type)
			 (if (eq? type 'der)
			     make-der-sequence
			     make-ber-sequence)))))))
(define-syntax asn1-set
  (syntax-rules ()
    ((_ name (((slot spec* ...) ...) opts ...) ->this ->asn1-object)
     (base-collection name (((slot spec* ...) ...) opts ...)
		      (->this ber-set?)
		      (->asn1-object
		       (lambda (type)
			 (lambda (lis)
			   (if (eq? type 'der)
			       (make-der-set lis opts ...)
			       (make-ber-set lis opts ...)))))))))

(define-syntax asn1-choice
  (syntax-rules ()
    ((_ (name parents ...)
	(((slot spec* ...) ...) opts ...) ->this ->asn1-object)
     (begin
       (define-class name (<asn1-choice-encodable> parents ...)
	 ((type :init-keyword :type)
	  (value :init-keyword :value opts ...))
	 :definitions (list (list 'slot spec* ...) ...))
       (define specs (list (list 'slot spec* ...) ...))
       (define ->this (make-asn1-choice->asn1-encodable name specs))
       (define ->asn1-object (make-asn1-encodable->asn1-choice name specs))))))

;; Internal APIs
(define (raise-error class reason o . irr)
  (error 'asn1-object->asn1-encodable
	 (format
	  "Given ASN.1 object can't be converted to an object of ~a: ~a"
	  (class-name class)
	  reason)
	 (if (null? irr) o (cons o irr))))
(define (try-deserialize type entry optional?)
  (guard (e (else (if optional? #f (raise e))))
    (asn1-object->asn1-encodable type entry)))

(define (entry->object entry slot optional?)
  (define (multiple-of? entry slot)
    (case (slot-definition-option slot :multiple #f)
      ((sequence) (ber-sequence? entry))
      ((set)      (ber-set? entry))
      (else #f)))
  (define (try-multiple entry type)
    (let ((v* (map (lambda (e) (try-deserialize type e #t))
		   (asn1-collection->list entry))))
      (and (for-all (lambda (e) (is-a? e type)) v*)
	   v*)))
  (let ((type (slot-definition-option slot :type <asn1-object>)))
    (cond ((is-a? entry type) entry)
	  ((and (multiple-of? entry slot) (try-multiple entry type)))
	  ((and (asn1-collection? entry)
		;; not primitive
		(not (subtype? type <asn1-object>))
		(try-deserialize type entry optional?)))
	  ((subtype? type <asn1-choice-encodable>)
	   (let ((initargs (slot-ref type 'initargs)))
	     (choice->encodable type
				(cadr (memq :definitions initargs)) entry)))
	  (else #f))))
(define (tagged-obj->obj tagged-obj slot)
  (let ((obj (ber-tagged-object-obj tagged-obj)))
    (cond ((and (asn1-collection? obj)
		(slot-definition-option slot :converter #f)) =>
	   (lambda (conv) 
	     ;; When it's explicit, then it's explicitly a collection
	     ;; so don't decompose it here
	     (if (not (ber-tagged-object-explicit? tagged-obj))
		 (conv (asn1-collection->list obj))
		 (conv (list obj)))))
	  ((slot-definition-option slot :converter #f) =>
	   (lambda (conv)
	     (conv (if (or (not obj) (slot-definition-option slot :explicit #f))
		       obj
		       (der-octet-string->bytevector obj)))))
	  (else obj))))

(define (choice->encodable class slots o)
  (define (search-object o slots)
    (define tagged-obj? (ber-tagged-object? o))
    (define tag-no (and tagged-obj? (ber-tagged-object-tag-no o)))
    (let loop ((slots slots))
      (if (null? slots)
	  (values #f #f)
	  (let* ((slot (car slots))
		 (type (slot-definition-option (car slots) :type #f))
		 (tag (slot-definition-option (car slots) :tag #f))
		 (dummy-slot (list 'dummy :type type)))
	    ;; Maybe we should check during creation?
	    (unless type
	      (raise-error class "Invalid class definiton :type is required" o))
	    (cond ((and tagged-obj? tag (= tag-no tag)
			(entry->object (tagged-obj->obj o slot) slot #t)) =>
		   (lambda (obj) (values (slot-definition-name slot) obj)))
		  ((entry->object o dummy-slot #t) =>
		   (lambda (obj) (values (slot-definition-name slot) obj)))
		  (else (loop (cdr slots))))))))
		 
  (let-values (((type value) (search-object o slots)))
    (and type value
	 (make class :type type :value value))))
(define ((make-asn1-choice->asn1-encodable class slots) o)
  (or (choice->encodable class slots o)
      (raise-error class "No matching type" o)))

(define (%make-asn1-object->asn1-encodable class o)
  (define (asn1-object->slots class o slots)
    (define (err reason . irr)
      (apply raise-error class reason o irr))
    (define (entry->slot slot entry)
      (list (symbol->keyword (slot-definition-name slot)) entry))
    (define (optional? slot) (slot-definition-option slot :optional #f))

    (let loop ((entries (asn1-collection->list o)) (slots slots) (r '()))
      (cond ((and (null? entries) (null? slots))
	     ;; order doesn't matter ;)
	     (concatenate! r))
	    ((null? entries)
	     (if (or (null? slots) (for-all optional? slots))
		 (concatenate! r)
		 (err "missing field(s)")))
	    ((null? slots)
	     (if (null? entries)
		 (concatenate! r)
		 (err "too many fields")))
	    (else
	     (let* ((slot (car slots))
		    (entry (car entries))
		    (tag (slot-definition-option slot :tag #f))
		    (opt? (slot-definition-option slot :optional #f)))
	       ;; check tag first
	       (cond (tag
		      (cond ((and (ber-tagged-object? entry)
				  (= (ber-tagged-object-tag-no entry) tag))
			     (cond ((entry->object (tagged-obj->obj entry slot)
						   slot opt?) =>
				    (lambda (obj)
				      (loop (cdr entries) (cdr slots)
					    (cons (entry->slot slot obj) r))))
				   (else (err "Invalid tag object" entry))))
			    (opt? (loop entries (cdr slots) r))
			    (else (err "Missing tag object"))))
		     ((entry->object entry slot opt?) =>
		      (lambda (obj)
			(loop (cdr entries) (cdr slots)
			      (cons (entry->slot slot obj) r))))
		     (opt? (loop entries (cdr slots) r))
		     (else (err "Unknown field"
				(list (slot-definition-name slot) entry)))))))))
  
  (let ((slots (class-slots class)))
    (if (and (null? (cdr slots))
	     (eqv? #t (slot-definition-option (car slots) :multiple #f)))
	;; handle sequence / set of type separately
	;; TODO maybe we should marge?
	(let* ((slot (car slots))
	       (type (slot-definition-option slot :type <asn1-object>))
	       (key (slot-definition-option slot
					    :init-keyword :elements)))
	  (make class
	    key (map (lambda (o) (asn1-object->asn1-encodable type o))
		     (asn1-collection->list o))))
	(apply make class (asn1-object->slots class o slots)))))

(define-syntax make-asn1-object->asn1-encodable
  (syntax-rules ()
    ((_ name type?)
     (let ((class name))
       (lambda ((name type?))
	 (define o name)
	 (%make-asn1-object->asn1-encodable class o))))))

(define (ensure-asn1-object o type)
  (cond ((asn1-object? o) o)
	((asn1-encodable? o) (asn1-encodable->asn1-object o type))
	;; filter-map will strip out this
	(else o)))
(define (->asn1-object o s type)
  (define (collection-ctr ctr-type type)
    (case ctr-type
      ((sequence) (if (eq? type 'ber) make-ber-sequence make-der-sequence))
      ((set) (if (eq? type 'ber) make-ber-set make-der-set))
      (else (assertion-violation '->asn-object "Unknown collection type"
				 ctr-type))))
  (cond ((slot-definition-option s :multiple #f) =>
	 (lambda (ctr-type)
	   (let ((s2 (list
		      (slot-definition-name s)
		      :type (slot-definition-option s :type <asn1-encodable>))))
	     ((collection-ctr ctr-type type)
	      (map (lambda (o) (->asn1-object o s2 type)) o)))))
	(else
	 (let ((tag (slot-definition-option s :tag #f))
	       (explicit? (slot-definition-option s :explicit #f))
	       (obj (ensure-asn1-object o type)))
	   (and obj
		(if tag
		    (make (if (eq? type 'ber)
			      <ber-tagged-object>
			      <der-tagged-object>)
		      :tag-no tag :explicit? explicit? :obj obj)
		    obj))))))

(define ((make-asn1-encodable->asn1-choice class slots) o type)
  (define (search-slot type slots)
    (find (lambda (slot)
	    (eq? (slot-definition-name slot) type)) slots))
  (let* ((obj-type (slot-ref o 'type))
	 (value (slot-ref o 'value))
	 (slot (search-slot obj-type slots)))
    (->asn1-object value slot type)))

(define ((make-asn1-encodable->asn1-object class make-ctr) o type)
  (define (object->asn1-object o)
    (let ((slots (class-slots (class-of o))))
      (if (and (null? (cdr slots))
	       (slot-definition-option (car slots) :multiple #f))
	  (map (lambda (o) (ensure-asn1-object o type)) (slot-ref o 'of))
	  (filter-map (lambda (s)
			(->asn1-object (slot-ref o (slot-definition-name s))
				       s type))
		      slots))))
  ((make-ctr type) (object->asn1-object o)))
  
)
