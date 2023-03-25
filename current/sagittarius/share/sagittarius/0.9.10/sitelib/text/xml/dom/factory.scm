;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; text/xml/dom/factory.scm - DOM tree factory
;;;  
;;;   Copyright (c) 2018  Takashi Kato  <ktakashi@ymail.com>
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
(library (text xml dom factory)
    (export input-port->dom-tree
	    xml-file->dom-tree
	    ;; sxml->dom-tree
	    input-port->tolerant-dom-tree
	    xml-file->tolerant-dom-tree

	    ;; write-dom-tree

	    ;; options
	    make-xml-document-factory-options
	    xml-document-factory-options?)
    (import (rnrs)
	    (peg)
	    (match)
	    (text xml dom parser)
	    (text xml dom nodes)
	    (sagittarius)
	    (sagittarius generators)
	    (srfi :1 lists)
	    (srfi :13 strings)
	    (srfi :39 parameters)
	    (srfi :127 lseqs))

;;; utility
(define (lseq->xml in)
  (let-values (((s v n) ($xml:document in)))
    (if (parse-success? s)
	(values v n)
	(error 'parse-xml "Failed to parse XML document" v (list->string n)))))
(define (parse-xml in)
  (let-values (((v nl) (lseq->xml (generator->lseq (port->char-generator in)))))
    (if (not (null? nl))
	(error 'parse-xml "XML document contains extra data" nl)
	v)))

;; TODO maybe this should be move to constructing part
(define-record-type xml-document-factory-options
  (fields namespace-aware?
	  xinclude-aware?
	  validating?
	  whitespace?
	  expand-entity-reference?
	  expand-character-reference?
	  ignore-comments?
	  coalescing?)
  (protocol (lambda (p)
	      ;; the options are taken from Java.
	      (lambda (:key (namespace-aware? #t)
			    (xinclude-aware? #f)
			    (validating? #f)
			    (whitespace? #f)
			    (expand-entity-reference? #t)
			    (expand-character-reference? #t)
			    (ignore-comments? #f)
			    (coalescing? #f))
		(p namespace-aware? xinclude-aware?
		   validating? whitespace? expand-entity-reference?
		   expand-character-reference?
		   ignore-comments? coalescing?)))))
(define-syntax %expand-entity?
  (syntax-rules ()
    ((_)
     (xml-document-factory-options-expand-entity-reference?
      (*factory-options*)))))
(define-syntax %expand-char-ref?
  (syntax-rules ()
    ((_)
     (xml-document-factory-options-expand-character-reference?
      (*factory-options*)))))

(define +default-factory-option+ (make-xml-document-factory-options))

;; internal parameter
(define *factory-options* (make-parameter #f))
(define *root-document* (make-parameter #f))

(define (input-port->dom-tree in :key (option +default-factory-option+) (uri #f))
  (let ((parsed (parse-xml in))
	(document (make-xml-document uri)))
    (parameterize ((*factory-options* option)
		   (*root-document* document))
      (dispatch-factory parsed)
      document)))

(define (input-port->tolerant-dom-tree in :key (option +default-factory-option+)
				       (uri #f))
  (define document (make-xml-document uri))
  (define lseq (generator->lseq (port->char-generator in)))
  (let loop ((lseq lseq))
    (if (null? lseq)
	document
	(let-values (((v nl) (lseq->xml lseq)))
	  (parameterize ((*factory-options* option)
			 (*root-document* document))
	    (dispatch-factory v)
	    (loop nl))))))

(define (xml-file->dom-tree file . opt)
  (call-with-input-file file
    (lambda (in) (apply input-port->dom-tree in :uri (absolute-path file) opt))))

(define (xml-file->tolerant-dom-tree file . opt)
  (call-with-input-file file
    (lambda (in) (apply input-port->tolerant-dom-tree in
			:uri (absolute-path file) opt))))

(define *factory-table* (make-eq-hashtable))
(define (dispatch-factory tree)
  (cond ((pair? tree)
	 (let ((name (car tree)))
	   (cond ((hashtable-ref *factory-table* name #f) =>
		  (lambda (proc) (proc tree (*root-document*))))
		 (else tree))))
	((string? tree)
	 (document:create-text-node (*root-document*) tree))
	(else 
	 (assertion-violation 'document-factory "Unknown value" tree))))
(define-syntax define-factory
  (lambda (x)
    (define (->name name)
      (string->symbol
       (string-append (symbol->string (syntax->datum name)) "-factory")))
    (syntax-case x ()
      ((k (name root-document) body ...)
       (with-syntax ((defname (datum->syntax #'k (->name #'name))))
	 #'(define defname
	     (let ((proc (lambda (name root-document) body ...)))
	       (hashtable-set! *factory-table* 'name proc)
	       proc)))))))

;; The factories depend on the correctness of the input tree.
;; at this moment, we don't validate.
(define-factory (document root-document)
  (dispatch-factory (cadr document)) ;; prolog
  (let ((element (dispatch-factory (caddr document)))
	(misc (map dispatch-factory (cdddr document))))
    (node:append-child! root-document element)
    (document-document-element-set! root-document element)
    (for-each (lambda (node) (node:append-child! root-document node)) misc)))

;; prolog will be handled destructively
(define-factory (prolog root-document)
  (cond ((cadr prolog) => dispatch-factory))
  (let ((misc1 (map dispatch-factory (cdaddr prolog)))
	(doctype (cond ((cadddr prolog) => dispatch-factory) (else #f)))
	(misc2 (map dispatch-factory (cdar (cddddr prolog)))))
    (for-each (lambda (node) (node:append-child! root-document node)) misc1)
    (when doctype
      (document-doctype-set! root-document doctype)
      (node:append-child! root-document doctype))
    (for-each (lambda (node) (node:append-child! root-document node)) misc2)))

(define-factory (xml-decl root-document)
  (let ((version (cadr xml-decl))
	(encode (caddr xml-decl))
	(standalone (cadddr xml-decl)))
    ;; put info to root-document
    (document-xml-version-set! root-document (cadr version))
    (when encode (document-character-set-set! root-document (cadr encode)))
    (when standalone
      (document-xml-standalone?-set! root-document
				     (string=? (cadr standalone) "yes")))))

(define-factory (comment root-document)
  (document:create-comment root-document (cadr comment)))

(define-factory (PI root-document)
  (document:create-processing-instruction root-document (cadr PI) (caddr PI)))

(define-factory (!doctype root-document)
  (define (parse-id id)
    (cond ((not id) (values #f #f))
	  ((eq? (car id) 'system) (values #f (cadr id)))
	  ((eq? (car id) 'public) (values (cadr id) (caddr id)))
	  (else (assertion-violation '!doctype "Invalid external ID" id))))
  (define (handle-subset doctype subset)
    (define (set-it table e)
      (if (named-node-map:contains? table e)
	  (assertion-violation '!doctype "Duplicate definition"
			       (node-node-type e) e)
	  (named-node-map:set-named-item! table e)))
    (define (ensure-entry table name)
      (let ((et (named-node-map:get-named-item table name)))
	(cond (et)
	      (else
	       (set-it table (document:create-element-type root-document
							   name '()))
	       (ensure-entry table name)))))
    (let ((e (dispatch-factory subset)))
      (cond ((entity? e) (set-it (document-type-entities doctype) e))
	    ((element-type? e) (set-it (document-type-elements doctype) e))
	    ((and (pair? e) (attdef? (cdr e)))
	     (let* ((e* (document-type-elements doctype))
		    (et (ensure-entry e* (car e))))
	       (element-type-attlist-append! et (cdr e))))
	    ;; TODO the rest
	    )))
  (let ((name (cadr !doctype))
	(id (caddr !doctype))
	(subsets (cadddr !doctype)))
    (let-values (((public-id system-id) (parse-id id)))
      (let ((doctype (document:create-document-type root-document
						    name public-id system-id)))
	(for-each (lambda (subset) (handle-subset doctype subset))
		  ;; !element needs to be handled first...
		  (list-sort (lambda (a b)
			       (cond ((eq? '!element (car a)) #t)
				     ((eq? '!element (car b)) #f)
				     (else #f))) ;; don't care
			       (cdr subsets)))
	doctype))))

(define-factory (!entity root-document)
  (define (handle-parameter-entity entity)
    (assertion-violation '!entity "not supported yet"))
  (define (handle-general-entity entity)
    (let ((name (car entity))
	  (value (cadr entity)))
      (cond ((eq? (car value) 'entity-value)
	     (document:create-entity/value root-document name (cadr value)))
	    ((eq? (car value) 'public)
	     (let ((maybe-notation (cdddr value)))
	       (if (null? maybe-notation)
		   (apply document:create-entity/public-id root-document name
			  (cdr value))
		   (document:create-entity/public-id root-document name
						     (cadr value) (caddr value)
						     (cadar maybe-notation)))))
	    ((eq? (car value) 'system)
	     (let ((maybe-notation (cddr value)))
	       (if (null? maybe-notation)
		   (document:create-entity/system-id root-document name
						     (cadr value))
		   (document:create-entity/system-id root-document name
						     (cadr value)
						     (cadar maybe-notation)))))
	    (else
	     (assertion-violation 'general-entity "unknown entity" !entity)))))
  (if (eq? 'pe (cadr !entity))
      (handle-parameter-entity (cddr !entity))
      (handle-general-entity (cddr !entity))))

(define-factory (!attlist root-document)
  (define (make-it def)
    (define (parse def)
      (match def
	('required (values 'required #f))
	('implied  (values 'implied #f))
	((_ 'fixed v)  (values 'fixed v))
	((_ v)         (values #f v))
	(_ (assertion-violation 'att-value "Unknown format" def))))
    (let ((n (cadr def))
	  (t (caddr def)))
      (let-values (((decl-type default-value) (parse (cadddr def))))
	(make-attdef n t decl-type default-value))))
  (let ((element-name (cadr !attlist))
	(att-def (make-it (caddr !attlist))))
    (cons element-name att-def)))

(define-factory (!element root-document)
  (let ((name (cadr !element))
	(spec (caddr !element)))
    (document:create-element-type root-document name spec)))
    
(define-factory (element root-document)
  (define doctype (document-doctype root-document))
  (define element-types (and doctype (document-type-elements doctype)))

  (define (check-default-attribute e)
    (define element-type
      (and element-types
	   (named-node-map:get-named-item element-types
					  (node-node-name e))))
    (and element-type
	 (let ((attlist (element-type-attlist element-type)))
	   (node-list-for-each
	    (lambda (attdef)
	      ;; we only handle CDATA for now 
	      (when (and (eq? (attdef-att-type attdef) 'cdata)
			 (attdef-att-value attdef))
		(let ((n (attdef-name attdef)))
		  (element:set-attribute! e n (attdef-att-value attdef)))))
	    attlist))))
  
  (define (normalize-attr-values e)
    (define element-type
      (and element-types
	   (named-node-map:get-named-item element-types
					  (node-node-name e))))
    (define (normalize-value v)
      (let-values (((out e) (open-string-output-port)))
	(let loop ((l (string->list (string-trim-both v))) (space? #f))
	  (cond ((null? l) (e))
		((char=? #\space (car l))
		 (unless space? (put-char out #\space))
		 (loop (cdr l) #t))
		(else (put-char out (car l)) (loop (cdr l) #f))))))
    (and element-type
	 (let ((attlist (element-type-attlist element-type)))
	   (node-list-for-each
	    (lambda (attdef)
	      ;; we only handle CDATA for now 
	      (when (memq (attdef-att-type attdef) '(id nmtoken nmtokens))
		(cond ((element:get-attribute-node e (attdef-name attdef)) =>
		       (lambda (attr)
			 (attr-value-set! attr
			  (normalize-value (attr-value attr))))))))
	    attlist))))

  (define (make-element name)
    (if (qname? name)
	(document:create-element-qname root-document
				       (qname-namespace name)
				       (qname-prefix name)
				       (qname-local-part name))
	(document:create-element root-document name)))
  (define (->attribute-node attribute)
    (define (merge-attribute-value val)
      (if (and (string? (car val)) (null? (cdr val)))
	  (car val)
	  (let-values (((out extract) (open-string-output-port)))
	    (define (write-it v)
	      (cond ((string? v) (put-string out v))
		    ((and (pair? v) (eq? 'entity-ref (car v)))
		     (put-string out (expand-entity root-document v)))
		    ;; we use expand char-ref in attributes
		    ((and (pair? v) (eq? 'char-ref (car v)))
		     (put-char out (integer->char (caddr v))))
		    (else (assertion-violation
			   'element "Invalid attribute value" v val))))
	    (for-each write-it val)
	    (extract))))
    (define (set-value attr)
      (unless (null? (cdr attribute)) ;; attr="" case
	(attr-value-set! attr (merge-attribute-value (cdr attribute))))
      attr)
    (let ((name (car attribute)))
      (cond ((qname? name)
	     (set-value
	      (document:create-attribute-qname root-document
					       (qname-namespace name)
					       (qname-prefix name)
					       (qname-local-part name))))
	    ((string? name)
	     (set-value (document:create-attribute root-document name)))
	    (else
	     ;; xmlns
	     ;; it seems like this (at least on Firefox)
	     (let ((local-name (cadr name))
		   (namespace "http://www.w3.org/2000/xmlns/"))
	       (set-value
		(if local-name
		    (document:create-attribute-qname root-document
						     namespace "xmlns"
						     local-name)
		    (document:create-attribute-qname root-document
						     namespace ""
						     "xmlns"))))))))
  (define (merge-text elements)
    (define (create-empty-text) (document:create-text-node root-document #f))
    (let-values (((out extract) (open-string-output-port)))
      (define (flush text)
	(when text (character-data-data-set! text (extract))))
      (let loop ((elements elements) (r '()) (text #f))
	(if (null? elements)
	    (begin (flush text) (reverse! r))
	    (let ((element (car elements)))
	      (cond ((and (eqv? (node-node-type element) +text-node+)
			  (not (char-ref-text? element)))
		     (let ((new (or text (create-empty-text))))
		       (put-string out (character-data-data element))
		       (loop (cdr elements)
			     (if text r (cons new r))
			     new)))
		    (else
		     (flush text)
		     (loop (cdr elements) (cons (car elements) r) #f))))))))
		    
  (let ((name (cadr element))
	(attributes (caddr element))
	(content (cdddr element)))
    (define elm (make-element name))
    (when doctype (check-default-attribute elm))
    (for-each (lambda (attr) (element:set-attribute-node-ns! elm attr))
	      (filter-map ->attribute-node (cdr attributes)))
    (for-each (lambda (node) (node:append-child! elm node))
	      (merge-text (map dispatch-factory content)))
    (normalize-attr-values elm)
    elm))

(define-constant +predefined-entities+
  '(("quot" . "\"")
    ("gt"   . ">")
    ("lt"   . "<")
    ("amp"  . "&")
    ("apos" . "'")))
(define (expand-entity root-document entity-ref)
  (define name (cadr entity-ref))
  (let ((entities (document-type-entities (document-doctype root-document))))
    (cond ((named-node-map:get-named-item entities name) =>
	   (lambda (e)
	     (cond ((entity-entity-value e))
		   (else (assertion-violation
			  'entity-ref "External entity is not supported yet"
			  name)))))
	  ((assoc name +predefined-entities+) => cdr)
	  (else (assertion-violation 'entity-ref "Unknown entity name" name)))))

(define-factory (entity-ref root-document)
  (define (->text-node value) (document:create-text-node root-document value))
  (if (%expand-entity?)
      (->text-node (expand-entity root-document entity-ref))
      (let ((name (cadr entity-ref)))
	(document:create-entity-reference root-document name))))

(define-factory (char-ref root-document)
  (let ((s (string (integer->char (caddr char-ref)))))
    (if (%expand-char-ref?)
	(document:create-text-node root-document s)
	(document:create-char-ref-text root-document s char-ref))))

(define-factory (cdata root-document)
  (document:create-cdata-section root-document (cadr cdata)))
)
