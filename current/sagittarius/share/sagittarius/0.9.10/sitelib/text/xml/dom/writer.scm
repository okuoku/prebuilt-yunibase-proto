;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; text/xml/dom/writer.scm - DOM tree writer
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
(library (text xml dom writer)
    (export make-dom-writer
	    (rename (%make-xml-write-options make-xml-write-options))
	    xml-write-options-builder
	    *xml:default-options*
	    *xml:c14n*
	    *xml:c14n-w/comment*
	    (rename (*xml:c14n* *xml:c14n11*)
		    (*xml:c14n-w/comment* *xml:c14n11-w/comment*))
	    *xml:exc-c14n*
	    *xml:exc-c14n-w/comment*)
    (import (rnrs)
	    (record builder)
	    (text xml dom nodes)
	    (text xml dom util)
	    (srfi :13 strings)
	    (srfi :14 char-sets)
	    (srfi :117 list-queues))

;;; taken from the option list of fn:serialize
;;; https://www.w3.org/TR/xpath-functions-31/#func-serialize
(define-record-type xml-write-options
  (fields emit-internal-dtd? ;; keep
	  strict?	     ;; keep

	  allow-duplicate-names?	; default #f
	  byte-order-mark?		; default #f
	  cdata-section-elements	; default '()
	  doctype-public		; default absent, #f
	  doctype-system		; default absent, #f
	  encoding			; default "utf-8"
	  escape-uri-attribute? 	; default #t
	  html-version			; default 5
	  include-content-type?		; default #t
	  indent?			; default #f
	  item-separator		; default absent, #f
	  json-node-output-method	; default xml
	  media-type			; text/xml?
	  normalization-form		; default none
	  omit-xml-declaration?		; default #t
	  standalone?			; default '(), omit
	  suppress-indentation		; default '()
	  undeclare-prefixes?		; default #f
	  use-character-maps		; default empty map
	  version			; default "1.0"
	  ;; for canonicalisation
	  use-inline-element?		; default #t
	  write-comment?		; default #t
	  write-doctype?		; default #t
	  write-cdata?			; default #t
	  exclusive?			; default #t
	  ))

(define (%make-xml-write-options emit-internal-dtd? strict?
				 :key (allow-duplicate-names #f)
				      (byte-order-mark #f)
				      (cdata-section-elements '())
				      (doctype-public #f)
				      (doctype-system #f)
				      (encoding "utf-8")
				      (escape-uri-attribute #t)
				      (html-version 5)
				      (include-content-type #t)
				      (indent #f)
				      (item-separator #f)
				      (json-node-output-method "xml")
				      (media-type "text/xml")
				      (normalization-form "none")
				      (omit-xml-declaration #t)
				      (standalone '())
				      (suppress-indentation '())
				      (undeclare-prefixes #f)
				      (use-character-maps #f)
				      (version "1.0"))
  (make-xml-write-options emit-internal-dtd? strict?
			  allow-duplicate-names byte-order-mark
			  cdata-section-elements doctype-public
			  doctype-system encoding escape-uri-attribute
			  html-version include-content-type indent
			  item-separator json-node-output-method
			  media-type normalization-form omit-xml-declaration
			  standalone suppress-indentation undeclare-prefixes
			  use-character-maps version
			  #t #t #t #t #t))

(define-syntax xml-write-options-builder
  (make-record-builder xml-write-options
    ((cdata-section-elements '())
     (encoding "utf-8")
     (escape-uri-attribute #t)
     (html-version 5)
     (include-content-type #t)
     (json-node-output-method "xml")
     (media-type "xml")
     (normalization-form "none")
     (omit-xml-declaration? #t)
     (standalone? '())
     (suppress-indentation '())
     (use-character-maps (make-eq-hashtable))
     (version "1.0")
     (use-inline-element? #t)
     (write-comment? #t)
     (write-doctype? #t)
     (write-cdata? #t)
     (exclusive? #t))))

;; for now not strict by default
(define *xml:default-options* (xml-write-options-builder))
;; canonicalisation options
(define *xml:c14n* (xml-write-options-builder
		    (use-inline-element? #f)
		    (write-comment? #f)
		    (write-doctype? #f)
		    (write-cdata? #f)
		    (exclusive? #f)))
(define *xml:exc-c14n* (xml-write-options-builder
			(use-inline-element? #f)
			(write-comment? #f)
			(write-doctype? #f)
			(write-cdata? #f)
			(exclusive? #t)))
(define *xml:c14n-w/comment* (xml-write-options-builder 
			      (use-inline-element? #f)
			      (write-doctype? #f)
			      (write-cdata? #f)
			      (exclusive? #f)))
(define *xml:exc-c14n-w/comment* (xml-write-options-builder
				  (use-inline-element? #f)
				  (write-doctype? #f)
				  (write-cdata? #f)
				  (exclusive? #t)))

(define make-dom-writer
  (case-lambda
   (() (make-dom-writer *xml:default-options*))
   ((options)
    (case-lambda
     ((tree) (write-dom tree options (current-output-port)))
     ((tree out) (write-dom tree options out))))))
   
(define (write-dom tree options out)
  (define (emit-xml-decl tree options out)
    (and (not (xml-write-options-omit-xml-declaration? options))
	 (write-xml-decl tree options out)))  
  (when (and (not (document? tree))
	     (xml-write-options-strict? options))
    (assertion-violation 'write-dom "DOM document is required" tree))

  (if (document? tree)
      (let ((children (node-children tree)))
	(let-values (((out-tmp e) (open-string-output-port)))
	  (let loop ((nodes (list-queue-list children))
		     (written? (emit-xml-decl tree options out)))
	    (unless (null? nodes)
	      (let ((r (write-node (car nodes) (car nodes) options out-tmp)))
		(and written? r (put-char out #\newline))
		(put-string out (e))
		(loop (cdr nodes) (or written? r)))))))
      (write-node tree tree options out)))

(define (write-xml-decl tree options out)
  (define (get-version tree options)
    (cond ((document-xml-version tree))
	  (else (xml-write-options-version options))))
  (put-string out "<?xml")
  (put-string out " version=\"")
  (put-string out (get-version tree options))
  (put-string out "\"")
  ;; not sure how to handle encoding, should we use value from the option?
  (put-string out " encoding=\"")
  (put-string out (document-character-set tree))
  (put-string out "\"")
  (unless (null? (xml-write-options-standalone? options))
    (put-string out " standalone=\"")
    (put-string out (if (or (document-xml-standalone? tree)
			    (xml-write-options-standalone? options))
			"yes"
			"no"))
    (put-string out "\""))
  (put-string out "?>"))

(define (write-node root-node tree options out)
  (cond ((hashtable-ref *writer-table* (node-node-type tree) #f) =>
	 (lambda (writer) (writer root-node tree options out)))
	(else (assertion-violation 'write-node "Unknown DOM node" tree))))

(define *writer-table* (make-eqv-hashtable))
(define-syntax define-node-writer
  (syntax-rules ()
    ((_ type (name root tree options out) body ...)
     (define name
       (let ((name (lambda (root tree options out) body ...)))
	 (hashtable-set! *writer-table* type name)
	 name)))))

(define (collect-parent-namespaces e0 options)
  (define exclusive? (xml-write-options-exclusive? options))
  (element:collect-parent-namespaces e0 exclusive?))

(define-node-writer +element-node+ (element-writer root e options out)
  (define exclusive? (xml-write-options-exclusive? options))
  (define (skip-namespace? e attr)
    (element:skip-namespace? e attr root exclusive?))

  (define (do-it e options out)
    (let ((name (node-node-name e)))
      (put-char out #\<)
      (put-string out name)
      (when (element:has-attributes? e)
	(let ((attrs (element-attributes e)))
	  (do ((len (named-node-map-length attrs))
	       (i 0 (+ i 1)))
	      ((= i len))
	    (let ((attr (named-node-map:item attrs i)))
	      (unless (skip-namespace? e attr)
		(put-char out #\space)
		(write-node root attr options out))))))
      (let ((content (list-queue-list (node-children e))))
	(cond ((null? content)
	       (if (xml-write-options-use-inline-element? options)
		   (put-string out "/>")
		   (begin
		     (put-string out "></")
		     (put-string out name)
		     (put-char out #\>))))
	      (else
	       (put-char out #\>)
	       (for-each (lambda (child)
			   (write-node root child options out)) content)
	       (put-string out "</")
	       (put-string out name)
	       (put-char out #\>))))))
  (if (eq? root e)
      (let ((ns (collect-parent-namespaces e options)))
	(if (null? ns)
	    (do-it e options out)
	    ;; might be too much...
	    (dynamic-wind
		(lambda ()
		  (for-each (lambda (a) (element:set-attribute-node! e a)) ns))
		(lambda () (do-it e options out))
		(lambda ()
		  (for-each (lambda (a) (element:remove-attribute-node! e a))
			    ns)))))
      (do-it e options out)))

(define (make-write/escape attr?)
  (define (write/attr out s alt)
    (if attr?
	(put-string out alt)
	(put-string out s)))
  (lambda (out ch)
    (case ch
      ((#\<)
       ;; canonical form escapes < on attribute, so for now
       ;;(write/attr out "&lt;" "<")
       (put-string out "&lt;"))
      ((#\>)
       (write/attr out "&gt;" ">"))
      ((#\&) (put-string out "&amp;"))
      ((#\") (write/attr out "\"" "&quot;"))
      ((#\') (put-char out ch))		; should we use &apos; for text?
      ;; handle newline differently, this will be shown in the text without
      ;; escaped (not in attribute though)
      ((#\newline) (write/attr out "\n" "&#xA;"))
      (else
       (if (char-set-contains? char-set:iso-control ch)
	   (let ((hex (number->string (char->integer ch) 16)))
	     (put-string out "&#x")
	     (put-string out (string-upcase hex))
	     (put-char out #\;))
	   (put-char out ch))))))
(define write/attr-escape (make-write/escape #t))

(define-node-writer +attribute-node+ (attribute-write root a options out)
  (define (write-it ch) (write/attr-escape out ch))
  (put-string out (attr-name a))
  (put-string out "=\"")
  (string-for-each write-it (attr-value a))
  (put-string out "\""))

(define write/escape (make-write/escape #f))
(define-node-writer +text-node+ (text-write root t options out)
  (define (write-it ch) (write/escape out ch))
  (if (char-ref-text? t)
      (let* ((source (node-source t))
	     (radix (cadr source))
	     (value (caddr source)))
	(case radix
	  ((10) (put-string out "&#"))
	  ((16) (put-string out "&#x"))
	  (else (assertion-violation 'character-ref "invalid radix")))
	(put-string out (number->string value radix))
	(put-char out #\;))
      (string-for-each write-it (character-data-data t))))

(define-node-writer +cdata-section-node+ (cdata-writer root t options out)
  (define (write-it ch) (write/escape out ch))
  (cond ((xml-write-options-write-cdata? options)
	 (put-string out "<![CDATA[")
	 (put-string out (character-data-data t))
	 (put-string out "]]>"))
	(else
	 (string-for-each write-it (character-data-data t)))))

(define-node-writer +entity-reference-node+ (entity-reference rt er options out)
  (put-char out #\&)
  (put-string out (node-node-name er))
  (put-char out #\;))

(define-node-writer +comment-node+ (comment-writer root tree options out)
  (and (xml-write-options-write-comment? options)
       (put-string out "<!--")
       (put-string out (character-data-data tree))
       (put-string out "-->")))
(define-node-writer +processing-instruction-node+ (pi-writer rt pi options out)
  (put-string out "<?")
  (put-string out (processing-instruction-target pi))
  (let ((data (character-data-data pi)))
    (unless (zero? (string-length data))
      (put-string out " ")
      (put-string out data)))
  (put-string out "?>"))
(define-node-writer +document-type-node+ (doctype-writer rt doctype options out)
  (define (emit-doctype-public&system public-id system-id)
    (put-string out "<!DOCTYPE ")
    (put-string out (document-type-name doctype))
    (put-string out " PUBLIC ")
    (put-string out (document-type-public-id doctype))
    (put-string out " ")
    (put-string out (document-type-system-id doctype))
    (put-string out ">"))
  (define (emit-doctype-system system-id)
    (put-string out "<!DOCTYPE ")
    (put-string out (document-type-name doctype))
    (put-string out " SYSTEM ")
    (put-string out (document-type-system-id doctype))
    ;; TODO intSubst?
    (put-string out ">"))
  (and (xml-write-options-write-doctype? options)
       (cond ((and (document-type-public-id doctype)
		   (document-type-system-id doctype))
	      (emit-doctype-public&system (document-type-public-id doctype)
					  (document-type-system-id doctype)))
	     ((document-type-system-id doctype)
	      (emit-doctype-system (document-type-system-id doctype)))
	     ((and (xml-write-options-doctype-public options)
		   (xml-write-options-doctype-system options))
	      (emit-doctype-public&system
	       (xml-write-options-doctype-public options)
	       (xml-write-options-doctype-system options)))
	     ((xml-write-options-doctype-system options)
	      (emit-doctype-system (xml-write-options-doctype-system options)))
	     ((xml-write-options-emit-internal-dtd? options)
	      (assertion-violation 'doctype-writer "not supported yet")))))

)
