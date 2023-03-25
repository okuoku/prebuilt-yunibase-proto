;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; text/markdown/parser/link-reference.scm - Link reference definition parser
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

#!nounbound
(library (text markdown parser link-reference)
    (export (rename (reference-definition <reference-definition>))
	    reference-definition?
	    reference-definition-label
	    
	    make-link-reference-definition link-reference-definition?
	    link-reference-definition-destination
	    link-reference-definition-title

	    link-scanner:scan-link-label-content!
	    link-scanner:scan-link-destination!
	    link-scanner:scan-link-title!
	    link-scanner:scan-link-title-content!
	    
	    make-reference-definitions
	    reference-definitions?
	    reference-definitions:add!
	    reference-definitions:get

	    make-link-reference-definition-parser
	    link-reference-definition-parser?
	    link-reference-definition-parser-source-locations
	    link-reference-definition-parser:parse!
	    link-reference-definition-parser:add-source-location!
	    link-reference-definition-parser:paragraph-lines
	    link-reference-definition-parser:definitions)
    (import (rnrs)
	    (srfi :13 strings)
	    (srfi :14 char-sets)
	    (srfi :117 list-queues)
	    (text markdown parser escaping)
	    (text markdown parser nodes)
	    (text markdown parser parsing)
	    (text markdown parser scanner)
	    (text markdown parser source))

(define-record-type reference-definition
  (parent <source-aware>)
  (fields label)
  (protocol (lambda (n)
	      (lambda (label)
		((n) label)))))
  
(define-record-type link-reference-definition
  (parent reference-definition)
  (fields destination title)
  (protocol (lambda (n)
	      (lambda (label destination title)
		((n label) destination title)))))

(define (link-scanner:scan-link-label-content! scanner)
  (let loop ()
    (if (scanner:has-next? scanner)
	(case (scanner:peek scanner)
	  ((#\\)
	   (scanner:next! scanner)
	   (when (parsing:escapable? (scanner:peek scanner))
	     (scanner:next! scanner))
	   (loop))
	  ((#\]) #t)
	  ((#\[) #f)
	  (else (scanner:next! scanner) (loop)))
	#t)))

(define (link-scanner:scan-link-destination! scanner)
  (define (balanced-paren scanner)
    (let loop ((depth 0) (empty? #t))
      (if (scanner:has-next? scanner)
	  (let ((c (scanner:peek scanner)))
	    (case c
	      ((#\space) (not empty?))
	      ((#\\)
	       (scanner:next! scanner)
	       (when (parsing:escapable? (scanner:peek scanner))
		 (scanner:next! scanner))
	       (loop depth #f))
	      ((#\() (scanner:next! scanner) (loop (+ depth 1) #f))
	      ((#\))
	       (cond ((zero? depth) #t)
		     (else
		      (scanner:next! scanner)
		      (loop (- depth 1) empty?))))
	      (else
	       (cond ((char-set-contains? char-set:iso-control c)
		      (not empty?))
		     (else
		      (scanner:next! scanner)
		      (loop depth #f))))))
	  #t)))
  (cond ((not (scanner:has-next? scanner)) #f)
	((scanner:next-char? scanner #\<)
	 (let loop ()
	   (if (scanner:has-next? scanner)
	       (case (scanner:peek scanner)
		 ((#\\)
		  (scanner:next! scanner)
		  (when (parsing:escapable? (scanner:peek scanner))
		    (scanner:next! scanner))
		  (loop))
		 ((#\newline #\<) #f)
		 ((#\>) (scanner:next! scanner) #t)
		 (else (scanner:next! scanner) (loop)))
	       #f)))
	(else
	 (balanced-paren scanner))))

(define (link-scanner:scan-link-title! scanner)
  (define (check-end-delimiter scanner)
    (case (scanner:peek scanner)
      ((#\") #\")
      ((#\') #\')
      ((#\() #\))
      (else #f)))
  (cond ((not (scanner:has-next? scanner)) #f)
	((check-end-delimiter scanner) =>
	 (lambda (end-delimiter)
	   (scanner:next! scanner)
	   (cond ((not (link-scanner:scan-link-title-content!
			scanner end-delimiter)) #f)
		 ((not (scanner:has-next? scanner)) #f)
		 (else (scanner:next! scanner) #t))))
	(else #f)))

(define (link-scanner:scan-link-title-content! scanner end-delimiter)
  (let loop ()
    (if (scanner:has-next? scanner)
	(let ((c (scanner:peek scanner)))
	  (cond ((eqv? c #\\)
		 (scanner:next! scanner)
		 (when (parsing:escapable? (scanner:peek scanner))
		   (scanner:next! scanner))
		 (loop))
		((eqv? c end-delimiter))
		((and (eqv? end-delimiter #\)) (eqv? c #\))) #f)
		(else (scanner:next! scanner) (loop))))
	#t)))

(define-record-type reference-definitions
  (fields definitions)
  (protocol (lambda (p)
	      (lambda ()
		(p (make-hashtable string-hash string=?))))))
(define (reference-definitions:add! lrd def)
  (define label (reference-definition-label def))
  (hashtable-update! (reference-definitions-definitions lrd)
		     (escaping:normalize-label label)
		     (lambda (v) v) ;; so it doesn't update anything
		     def))

(define (reference-definitions:get lrd label)
  (hashtable-ref (reference-definitions-definitions lrd)
		 (escaping:normalize-label label)
		 #f))


(define-record-type link-reference-definition-parser
  (fields paragraph-lines
	  definitions
	  source-locations
	  ;; stateful parser...
	  (mutable label)
	  (mutable destination)
	  (mutable title-delimiter)
	  (mutable title)
	  (mutable valid?)
	  (mutable state)
	  )
  (protocol
   (lambda (p)
     (lambda ()
       (p (list-queue) (list-queue) (list-queue) #f #f #f #f #f 'start)))))

;; Use list queue as a string buffer (will be joined with "")
(define string-builder list-queue)
(define (string-builder:append! l s) (list-queue-add-back! l s))
(define (string-builder:length sb)
  (fold-left (lambda (acc s) (+ acc (string-length s))) 0
	     (list-queue-list sb)))
(define (string-builder->string sb) (string-join (list-queue-list sb) ""))

(define (link-reference-definition-parser:parse! lrp line)
  (define pl (link-reference-definition-parser-paragraph-lines lrp))
  (define state (link-reference-definition-parser-state lrp))

  (define (start-definition lrp scanner)
    (scanner:whitespace scanner)
    (and (scanner:next-char? scanner #\[)
	 (link-reference-definition-parser-state-set! lrp 'label)
	 (let ((sb (string-builder)))
	   (link-reference-definition-parser-label-set! lrp sb)
	   (unless (scanner:has-next? scanner)
	     (string-builder:append! sb "\n")))
	 #t))
	 
  (define (label lrp scanner)
    (define start (scanner:position scanner))
    (define label (link-reference-definition-parser-label lrp))
    (define (check-label-length label) (> (string-builder:length label) 999))
    (and (link-scanner:scan-link-label-content! scanner)
	 (let* ((p (scanner:position scanner))
		(s (scanner:source scanner start p)))
	   (list-queue-add-back! label (source-lines:content s))
	   (cond ((not (scanner:has-next? scanner))
		  (list-queue-add-back! label "\n")
		  #t)
		 ((scanner:next-char? scanner #\])
		  ;; end of label
		  (cond ((not (scanner:next-char? scanner #\:)) #f)
			((check-label-length label) #f)
			((string-null? (escaping:normalize-label
					(string-builder->string label))) #f)
			(else
			 (link-reference-definition-parser-state-set! lrp
			  'destination)
			 (scanner:whitespace scanner)
			 #t)))
		 (else #f)))))
  (define (destination lrp scanner)
    (scanner:whitespace scanner)
    (let ((start (scanner:position scanner)))
      (and (link-scanner:scan-link-destination! scanner)
	   (let* ((p (scanner:position scanner))
		  (s (source-lines:content (scanner:source scanner start p))))
	     (link-reference-definition-parser-destination-set! lrp
	      (if (eqv? (string-ref s 0) #\<)
		  (substring s 1 (- (string-length s) 1))
		  s))
	     (let ((ws (scanner:whitespace scanner)))
	       (cond ((not (scanner:has-next? scanner))
		      (link-reference-definition-parser-valid?-set! lrp #t)
		      (list-queue-clear!
		       (link-reference-definition-parser-paragraph-lines lrp))
		      (link-reference-definition-parser-state-set! lrp
		       'start-title)
		      #t)
		     ((zero? ws) #f)
		     (else
		      (link-reference-definition-parser-state-set! lrp
		       'start-title)
		      #t)))))))
		      
  (define (start-title lrp scanner)
    (define (check-delimiter lrp c)
      (case c
	((#\" #\') c)
	((#\() #\))
	(else #f)))
    (scanner:whitespace scanner)
    (if (scanner:has-next? scanner)
	(let ((delim (check-delimiter lrp (scanner:peek scanner))))
	  (link-reference-definition-parser-title-delimiter-set! lrp delim)
	  (cond (delim
		 (link-reference-definition-parser-state-set! lrp 'title)
		 (let ((sb (string-builder)))
		   (scanner:next! scanner)
		   (unless (scanner:has-next? scanner)
		     (string-builder:append! sb "\n"))
		   (link-reference-definition-parser-title-set! lrp sb)))
		(else
		 (link-reference-definition-parser:finish-reference lrp)
		 (link-reference-definition-parser-state-set! lrp 'start)))
	  #t)
	(begin
	  (link-reference-definition-parser-state-set! lrp 'start)
	  #t)))

  (define (title lrp scanner)
    (define start (scanner:position scanner))
    (define delim (link-reference-definition-parser-title-delimiter lrp))

    (and (link-scanner:scan-link-title-content! scanner delim)
	 (let* ((p (scanner:position scanner))
		(s (source-lines:content (scanner:source scanner start p)))
		(title (link-reference-definition-parser-title lrp)))
	   (string-builder:append! title s)
	   (cond ((scanner:has-next? scanner)
		  (scanner:next! scanner)
		  (scanner:whitespace scanner)
		  (and (not (scanner:has-next? scanner))
		       (link-reference-definition-parser-valid?-set! lrp #t)
		       (link-reference-definition-parser:finish-reference lrp)
		       (list-queue-clear!
			(link-reference-definition-parser-paragraph-lines lrp))
		       (link-reference-definition-parser-state-set! lrp 'start)
		       #t))
		 (else
		  (string-builder:append! title "\n")
		  #t)))))
  
  (define (try-parse lrp scanner)
    (define state (link-reference-definition-parser-state lrp))
    (case state
      ((start) (start-definition lrp scanner))
      ((label) (label lrp scanner))
      ((destination) (destination lrp scanner))
      ((start-title) (start-title lrp scanner))
      ((title) (title lrp scanner))
      (else (assertion-violation 'link-reference-definition-parser:parse!:
				 "[BUG] Unknown parsing state" state))))
  (list-queue-add-back! pl line)
  (unless (eq? state 'paragraph)
    (let loop ((scanner (scanner:of (source-lines:of line))))
      (cond ((not (scanner:has-next? scanner))) ;; no more input
	    ((try-parse lrp scanner) (loop scanner))
	    (else
	     (link-reference-definition-parser-state-set! lrp 'paragraph))))))

(define (link-reference-definition-parser:finish-reference lrp)
  (when (link-reference-definition-parser-valid? lrp)
    (let* ((d (escaping:unescape
	       (link-reference-definition-parser-destination lrp)))
	   (t (cond ((link-reference-definition-parser-title lrp) =>
		     (lambda (sb)
		       (escaping:unescape (string-builder->string sb))))
		    (else #f)))
	   (l (string-builder->string
	       (link-reference-definition-parser-label lrp)))
	   (def (make-link-reference-definition l d t))
	   (loc (link-reference-definition-parser-source-locations lrp))
	   (defs (link-reference-definition-parser-definitions lrp)))
      (source-aware:locations-set! def loc)
      (list-queue-clear! loc)
      (list-queue-add-back! defs def)
      (link-reference-definition-parser-label-set! lrp #f)
      (link-reference-definition-parser-valid?-set! lrp #f)
      (link-reference-definition-parser-destination-set! lrp #f)
      (link-reference-definition-parser-title-set! lrp #f))))

(define (link-reference-definition-parser:add-source-location! lrp loc)
  (define sp* (link-reference-definition-parser-source-locations lrp))
  (list-queue-add-back! sp* loc))

(define (link-reference-definition-parser:paragraph-lines lrp)
  (source-lines:of (link-reference-definition-parser-paragraph-lines lrp)))

(define (link-reference-definition-parser:definitions lrp)
  (link-reference-definition-parser:finish-reference lrp)
  (list-queue-list (link-reference-definition-parser-definitions lrp)))

)
