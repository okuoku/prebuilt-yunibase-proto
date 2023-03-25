;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; peg/primitives.scm - PEG
;;;  
;;;   Copyright (c) 2017  Takashi Kato  <ktakashi@ymail.com>
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
(library (peg primitives)
    (export $return $fail $expect
	    $eof $any $empty
	    $satisfy $not
	    $seq $or $many
	    $peek $raw

	    $debug
	    ;; value returning
	    return-result return-failure return-expect return-unexpect

	    ;; state extension
	    (rename (parse-state <parse-state>))
	    parse-success? parse-fail? parse-expect? parse-unexpect?
	    +parse-success+ +parse-fail+ +parse-expect+ +parse-unexpect+

	    describe-parse-failure-reason
	    )
    (import (rnrs)
	    (rename (only (srfi :1) reverse! alist-cons)
		    (alist-cons acons))
	    (srfi :13 strings)
	    (srfi :127 lseqs))

(define-record-type parse-state)
(define-record-type parse-success (parent parse-state))
(define-record-type parse-fail    (parent parse-state))
(define-record-type parse-expect  (parent parse-state))
(define-record-type parse-unexpect  (parent parse-state))

(define (describe-parse-failure-reason reason)
  (define (rec reason)
    (cond ((pair? reason)
	   ;; we handle common case here
	   ;; (state (state (state ...)))
	   (cond ((and (pair? (car reason)) (parse-state? (caar reason)))
		  (let ((f (rec (cdar reason)))
			(e (rec (cdr reason))))
		    (cond ((string-null? f) e)
			  ((string-null? e) f)
			  (else (string-append f "\n" e)))))
		 (else
		  (let ((f (rec (car reason)))
			(e (rec (cdr reason))))
		    (cond ((string-null? f) e)
			  ((string-null? e) f)
			  (else (string-append f " " e)))))))
	  ((parse-state? reason) "")
	  ((null? reason) "")
	  ((symbol? reason) (symbol->string reason))
	  ((number? reason) (number->string reason))
	  (else
	   (let-values (((out extract) (open-string-output-port)))
	     (write reason out)
	     (extract)))))
  (if (pair? reason)
      (string-join (map rec reason) "\n")
      reason))

(define +parse-success+ (make-parse-success))
(define +parse-fail+    (make-parse-fail))
(define +parse-expect+  (make-parse-expect))
(define +parse-unexpect+  (make-parse-expect))

(define-syntax return-result
  (syntax-rules ()
    ((_ v l) (values +parse-success+ v l))))
(define-syntax return-failure
  (syntax-rules ()
    ((_ m l) (values +parse-fail+ m l))))
(define-syntax return-expect
  (syntax-rules ()
    ((_ m l) (values +parse-expect+ m l))))
(define-syntax return-unexpect
  (syntax-rules ()
    ((_ m l) (values +parse-unexpect+ m l))))

(define $$return
  (case-lambda
   ((v) (lambda (l) (values +parse-success+ v l)))
   ((v state) (lambda (l) (values state v l)))
   ((v state l) (lambda (_) (values state v l)))))
(define-syntax $return
  (lambda (x)
    (syntax-case x ()
      ((_ v) #'(lambda (l) (values +parse-success+ v l)))
      ((_ v state) #'(lambda (l) (values state v l)))
      ((_ v state l) #'(lambda (_) (values state v l)))
      (k (identifier? #'k) #'$$return))))

(define ($expect msg)
  (lambda (l)
    (return-expect msg l)))

(define ($fail msg)
  (lambda (l)
    (return-failure msg l)))

(define ($not parser)
  (lambda (l)
    (let-values (((s v nl) (parser l)))
      (if (parse-success? s)
	  (return-unexpect v l)
	  (return-result #f nl)))))

(define (expected expect v l)
  (if (string? expect)
      (return-expect expect l)
      (return-expect `((Expected . ,expect) (got . ,v)) l)))

(define ($$satisfy pred . maybe-expect)
  (define expect (if (null? maybe-expect) pred (car maybe-expect)))
  (lambda (l)
    (if (null? l)
	(expected expect 'eof l)
	(let ((v (lseq-car l)))
	  (if (pred v)
	      (return-result v (lseq-cdr l))
	      (expected expect v l))))))
(define-syntax $satisfy
  (lambda (x)
    (syntax-case x ()
      ((_ pred) #'($satisfy pred 'pred))
      ((_ pred expect)
       #'(lambda (l)
	   (if (null? l)
	       (expected expect 'eof l)
	       (let ((v (lseq-car l)))
		 (if (pred v)
		     (return-result v (lseq-cdr l))
		     (expected expect v l))))))
      (k (identifier? #'k) #'$$satisfy))))

(define $eof
  (lambda (l)
    (if (null? l)
	(return-result '() l)
	(return-expect "EOF" l))))
;; the same as ($not $eof)) but better performance
(define $any
  (lambda (l)
    (if (null? l)
	(return-unexpect "EOF" l)
	(return-result (lseq-car l) (lseq-cdr l)))))

;; empty match.
;; epsilon in context of BNF term.
(define ($empty v) (lambda (l) (return-result v l)))

;; ordered choice
(define ($or . expr)
  (define (fail rs l)
    (define (merge-failures rs l) )
    (cond ((null? rs) (return-failure "Empty expression" l))
	  ;; one error
	  ((null? (cdr rs)) (values (caar rs) (cdar rs) l))
	  ;; return the first state and the rest
	  (else (values (caar rs) rs l))))
  (lambda (l)
    (let loop ((e* expr) (rs '()))
      (if (null? e*)
	  (fail rs l)
	  (let-values (((s v nl) ((car e*) l)))
	    (cond ((parse-success? s) (return-result v nl))
		  ((parse-fail? s) (values s v l))
		  (else (loop (cdr e*) (acons s v rs)))))))))

;; greedy option and repetition
(define ($many parser . count)
  (define at-least (if (null? count) 0 (car count)))
  (define at-most (if (or (null? count) (null? (cdr count)))
		      +inf.0
		      (cadr count)))
  (when (> at-least at-most)
    (assertion-violation '$many "invalid at-least and at-most"
			 at-least at-most))
  (lambda (ol)
    (let loop ((vs '()) (l ol) (count 0))
      (if (>= count at-most)
	  (return-result (reverse! vs) l)
	  (let-values (((s v nl) (parser l)))
	    (cond ((parse-success? s)
		   (loop (cons v vs) nl (+ count 1)))
		  ((<= at-least count)
		   (return-result (reverse! vs) l))
		  ;; return original input
		  (else (values s v ol))))))))

(define ($peek parser)
  (lambda (l)
    (let-values (((s v ignore) (parser l)))
      (if (parse-success? s)
	  (values s v l)
	  (return-expect parser l)))))

;; derived?
(define ($seq . expr)
  (lambda (l)
    (let loop ((e* expr) (nl l))
      (if (null? e*)
	  (return-result #f nl)
	  (let-values (((s v nl2) ((car e*) nl)))
	    (if (parse-success? s)
		(if (null? (cdr e*))
		    (values s v nl2)
		    (loop (cdr e*) nl2))
		(values s v l)))))))

(define ($debug parser)
  (lambda (s)
    (let-values (((r v s) (parser s)))
      (display "status : ") (write r) (newline)
      (display "result : ") (write v) (newline)
      (display "rest   : ") (write s) (newline)
      (values r v s))))

(define ($raw parser)
  (lambda (l)
    (let-values (((s v nl) (parser l)))
      (if (parse-success? s)
	  ;; okay just take entire content from `l` to `nl`
	  (do ((r '() (cons (lseq-car l) r)) (l l (lseq-cdr l)))
	      ((eq? l nl) (return-result (reverse! r) nl)))
	  (return-expect parser l)))))

)
	    
