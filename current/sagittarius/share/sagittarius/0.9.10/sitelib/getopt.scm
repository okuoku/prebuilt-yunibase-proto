;;; -*- mode: scheme; coding: utf-8; -*-
;;;
;;; getopt.scm - thin wrapper of SRFI-37.
;;;  
;;;   Copyright (c) 2010-2013  Takashi Kato  <ktakashi@ymail.com>
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

(library (getopt)
    (export with-args)
    (import (rnrs) 
	    (sagittarius)
	    (srfi :37 args-fold))

  (define (pack-args name args)
    (define (finish r) (if (null? r) #f (cons #f r)))
    (let loop ((args args) (r '()))
      (cond ((null? args) (finish r))
	    ((member name args 
		     (lambda (name slot)
		       (and (pair? slot) (eq? (car slot) name))))
	     => (lambda (args)
		  (loop (cdr args) (cons (cdar args) r))))
	    (else (finish r)))))

  (define-syntax with-args
    (lambda (x)
      (define (bindings&options opts args ro rb)
	(define (next name short long default assq req?)
	  (let ((short (or (syntax->datum short) 
			   (datum->syntax name #\null))))
	    (bindings&options
	     (cdr opts) args
	     (cons #`(option '(#,short #,long) #,req? #f 
			     (lambda (opt n arg alist vs)
			       (values (acons '#,name (if #,req? arg #t) alist)
				       vs)))
		   ro)
	     (cons #`(#,name (cond ((#,assq '#,name #,args) => cdr)
				   (else #,default))) rb))))
	(syntax-case opts (*)
	  (((name (short long) * default) . rest)
	   (next #'name #'short #'long #'default #'pack-args #'#t))
	  (((name (short long) req? default) . rest)
	   (next #'name #'short #'long #'default #'assq #'req?))
	  (rest (identifier? #'rest)
		(list (cons (list #'rest #'(reverse! rest)) rb)
		      (cons #'list (reverse! ro)) #'rest #t))
	  (() 
	   (with-syntax (((dummy) (generate-temporaries '(dummy))))
	     (list (cons (list #'dummy #'(reverse! dummy)) rb)
		   (cons #'list (reverse! ro)) #'dummy #f)))
	  (_ (syntax-violation 'with-args "malformed option spec"
			       (syntax->datum 'opts)))))
      (syntax-case x ()
	((_ args opts body ...)
	 (with-syntax (((alist) (generate-temporaries '(alist))))
	   (with-syntax ((((bindings ...) options rest allow?)
			  (bindings&options #'opts #'alist '() '())))
	     #'(let-values (((alist rest)
			     (args-fold args options
			      (lambda (opt name arg . argv)
				(define (fixup-args n arg)
				  (let ((n (if (char? n)
					       (format "-~a" n)
					       (string-append "--" n))))
				    (if arg
					(list n arg)
					(list n))))
				(if allow?
				    (values (car argv)
					    `(,@(fixup-args name arg)
					      ,@(cadr argv)))
				    (assertion-violation 'with-args
				     "Unknown option" name)))
			      (lambda (operand alist argv)
				(values alist (cons operand argv)))
			      '() '())))
		 (let (bindings ...)
		   body ...))))))))

#|
(define-syntax with-args
  (er-macro-transformer
   (lambda (form rename _)
     (let ((_let-values (rename 'let-values)) (_args-fold (rename 'args-fold))
	   (_lambda (rename 'lambda))
	   (_assertion-violation (rename 'assertion-violation))
	   (_values (rename 'values)) (_option (rename 'option))
	   (_acons  (rename 'acons))  (_if    (rename 'if))
	   (_let    (rename 'let))    (_assq  (rename 'assq))
	   (_cdr    (rename 'cdr))    (_quote (rename 'quote))
	   (_cond   (rename 'cond))   (_=>    (rename '=>))
	   (_else   (rename 'else))   (_cons  (rename 'cons))
	   (_reverse! (rename 'reverse!)))
       (define (construct-options options)
	 (let loop ((options options)
		    (acc '()))
	   (match options
	     (((name (short long) req? default) . rest)
	      (loop (cdr options)
		    (cons `(,_option '(,short ,long) ,req? #f
				   (,_lambda (opt n arg alist vs)
				     (,_values (,_acons (,_quote ,name)
							(,_if ,req? arg #t)
						    alist) vs))) acc)))
	     ((? variable? rest) (values (list (reverse! acc)) rest))
	     (() (values (list (reverse! acc)) #f))
	     (_ (syntax-violation 'with-args
				  "malformed option spec" options)))))

       (define (construct-binding options rest args)
	 (let loop ((options options)
		    (acc '()))
	   (match options
	     (((name (short long) req? default) . rest)
	      (loop (cdr options)
		    (cons `(,name (,_cond ((,_assq (,_quote ,name) ,args)
					   ,_=> ,_cdr)
					  (,_else ,default))) acc)))
	     (_ (reverse! (cons (list rest `(,_reverse! ,rest)) acc)))))
	 )
       (match form
	 ((_ args opts . body)
	  (let*-values (((options rest) (construct-options opts))
			((_alist) (gensym "alist"))
			((_argv)  (if rest rest (gensym "argv"))))
	    `(,_let-values (((,_alist ,_argv)
			    (,_args-fold ,args
				      ,(fold-left cons 'list options)
				      (,_lambda (opt name arg . _)
					(,_assertion-violation "with-args"
					  "Unknown option" name))
				      (,_lambda (operand alist argv)
					(,_values alist (,_cons operand argv)))
				      '() '())))
	       (,_let (,@(construct-binding opts _argv _alist))
		 ,@body))))
	 (_
	  (syntax-violation 'with-args
			    "malformed with-args" form)))))))
|#
)
