;; -*- mode: scheme; coding: utf-8; -*-
;; This file is a part of Sagittarius Scheme system.
#!core
(library (sagittarius reader)
    (export define-reader-macro
	    define-dispatch-macro
	    get-macro-character
	    set-macro-character
	    make-dispatch-macro-character
	    get-dispatch-macro-character
	    set-dispatch-macro-character
	    read-delimited-list
	    delimited-char?

	    define-reader

	    ;; for error
	    raise-i/o-read-error
	    )
    (import (rnrs)
	    (core errors)
	    (sagittarius))

  (define-syntax define-reader-macro
    (syntax-rules ()
      ((_ c (name . args) body ...)
       (define-reader-macro :define name c (lambda args body ...)))
      ((_ name c proc)
       (define-reader-macro :define name c proc #f))
      ((_ name c proc non-term?)
       (define-reader-macro :define name c proc non-term?))
      ((k :define name c proc? non-term?)
       (define name 
	 (let ((proc proc?))
	   (unless (char? 'c)
	     (assertion-violation 'k
				  (format "character requireb but got ~s" 'c)))
	   (unless (procedure? proc)
	     (assertion-violation 'k
		  (format "procedure requireb but got ~s" proc)))
	   (%insert-macro-character c proc (current-library) non-term?)
	   proc)))))

  (define-syntax define-dispatch-macro
    (syntax-rules ()
      ((_ c sc (name . args) body ...)
       (define-dispatch-macro :define name c sc (lambda args body ...) #f))
      ((_ name c sc proc)
       (define-dispatch-macro :define name c sc proc #f))
      ((_ name c sc proc non-term?)
       (define-dispatch-macro :define name c sc proc non-term?))
      ((k :define name c sc proc? non-term?)
       (define name 
	 (let ((proc proc?))
	   ;; should we make this macro syntax-case and check this
	   ;; in syntax level?
	   (unless (and (char? 'c) (char? 'sc))
	     (assertion-violation 'k
		  (format "character requireb but got ~s and ~s" 'c 'sc)))
	   
	   (unless (procedure? proc)
	     (assertion-violation 'k
			  (format "procedure requireb but got ~s" proc)))
	   (%insert-dispatch-macro-character c sc proc (current-library)
					     non-term?)
	   proc)))))

  (define-syntax define-reader
    (syntax-rules ()
      ((_ (name port) expr ...)
       (define-reader name (lambda (port) expr ...)))
      ((_ name proc)
       (define name 
	 (let ((p proc))
	   (%library-reader-set! (current-library) proc)
	   p)))))
)
