;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/remote-repl.scm - remote REPL library.
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

#!nounbound
(library (sagittarius remote-repl input)
    (export :export-reader-macro
	    make-directive
	    directive?
	    directive-name)
    (import (rnrs)
	    (sagittarius)
	    (sagittarius reader))
  ;; internal record to detect if read datum is directive or not
  (define-record-type directive
    (fields name))
  ;; create reader macro
  (define hash-bang-reader (get-dispatch-macro-character #\# #\!))
  (define-dispatch-macro #\# #\!
    (remote-repl-hashbang-reader in subchar param ctx)
    (let ((name (read in)))
      (unless (symbol? name)
	;; FIXME this must be &lexical
	(assertion-violation 'hash-bang-reader "invalid #! expression" name))
      (make-directive (apply-directive! in name ctx))))

  )

(library (sagittarius remote-repl)
    (export connect-remote-repl
	    make-remote-repl
	    make-username&password-authenticate)
    (import (rnrs)
	    (rnrs eval)
	    (sagittarius)
	    (sagittarius interactive)
	    (sagittarius control)
	    (sagittarius object)
	    (sagittarius socket)
	    (sagittarius vm)
	    (sagittarius stty)
	    (sagittarius remote-repl input)
	    (sagittarius crypto digests)
	    (rfc tls)
	    (srfi :18 multithreading)
	    (srfi :26 cut)
	    (srfi :39 parameters)
	    (pp))
  (define (send-datum socket datum) (socket-send socket datum))

  (define (make-remote-repl-input-port source)
    (define (read! str start count)
      (let ((r (get-string-n! source str start count)))
	(if (eof-object? r)
	    0
	    r)))
    (define (close) #t) ;; do nothing
    (define (position) (port-position source))
    (define (set-position! p) (set-port-position! source p))
    (define pos? (port-has-port-position? source))
    (define set-pos? (port-has-set-port-position!? source))
    (make-custom-textual-input-port "remote-repl-port" read!
				    (and pos? position)
				    (and set-pos? set-position!)
				    close))
  
  ;; remote repl protocol
  ;; client
  ;;   send datum: (:datum <datum>)
  ;;   exit      : (:exit)
  ;;   directive : (:directive name)
  ;; server
  ;;   on auth   : (:request-authenticate) for authenticate
  ;;               (:response-authenticate <result>)
  ;;   on success: (:values <value 1> ... <value n>)
  ;;   on error  : (:error <expression> <message>)
  ;;   on exit   : (:exit)
  ;;   i/o       : (:stdout string) for standard output
  ;;             : (:stderr string) for standard error

  (define (pack-data tag . data)
    (let-values (((out extract) (open-bytevector-output-port)))
      (let ((o (transcoded-port out (native-transcoder))))
	(display #\( o)
	(write tag o)
	(for-each (^d (display #\space o) (write d o)) data)
	(display #\) o)
	(extract))))

  (define-syntax send-packed-data
    (syntax-rules ()
      ((_ socket tag data ...)
       (send-datum socket (pack-data tag data ...)))))

  (define (connect-remote-repl node service
			       :key (secure #f)
				    (authority #f)
				    (quiet #f))
    (define (make-evaluator socket)
      (lambda (form env)
	(if (directive? form)
	    (send-packed-data socket :directive (directive-name form))
	    (send-packed-data socket :datum form))))
    (define (handle-values v)
      (for-each (lambda (v) (display v) (newline)) v))
    (define (make-printer port)
      (lambda args
	(let loop ()
	  (define (check) (when (port-ready? port) (loop)))
	  (let* ((command (read/ss port)))
	    (unless (eof-object? command)
	      (case (car command)
		((:values) (handle-values (cdr command)) (check))
		((:error)
		 (format #t "error in: ~s~%" (cadr command))
		 (display (caddr command)) (newline)
		 (check))
		((:exit) (display ";; finished") (newline) (exit 0))
		 ;; I/O
		((:stdout)
		 (display (cadr command))
		 (flush-output-port (current-output-port))
		 (loop))
		((:stderr)
		 (display (cadr command) (current-error-port))
		 (flush-output-port (current-error-port))
		 (loop))
		(else 
		 (format #t "protocol error: unknown tag ~s: ~a\n"
			 (car command) (get-string-all port)))))))))
    (define (authenticate socket in/out)
      (define (wait-response)
	(let loop ()
	  (let ((command (read/ss in/out)))
	    (case (car command)
	      ((:response-authenticate) (cadr command))
	      ((:alert) (display (cadr command)) (newline) (loop))
	      ((:prompt :prompt-noecho) =>
	       (lambda (type)
		 ;; TODO should we use read/ss? get-line seems good enough
		 (define (read-input type)
		   (case type
		     ((:prompt-noecho)
		      (let ((r (with-stty '(not echo) (lambda () (read/ss)))))
			;; make a bit nicer
			(newline (current-output-port))
			r))
		     (else (read/ss))))
		 (display (cadr command))
		 (flush-output-port)
		 (send-packed-data socket :user-input (read-input type))
		 (loop)))))))
      (let ((command (read/ss in/out)))
	(case (car command)
	  ((:request-authenticate) (wait-response))
	  ((:no-authenticate) #t))))
    (call-with-socket (if secure
			  (make-client-tls-socket node service
						  :certificates
						  (if authority
						      (list authority)
						      '()))
			  (make-client-socket node service))
      (lambda (socket)
	(define in/out (transcoded-port (socket-port socket #f) 
					(native-transcoder)))
	(let-values (((name ip port) (socket-info-values socket)))
	  (unless quiet
	    (format #t "~%connect: ~a:~a (enter ^D to exit) ~%~%"
		    name port)))
	(when (authenticate socket in/out)
	  (parameterize ((current-evaluator (make-evaluator socket))
			 (current-printer   (make-printer in/out))
			 (current-exit (lambda ()
					 (send-packed-data socket :exit)))
			 (current-input-port (make-remote-repl-input-port
					      (current-input-port))))
	    (apply-directive! (current-input-port)
			      'read-macro=sagittarius/remote-repl/input
			      (make-read-context :change-mode #t))
	    (read-eval-print-loop #f)))))
    (unless quiet (format #t "~%[exit]~%"))
    (flush-output-port))

  ;; TODO what should we get?
  (define (make-username&password-authenticate username password
					       :key (digest *digest:sha-1*)
						    (max-try 3))
    (define md (make-message-digest digest))
    (let* ((bv (string->utf8 (string-append username "&" password)))
	   (credential (digest-message md bv)))
      (lambda (socket)
	(define port (transcoded-port (socket-port socket #f) 
				      (native-transcoder)))
	(define (read-response socket)
	  (let ((command (read/ss port)))
	    (case (car command)
	      ((:user-input) (cadr command))
	      (else ;; how should we handle?
	       => (^r
		   (error 'username&password-authenticate
			  "invalid response" r))))))
	(let loop ((count 1))
	  (send-packed-data socket :prompt "username> ")
	  (let1 input-username (read-response socket)
	    (send-packed-data socket :prompt-noecho "password> ")
	    (let* ((input-password (read-response socket))
		   (input (string->utf8 
			   (string-append (->string input-username) "&"
					  (->string input-password))))
		   (h (digest-message md input)))
	      (cond ((bytevector=? credential h))
		    ((= count max-try)
		     ;; TODO log it
		     (send-packed-data socket :alert "login failed, disconnect")
		     #f)
		    (else (loop (+ count 1))))))))))
		    
  (define (make-remote-repl service :key (secure #f)
			    (authenticate #f)
			    (certificates '())
			    (private-key #f)
			    (authority #f)
			    (environment (find-library 'user #t))
			    (log (current-output-port)))
    (define-syntax logging
      (syntax-rules ()
	((_ socket fmt args ...)
	 (when log
	   (let-values (((name ip port) (socket-info-values socket)))
	     (format log (string-append "remote-repl: [~a(~a):~a] " fmt "~%")
		     name (ip-address->string ip) port args ...))))))
    (define (detach socket)
      (define in/out (transcoded-port (socket-port socket #f) 
				      (native-transcoder)))
      (define *context* (make-read-context :change-mode #t))
      (define (main-loop in out err)
	(define (set-ports! p out err)
	  (current-input-port p)
	  (current-output-port out)
	  (current-error-port err))
	(define (restore-ports!)
	  (current-input-port in)
	  (current-output-port out)
	  (current-error-port err))
	(define (data->string r) (map (^e (format "~s" e)) r))
	(let ((stop? #f) (current-expression #f))
	  (logging socket "connect")
	  (let loop ()
	    (call/cc
	     (lambda (continue)
	       (with-exception-handler
		(lambda (c)
		  (restore-ports!)
		  (logging socket "error: ~a" (if (message-condition? c)
						  (condition-message c)
						  c))
		  ;; socket connection
		  (let-values (((out extract) (open-string-output-port)))
		    (report-error c out)
		    (send-packed-data socket 
		      :error (format "~s" current-expression) (extract)))
		  (and (serious-condition? c) (continue)))
		(lambda ()
		  (let ((command (read/ss in/out)))
		    (case (car command)
		      ;; we need to handle both keyword and symbol
		      ;; since directive can be evaluated on the
		      ;; socket port. (c.f. #!r6rs invalidates keyword
		      ;; reading)
		      ((:directive |:directive|)
		       (let ((n (cadr command)))
			 (apply-directive! in/out n *context*)
			 (send-datum socket (pack-data :values))))
		      ((:datum |:datum|)
		       (let ((e (cadr command))
			     (p in/out))
			 (let-values (((out extract)
				       (open-string-output-port))
				      ((err eextract)
				       (open-string-output-port)))
			   (set-ports! p out err)
			   (set! current-expression e)
			   (if (eof-object? e)
			       (set! stop? #t)
			       (receive r (eval e environment)
				 (let ((output (extract))
				       (errout (eextract)))
				   (unless (zero? (string-length output))
				     (send-datum socket
						 (pack-data :stdout output)))
				   (unless (zero? (string-length errout))
				     (send-datum socket
						 (pack-data :stderr errout))))
				 (send-datum 
				  socket
				  (apply pack-data
					 :values (data->string r))))))))
		      ((:exit |:exit|)
		       (logging socket "disconnect")
		       (send-packed-data socket :exit)
		       (set! stop? #t))
		      (else 
		       => (^t
			   ;; invalid protocol
			   (send-packed-data 
			    socket :error "[internal]"
			    (format "unknown tag ~s" t))))))))))
	    (restore-ports!)
	    (unless stop? (loop)))))
      (define (do-authenticate socket)
	(if authenticate
	    ;; send authenticate request
	    (begin
	      (send-packed-data socket :request-authenticate)
	      (let1 r (authenticate socket)
		(send-packed-data socket :response-authenticate r)
		r))
	    (send-packed-data socket :no-authenticate)))
	
      (lambda ()
	(when secure
	  (logging socket "TLS handshake")
	  (with-exception-handler
	   (lambda (c)
	     (logging socket "TLS handshake failed!"))
	   (lambda () (tls-server-handshake socket))))
	(call-with-socket socket
	  (lambda (socket)
	    (let ((in    (current-input-port))
		  (out   (current-output-port))
		  (err   (current-error-port)))
	      (when (do-authenticate socket)
		(main-loop in out err)))))))
    (let1 server (if secure
		     (make-server-tls-socket service certificates
					     :private-key private-key
					     :authorities (list authority))
		     (make-server-socket service))
      (format log "~%remote-repl: ~a~%" (socket-name server))
      (values 
       (lambda ()
	(let loop ()
	  (let1 socket (socket-accept server :handshake #f)
	    (logging socket "accept")
	    (thread-start! (make-thread (detach socket)))
	    (loop))))
       server)))

)
