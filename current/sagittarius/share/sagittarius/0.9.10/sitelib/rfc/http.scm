;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/http.scm - HTTP protocol library.
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

#!read-macro=sagittarius/regex
#!read-macro=sagittarius/bv-string
#!nounbound
(library (rfc http)
    (export &http-error
	    http-error?

	    ;; connection
	    http-connection?
	    make-http-connection

	    *http-user-agent*
	    http-compose-query
	    http-compose-form-data

	    http-get
	    http-head
	    http-post
	    http-put
	    http-delete
	    http-request

	    ;; senders
	    http-debug-sender ;; better than nothing
	    http-null-sender
	    http-multipart-sender
	    http-blob-sender
	    http-string-sender
	    ;; receiver
	    http-string-receiver
	    http-binary-receiver
	    http-null-receiver
	    http-oport-receiver
	    http-file-receiver
	    http-gzip-receiver
	    http-cond-receiver

	    *http-default-redirect-handler*
	    ;; for convenience
	    http-lookup-auth-handler
	    url-server&path
	    (rename (options->request-headers list->request-headers))
	    )
    (import (except (rnrs) put-string get-line)
	    (sagittarius)
	    (sagittarius regex)
	    (sagittarius socket)
	    (sagittarius control)
	    (sagittarius crypto random)
	    (sagittarius crypto digests)
	    (clos user)
	    (srfi :1 lists)
	    (srfi :2 and-let*)
	    (srfi :13 strings)
	    (srfi :14 char-sets)
	    (srfi :19 time)
	    (srfi :39 parameters)
	    (match)
	    (encoding decoder)
	    (util list)
	    (util port)
	    (util file)
	    (util bytevector)
	    (rfc :5322)
	    (rfc uri)
	    (rfc mime)
	    (rfc base64)
	    (rfc tls)
	    (rfc gzip)
	    (rfc cookie)
	    (prefix (binary io) binary:)
	    (util bytevector))

  ;; for my sake
  (define (put-string out m)
    (put-bytevector out (string->utf8 m)))
  (define (binary:format out . rest)
    (put-bytevector out (string->utf8 (apply format rest))))
  (define (get-line in)
    ;; \r\n would be \r for binary:get-line so trim it
    (bytevector-trim-right (binary:get-line in) '(#x0d)))

  (define-condition-type &http-error &error
    make-http-error http-error?)
  
  (define (raise-http-error who msg . irritants)
    (raise (apply condition
		  (filter values
			  (list (make-http-error)
				(and who (make-who-condition who))
				(make-message-condition msg)
				(make-irritants-condition irritants))))))

  (define-class <http-connection> ()
    (;; server[:port]
     (server :init-keyword :server :init-value #f
	     :reader http-connection-server
	     :writer http-connection-server-set!)
     ;; A socket for persistent connection.
     (socket :init-keyword :socket :init-value #f
	     :reader http-connection-socket)
     (proxy  :init-keyword :proxy :init-value #f
	     :reader http-connection-proxy
	     :writer http-connection-proxy-set!)
     (extra-headers :init-keyword :extra-headers :init-value '()
		    :reader http-connection-extra-headers
		    :writer http-connection-extra-headers-set!)
     (secure :init-keyword :secure :init-value #f
	     :reader http-connection-secure
	     :writer http-connection-secure-set!)
     (auth-handler :init-keyword :auth-handler :init-value #f
		   :reader http-connection-auth-handler
		   :writer http-connection-auth-handler-set!)
     (auth-user :init-keyword :auth-user :init-value #f
		:reader http-connection-auth-user
		:writer http-connection-auth-user-set!)
     (auth-password :init-keyword :auth-password :init-value #f
		    :reader http-connection-auth-password
		    :writer http-connection-auth-password-set!)
     (cookie-jar :init-keyword :cookie-jar :init-value #f
		 :reader http-connection-cookie-jar
		 :writer http-connection-cookie-jar-set!)))
  (define (make-http-connection server . args)
    (apply make <http-connection> :server server args))
  (define (http-connection? o) (is-a? o <http-connection>))


  (define *http-user-agent* (make-parameter (format "sagittarius.http/~a"
						  (sagittarius-version))))
  ;; redirect
  (define (redirect conn proto new-server)
    (let1 orig-server (http-connection-server conn)
      (unless (and (string=? orig-server new-server)
		   (eq? (http-connection-secure conn) (equal? proto "https")))
	(http-connection-server-set! conn new-server)
	(http-connection-secure-set! conn (equal? proto "https"))))
    conn)

  (define *http-default-redirect-handler*
    (make-parameter
     (lambda (method code headers body)
       (and-let* ((loc (rfc5322-header-ref headers "location")))
	 (case (string->number code)
	   ((300 301 305 307)
	    (case method ((GET HEAD) `(,method . ,loc)) (else #f)))
	   ((302 303)
	    (case method
	      ((GET HEAD) `(,method . ,loc))
	      (else `(GET . ,loc))))
	   (else #f))))))
  ;; 
  (define (http-request method server request-uri
			:key (host #f)
			     (redirect-handler #t)
			     (no-redirect #f)
			     auth-handler
			     auth-user
			     auth-password
			     proxy
			     extra-headers
			     (user-agent (*http-user-agent*))
			     secure
			     (receiver (http-string-receiver))
			     (sender #f)
			     cookie-jar
			     (trace #f) ;; for debug but hmmmm
			     (enc :request-encoding 'utf-8)
			:allow-other-keys opts)
    (let1 conn (ensure-connection server auth-handler auth-user auth-password
				  proxy secure extra-headers cookie-jar)
      (request-response
       method conn host (ensure-request-uri request-uri enc)
       sender receiver `(:user-agent ,user-agent ,@opts)
       '() no-redirect redirect-handler enc trace)))

  (define (server->socket server port make-socket)
    (cond ((matches #/([^:]+):(\d+)/ server)
	   => (lambda (m) (make-socket (m 1) (m 2))))
	  (else (make-socket server port))))

  (define (make-trace-port trace port)
    (define (read! bv start count)
      (let ((n (get-bytevector-n! port bv start count)))
	(unless (eof-object? n) (put-bytevector trace bv start n))
	(if (eof-object? n) 0 n)))
    (define (write! bv start count)
      (put-bytevector trace bv start count)
      (put-bytevector port bv start count)
      count)
    (define (close) (close-port port))
    (if (and (output-port? trace) (binary-port? trace))
	(make-custom-binary-input/output-port "trace-port"
					      read! write! #f #f close)
	port))
  (define (with-connection conn proc trace)
    (let* ((secure? (http-connection-secure conn))
	   (make-socket (if secure? make-client-tls-socket make-client-socket))
	   (port (if secure? "443" "80")))
      (let* ((s (server->socket (or (http-connection-proxy conn)
				    (http-connection-server conn))
				port make-socket))
	     (p (buffered-port (make-trace-port trace (socket-port s #f))
			       (buffer-mode block))))
	(unwind-protect
	    (proc p (http-connection-extra-headers conn))
	  (close-port p)
	  (socket-shutdown s SHUT_RDWR)
	  (socket-close s)))))

  (define (request-response method conn host request-uri
			    sender receiver options 
			    history no-redirect redirect-handler enc trace
			    :optional (auth-header '()))

    (define no-body-replies '("204" "304"))
    (receive (host uri)
	(consider-proxy conn (or host (http-connection-server conn))
			request-uri)
      (define (connection-info)
	`((host ,host)
	  (method ,method)
	  (uri  ,uri)
	  (request-uri ,request-uri)))
      ;; redirect
      (define (handle-redirect in/out code headers)
	(and-let* (( (not no-redirect) )
		   ( (string-prefix? "3" code) )
		   (h (case redirect-handler
			((#t) (*http-default-redirect-handler*))
			((#f) #f)
			(else redirect-handler)))
		   (r (h method code headers
			 (receive-body in/out code headers
				       (http-string-receiver))))
		   (method (car r))
		   (loc (cdr r)))
	  (receive (uri proto new-server path*)
	      (canonical-uri conn loc (http-connection-server conn))
	    (when (or (member uri history)
		      (> (length history) 20))
	      (raise-http-error 'http-request
				(format "redirection is looping via ~a"
					uri)
				(http-connection-server conn)))
	    (request-response
	     method conn
	     (http-connection-server (redirect conn proto new-server))
	     path* sender receiver options
	     (cons uri history) no-redirect redirect-handler enc trace))))

      ;; authentication
      (define (handle-authentication in/out code headers 
				     auth-handler auth-user auth-password)
	(and-let* (( (string=? code "401") )
		   ;; handler must call body thunk explicitly
		   (body (lambda ()
			   (receive-body in/out code headers 
					 (http-binary-receiver))))
		   (auth-headers
		    (cond (auth-handler
			   ;; never use
			   (http-connection-auth-handler-set! conn #f)
			   (auth-handler (connection-info) 
					 auth-user auth-password headers body))
			  ((and auth-user auth-password
				(http-lookup-auth-handler headers))
			   => (lambda (handler)
				;; reset
				(http-connection-auth-user-set! conn #f)
				(http-connection-auth-password-set! conn #f)
				(handler (connection-info) 
					 auth-user auth-password headers body)))
			  (else #f))))
	  (request-response method conn host request-uri
			    sender receiver options
			    history no-redirect redirect-handler enc trace
			    auth-headers)))

      (let ((auth-handler  (http-connection-auth-handler conn))
	    (auth-user     (http-connection-auth-user conn))
	    (auth-password (http-connection-auth-password conn)))
	(with-connection conn
	 (lambda (in/out ext-header)
	   (send-request in/out method host uri sender ext-header
			 (add-cookie-header conn request-uri options)
			 enc auth-header)
	   (flush-output-port in/out)
	   (receive (code headers) (receive-header in/out)
	     (add-to-cookie-jar conn headers)
	     (or (handle-redirect in/out code headers)
		 (handle-authentication in/out code headers 
					auth-handler auth-user auth-password)
		 (values code headers
			 (and (not (eq? method 'HEAD))
			      (not (member code no-body-replies))
			      (receive-body in/out code 
					    headers receiver))))))
	 trace))))

  (define (add-to-cookie-jar conn headers)
    (define cookie-jar (http-connection-cookie-jar conn))
    (define (add-cookie header)
      (when (string=? "set-cookie" (car header))
	(let ((cookie (parse-cookie-string (cadr header))))
	  (cookie-jar-add-cookie! cookie-jar cookie))))
    (when cookie-jar (for-each add-cookie headers)))
  
  (define path-component-set
    (char-set-difference char-set:ascii char-set:iso-control (char-set #\/)))
  (define (add-cookie-header conn uri options)
    (define cookie-jar (http-connection-cookie-jar conn))
    (define (add-them conn cookie-jar options)
      (define secure (http-connection-secure conn))
      (let-values (((h path q f) (uri-decompose-hierarchical uri)))
	(define (predicate cookie)
	  (define (match-secure cookie)
	    (if (cookie-secure? cookie) secure #t))
	  (define (match-path cookie)
	    (define cpath (cookie-path cookie))
	    (if cpath
		(let loop ((cp* (string-tokenize cpath path-component-set))
			   (pp* (string-tokenize path path-component-set)))
		  (cond ((null? cp*) #t)
			((null? pp*) #f)
			((string=? (car cp*) (car pp*))
			 (loop (cdr cp*) (cdr pp*)))
			(else #f)))
		#t))
	  ;; TODO how to handle max-age?
	  (define (not-expired cookie)
	    (define expires (cookie-expires cookie))
	    (if expires
		(time<? (current-time) (date->time-utc expires))
		#t))
	  (and (not-expired cookie)
	       (match-secure cookie)
	       (match-path cookie)))
	(let ((cookies (cookie-jar->cookies cookie-jar predicate)))
	  (if (null? cookies)
	      options
	      `(:cookie ,(cookies->string cookies) ,@options)))))
    (if cookie-jar
	(add-them conn cookie-jar options)
	options))
    
  (define (canonical-uri conn uri host)
    (let*-values (((scheme specific) (uri-scheme&specific uri))
		  ((h p q f) (uri-decompose-hierarchical specific)))
      (let ((scheme (or scheme (if (http-connection-secure conn)
				   "https" "http")))
	    (host (or h host)))
	(values (uri-compose :scheme scheme :host host
			     :path p :query q :fragment f)
		scheme
		host
		;; drop "//"
		(string-drop (uri-compose :path p :query q :fragment f) 2)))))
  
  (define (consider-proxy conn host uri)
    (if (http-connection-proxy conn)
	(values host (uri-compose :scheme "http"
				  :host (http-connection-secure conn)
				  :path* uri))
	(values host uri)))

  ;; send
  (define (send-request out method host uri sender ext-headers options enc
			auth-headers)
    (binary:format out "~a ~a HTTP/1.1\r\n" method uri)
    (if (procedure? sender)
	(sender (options->request-headers `(:host ,host ,@options)) enc
		(lambda (hdrs)
		  (send-headers hdrs out ext-headers auth-headers)
		  (let ((chunked? (equal? (rfc5322-header-ref
					   hdrs "transfer-encoding")
					  "chunked")))
		    (lambda (size)
		      (when chunked? (binary:format out "~x\r\n" size))
		      (flush-output-port out)
		      out))))
	(send-headers (options->request-headers `(:host ,host ,@options)) out
		      ext-headers auth-headers)))

  (define (send-headers hdrs out ext-header auth-headers)
    (define (send hdrs)
      (for-each (lambda (hdr)
		  (binary:format out "~a: ~a\r\n" (car hdr) (cadr hdr)))
		hdrs))
    (send hdrs)
    (send auth-headers)
    (send ext-header)
    (put-string out "\r\n")
    (flush-output-port out))

  (define (options->request-headers options)
    (let loop ((options options) (r '()))
      (if (or (null? options) (null? (cdr options)))
	  (reverse r)
	  (loop (cddr options)
		`((,(format "~a" (car options)) 
		   ,(format "~a" (cadr options))) ,@r)))))


  ;; receive
  (define (receive-header remote)
    (receive (code reason) (parse-status-line (utf8->string (get-line remote)))
      (values code (rfc5322-read-headers remote))))

  (define (parse-status-line line)
    (cond ((eof-object? line)
	   (raise-http-error 'parse-status-line
			     "http reply contains no data"))
	  ((looking-at #/[\w\/.]+\s+(\d\d\d)\s+(.*)/ line)
	   => (lambda (m) (values (m 1) (m 2))))
	  (else (raise-http-error 'parse-status-line
				  "bad reply from server"
				  line))))

  (define (receive-body remote code headers receiver)
    (let1 total (and-let* ((p (assoc "content-length" headers)))
		  (string->number (cadr p)))
      (cond ((assoc "transfer-encoding" headers)
	     => (lambda (p)
		  (if (equal? (cadr p) "chunked")
		      (receive-body-chunked remote code headers total receiver)
		      (raise-http-error 'receive-body
					"unsupported transfer-encoding"
					(cadr p)))))
	    (else (receive-body-once remote code headers total receiver)))))

  (define (receive-body-once remote code headers total receiver)
    (let1 rest total
      (define (callback)
	(if (equal? rest 0)
	    (values remote 0)
	    (begin (set! rest 0) (values remote total))))
      (receiver code headers total callback)))

  (define (receive-body-chunked remote code headers total receiver)
    (define chunk-size #f)
    (define condition #f)
    (define (callback)
      (if (equal? chunk-size 0)
	  (values remote 0)
	  ;; If we get an error during receiving from the server, we need
	  ;; to return -1 to give the chance to the receiver to clean up
	  ;; things. After the receiver returns we reraise the condition.
	  (guard (e (else (set! condition e) (values remote -1)))
	    ;; If we've already handled some chunks, we need to skip
	    ;; the tailing CRLF of the previous chunk
	    (when chunk-size (get-line remote))
	    (let1 line (get-line remote)
	      (when (eof-object? line)
		(raise-http-error 'receive-body-chunked
				  "chunked body ended prematurely"))
	      (cond ((#/^([0-9a-fA-F]+)/ (utf8->string line))
		     => (lambda (m)
			  (let1 digits (m 1)
			    (set! chunk-size (string->number digits 16))
			    (if (zero? chunk-size)
				(do ((line (get-line remote) (get-line remote)))
				    ((or (eof-object? line)
					 (zero? (bytevector-length line)))
				     (values remote 0)))
				(values remote chunk-size)))))
		    (else
		     ;; something is wrong
		     (raise-http-error 'receive-body-chunked
				       "bad line in chunked data"
				       line)))))))
    (begin0 (receiver code headers total callback)
      (when condition (raise condition))))

	
  (define (lookup-encoding hdrs)
    (or (and-let* ((c (rfc5322-header-ref hdrs "content-type"))
		   ( c )
		   (params (mime-parse-content-type c))
		   ( (list? (cddr params)) )
		   (attr (cddr params))
		   (charset (assoc "charset" attr))
		   ( charset ))
	  (lookup-decoder (cdr charset)))
	(utf-8-codec)))

  ;; pre-defined receivers
  (define (ensured-copy sink remote size)
    (let loop ((size size))
      (let ((n (copy-binary-port sink remote :size size)))
	(when (and size (not (= n size)))
	  (loop (- size n))))))
      
  (define (http-string-receiver)
    (lambda (code hdrs total retr)
      (let loop ((sink (open-output-bytevector)))
	(receive (remote size) (retr)
	  (cond ((eqv? size 0)
		 (bytevector->string (get-output-bytevector sink)
				     (make-transcoder (lookup-encoding hdrs))))
		((or (not size) (> size 0))
		 (ensured-copy sink remote size)
		 (loop sink)))))))

  (define (http-binary-receiver)
    (lambda (code hdrs total retr)
      (let loop ((sink (open-output-bytevector)))
	(receive (remote size) (retr)
	  (cond ((eqv? size 0) (get-output-bytevector sink))
		((or (not size) (> size 0))
		 (ensured-copy sink remote size)
		 (loop sink)))))))

  (define (http-null-receiver)
    (lambda (code hdrs total retr)
      (let loop ((sink (open-file-output-port (null-device)
					      (file-options no-fail))))
	(receive (remote size) (retr)
	  (cond ((and size (<= size 0)) (close-output-port sink))
		(else (ensured-copy sink remote size)
		      (loop sink)))))))

  ;; sink must be binary-port
  (define (http-oport-receiver sink flusher)
    (check-arg binary-port? sink 'http-oport-receiver)
    (lambda (code hdrs total retr)
      (let loop ()
	(receive (remote size) (retr)
	  (cond ((and size (<= size 0)) (flusher sink hdrs))
		(else
		 (ensured-copy sink remote size)
		 (loop)))))))

  (define (http-file-receiver filename :key (temporary? #f))
    (lambda (code hdrs total retr)
      (receive (port tmpname) (make-temporary-file filename)
	(let loop ()
	  (receive (remote size) (retr)
	    (cond ((or (not size) (> size 0))
		   (ensured-copy port remote size) (loop))
		  ((= size 0)
		   (close-output-port port)
		   (if temporary?
		       tmpname
		       (begin (rename-file tmpname filename) filename)))
		  (else (close-output-port port) (delete-file tmpname))))))))

  (define (http-gzip-receiver receiver)
    (lambda (code hdrs total retr)
      (define (read-all out remote)
	(let loop ((all 0))
	  (let-values (((remote size) (retr)))
	    (if (zero? size)
		all
		(begin
		  (ensured-copy out remote size)
		  (loop (+ all size)))))))
      (define (callback in size)
	(let ((rest size))
	  (lambda ()
	    (if (equal? rest 0)
		(values in 0)
		(begin (set! rest 0) (values in size))))))
      ;; convert to gzip port iff content-encoding is specified to gzip
      ;; otherwise just call given receiver
      (if (equal? (rfc5322-header-ref hdrs "content-encoding") "gzip")
	  (let ((in/out (binary:open-chunked-binary-input/output-port)))
	    ;; read everything to complete gzip input, otherwise
	    ;; gzip port blocks to read next byte to complete input.
	    (let ((new-size (read-all in/out retr)))
	      (set-port-position! in/out 0)
	      (let ((gin (open-gzip-input-port in/out :owner? #t)))
		(receiver code hdrs total (callback gin #f)))))
	  (receiver code hdrs total retr))))

  (define-syntax http-cond-receiver
    (syntax-rules (else =>)
      ((_) (http-null-receiver))
      ((_ (else . exprs)) (begin . exprs))
      ((_ (cc => proc) . rest)
       (lambda (code hdrs total retr)
	 ((if (match-status-code? cc code)
	      proc
	      (http-cond-receiver . rest))
	  code hdrs total retr)))
      ((_ (cc . exprs) . rest)
       (lambda (code hdrs total retr)
	 ((if (match-status-code? cc code '(cc . exprs))
	      (begin . exprs)
	      (http-cond-receiver . rest))
	  code hdrs total retr)))
      ((_ other . rest)
       (syntax-violation 'http-cond-receiver
			 "invalid clause in http-cond-receiver"
			 other))))

  (define (match-status-code? pattern code clause)
    (cond ((string? pattern)  (equal? pattern code))
	  ((regex-pattern? pattern) (pattern code))
	  (else (error 'match-status-code?
		       "invalid pattern in a clause of http-cond-receiver"
		       clause))))

  ;; query and request body composition
  (define (http-compose-query path params :optional (encoding 'utf-8))
    (define (esc s) (uri-encode-string (format "~a" s) :encoding encoding))
    (define (query-1 n&v)
      (match n&v
	((name value) (format "~a=~a" (esc name) (esc value)))
	(_ (assertion-violation 'http-compose-query
				"invalid request-uri form" params))))
    (define (query) (string-concatenate (intersperse "&" (map query-1 params))))
    (cond ((not path) (query))
	  ((null? params) path)
	  (else (format "~a?~a" path (query)))))

  ;; multipart/form-data composition [RFC2388]
  ;; <params> : (<params> ...)
  ;; <param>  : (<name> <value>)
  ;;          | (<name> <key> <value> <key2> <value2> ...)
  ;; <key>    : :value | :file | :content-type | :content-transfer-encoding
  ;;          | other keyword (used as a header name)
  (define (http-compose-form-data params port
				  :optional (encoding 'utf-8))
    (define (translate-param param)
      (match param
	((name value) (translate-param `(,name :value ,value)))
	((name . kvs)
	 (unless (even? (length kvs))
	   (assertion-violation
	    'http-compose-form-data
	    "invalid parameter format to create multipart/form-data") param)
	 (let-keywords kvs ((value "")
			    (file #f)
			    (content-type #f)
			    (content-transfer-encoding #f) . other-keys)
	   `(,(canonical-content-type (mime-parse-content-type content-type)
				      value file)
	     (("content-transfer-encoding" 
	       ,(or content-transfer-encoding "binary"))
	      ("content-disposition" ,(make-content-disposition name file))
	      ,@(map (lambda (x) (format "~a" x)) (slices other-keys 2)))
	     ,(cond (file `(file ,file))
		    ((bytevector? value) value)
		    (else (format "~a" value))))))))
    (define (canonical-content-type ct value file)
      (match ct
	(#f (if (or file (bytevector? value))
		'("application" "octet-stream")
		`("text" "plain" ("charset" . ,(symbol->string encoding)))))
	((type subtype . options)
	 (if (assoc "charset" options)
	     ct
	     `(,type ,subtype ("charset" . 
			       ,(format "~a" encoding)) ,@options)))))
    (define (make-content-disposition name file)
      (call-with-string-output-port
       (lambda (out)
	 (display "form-data" out)
	 (mime-compose-parameters `(("name" . ,name)
				    ,@(cond-list 
				       (file `("filename" . ,file))))
				  out))))
    (if (not port)
	(mime-compose-message-string (map translate-param params))
	(mime-compose-message (map translate-param params) port)))

  (define (ensure-request-uri request-uri enc)
    (match request-uri
      ((? string?) request-uri)
      ((path n&v ...) (http-compose-query path n&v enc))
      (_ (error "Invalid request-uri form for http request API" request-uri))))

  (define (ensure-connection server auth-handler auth-user auth-password
			     proxy secure extra-headers cookie-jar)
    (let1 conn (cond ((http-connection? server) server)
		     ((string? server) (make-http-connection server))
		     (else
		      (raise-http-error 'ensure-connection
					"bad type of argument for server: must be an <http-connection> object or a string of the server's name" 
					server)))
      (let-syntax ((check-override
		    (lambda (x)
		      (syntax-case x ()
			((k id)
			 (with-syntax ((name 
					(datum->syntax 
					 #'k
					 (string->symbol
					  (format "http-connection-~a-set!"
						  (syntax->datum #'id))))))
			   #'(unless (undefined? id)
			       (name conn id))))))))
	(check-override auth-handler)
	(check-override auth-user)
	(check-override auth-password)
	(check-override proxy)
	(check-override extra-headers)
	(check-override secure)
	(check-override cookie-jar)
	conn)))
		     

  ;; shortcuts for specific requests
  (define (http-get server request-uri . options)
    (apply %http-request-adaptor 'GET server request-uri #f options))

  (define (http-head server request-uri . options)
    (apply %http-request-adaptor 'HEAD server request-uri #f options))

  (define (http-post server request-uri body . options)
    (apply %http-request-adaptor 'POST server request-uri body options))

  (define (http-put server request-uri body . options)
    (apply %http-request-adaptor 'PUT server request-uri body options))

  (define (http-delete server request-uri . options)
    (apply %http-request-adaptor 'DELETE server request-uri #f options))

  (define (%http-request-adaptor method server request-uri body
				 :key receiver (debug #f) (sink #f) (flusher #f)
				 :allow-other-keys opts)
    (define recvr
      (if (or sink flusher)
	  (http-oport-receiver (or sink (open-output-bytevector))
			       (or flusher (lambda (s h) 
					     (get-output-bytevector s))))
	  receiver))
    (define (make-sender sender)
      (if (and (output-port? debug) (binary-port? debug))
	  (http-debug-sender debug sender)
	  sender))
    (apply http-request method server request-uri
	   :sender (make-sender
		    (cond ((not body) (http-null-sender))
			  ((list? body) (http-multipart-sender body))
			  (else (http-blob-sender body))))
	   :receiver recvr opts))

  ;; senders
  ;; TODO make it generic to reuse
  (define (make-tee-port out1 out2)
    (define (write! bv start count)
      (put-bytevector out1 bv start count)
      (put-bytevector out2 bv start count)
      count)
    (define (close) #t) ;; do nothing
    (make-custom-binary-output-port "tee-port" write! #f #f close))
  (define (http-debug-sender sink sender :key (body #f))
    (define (debug-header-sink header-sink)
      (lambda (hdrs)
	(send-headers hdrs sink '() '())
	(if body
	    (let ((body-sink (header-sink hdrs)))
	      (lambda (size)
		(let ((out (body-sink size)))
		  (make-tee-port out sink))))
	    (header-sink hdrs))))
    (lambda (hdrs encoding header-sink)
      (sender hdrs encoding (debug-header-sink header-sink))))
  
  (define (http-null-sender)
    (lambda (hdrs encoding header-sink)
      (let ((body-sink (header-sink `(("content-length" "0") ,@hdrs))))
	(body-sink 0))))

  (define (http-string-sender str)
    (lambda (hdrs encoding header-sink)
      (let* ((encoder (make-transcoder (lookup-decoder encoding) 'none))
	     (body (string->bytevector str encoder))
	     (size (bytevector-length body))
	     (body-sink (header-sink `(("content-length" ,(number->string size))
				       ,@hdrs)))
	     (port (body-sink size)))
	(put-bytevector port body 0 size)
	(body-sink 0))))

  (define (http-blob-sender blob)
    (lambda (hdrs encoding header-sink)
      (let* ((data (if (string? blob) (string->utf8 blob) blob))
	     (size (bytevector-length data))
	     ;; TODO add "content-type: type/subtype; charset=utf-8" when
	     ;; blob was string
	     (body-sink (header-sink `(("content-length" ,(number->string size))
				       ,@hdrs)))
	     (port (body-sink size)))
	(put-bytevector port data 0 size)
	(body-sink 0))))

  (define (http-multipart-sender params)
    (lambda (hdrs encoding header-sink)
      (let-values (((body boundary) 
		    (http-compose-form-data params #f encoding)))
	(let* ((size (string-length body))
	       (hdrs `(("content-length" ,(number->string size))
		       ("mime-version" "1.0")
		       ("content-type" ,(string-append
					 "multipart/form-data; boundary=\""
					 boundary
					 "\""))
		       ,@(alist-delete "content-type" hdrs equal?)))
	       (body-sink (header-sink hdrs))
	       (port (body-sink size)))
	  ;; string->utf8 isn't correct so use one by one for now
	  (dotimes (i size) (put-u8 port (char->integer (string-ref body i))))
	  (body-sink 0)))))

  ;; authentication handling
  (define (http-lookup-auth-handler headers)
    (and-let* ((hdr (rfc5322-header-ref headers "www-authenticate"))
	       (m   (#/^(\w+?)\s+/ hdr))
	       (type (string-downcase (m 1))))
      (cond ((assoc type *supported-auth-handlers*) =>
	     (lambda (slot)
	       ((cdr slot) hdr)))
	    (else #f))))

  (define (http-basic-auth-handler-generator hdr)
    (lambda (_ user password . not-used)
      (let ((msg (format "~a:~a" user password)))
	`(("authorization" ,(format "Basic ~a" 
				   (utf8->string
				    (base64-encode (string->utf8 msg)))))))))

  ;; handling Digest authentication
  ;; RFC 2617
  (define (http-digest-auth-handler-generator hdr)
    (define (parse-digest-header hdr)
      (or (and-let* ((m (#/^Digest\s+(.+)/ hdr))
		     (info (m 1)))
	    (apply values (string-split info #/\s*,\s*/)))
	  (error 'http-digest-auth-handler-generator
		 "Unknown header format" hdr)))
    (define (parse-qop qop)
      (and-let* ((m (#/^qop="(.+)?"/ qop))
		 (auths (string-split (m 1) #\,)))
	;; don't support auth-int for now...
	(cond ((member "auth" auths) "auth")
	      (else
	       (error 'http-digest-auth-handler-generator
		      "qop contains only auth-int (not supported)")))))
    (define (parse-realm realm)
      (and-let* ((m (#/^realm="(.+)?"/ realm)))
	(m 1)))
    (define (parse-nonce nonce)
      (and-let* ((m (#/^nonce="(.+)?"/ nonce)))
	(m 1)))
    (define (parse-algorithm algo)
      (rlet1 name (substring algo (string-length "algorithm=")
			    (string-length algo))
	(unless (string=? name "MD5")
	  (error 'http-digest-auth-handler-generator
		 "Non MD5 algorithm is not supported" name))))
    ;; parse and get all information
    (let-values (((realm nonce algorithm qop) (parse-digest-header hdr)))
      (let ((qop-auth (parse-qop qop))
	    (hash-name (parse-algorithm algorithm))
	    (realm-value (parse-realm realm))
	    (nonce-value (parse-nonce nonce))
	    ;; TODO generate nonce by request count!!
	    (nc 0)
	    (cnonce (bytevector->hex-string
		     (random-generator-read-random-bytes
		      (secure-random-generator *prng:chacha20*) 8)))
	    (digester (make-message-digest *digest:md5*)))
	(lambda (info user password headers body)
	  (define (md5-hex s)
	    (bytevector->hex-string (digest-message digester (string->utf8 s))))
	  (set! nc (+ nc 1))
	  (let* ((a1 (string-append user ":" realm-value ":" password))
		 (ha1 (md5-hex a1))
		 (a2 (format "~a:~a" (cadr (assq 'method info))
			     (cadr (assq 'request-uri info))))
		 (ha2 (md5-hex a2))
		 (resp (format "~a:~a:~8,'0x:~a:~a:~a" 
			       ha1 nonce-value nc cnonce qop-auth ha2))
		 (hresp (md5-hex resp)))
	    `(("authorization"
	       ,(format "Digest username=~s, realm=~s, nonce=~s, uri=~s, \
                         algorithm=MD5, response=~s, qop=auth, nc=~8,'0x, \
                         cnonce=~s" 
			user realm-value nonce-value 
			(cadr (assq 'request-uri info))
			hresp nc cnonce))))))))

  (define *supported-auth-handlers*
    `(("basic"  . ,http-basic-auth-handler-generator)
      ("digest" . ,http-digest-auth-handler-generator)))

  ;;(define (http-default-auth-handler . _) #f)

  ;; I needed to write this thing everywhere...
  (define (url-server&path url)
    (let*-values (((scheme specific) (uri-scheme&specific url))
		  ((auth path query frag)
		   (uri-decompose-hierarchical specific)))
      (values auth
	      ;; uri-compose always put // in front so remove it
	      (string-copy
	       (uri-compose :path path :query query :fragment frag) 2))))
)
