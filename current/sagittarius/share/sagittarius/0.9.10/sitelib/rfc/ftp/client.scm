;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/ftp/client.scm - FTP client
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
(library (rfc ftp client)
    (export ftp-transfer-type ftp-passive?
	    ;; commands
	    ftp-login ftp-login-with-socket ftp-quit 
	    ftp-mkdir ftp-chdir ftp-rmdir
	    ftp-current-directory (rename (ftp-current-directory ftp-pwd))
	    ftp-delete ftp-rename
	    ftp-stat ftp-system ftp-size
	    ftp-mdtm ftp-mtime
	    ftp-site
	    ftp-noop ftp-list ftp-name-list (rename (ftp-name-list ftp-ls))
	    ftp-help

	    ftp-get
	    ftp-put ftp-put-file
	    ftp-put-unique ftp-put-unique-file
	    <ftp-connection>

	    ;; receivers
	    ftp-binary-receiver
	    ftp-file-receiver

	    ;; condition
	    &ftp-error ftp-error? ftp-error-status
	    )
    (import (rnrs)
	    (sagittarius)
	    (sagittarius control)
	    (sagittarius object)
	    (sagittarius socket)
	    (sagittarius regex)
	    (clos user)
	    (srfi :13 strings)
	    (srfi :18 multithreading)
	    (srfi :19 time)
	    (srfi :26 cut)
	    (util file)
	    (util port)
	    ;; for FTPS support
	    (rfc tls)
	    (rfc ftp messages))

  (define-constant *default-ftp-port* "21")
  (define-constant *anonymous-user* "anonymous")
  (define-constant *anonymous-pass* "anonymous@")

  (define-class <ftp-connection> ()
    ((type :init-value 'binary :init-keyword :type :accessor ftp-transfer-type)
     (passive :init-keyword :passive :reader ftp-passive?)
     (socket :init-keyword :socket)))

  (define-condition-type &ftp-error &error make-ftp-error ftp-error?
    (status ftp-error-status))

  (define (ftp-error who status)
    (raise (apply condition
		  (filter values
			  (list (make-ftp-error (string-copy status 0 3))
				(and who (make-who-condition who))
				(make-message-condition status))))))

  (define (ftp-login-with-socket socket :key
				 (username *anonymous-user*)
				 (password (if (string=? username
							 *anonymous-user*) 
					       *anonymous-pass*
					       ""))
				 (authenticate #f)
				 (account "") 
				 ;; default passive,
				 ;; http://cr.yp.to/ftp/retr.html
				 ;; above says don't send PORT.
				 (passive #t))

    (define (do-authenticate conn)
      (let1 r1 (send-command conn "AUTH TLS")
	(let1 tls (socket->tls-socket (~ conn 'socket))
	  (set! (~ conn 'socket) (tls-client-handshake tls)))
	(simple-command conn "PBSZ 0")
	(simple-command conn "PROT P")))
    (define (do-login conn)
      (let retry ()
	(let1 res (get-response conn)
	  ;; well 120 is predefined so it's ok
	  (when (string=? res "120")
	    ;; FIXME the message must indicate how long it needs to wait
	    (thread-sleep! 0.001) (retry))))
      (when authenticate (do-authenticate conn))
      (let1 r1 (send-command conn "USER" username)
	(if (positive-intermediate? r1)
	    (let1 r2 (send-command conn "PASS" password)
	      (if (positive-intermediate? r2)
		  (send-command conn "ACCT" account)
		  r2))
	    r1)))
    (let* ((conn (make <ftp-connection>
		   :passive passive
		   :socket socket))
	   (res (do-login conn)))
      (if (positive-completion? res)
	  conn
	  (ftp-error 'ftp-login res))))

  (define (ftp-login host :key (port *default-ftp-port*)
		     :allow-other-keys opts)
    (let1 socket (make-client-socket host port)
      (apply ftp-login-with-socket socket opts)))

  (define (ftp-quit conn)
    (unwind-protect
	(simple-command conn "QUIT")
      (and-let* ((s (~ conn 'socket)))
	(socket-close s)
	(set! (~ conn 'socket) #f))))

  ;; rmd
  (define (ftp-rmdir conn dirname) (simple-command conn "RMD" dirname))
  ;; stat
  (define (ftp-stat conn . opt) (apply simple-command conn "STAT" opt))
  ;; syst
  (define (ftp-system conn) (string-drop (simple-command conn "SYST") 4))
  ;; size
  (define (ftp-size conn path) 
    (ftp-set-type conn)
    (string->number (string-drop (simple-command conn "SIZE" path) 4)))
  ;; mkd
  (define (ftp-mkdir conn dirname)
    (parse-257 (simple-command conn "MKD" dirname)))
  ;; pwd
  (define (ftp-current-directory conn) (parse-257 (simple-command conn "PWD")))
  (define (parse-257 res)
    (regex-match-if (#/^257 \"((?:[^\"]|\"\")+)\"/ res) (_ dirname) ;; |)))
      (regex-replace-all #/""/ dirname "\"")
      (ftp-error #f res)))

  ;; cdup or cwd
  (define (ftp-chdir conn path)
    (if (string=? path "..")
	(simple-command conn "CDUP")
	(simple-command conn "CWD" path)))

  ;; SITE
  (define (ftp-site conn arg) (simple-command conn "SITE" arg))
  ;; MDTM
  (define (ftp-mdtm conn path) (simple-command conn "MDTM" path))

  (define (ftp-mtime conn path :optional (local-time? #f))
    (regex-match-let (#/(\d{4})(\d\d)(\d\d)(\d\d)(\d\d)(\d\d)/ 
			(ftp-mdtm conn path))
	(#f year month day hour min sec)
      (date->time-utc
       (make-date 0
		  (string->number sec) (string->number min) (string->number hour)
		  (string->number day) (string->number month)
		  (string->number year)
		  (if local-time? (date-zone-offset (current-date)) 0)))))
  
  ;; NOOP
  (define (ftp-noop conn) (simple-command conn "NOOP"))
  
  ;; LIST
  ;; NLIST
  (define (make-lister cmd)
    (lambda (conn . opt)
      (req&recv conn
		(cut apply send-command conn cmd opt)
		(lambda (in) 
		  (port->string-list 
		   (transcoded-port in (native-transcoder))))
		'ascii)))
  (define ftp-list (make-lister "LIST"))
  (define ftp-name-list (make-lister "NLST"))

  ;; dele
  (define (ftp-delete conn path) (simple-command conn "DELE" path))
  ;; help
  (define (ftp-help conn . opt) (apply simple-command conn "HELP" opt))

  ;; ftp-get receiver
  (define (ensure-copy out in)
    (copy-binary-port out in)
    (when (port-ready? in) (ensure-copy out in)))
  (define (ftp-binary-receiver)
    (let-values (((out extract) (open-bytevector-output-port)))
      (lambda (in)
	(ensure-copy out in)
	(extract))))
  (define (ftp-file-receiver filename)
    (lambda (in)
      (call-with-output-file filename
	(lambda (out)
	  (ensure-copy out in))
	:transcoder #f)))


  (define (ftp-get conn path :key (receiver (ftp-binary-receiver)))
    (req&recv conn
	      (cut send-command conn "RETR" path)
	      receiver))

  ;; STOR
  (define (ftp-put-file conn from-file
			:optional (to-file (path-basename from-file)))
    (call-with-input-file from-file
      (cut ftp-put conn <> to-file)
      :transcoder #f))

  (define (ftp-put conn bin to-file)
    (receive (res _) (req&send conn (cut send-command conn "STOR" to-file) bin)
      res))

  ;; STOU
  (define (ftp-put-unique conn bin)
    (req&send conn (cut send-command conn "STORU") bin))

  (define (ftp-put-unique-file conn from-file)
    (call-with-input-file from-file
      (cut ftp-put-unique conn <>)
      :transcoder #f))

  ;; RNFR
  ;; RNTO
  (define (ftp-rename conn from to)
    (let1 res1 (send-command conn "RNFR" from)
      (if (not (string-prefix? "3" res1))
	  (ftp-error 'ftp-rename res1)
	  (let1 res2 (send-command conn "RNTO" to)
	    (if (not (string-prefix? "2" res2))
		(ftp-error 'ftp-rename res2)
		res2)))))

  ;; low level stuff
  (define (send-command conn cmd . args)
    (define space (string->utf8 " "))
    (define crlf (string->utf8 "\r\n"))
    (let ((s (~ conn 'socket))
	  (msg (bytevector-concatenate 
		(cons (string->utf8 cmd)
		      (map (lambda (m)
			     (bytevector-append space (string->utf8 m)))
			   args)))))
      ;; (format #t "--> ~a~%" (utf8->string msg))
      (socket-send s (bytevector-append msg crlf))
      (get-response conn)))

  (define (simple-command conn cmd . args)
    (let1 res (apply send-command conn cmd  args)
      ;; (format #t "<-- ~a~%~%" res)
      (if (positive-completion? res)
	  res
	  (ftp-error cmd res))))

  (define (get-response conn)
    ;; currently sagittarius socket port closes socket when it's GCed
    ;; so we need to use socket API directly...
    (define lf (char->integer #\newline))
    ;; helper
    (define (recv-line s)
      (let loop ((r '()) (u8* (socket-recv s 1)))
	(let1 u8 (bytevector-u8-ref u8* 0)
	  (if (= u8 lf)
	      (string-trim-right 
	       (utf8->string (u8-list->bytevector (reverse! (cons u8 r)))))
	      (loop (cons u8 r) (socket-recv s 1))))))
    (define (get-resp s)
      (let1 l (recv-line s)
	(regex-match-if (#/^(\d\d\d)-/ l) (#f code)
	  (let loop ((rs (list l))
		     (line (recv-line s)))
	    (if (and (string-prefix? code line)
		     (char=? (string-ref line 3) #\space))
		(string-concatenate (reverse! rs))
		(loop (cons line rs) (recv-line s))))
	  l)))
    (let1 res (get-resp (~ conn 'socket))
      (if (or (transient-negative-completion? res)
	      (permanent-negative-completion? res))
	  (ftp-error 'get-response res)
	  res)))

  (define (call-with-data-connection conn proc)
    (define (dcsock)
      (if (ftp-passive? conn)
	  (if (ipv4? conn)
	      (parse-227 (simple-command conn "PASV"))
	      (parse-229 (simple-command conn "EPSV")))
	  (make-asock)))
    (define (ipv4? conn)
      (let1 info (socket-peer (~ conn 'socket))
	(if info
	    (let1 bv (ip-address->bytevector (~ info 'ip-address))
	      (= (bytevector-length bv) 4))
	    ;; very bad
	    #t)))

    (define (parse-227 res)
      (if (not (string-prefix? "227" res))
	  (ftp-error #f res)
	  (regex-match-let (#/\((\d+),(\d+),(\d+),(\d+),(\d+),(\d+)\)/ res)
	      (#f h1 h2 h3 h4 p1 p2)
	    (let1 ds (make-client-socket (string-join (list h1 h2 h3 h4) ".")
					 (number->string 
					  (+ (* (->integer p1) 256)
					     (->integer p2))))
	      (values (lambda () ds) (lambda () (socket-shutdown ds 2)))))))
    (define (parse-229 res)
      (define (compose-regex s)
	(format "\\~a\\~a\\~a([^\\~a]+)\\~d" s s s s s))

      (if (not (string-prefix? "229" res))
	  (ftp-error #f res)
	  ;; response format (<d><d><d><tcp-port><d>)
	  ;; we need tcp-port
	  (regex-match-let (#/\((.+?)\)/ res) (#f s)
	    (let1 pattern (regex (compose-regex (substring s 0 1)))
	      (regex-match-let (pattern s) (#f port)

		(let1 ds (make-client-socket (ip-address->string
					      (~ (socket-info (~ conn 'socket))
						 'ip-address))
					     port
					     AF_INET6)
		  (values (lambda () ds)
			  (lambda () (socket-shutdown ds 2)))))))))

    (define (make-asock)
      ;; NOTE following socket APIs are not documented yet.
      (define (make-ftp-server-socket ip4?)
	(define info (socket-info (~ conn 'socket)))
	(let ((s (make-socket (if ip4? AF_INET AF_INET6) SOCK_STREAM))
	      (hints (make-hint-addrinfo :family (if ip4? AF_INET AF_INET6)
					 :socktype SOCK_STREAM)))
	  (unless (socket-bind! s (get-addrinfo (ip-address->string 
						 (~ info 'ip-address))
						#f hints))
	    (ftp-error 'ftp "failed to bind socket" s))
	  (unless (socket-listen! s SOMAXCONN)
	    (ftp-error 'ftp "failed to listen socket" s))
	  s))

      (let* ((ip4? (ipv4? conn))
	     (asock (make-ftp-server-socket ip4?))
	     (info (socket-info asock))
	     (port (~ info 'port))
	     (ip   (ip-address->string (~ info 'ip-address))))
	(if ip4?
	    (regex-match-let (#/(\d+)\.(\d+)\.(\d+)\.(\d+)/ ip)
		(#f a b c d)
	      (receive (q r) (div-and-mod port 256)
		(simple-command conn "PORT"
				(format "~A,~A,~A,~A,~A,~A" a b c d q r))))
	    (simple-command conn "EPRT" (format #f "|2|~A|~A|" ip port)))
	(let1 ds #f
	  (values (lambda () (set! ds (socket-accept asock)) ds)
		  (lambda ()
		    (when ds (socket-shutdown ds 2))
		    (socket-shutdown asock 2))))))
    (receive (sock-thunk cleanup-thunk) (dcsock)
      (unwind-protect (proc sock-thunk) (cleanup-thunk))))

  (define (ftp-set-type conn :optional (type (ftp-transfer-type conn)))
    (simple-command conn "TYPE"
		    (case type
		      ((ascii) "A")
		      ((binary image) "I")
		      (else
		       (error 'ftp-set-type "Invalid transfer type:" type)))))

  (define (req&recv conn cmdproc reader . opt)
    (apply ftp-set-type conn opt)
    (call-with-data-connection conn
      (lambda (get-data-socket)
	(let1 res (cmdproc)
	  (if (positive-preliminary? res)
	      (begin0
		(let1 s (if (is-a? (~ conn 'socket) <tls-socket>)
			    (tls-client-handshake
			     (socket->tls-socket (get-data-socket)))
			    (get-data-socket))
		  (unwind-protect (reader (socket-port s #f))
		    (socket-shutdown s SHUT_RDWR) 
		    (socket-close s)))
		(let1 res2 (get-response conn)
		  (unless (positive-completion? res2)
		    (ftp-error 'ftp-get res2)))))))))

  (define (req&send conn cmdproc port)
    (define (copy-data get-data-socket)
      (let1 dst (socket-port (if (is-a? (~ conn 'socket) <tls-socket>)
				 (tls-client-handshake
				  (socket->tls-socket (get-data-socket)))
				 (get-data-socket))
			     #f)
	(copy-binary-port dst port)
	(flush-output-port dst)
	(close-port dst)))

    (define (send-data)
      (call-with-data-connection conn
	(lambda (get-data-socket)
	  (let1 res (cmdproc)
	    (regex-match-case res
	      (#/^1\d\d FILE: (.+)$/ (#f dst-path)
		;; RFC 1123 - 4.1.2.9
		(copy-data get-data-socket) dst-path)
	      (#/^1/ ()
		(copy-data get-data-socket) #f)
	      (else (ftp-error 'ftp-put res)))))))

    (define (retrieve-response res dst-path)
      (cond ((positive-preliminary? res)
	     (retrieve-response (get-response conn) dst-path))
	    ((positive-completion? res) (values res dst-path))
	    (else (ftp-error #f res))))

    (ftp-set-type conn)
    (let1 dst-path (send-data)
      (retrieve-response (get-response conn) dst-path)))
)
