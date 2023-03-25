;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/websocket/engine/http.scm - RFC 6455 Websocket HTTP1.1 engine
;;;  
;;;   Copyright (c) 2010-2016  Takashi Kato  <ktakashi@ymail.com>
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

#!read-macro=sagittarius/bv-string
#!read-macro=sagittarius/regex
(library (rfc websocket engine http)
  (export make-websocket-client-engine
	  make-websocket-server-engine
	  websocket-error-http-status
	  websocket-error-http-message)
  (import (rnrs)
	  (sagittarius socket)
	  (rfc websocket engine)
	  (rfc websocket conditions)
	  (rfc :5322)
	  (rfc tls)
	  (rfc uri)
	  (rfc base64)
	  (srfi :1 lists)
	  (srfi :2 and-let*)
	  (srfi :13 strings)
	  (prefix (binary io) binary:)
	  (sagittarius)
	  (sagittarius regex)
	  (sagittarius crypto random)
	  (sagittarius crypto digests))

(define-condition-type &websocket-http-engine &websocket-engine
  make-websocket-http-engine-error websocket-http-engine-error?)
(define-condition-type &websocket-http-status &websocket-http-engine
  make-websocket-http-status-error websocket-http-stauts-error?
  (status websocket-error-http-status)
  (message websocket-error-http-message))

(define (websocket-http-engine-error who msg . irr)
  (raise (condition (make-websocket-http-engine-error)
		    (make-who-condition who)
		    (make-message-condition msg)
		    (make-irritants-condition irr))))

(define (websocket-http-status-error who msg status http-msg)
  (raise (condition (make-websocket-http-status-error status http-msg)
		    (make-who-condition who)
		    (make-message-condition msg))))


;;; Client handshake for HTTP/1.1

(define (make-websocket-client-engine)
  (make-websocket-engine http-websocket-handshake))

(define *uuid* #*"258EAFA5-E914-47DA-95CA-C5AB0DC85B11")

;; utility
(define-syntax put-bytevector*
  (syntax-rules ()
    ((_ out) (begin))
    ((_ out bv bvs ...)
     (begin 
       (put-bytevector out bv)
       (put-bytevector* out bvs ...)))))
(define (put-comma-string out s)
  (put-bytevector out (string->utf8 (string-join s ", "))))
(define (put-other-headers in/out others)
  (define (re-raise e)
    (apply websocket-http-engine-error 
	   'websocket-http-handshake
	   (or (and (message-condition? e) (condition-message e))
	       "Failed to write headers")
	   (or (and (irritants-condition? e) (condition-irritants e)
	       others))))
  (unless (null? others)
    (guard (e (else (re-raise e)))
      (let-values (((out extract) (open-string-output-port)))
	(rfc5322-write-headers others :output out :continue #t)
	(put-bytevector in/out (string->utf8 (extract)))))))

(define *prng* (secure-random-generator *prng:chacha20*))
(define (read-random-bytes size)
  (random-generator-read-random-bytes *prng* size))
(define (http-websocket-handshake engine in/out uri
				  :optional (protocols '()) (extensions '())
				  :rest others)
  
  (define (send-websocket-handshake in/out key)
    (let-values (((scheme ui host port path query frag) (uri-parse uri)))
      (let ((request-path
	     ;; FIXME non ASCII path and query
	     (string->utf8
	      ;; drop //
	      (string-drop (uri-compose :path path :query query) 2))))
	(put-bytevector* in/out #*"GET " request-path #*" HTTP/1.1\r\n")
	;; put it here, so that if the headers are not valid, then
	;; it'd fail (e.g. header contains ("\r\n" "\r\n") or so)
	(put-bytevector* in/out #*"Host: " (string->utf8 host) #*"\r\n")
	(put-bytevector* in/out #*"Connection: Upgrade\r\n")
	(put-bytevector* in/out #*"Upgrade: websocket\r\n")
	(put-bytevector* in/out #*"Sec-WebSocket-Key: " key #*"\r\n")
	(put-bytevector* in/out #*"Sec-WebSocket-Version: 13\r\n")
	(unless (null? protocols)
	  (put-bytevector* in/out #*"Sec-WebSocket-Protocol: ")
	  (put-comma-string in/out protocols)
	  (put-bytevector* in/out #*"\r\n"))
	(unless (null? extensions)
	  (put-bytevector* in/out #*"Sec-WebSocket-Extensions: ")
	  (put-comma-string in/out extensions)
	  (put-bytevector* in/out #*"\r\n"))
	(put-other-headers in/out others)
	(put-bytevector* in/out #*"\r\n")
	(flush-output-port in/out))))

  (define (check-first-line line)
    (cond ((eof-object? line)
	   (websocket-http-engine-error 'http-websocket-handshake
					"Unexpected EOF"))
	  ((#/HTTP\/1.1 101 [\w\s]+/ line) #t)
	  ((#/HTTP\/1.1 (\d\d\d) ([\w\s]+)?/ line) =>
	   (lambda (m)
	     (websocket-http-status-error 'http-websocket-handshake
					  "Server returned non 101"
					  (utf8->string (m 1))
					  (utf8->string (m 2)))))
	  (else (websocket-http-engine-error 'http-websocket-handshake
					     "Unknown status line"
					     (utf8->string line)))))
  (define (check-header headers field expected)
    (unless (equal? expected (rfc5322-header-ref headers field))
      (websocket-http-engine-error 'http-websocket-handshake
				   "Unexpected field value" field)))
  (define (check-header-contains headers field oneof)
    (or (and-let* ((v (rfc5322-header-ref headers field)))
	  (member v oneof))
	(websocket-http-engine-error 'http-websocket-handshake
				     "Unexpected field value" field)))
  (define (sha1 msg)
    (let ((md (make-message-digest *digest:sha-1*)))
      (digest-message md msg)))
  (let ((key (base64-encode (read-random-bytes 16))))
    (send-websocket-handshake in/out key)
    (check-first-line (binary:get-line in/out :eol #*"\r\n"))
    (let ((headers (rfc5322-read-headers in/out))
	  (expected-accept (utf8->string
			    (base64-encode
			     (sha1 (bytevector-append key *uuid*))))))
      (check-header headers "Upgrade" "websocket")
      (check-header headers "Connection" "Upgrade")
      (check-header headers "Sec-WebSocket-Accept" expected-accept)
      (if (null? protocols)
	  (or (not (rfc5322-header-ref headers "Sec-WebSocket-Protocol"))
	      (check-header headers "Sec-WebSocket-Protocol" ""))
	  (check-header-contains headers "Sec-WebSocket-Protocol" protocols))
      
      (values (rfc5322-header-ref headers "Sec-WebSocket-Protocol")
	      (rfc5322-header-ref headers "Sec-WebSocket-Extensions")
	      headers))))

;;; Server handshake for HTTP/1.1

(define (make-websocket-server-engine)
  (make-websocket-engine  http-websocket-server-handshake))

;; Assume the first line is already read.
;; NB: the path should already be processed by the server
;;     otherwise it can't see if the requested path is
;;     for WebSocket or not.
;; Caveat: if that's the case, how could we know on HTTP/2?
(define (http-websocket-server-handshake engine in/out uri
		      :optional (protocols '()) (extensions '())
		      :rest others)
  ;; read it here, it's needed anyway
  (define headers (rfc5322-read-headers in/out))
  (define (string-not-null? s) (not (string-null? s)))
  (define (bad-request msg)
    (websocket-http-engine-error 'http-websocket-server-handshake msg))
  (define (check-headers headers)
    (define (check-header field expected equal?)
      (equal? (rfc5322-header-ref headers field) expected))
    (and (check-header "Connection" "Upgrade" string-contains)
	 (check-header "Upgrade" "websocket" equal?)
	 (check-header "Sec-WebSocket-Version" "13" equal?)
	 (string-not-null? (rfc5322-header-ref headers "Host" ""))
	 (rfc5322-header-ref headers "Sec-WebSocket-Key")))
  (define (split-header field)
    (string-split (rfc5322-header-ref headers field "") #/\s*,\s*/))
  (define (take-one-of l1 l2)
    (cond ((and (null? l1) (null? l2)) #f)
	  ((or (null? l1) (null? l2))
	   (bad-request "Bad sub protocol request"))
	  (else
	   (let ((l (lset-intersection string=? l1 l2)))
	     (if (null? l)
		 (bad-request "Sub protocol not supported")
		 (car l))))))
    
  (define (verify-key key)
    (define (calculate-key key)
      (base64-encode 
       (hash SHA-1 (bytevector-append (string->utf8 key) *uuid*))))
    (when (string-null? key) (bad-request "Empty Sec-WebSocket-Key"))

    (let* ((server-key (calculate-key key))
	   (client-protocols (split-header "Sec-WebSocket-Protocol"))
	   ;; this isn't properly done
	   (client-extensions (split-header "Sec-WebSocket-Extensions"))
	   (accepted-protocol (take-one-of client-protocols protocols))
	   ;; TODO like this?
	   (accepted-extensions
	    (lset-intersection string=? client-extensions extensions)))
      (put-bytevector* in/out #*"HTTP/1.1 101 Switch protocol\r\n")
      (put-bytevector* in/out #*"Upgrade: websocket\r\n")
      (put-bytevector* in/out #*"Connection: Upgrade\r\n")
      (put-bytevector* in/out #*"Sec-WebSocket-Accept: " server-key #*"\r\n")
      (when accepted-protocol
	(put-bytevector* in/out #*"Sec-WebSocket-Protocol: "
			 (string->utf8 accepted-protocol) #*"\r\n"))
      (unless (null? accepted-extensions)
	(put-bytevector* in/out #*"Sec-WebSocket-Extensions: ")
	(put-comma-string in/out accepted-extensions)
	(put-bytevector* in/out #*"\r\n"))
      (put-other-headers in/out others)
      (put-bytevector* in/out #*"\r\n")
      (flush-output-port in/out)
      (values accepted-protocol accepted-extensions headers)))
  
  (cond ((check-headers headers) => verify-key)
	(else (bad-request "Missing request headers")))
  )
)
