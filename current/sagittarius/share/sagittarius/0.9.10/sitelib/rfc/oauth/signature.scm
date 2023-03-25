;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/oauth/signature.scm - OAuth1 signature procedures
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

;; reference: https://tools.ietf.org/html/rfc5849
#!read-macro=sagittarius/bv-string
#!nounbound
(library (rfc oauth signature)
    (export oauth-signer? oauth-signer-process! oauth-signer-done!
	    oauth-signer-method oauth-signer-clone
	    
	    oauth-verifier? oauth-verifier-verify
	    oauth-verifier-clone
	    make-oauth-hmac-sha1-signer make-oauth-hmac-sha1-verifier
	    make-oauth-rsa-sha1-signer make-oauth-rsa-sha1-verifier
	    make-oauth-plaintext-signer make-oauth-plaintext-verifier

	    oauth-construct-base-string-uri
	    oauth-encode-string

	    oauth-normalize-parameters
	    )
    (import (rnrs)
	    (rfc http-connections)
	    (rfc base64)
	    (rfc uri)
	    (srfi :13)
	    (srfi :14)
	    (util bytevector)
	    (sagittarius crypto mac)
	    (sagittarius crypto digests)
	    (sagittarius crypto signatures))
  (define-record-type oauth-signer
    (fields process done method cloner))
  (define (oauth-signer-process! signer msg)
    ((oauth-signer-process signer) msg))
  (define (oauth-signer-done! signer)
    ((oauth-signer-done signer)))
  (define (oauth-signer-clone signer)
    ((oauth-signer-cloner signer)))
  
  (define-record-type oauth-verifier
    (fields %verify cloner))
  (define (oauth-verifier-verify verify msg signature)
    ((oauth-verifier-%verify verify) msg signature))
  (define (oauth-verifier-clone verify)
    ((oauth-verifier-cloner verify)))

  (define (->base64 bv) (utf8->string (base64-encode bv :line-width #f)))
  ;; 3.4.2 HMAC-SHA1
  (define (append-secrets cs ts) (bytevector-append cs #*"&" ts))
  (define make-oauth-hmac-sha1-signer
    (case-lambda
     ((secret) (make-oauth-hmac-sha1-signer secret #vu8()))
     ((consumer-secret token-secret)
      (define hmac
	(make-mac *mac:hmac* (append-secrets consumer-secret token-secret)
		  :digest *digest:sha-1*))
      (mac-init! hmac)
      (make-oauth-signer
       (lambda (msg) (mac-process! hmac msg))
       (lambda () (let ((out (make-bytevector (mac-mac-size hmac))))
		    (mac-done! hmac out 0 (bytevector-length out))
		    (mac-init! hmac) ;; for next time if needed
		    (->base64 out)))
       'HMAC-SHA1
       (lambda ()
	 (make-oauth-hmac-sha1-signer consumer-secret token-secret))))))
  (define make-oauth-hmac-sha1-verifier
    (case-lambda
     ((secret) (make-oauth-hmac-sha1-verifier secret #vu8()))
     ((consumer-secret token-secret)
      (define hmac
	(make-mac *mac:hmac* (append-secrets consumer-secret token-secret)
		  :digest *digest:sha-1*))
      (make-oauth-verifier
       (lambda (msg signature)
	 (verify-mac hmac msg
		     (base64-decode-string signature :transcoder #f)))
       (lambda ()
	 (make-oauth-hmac-sha1-verifier consumer-secret token-secret))))))

  ;; 3.4.3 RSA-SHA1
  ;; private key must be provided before hand.
  (define (make-oauth-rsa-sha1-signer private-key)
    (define signer (make-signer *signature:rsa* private-key
				:encoder pkcs1-emsa-v1.5-encode
				:digest *digest:sha-1*))
    (signer-init! signer)
    (values
     (make-oauth-signer
      (lambda (msg) (signer-process! signer msg))
      (lambda () (let ((r (->base64 (signer-sign! signer))))
		   (signer-init! signer)
		   r))
      'RSA-SHA1
      (lambda () (make-oauth-rsa-sha1-signer private-key)))))
  (define (make-oauth-rsa-sha1-verifier public-key)
    (define verifier (make-verifier *signature:rsa* public-key
				    :verifier pkcs1-emsa-v1.5-verify
				    :digest *digest:sha-1*))
    (make-oauth-verifier
     (lambda (msg signature)
       (verifier-verify-signature verifier msg
	(base64-decode-string signature :transcoder #f)))
     (lambda ()
       (make-oauth-rsa-sha1-verifier public-key))))
  ;; 3.4.4 PLAINTEXT
  ;; secret must be the same as HMAC-SHA1
  (define (make-oauth-plaintext-signer secret)
    (let ((r (->base64 secret)))
      (make-oauth-signer (lambda (msg) #t) (lambda () r) 'PLAINTEXT
			 (lambda () (make-oauth-plaintext-signer secret)))))
  (define (make-oauth-plaintext-verifier secret)
    (make-oauth-verifier
     (lambda (msg signature)
       (unless (bytevector=? msg
			     (base64-decode-string signature :transcoder #f))
	 (assertion-violation 'oauth-plaintext-verifier "inconsistent")))
     (lambda ()
       (make-oauth-plaintext-verifier secret))))

  ;; 3.4 Signature Base String
  ;; 3.4.1.2 Base String URI
  (define (oauth-construct-base-string-uri http-connection path)
    (define secure? (http-connection-secure? http-connection))
    (define (remove-query path)
      (cond ((string-index-right path #\?) =>
	     (lambda (p) (string-copy path 0 p)))
	    (else path)))
    (let-values (((server port) (http-connection-server&port http-connection)))
      (let ((scheme (if secure? "https://" "http://"))
	    (path   (remove-query path)))
	(string-append scheme (string-downcase server)
		       (if (not (or (string=? "443" port) (string=? "80" port)))
			   (string-append ":" port)
			   "")
		       path))))

  ;; 3.4.1.3.2 Parameters Normalization
  ;; input = ((str str) ...)
  ;; output = ((bv bv) ...)
  (define (oauth-normalize-parameters alist)
    (list-sort (lambda (a b)
		 (if (bytevector=? (car a) (car b))
		     (bytevector<? (cdr a) (cdr b))
		     (bytevector<? (car a) (car b))))
	       (map (lambda (s) (cons (oauth-encode-string (car s))
				      (oauth-encode-string (cadr s)))) alist)))
  
  ;; 3.6.  Percent Encoding
  ;; we return encoded value as bytevector for convenience.
  (define (oauth-encode-string s)
    (let-values (((out extract) (open-bytevector-output-port)))
      (uri-encode (open-bytevector-input-port (string->utf8 s)) out)
      (extract)))
)
