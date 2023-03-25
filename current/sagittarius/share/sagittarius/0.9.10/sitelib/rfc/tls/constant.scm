;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/tls/constant.scm - TLS 1.0 - 1.2 protocol library.
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

;; Caution this library is not well tested and not secure yet.
(library (rfc tls constant)
    (export *tls-version-1.2*
	    *tls-version-1.1*
	    *tls-version-1.0*
	    ;; ciphers
	    TLS-NULL-WITH-NULL-NULL
	    TLS-RSA-WITH-NULL-MD5
	    TLS-RSA-WITH-NULL-SHA
	    TLS-RSA-WITH-NULL-SHA256
	    ;; ws don't support rc4
	    ;;TLS-RSA-WITH-RC4-128-MD5
	    ;;TLS-RSA-WITH-RC4-128-SHA
	    TLS-RSA-WITH-3DES-EDE-CBC-SHA
	    TLS-RSA-WITH-AES-128-CBC-SHA
	    TLS-RSA-WITH-AES-256-CBC-SHA
	    TLS-RSA-WITH-AES-128-CBC-SHA256
	    TLS-RSA-WITH-AES-256-CBC-SHA256
	    TLS-DHE-RSA-WITH-3DES-EDE-CBC-SHA
	    TLS-DHE-RSA-WITH-AES-128-CBC-SHA
	    TLS-DHE-RSA-WITH-AES-256-CBC-SHA
	    TLS-DHE-RSA-WITH-AES-128-CBC-SHA256
	    TLS-DHE-RSA-WITH-AES-256-CBC-SHA256
	    TLS-DH-anon-WITH-3DES-EDE-CBC-SHA
	    TLS-DH-anon-WITH-AES-128-CBC-SHA
	    TLS-DH-anon-WITH-AES-256-CBC-SHA
	    TLS-DH-anon-WITH-AES-128-CBC-SHA256
	    TLS-DH-anon-WITH-AES-256-CBC-SHA256

	    *cipher-suites*
	    ;; record types
	    *change-cipher-spec*
	    *alert*
	    *handshake*
	    *application-data*
	    ;; alert
	    *warning*
	    *fatal*
	    *close-notify*
	    *unexpected-message*
	    *bad-record-mac*
	    ;;*decryption-failed-RESERVED*
	    *record-overflow*
	    *decompression-failure*
	    *handshake-failure*
	    ;;*no-certificate-RESERVED*
	    *bad-certificate*
	    *unsupported-certificate*
	    *certificate-revoked*
	    *certificate-expired*
	    *certificate-unknown*
	    *illegal-parameter*
	    *unknown-ca*
	    *access-denied*
	    *decode-error*
	    *decrypt-error*
	    ;;*export-restriction-RESERVED*
	    *protocol-version*
	    *insufficient-security*
	    *internal-error*
	    *user-canceled*
	    *no-renegotiation*
	    *unsupported-extension*
	    ;; handshake types
	    *hello-request*
	    *client-hello*
	    *server-hello*
	    *certificate*
	    *server-key-echange*
	    *certificate-request*
	    *server-hello-done*
	    *certificate-verify*
	    *client-key-exchange*
	    *finished*
	    ;; key exchange algorithm set
	    ;;*rsa-key-exchange-algorithms*
	    *dh-key-exchange-algorithms*
	    *dh-anon-key-exchange-algorithms*
	    ;; For TLS 1.2
	    *supported-signatures*
	    *supported-hashes*

	    ;; extensions RFC6066
	    *server-name*
	    ;; extension of RFC 7301
	    *application-layer-protocol-negotiation*
	    *max-fragment-lenght*
	    *client-certificate-url*
	    *trusted-ca-keys*
	    *truncate-hmac*
	    *status-request*
	    ;; this is in RFC5246
	    *signature-algorithms*
	    ;; RFC6066 section 3 server name indication 
	    *host-name*
	    )
    (import (rnrs)
	    (sagittarius)
	    (rename (sagittarius crypto digests)
		    (*digest:sha-1* SHA-1)
		    (*digest:sha-224* SHA-224)
		    (*digest:sha-256* SHA-256)
		    (*digest:sha-384* SHA-384)
		    (*digest:sha-512* SHA-512)
		    (*digest:md5* MD5))
	    (rename (sagittarius crypto ciphers)
		    (*scheme:rsa* RSA)
		    (*scheme:des3* DES3)
		    (*scheme:aes* AES)))
  ;; default client version.
  ;; but we also need to support TLS 1.0 or else
  ;; most of servers don't support 1.2 ...
  (define-constant *tls-version-1.2* #x0303)
  (define-constant *tls-version-1.1* #x0302)
  (define-constant *tls-version-1.0* #x0301)


  ;; A.5. The Cipher Suite
  ;; cipher suites
  (define-constant TLS-NULL-WITH-NULL-NULL         #x0000)
  (define-constant TLS-RSA-WITH-NULL-MD5           #x0001)
  (define-constant TLS-RSA-WITH-NULL-SHA           #x0002)
  (define-constant TLS-RSA-WITH-NULL-SHA256        #x003B)
  ;; we don't support rc4
  ;;(define-constant TLS-RSA-WITH-RC4-128-MD5        #x0004)
  ;;(define-constant TLS-RSA-WITH-RC4-128-SHA        #x0005)
  (define-constant TLS-RSA-WITH-3DES-EDE-CBC-SHA   #x000A)
  (define-constant TLS-RSA-WITH-AES-128-CBC-SHA    #x002F)
  (define-constant TLS-RSA-WITH-AES-256-CBC-SHA    #x0035)
  (define-constant TLS-RSA-WITH-AES-128-CBC-SHA256 #x003C)
  (define-constant TLS-RSA-WITH-AES-256-CBC-SHA256 #x003D)

  ;; DHE
  (define-constant TLS-DHE-RSA-WITH-3DES-EDE-CBC-SHA #x0016)
  (define-constant TLS-DHE-RSA-WITH-AES-128-CBC-SHA  #x0033)
  (define-constant TLS-DHE-RSA-WITH-AES-256-CBC-SHA  #x0039)
  (define-constant TLS-DHE-RSA-WITH-AES-128-CBC-SHA256  #x0067)
  (define-constant TLS-DHE-RSA-WITH-AES-256-CBC-SHA256  #x006B)

  ;; anonymous key exchange
  (define-constant TLS-DH-anon-WITH-3DES-EDE-CBC-SHA   #x001B)
  (define-constant TLS-DH-anon-WITH-AES-128-CBC-SHA    #x0034)
  (define-constant TLS-DH-anon-WITH-AES-256-CBC-SHA    #x003A)
  (define-constant TLS-DH-anon-WITH-AES-128-CBC-SHA256 #x006C)
  (define-constant TLS-DH-anon-WITH-AES-256-CBC-SHA256 #x006D)

  (define-constant *dh-key-exchange-algorithms*
    `(
      ,TLS-DHE-RSA-WITH-3DES-EDE-CBC-SHA
      ,TLS-DHE-RSA-WITH-AES-256-CBC-SHA
      ,TLS-DHE-RSA-WITH-AES-128-CBC-SHA
      ,TLS-DHE-RSA-WITH-AES-128-CBC-SHA256
      ,TLS-DHE-RSA-WITH-AES-256-CBC-SHA256
      ))
  (define-constant *dh-anon-key-exchange-algorithms*
    `(
      ,TLS-DH-anon-WITH-3DES-EDE-CBC-SHA
      ,TLS-DH-anon-WITH-AES-128-CBC-SHA
      ,TLS-DH-anon-WITH-AES-256-CBC-SHA
      ,TLS-DH-anon-WITH-AES-128-CBC-SHA256
      ,TLS-DH-anon-WITH-AES-256-CBC-SHA256
      ))

  (define *cipher-suites*
    `(
      ;; cipher suite number   .   public key  (scheme . key-length) hash
      ;; DHE
      (,TLS-DHE-RSA-WITH-AES-256-CBC-SHA256  . (,RSA (,AES . 32) ,SHA-256))
      (,TLS-DHE-RSA-WITH-AES-128-CBC-SHA256  . (,RSA (,AES . 16) ,SHA-256))
      (,TLS-DHE-RSA-WITH-AES-256-CBC-SHA  . (,RSA (,AES . 32) ,SHA-1))
      (,TLS-DHE-RSA-WITH-3DES-EDE-CBC-SHA . (,RSA (,DES3 . 24) ,SHA-1))
      (,TLS-DHE-RSA-WITH-AES-128-CBC-SHA  . (,RSA (,AES . 16) ,SHA-1))
      ;;(,TLS-RSA-WITH-RC4-128-MD5        . (,RSA (RC4 . 16) MD5))
      ;;(,TLS-RSA-WITH-RC4-128-SHA        . (,RSA (RC4 . 16) SHA-1))
      (,TLS-RSA-WITH-3DES-EDE-CBC-SHA   . (,RSA (,DES3 . 24) ,SHA-1))
      (,TLS-RSA-WITH-AES-128-CBC-SHA    . (,RSA (,AES . 16) ,SHA-1))
      (,TLS-RSA-WITH-AES-256-CBC-SHA    . (,RSA (,AES . 32) ,SHA-1))
      (,TLS-RSA-WITH-AES-128-CBC-SHA256 . (,RSA (,AES . 16) ,SHA-256))
      (,TLS-RSA-WITH-AES-256-CBC-SHA256 . (,RSA (,AES . 32) ,SHA-256))
      ;; should be very low priority
      (,TLS-DH-anon-WITH-AES-256-CBC-SHA256 . (#f (,AES . 32) ,SHA-256))
      (,TLS-DH-anon-WITH-AES-128-CBC-SHA256 . (#f (,AES . 16) ,SHA-256))
      (,TLS-DH-anon-WITH-AES-256-CBC-SHA    . (#f (,AES . 32) ,SHA-1))
      (,TLS-DH-anon-WITH-3DES-EDE-CBC-SHA   . (#f (,DES3 . 24) ,SHA-1))
      (,TLS-DH-anon-WITH-AES-128-CBC-SHA    . (#f (,AES . 16) ,SHA-1))

      ;; do not support this (not only for security reason...)
      ;;(,TLS-NULL-WITH-NULL-NULL         . (#f #f #f))
      ;; I guess these need client certificate...
      ;;(,TLS-RSA-WITH-NULL-MD5	        . (,RSA #f ,MD5))
      ;;(,TLS-RSA-WITH-NULL-SHA	        . (,RSA #f ,SHA-1))
      ;;(,TLS-RSA-WITH-NULL-SHA256        . (,RSA #f ,SHA-256))
      ))

  ;; 6.2 Record Layer
  ;; record types
  (define-constant *change-cipher-spec* 20)
  (define-constant *alert*              21)
  (define-constant *handshake*          22)
  (define-constant *application-data*   23)

  ;; 7.2 Alert Protocol
  (define-constant *warning* 1)
  (define-constant *fatal*   2)

  (define-constant *close-notify*                0)
  (define-constant *unexpected-message*          10)
  (define-constant *bad-record-mac*              20)
  ;;(define-constant *decryption-failed-RESERVED*  21)
  (define-constant *record-overflow*             22)
  (define-constant *decompression-failure*       30)
  (define-constant *handshake-failure*           40)
  ;;(define-constant *no-certificate-RESERVED*     41)
  (define-constant *bad-certificate*             42)
  (define-constant *unsupported-certificate*     43)
  (define-constant *certificate-revoked*         44)
  (define-constant *certificate-expired*         45)
  (define-constant *certificate-unknown*         46)
  (define-constant *illegal-parameter*           47)
  (define-constant *unknown-ca*                  48)
  (define-constant *access-denied*               49)
  (define-constant *decode-error*                50)
  (define-constant *decrypt-error*               51)
  ;;(define-constant *export-restriction-RESERVED* 60)
  (define-constant *protocol-version*            70)
  (define-constant *insufficient-security*       71)
  (define-constant *internal-error*              80)
  (define-constant *user-canceled*               90)
  (define-constant *no-renegotiation*            100)
  (define-constant *unsupported-extension*       110)


  ;; 7.4 Handshake Protocol
  ;; handshake types
  (define-constant *hello-request*        0)
  (define-constant *client-hello*         1)
  (define-constant *server-hello*         2)
  (define-constant *certificate*          11)
  (define-constant *server-key-echange*   12)
  (define-constant *certificate-request*  13)
  (define-constant *server-hello-done*    14)
  (define-constant *certificate-verify*   15)
  (define-constant *client-key-exchange*  16)
  (define-constant *finished*             20)

  ;; RFC 5246 7.4.1.4.1
  ;; we don't support no hash
  ;;(define-constant *none*   0)
  (define-constant *md5*    1)
  (define-constant *sha1*   2)
  (define-constant *sha224* 3)
  (define-constant *sha256* 4)
  (define-constant *sha384* 5)
  (define-constant *sha512* 6)
  ;; we only supports RSA as a signature algorithm
  (define-constant *rsa* 1)

  (define *supported-hashes*
    `((,*md5*    . ,MD5)
      (,*sha1*   . ,SHA-1)
      (,*sha224* . ,SHA-224)
      (,*sha256* . ,SHA-256)
      (,*sha384* . ,SHA-384)
      (,*sha512* . ,SHA-512)))

  (define *supported-signatures* `((,*rsa* . ,RSA)))

  (define-constant *server-name* 0)
  (define-constant *application-layer-protocol-negotiation* 16)
  ;; for now followings are not supported 
  (define-constant *max-fragment-lenght* 1)
  (define-constant *client-certificate-url* 2)
  (define-constant *trusted-ca-keys* 3)
  (define-constant *truncate-hmac* 4)
  (define-constant *status-request* 5)
  ;; RFC5246 section 7.4.1.4
  (define-constant *signature-algorithms* 13)

  ;; server name indication
  (define-constant *host-name* 0)
)
