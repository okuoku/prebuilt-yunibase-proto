;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/crypto/mac/hmac.scm - HMAC
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
(library (sagittarius crypto mac hmac)
    (export *mac:hmac*
	    hmac-oid->digest-oid
	    digest-oid->hmac-oid
	    *oid-hmac/md5*
	    *oid-hmac/tiger*
	    *oid-hmac/ripemd-160*
	    *oid-hmac/sha1*
	    *oid-hmac/sha224*
	    *oid-hmac/sha256*
	    *oid-hmac/sha384*
	    *oid-hmac/sha512*
	    *oid-hmac/sha512/224*
	    *oid-hmac/sha512/256*
	    *oid-hmac/sha3-224*
	    *oid-hmac/sha3-256*
	    *oid-hmac/sha3-384*
	    *oid-hmac/sha3-512*)
    (import (rnrs)
	    (clos user)
	    (sagittarius crypto mac types)
	    (rename (sagittarius crypto digests descriptors)
		    (tc-digest-descriptor? builtin-digest-descriptor?))
	    (prefix (sagittarius crypto tomcrypt) tc:))

(define *mac:hmac* :hmac)

(define-method mac-state-initializer ((m (eql *mac:hmac*)) (key <bytevector>)
	      :key ((digest builtin-digest-descriptor?) #f)
	      :allow-other-keys)
  (values (lambda () (tc:hmac-init (tc-digest-descriptor-digest digest) key))
	  (digest-descriptor-digest-size digest)
	  (digest-oid->hmac-oid (digest-descriptor-oid digest))))

(define-method mac-state-processor ((s (eql *mac:hmac*))) tc:hmac-process!)
(define-method mac-state-finalizer ((s (eql *mac:hmac*))) tc:hmac-done!)

(define *oid-hmac/md5*        "1.3.6.1.5.5.8.1.1")
(define *oid-hmac/tiger*      "1.2.840.113549.2.3")
(define *oid-hmac/ripemd-160* "1.3.6.1.5.5.8.1.4")
(define *oid-hmac/sha1*       "1.2.840.113549.2.7")
(define *oid-hmac/sha224*     "1.2.840.113549.2.8")
(define *oid-hmac/sha256*     "1.2.840.113549.2.9")
(define *oid-hmac/sha384*     "1.2.840.113549.2.10")
(define *oid-hmac/sha512*     "1.2.840.113549.2.11")
(define *oid-hmac/sha512/224* "1.2.840.113549.2.12")
(define *oid-hmac/sha512/256* "1.2.840.113549.2.13")
(define *oid-hmac/sha3-224*   "2.16.840.1.101.3.4.2.13")
(define *oid-hmac/sha3-256*   "2.16.840.1.101.3.4.2.14")
(define *oid-hmac/sha3-384*   "2.16.840.1.101.3.4.2.15")
(define *oid-hmac/sha3-512*   "2.16.840.1.101.3.4.2.16")

(define *digest-oid->hmac-oid-map*
  `((,(digest-descriptor-oid *digest:md5*)         . ,*oid-hmac/md5*)
    ;; This is old
    ;; (,(digest-descriptor-oid *digest:sha-1*)      . "1.3.6.1.5.5.8.1.2")
    (,(digest-descriptor-oid *digest:tiger-192*)   . ,*oid-hmac/tiger*)
    (,(digest-descriptor-oid *digest:ripemd-160*)  . ,*oid-hmac/ripemd-160*)
    ;; PKCS#5 HMAC OIDs
    (,(digest-descriptor-oid *digest:sha-1*)       . ,*oid-hmac/sha1*)
    (,(digest-descriptor-oid *digest:sha-224*)     . ,*oid-hmac/sha224*)
    (,(digest-descriptor-oid *digest:sha-256*)     . ,*oid-hmac/sha256*)
    (,(digest-descriptor-oid *digest:sha-384*)     . ,*oid-hmac/sha384*)
    (,(digest-descriptor-oid *digest:sha-512*)     . ,*oid-hmac/sha512*)
    (,(digest-descriptor-oid *digest:sha-512/224*) . ,*oid-hmac/sha512/224*)
    (,(digest-descriptor-oid *digest:sha-512/256*) . ,*oid-hmac/sha512/256*)
    (,(digest-descriptor-oid *digest:sha3-224*)    . ,*oid-hmac/sha3-224*)
    (,(digest-descriptor-oid *digest:sha3-256*)    . ,*oid-hmac/sha3-256*)
    (,(digest-descriptor-oid *digest:sha3-384*)    . ,*oid-hmac/sha3-384*)
    (,(digest-descriptor-oid *digest:sha3-512*)    . ,*oid-hmac/sha3-512*)
    ))
(define *hmac-oid->digest-oid-map*
  (map (lambda (a) (cons (cdr a) (car a))) *digest-oid->hmac-oid-map*))

(define (digest-oid->hmac-oid oid)
  (cond ((assoc oid *digest-oid->hmac-oid-map*) => cdr)
	(else #f)))

(define (hmac-oid->digest-oid oid)
  (cond ((assoc oid *hmac-oid->digest-oid-map*) => cdr)
	(else #f)))

)
