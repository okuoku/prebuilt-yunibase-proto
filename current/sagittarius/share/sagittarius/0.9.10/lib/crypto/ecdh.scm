;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; crypto/key/ecdh.scm - ECDH and ECCDH
;;;
;;;  Copyright (c) 2017 Takashi Kato. All rights reserved.
;;;
;;;  Redistribution and use in source and binary forms, with or without
;;;  modification, are permitted provided that the following conditions
;;;  are met:
;;;
;;;  1. Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;
;;;  2. Redistributions in binary form must reproduce the above copyright
;;;     notice, this list of conditions and the following disclaimer in the
;;;     documentation and/or other materials provided with the distribution.
;;;
;;;  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;  OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;  TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;; reference:
;;  - SEC1 v2
;;    http://www.secg.org/sec1-v2.pdf
;;  - 800-56A
;;    http://nvlpubs.nist.gov/nistpubs/SpecialPublications/NIST.SP.800-56Ar2.pdf
#!nounbound
(library (crypto ecdh)
    (export ECDH
	    ecdh-calculate-agreement
	    ecdhc-calculate-agreement)
    (import (rnrs)
	    (sagittarius)
	    (math ec)
	    (clos user)
	    (crypto key agreement)
	    (crypto ecdsa))
  (define ECDH :ecdh)
  ;; SEC1 v2, section 3.3.1
  ;; ec-param - a curve parameter
  ;; du       - private key d from ec-priavate-key
  ;; Qv       - public key Q from ec-public-key (ec-point)
  (define (ecdh-calculate-agreement ec-param du Qv)
    (define curve (ec-parameter-curve ec-param))
    (let ((P (ec-point-mul curve Qv du)))
      (when (ec-point-infinity? P) (error 'ecdh-calculate-agreement "invalid"))
      (ec-point-x P)))

  ;; SEC1 v2, section 3.3.2
  ;; ec-param - a curve parameter
  ;; du       - private key d from ec-priavate-key
  ;; Qv       - public key Q from ec-public-key (ec-point)
  ;; NB: prefix ECDEC is taken from cofactor agreement of bouncy castle
  (define (ecdhc-calculate-agreement ec-param du Qv)
    (define curve (ec-parameter-curve ec-param))
    (define h (ec-parameter-h ec-param))
    (define n (ec-parameter-n ec-param))
    (let ((P (ec-point-mul curve Qv (mod (* du h) n))))
      (when (ec-point-infinity? P) (error 'ecdh-calculate-agreement "invalid"))
      (ec-point-x P)))

  (define-method calculate-key-agreement ((m (eql ECDH))
					  (priv <ecdsa-private-key>)
					  (pub <ecdsa-public-key>))
    (define param (ecdsa-private-key-parameter priv))
    (unless (equal? (ec-parameter-curve param)
		    (ec-parameter-curve (ecdsa-public-key-parameter pub)))
      (assertion-violation 'calculate-key-agreement
			   "Key type are not the same"))
    ;; it's rather weird to return integer as secret key
    ;; so convert it to bytevector.
    ;; NOTE: actual implementation return integer for whatever the reason
    (integer->bytevector
     (ecdhc-calculate-agreement param (ecdsa-private-key-d priv)
				(ecdsa-public-key-Q pub))))
)
