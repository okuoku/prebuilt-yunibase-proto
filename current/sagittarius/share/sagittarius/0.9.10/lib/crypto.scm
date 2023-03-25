;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; crypto.scm Cryptographic library
;;; 
;;; This library is a wrapper for tomcrypto.
#!deprecated
(library (crypto)
    ;; we exports even super low level APIs
    ;; but it won't be documented, so if you use make-cipher, suggest-keysize
    ;; or make-builtin-cipher-spi it might be changed in future.
    (export :all)
    (import (crypto cipher)
	    (crypto key)
	    (crypto pkcs)
	    (crypto mac)
	    (crypto rsa)
	    (crypto dsa)
	    (crypto ecdsa)
	    (crypto eddsa)
	    (crypto rfc7748)
	    (crypto k-generator)
	    (crypto ecdh)
	    (crypto wrap)
	    (only (crypto spi) register-spi)))
