;;; -*- Scheme -*-
;;;
;;; pkcs 5.scm - Password Based Cryptography library.
;;;  
;;;   Copyright (c) 2010-2012  Takashi Kato  <ktakashi@ymail.com>
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

;; Key derivation, PBES1 and PBES2 are supported.
;; MAC is not yet.
;; Ref
;; - https://datatracker.ietf.org/doc/html/rfc2898 (deprecated)
;; - https://datatracker.ietf.org/doc/html/rfc8018
#!nounbound
(library (rsa pkcs :5)
    (export pbkdf-1 pbkdf-2 derive-key
	    PKCS5-S1 PKCS5-S2
	    pbe-with-md5-and-des pbe-with-sha1-and-des
	    pbe-with-md5-and-rc2 pbe-with-sha1-and-rc2
	    pbes2

	    make-pbe-parameter pbe-parameter?
	    pbe-parameter-salt pbe-parameter-iteration

	    generate-secret-key
	    make-pbes2-parameter pbes2-parameter?
	    make-pbkdf2-parameter
	    make-encryption-scheme
	    ;; To reuse cipher, we also need to export this method
	    derive-key&iv
	    <pbe-secret-key> 
	    <pbe-parameter>
	    
	    
	    <pbes2-parameter>
	    
	    <pbe-cipher-spi>)
    (import (rnrs)
	    (asn.1)
	    (clos core)
	    (clos user)
	    (crypto)
	    (math)
	    (rsa pkcs :10)
	    (rfc hmac)
	    (sagittarius)
	    (sagittarius control)
	    (sagittarius object)
	    (util bytevector))

  (define (pbkdf-2 P S c dk-len 
		   :key (algo :hash (hash-algorithm SHA-1))
		   (prf (hash-algorithm HMAC :key P :hash algo)))
    (define (F P S c i)
      (define (concat bv int)
	(let* ((len (bytevector-length bv))
	       (new (make-bytevector (+ len 4)))
	       (iv  (make-bytevector 4)))
	  (bytevector-u32-set! iv 0 int 'big)
	  (bytevector-copy! bv 0 new 0 len)
	  (bytevector-copy! iv 0 new len 4)
	  new))
      ;; create us
      (let* ((size (hash-size prf))
	     (buf (make-bytevector size))
	     (out (make-bytevector size)))
	(dotimes (j c)
	  (cond ((zero? j)
		 (hash! prf out (concat S i))
		 (bytevector-copy! out 0 buf 0 size))
		(else
		 (hash! prf buf buf)
		 (bytevector-xor! out out buf))))
	out))
    ;; (- (expt 2 32) 1) -> #xffffffff
    (when (> dk-len (* #xffffffff (hash-size prf)))
      (assertion-violation 'pbkdf-2 "derived key too long"))
    (let* ((h-len (hash-size prf))
	   (l (ceiling (/ dk-len h-len)))
	   (r (* (- dk-len (- l 1) h-len)))
	   (ts (make-vector l))
	   (unit-size (hash-size prf)))
      (dotimes (i l)
	(vector-set! ts i (F P S c (+ i 1))))
      ;; concat
      (let ((dk (make-bytevector dk-len)))
	(let loop ((stored 0)
		   (i 0))
	  (unless (= stored dk-len)
	    (let ((count (if (>= (- dk-len stored) unit-size)
			     unit-size
			     (- dk-len stored))))
	      (bytevector-copy! (vector-ref ts i) 0 dk stored count)
	      (loop (+ count stored) (+ i 1)))))
	dk)))

  (define (pbkdf-1 P S c dk-len :key (algo :hash (hash-algorithm SHA-1)))
    (when (> dk-len (hash-size algo))
      (assertion-violation 'pbkdf-1 "derived key too long"))
    (let* ((buf   (make-bytevector (hash-size algo)))
	   (out   (make-bytevector dk-len)))
      (hash-init! algo)
      (hash-process! algo P)
      (hash-process! algo S)
      (hash-done! algo buf)
      (dotimes (i (- c 1))
	(hash! algo buf buf))
      (bytevector-copy! buf 0 out 0 dk-len)
      out))

  (define (derive-key P S c dk-len :key (kdf pbkdf-2) :allow-other-keys rest)
    (unless (and (bytevector? P) (bytevector? S))
      (assertion-violation 'derive-key "bytevector required" P S))
    (unless (and (integer? c) (positive? c))
      (assertion-violation 'derive-key
			   "positive integer required for iteration count") c)
    (apply kdf P S c dk-len rest))

  (define-class <passward-based-secret-key> (<symmetric-key>)
    ((password :init-keyword :password)))
  
  (define-class <pbe-secret-key> (<passward-based-secret-key>)
    ((hash     :init-keyword :hash)
     (scheme   :init-keyword :scheme)
     (type     :init-keyword :type)
     (iv-size  :init-keyword :iv-size)
     (length   :init-keyword :length)))

  (define-class <pbes2-secret-key> (<passward-based-secret-key>) ())
  
  ;; markers  
  (define PKCS5-S1 :PKCS5-S1)
  (define PKCS5-S2 :PKCS5-S2)
  ;; the names are used also for cipher
  (define pbe-with-md5-and-des  :pbe-with-md5-and-des)
  (define pbe-with-sha1-and-des :pbe-with-sha1-and-des)
  (define pbe-with-md5-and-rc2  :pbe-with-md5-and-rc2)
  (define pbe-with-sha1-and-rc2 :pbe-with-sha1-and-rc2)
  (define pbes2 :pbes2)
  
  ;; PBE parameter, it holds salt and iteration count.
  (define-class <pbe-parameter> (<asn.1-encodable>)
    ((salt      :init-keyword :salt :reader pbe-parameter-salt)
     (iteration :init-keyword :iteration :reader pbe-parameter-iteration)))
  (define-method make-pbe-parameter ((salt <bytevector>)
				     (iteration <integer>))
    (unless (and (integer? iteration)
		 (positive? iteration))
      (assertion-violation 'make-pbe-parameter
			   "iteration must be a non negative exact integer"
			   iteration))
    (make <pbe-parameter> :salt salt :iteration iteration))
  (define (pbe-parameter? o) (is-a? o <pbe-parameter>))
  (define-method make-pbe-parameter ((salt <der-octet-string>)
				     (iteration <der-integer>))
    (make-pbe-parameter (der-octet-string-octets salt)
			(der-integer->integer iteration)))
  (define-method make-pbe-parameter ((seq <asn.1-sequence>))
    (make-pbe-parameter (asn.1-sequence-get seq 0)
			(asn.1-sequence-get seq 1)))
  (define-method der-encodable->der-object ((p <pbe-parameter>))
    (make-der-sequence (make-der-octet-string (slot-ref p 'salt))
		       (make-der-integer (slot-ref p 'iteration))))

  ;; PBES2 parameter
  (define *prf-oids*
    `(("1.2.840.113549.2.7" . ,SHA-1)
      ("1.2.840.113549.2.8" . ,SHA-224)
      ("1.2.840.113549.2.9" . ,SHA-256)
      ("1.2.840.113549.2.10" . ,SHA-384)
      ("1.2.840.113549.2.11" . ,SHA-512)
      ("1.2.840.113549.2.12" . ,SHA-512/224)
      ("1.2.840.113549.2.13" . ,SHA-512/256)))
  (define *reverse-prf-oid*
    (map (lambda (e) (cons (cdr e) (car e))) *prf-oids*))

  (define-class <pbkdf2-parameter> ()
    ((salt       :init-keyword :salt)
     (iteration  :init-keyword :iteration)
     (key-length :init-keyword :key-length)
     (prf        :init-keyword :prf)))
  (define-method make-pbkdf2-parameter ((salt <bytevector>)
					(iteration <integer>)
					(key-length <integer>)
					prf)
    (make <pbkdf2-parameter> :salt salt :iteration iteration
	  :key-length key-length :prf prf))  
  (define-method make-pbkdf2-parameter ((seq <asn.1-sequence>))
    (make-pbkdf2-parameter (make-algorithm-identifier seq)))
  (define-method make-pbkdf2-parameter ((aid <algorithm-identifier>))
    (unless (equal? (algorithm-identifier-id aid) "1.2.840.113549.1.5.12")
      (assertion-violation 'make-pbkdf2-parameter "Unknown OID" aid))
    (let* ((param (~ aid 'parameters))
	   (salt (asn.1-sequence-get param 0))
	   (prf (make-algorithm-identifier (asn.1-sequence-get param 3))))
      (make <pbkdf2-parameter>
	:salt (if (is-a? salt <der-octet-string>)
		  (der-octet-string-octets salt)
		  (assertion-violation 'make-pbkdf2-parameter
				       "PBKDF2-SaltSources is not supported"
				       salt))
	:iteration (der-integer->integer (asn.1-sequence-get param 1))
	:key-length (der-integer->integer (asn.1-sequence-get param 2))
	:prf (cond ((assoc (algorithm-identifier-id prf) *prf-oids*) => cdr)
		   (else (assertion-violation 'make-pbkdf2-parameter
					    "Unknown PRF OID" prf))))))
  (define-method der-encodable->der-object ((p <pbkdf2-parameter>))
    (define (oid->aid oid)
      (der-encodable->der-object (make-algorithm-identifier (cdr oid))))
    (make-algorithm-identifier "1.2.840.113549.1.5.12"
     (make-der-sequence
      (make-der-octet-string (~ p 'salt))
      (make-der-integer (~ p 'iteration))
      (make-der-integer (~ p 'key-length))
      (cond ((assq (~ p 'prf) *reverse-prf-oid*) => oid->aid)
	    (else (assertion-violation 'der-encodable->der-object
				       "Unknown PRF" (~ p 'prf)))))))
  (define-method write-object ((o <pbkdf2-parameter>) out)
    (format out "#<pbkdf2-parameter> salt=~a, iteration=~a, dkLen=~a prf=~a>"
	    (~ o 'salt)
	    (~ o 'iteration)
	    (~ o 'key-length)
	    (~ o 'prf)))

  (define *enc-oids*
    `(("2.16.840.1.101.3.4.1.2" .  ,AES-128) ;; AES128-CBC-Pad
      ("2.16.840.1.101.3.4.1.22" . ,AES-192) ;; AES192-CBC-Pad
      ("2.16.840.1.101.3.4.1.42" . ,AES-256) ;; AES256-CBC-Pad
      ))
  
  (define *reverse-enc-oids*
    (map (lambda (e) (cons (cdr e) (car e))) *enc-oids*))
  (define-class <pbes2-encryption-scheme> (<asn.1-encodable>)
    ((scheme :init-keyword :scheme)
     (iv :init-keyword :iv)))
  (define-method make-encryption-scheme (scheme (iv <bytevector>))
    (unless (assoc scheme *reverse-enc-oids*)
      (assertion-violation 'make-encryption-scheme
			   "Encryption scheme not supported" scheme))
    (make <pbes2-encryption-scheme> :scheme scheme :iv iv))

  (define-method make-encryption-scheme ((aid <algorithm-identifier>))
    (make <pbes2-encryption-scheme>
      :scheme (cond ((assoc (algorithm-identifier-id aid) *enc-oids*) => cdr)
		    (else (assertion-violation 'make-encryption-scheme
					       "Unknown encryption OID" aid)))
      :iv (and (let ((p (algorithm-identifier-parameters aid)))
		 (and (is-a? p <der-octet-string>)
		      (der-octet-string-octets p))))))

  (define-method der-encodable->der-object ((p <pbes2-encryption-scheme>))
    (der-encodable->der-object
     (make-algorithm-identifier
      (cond ((assq (~ p 'scheme) *reverse-enc-oids*) => cdr)
	    (else
	     (assertion-violation 'der-encodable->der-object
				  "Unknown encryption scheme" (~ p 'scheme))))
      (make-der-octet-string (~ p 'iv)))))
  (define-method write-object ((o <pbes2-encryption-scheme>) out)
    (format out "#<pbes2-encryption-scheme scheme=~a>" (~ o 'scheme)))
  
  (define-class <pbes2-parameter> ()
    ((key-derivation    :init-keyword :key-derivation)
     (encryption-scheme :init-keyword :encryption-scheme)))
  (define-method make-pbes2-parameter ((key-derive <pbkdf2-parameter>)
				       (enc-scheme <pbes2-encryption-scheme>))
    (make <pbes2-parameter>
      :key-derivation key-derive :encryption-scheme enc-scheme))
  (define-method make-pbes2-parameter ((seq <asn.1-sequence>))
    (make <pbes2-parameter>
      :key-derivation (make-pbkdf2-parameter (asn.1-sequence-get seq 0))
      :encryption-scheme (make-encryption-scheme (make-algorithm-identifier
						  (asn.1-sequence-get seq 1)))))
  (define (pbes2-parameter? o) (is-a? o <pbes2-parameter>))
  
  (define-method der-encodable->der-object ((p <pbes2-parameter>))
    (make-der-sequence (der-encodable->der-object (~ p 'key-derivation))
		       (der-encodable->der-object (~ p 'encryption-scheme)))
    #;(der-encodable->der-object
     (make-algorithm-identifier
      "1.2.840.113549.1.5"
      )))
  (define-method write-object ((o <pbes2-parameter>) out)
    (format out "#<pbes2-parameter ~a:~a>"
	    (~ o 'key-derivation)
	    (~ o 'encryption-scheme)))


  ;; secret key generation
  ;; DES: key length 8, iv length 8
#|
  (define-method generate-secret-key ((marker <pbe-md2-des>)
				      (password <string>))
    (make <pbe-secret-key> :password password :hash (hash-algorithm MD2)
	  :scheme DES :iv-size 8 :length 8
	  :type PKCS5-S1))
|#
  (define-method generate-secret-key ((m (eql pbe-with-md5-and-des))
				      (password <string>))
    (make <pbe-secret-key> :password password :hash (hash-algorithm MD5)
	  :scheme DES :iv-size 8 :length 8
	  :type PKCS5-S1))
  (define-method generate-secret-key ((m (eql pbe-with-sha1-and-des))
				      (password <string>))
    (make <pbe-secret-key> :password password :hash (hash-algorithm SHA-1)
	  :scheme DES :iv-size 8 :length 8
	  :type PKCS5-S1))
  ;; RC2 key length 8, iv length 8
#|
  (define-method generate-secret-key ((marker <pbe-md2-rc2>)
				      (password <string>))
    (make <pbe-secret-key> :password password :hash (hash-algorithm MD2)
	  :scheme RC2 :iv-size 8 :length 8
	  :type PKCS5-S1))
|#
  (define-method generate-secret-key ((m (eql pbe-with-md5-and-rc2))
				      (password <string>))
    (make <pbe-secret-key> :password password :hash (hash-algorithm MD5)
	  :scheme RC2 :iv-size 8 :length 8
	  :type PKCS5-S1))
  (define-method generate-secret-key ((m (eql pbe-with-sha1-and-rc2))
				      (password <string>))
    (make <pbe-secret-key> :password password :hash (hash-algorithm SHA-1)
	  :scheme RC2 :iv-size 8 :length 8
	  :type PKCS5-S1))

  (define-method generate-secret-key ((m (eql pbes2)) (password <string>))
    (make <pbes2-secret-key> :password password))
  
  ;; for pbe-cipher-spi we need derive derived key and iv from given
  ;; secret key and parameter(salt and iteration count)
  ;; see PKCS#5 or RFC2898 section 6.1.1 Encryption Operation
  (define (derive-key&iv-internal key param . option)
    (define (derive-pkcs5-key key parameter)
      (define (separate-key-iv dk key-len iv-len)
	(let ((k (make-bytevector key-len))
	      (iv (make-bytevector iv-len)))
	  (bytevector-copy! dk 0 k 0 key-len)
	  (bytevector-copy! dk key-len iv 0 iv-len)
	  (values k iv)))
      (let* ((key-len (slot-ref key 'length))
	     (iv-len (slot-ref key 'iv-size))
	     (derived-key (apply derive-key
				 (string->utf8 (slot-ref key 'password))
				 (slot-ref parameter 'salt)
				 (slot-ref parameter 'iteration)
				 (+ key-len iv-len)
				 ;;(hash-size (slot-ref key 'hash))
				 option)))
	(separate-key-iv derived-key key-len iv-len)))
    (derive-pkcs5-key key param))

  (define-method derive-key&iv ((m (eql PKCS5-S1))
				(key <pbe-secret-key>)
				(param <pbe-parameter>))
    (derive-key&iv-internal key param :kdf pbkdf-1 :hash (slot-ref key 'hash)))

  (define-method derive-key&iv ((m (eql PKCS5-S2))
				(key <pbe-secret-key>)
				(param <pbe-parameter>))
    (derive-key&iv-internal key param))

  (define-method derive-key&iv ((m (eql PKCS5-S2))
				(key <pbes2-secret-key>)
				(param <pbes2-parameter>))
    (let* ((derive-param (~ param 'key-derivation))
	   (enc-param (~ param 'encryption-scheme))
	   (password (string->utf8 (~ key 'password)))
	   (derived-key (pbkdf-2 password
				 (~ derive-param 'salt)
				 (~ derive-param 'iteration)
				 (~ derive-param 'key-length)
				 :hash (~ derive-param 'prf))))
      (values derived-key (~ enc-param 'iv))))

  ;; The PKCS#5 cipher. This can be used by PKCS#12 too.
  (define-class <password-based-cipher-spi> (<cipher-spi>)
    ((parameter :init-keyword :parameter)
     (cipher    :init-keyword :cipher)))

  (define (extract-key&param initargs key-type parameter-type)
    (let ((key (car initargs))
	  (rest (cdr initargs)))
      (unless (is-a? key key-type)
	(assertion-violation 'initialize
	  (format "key must be an instance of ~a" (class-name key-type)) key))
      (let-keywords* rest
	  ((parameter #f) . rest)
	(unless parameter
	  (assertion-violation 
	   'initialize
	   "required keyword argument :parameter is missing"))
	(unless (is-a? parameter parameter-type)
	  (assertion-violation 'initialize
	    (format "parameter must be an instance of ~a" 
		    (class-name parameter-type))
	    parameter))
	(values key parameter))))
  
  (define-class <pbe-cipher-spi> (<password-based-cipher-spi>) ())
  (define-method initialize ((spi <pbe-cipher-spi>) args)
    (let*-values (((key parameter)
		   (extract-key&param args <pbe-secret-key> <pbe-parameter>))
		  ((dkey iv)
		   (derive-key&iv (slot-ref key 'type) key parameter)))      
      (let* ((derived-secret-key
	      (generate-secret-key (slot-ref key 'scheme) dkey))
	     (cipher
	      (make-cipher (slot-ref key 'scheme) derived-secret-key
		:mode-parameter (make-composite-parameter
				 (make-mode-name-parameter MODE_CBC)
				 (make-padding-parameter pkcs5-padder)
				 (make-iv-parameter iv)))))
	(slot-set! spi 'name (format "pbe-~a" (slot-ref key 'scheme)))
	(slot-set! spi 'key derived-secret-key)
	(slot-set! spi 'encrypt (lambda (pt key) (cipher-encrypt cipher pt)))
	(slot-set! spi 'decrypt (lambda (ct key) (cipher-decrypt cipher ct)))
	(slot-set! spi 'padder #f)
	(slot-set! spi 'signer #f)
	(slot-set! spi 'verifier #f)
	(slot-set! spi 'keysize (slot-ref key 'length)))))

  (define-class <pbes2-cipher-spi> (<password-based-cipher-spi>) ())
  (define-method initialize ((spi <pbes2-cipher-spi>) args)
    (let*-values (((key parameter)
		   (extract-key&param args <pbes2-secret-key>
				      <pbes2-parameter>))
		  ((enc-scheme mode iv key-length)
		   (extract-pbes2-parameters parameter))
		  ((dkey iv) (derive-key&iv PKCS5-S2 key parameter)))
      (let* ((derived-secret-key (generate-secret-key enc-scheme dkey))
	     (cipher
	      (make-cipher enc-scheme derived-secret-key
		:mode-parameter (make-composite-parameter
				 (make-mode-name-parameter mode)
				 (make-padding-parameter pkcs5-padder)
				 (make-iv-parameter iv)))))
	(slot-set! spi 'name "pbes2-key")
	(slot-set! spi 'key derived-secret-key)
	(slot-set! spi 'encrypt (lambda (pt key) (cipher-encrypt cipher pt)))
	(slot-set! spi 'decrypt (lambda (ct key) (cipher-decrypt cipher ct)))
	(slot-set! spi 'padder #f)
	(slot-set! spi 'signer #f)
	(slot-set! spi 'verifier #f)
	(slot-set! spi 'keysize key-length))))

  (define (extract-pbes2-parameters parameter)
    (let ((key-param (~ parameter 'key-derivation))
	  (enc-param (~ parameter 'encryption-scheme)))
      (values (~ enc-param 'scheme)
	      MODE_CBC (~ enc-param 'iv) (~ key-param 'key-length))))
  
  ;; DES
  ;;(register-spi pbe-with-md2-and-des <pbe-cipher-spi>)
  (register-spi pbe-with-md5-and-des <pbe-cipher-spi>)
  (register-spi pbe-with-sha1-and-des <pbe-cipher-spi>)
  ;; RC2
  ;;(register-spi pbe-with-md2-and-rc2 <pbe-cipher-spi>)
  (register-spi pbe-with-md5-and-rc2 <pbe-cipher-spi>)
  (register-spi pbe-with-sha1-and-rc2 <pbe-cipher-spi>)

  (register-spi pbe-with-sha1-and-rc2 <pbe-cipher-spi>)

  (register-spi pbes2 <pbes2-cipher-spi>)
)
