;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/crypto/keys/operations/asymmetric/rsa.scm - RSA key op
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
(library (sagittarius crypto keys operations asymmetric rsa)
    (export generate-key-pair
	    generate-public-key
	    generate-private-key
	    import-public-key
	    import-private-key
	    export-public-key
	    export-private-key

	    *rsa-min-keysize*
	    *key:rsa*
	    
	    rsa-public-key? <rsa-public-key>
	    rsa-public-key-modulus
	    rsa-public-key-exponent

	    rsa-private-key? <rsa-private-key>
	    rsa-private-key-modulus
	    rsa-private-key-private-exponent

	    rsa-crt-private-key? <rsa-crt-private-key>
	    rsa-crt-private-key-public-exponent
	    rsa-crt-private-key-p
	    rsa-crt-private-key-q
	    rsa-crt-private-key-dP
	    rsa-crt-private-key-dQ
	    rsa-crt-private-key-qP)
    (import (rnrs)
	    (clos user)
	    (math modular)
	    (srfi :39 parameters)
	    (srfi :117 list-queues)
	    (sagittarius)
	    (sagittarius mop immutable)
	    (sagittarius crypto asn1)
	    (sagittarius crypto keys types)
	    (sagittarius crypto keys operations asymmetric apis)
	    (sagittarius crypto random)
	    (sagittarius crypto math prime))

(define *key:rsa* :rsa)
(define *rsa-min-keysize* (make-parameter 1024))

(define-class <rsa-key> () ())
(define-class <rsa-public-key> (<public-key> <immutable> <rsa-key>)
  ((modulus :init-keyword :modulus :reader rsa-public-key-modulus)
   (exponent :init-keyword :exponent :reader rsa-public-key-exponent)))
(define (rsa-public-key? o) (is-a? o <rsa-public-key>))
(define (make-rsa-public-key (modulus integer?) (exponent integer?))
  (make <rsa-public-key> :modulus modulus :exponent exponent))

(define-method object-equal? ((o1 <rsa-public-key>) (o2 <rsa-public-key>))
  (and (eqv? (rsa-public-key-modulus o1) (rsa-public-key-modulus o2))
       (eqv? (rsa-public-key-exponent o1) (rsa-public-key-exponent o2))))
(define-method write-object ((o <rsa-public-key>) (p <port>))
  (let-values (((out e) (open-string-output-port)))
    (format out "#<rsa-pubic-key~%")
    (format out "            modulus: ~x~%" (rsa-public-key-modulus o))
    (format out "    public exponent: ~x~%" (rsa-public-key-exponent o))
    (display #\> out)
    (display (e) p)))

(define-class <rsa-private-key> (<private-key> <immutable> <rsa-key>)
  ((modulus :init-keyword :modulus :reader rsa-private-key-modulus)
   (private-exponent :init-keyword :private-exponent
		     :reader rsa-private-key-private-exponent)))
(define (rsa-private-key? o) (is-a? o <rsa-private-key>))
(define (make-rsa-private-key (modulus integer?) (private-exponent integer?))
  (make <rsa-private-key> :modulus modulus :private-exponent private-exponent))

(define-class <rsa-crt-private-key> (<rsa-private-key>)
  ((public-exponent :init-keyword :public-exponent
		    :reader rsa-crt-private-key-public-exponent)
   (p :init-keyword :p :reader rsa-crt-private-key-p)
   (q :init-keyword :q :reader rsa-crt-private-key-q)
   (dP :init-keyword :dP :reader rsa-crt-private-key-dP)
   (dQ :init-keyword :dQ :reader rsa-crt-private-key-dQ)
   (qP :init-keyword :qP :reader rsa-crt-private-key-qP)))
(define (rsa-crt-private-key? o) (is-a? o <rsa-crt-private-key>))
(define (make-rsa-crt-private-key (modulus integer?)
				  (exponent integer?)
				  (private-exponent integer?)
				  (p probable-prime?) (q probable-prime?)
				  :key ((dP integer?) (mod private-exponent (- p 1)))
				       ((dQ integer?) (mod private-exponent (- q 1)))
				       ((qP integer?) (mod-inverse q p)))
  (make <rsa-crt-private-key> :modulus modulus
	:private-exponent private-exponent
	:public-exponent exponent :p p :q q :dP dP :dQ dQ :qP qP))

(define-method object-equal? ((o1 <rsa-private-key>) (o2 <rsa-private-key>))
  (and (eqv? (rsa-private-key-modulus o1) (rsa-private-key-modulus o2))
       (eqv? (rsa-private-key-private-exponent o1)
	     (rsa-private-key-private-exponent o2))))
(define-method object-equal? ((o1 <rsa-crt-private-key>)
			      (o2 <rsa-crt-private-key>))
  (and (call-next-method)
       (eqv? (rsa-crt-private-key-p o1) (rsa-crt-private-key-p o2))
       (eqv? (rsa-crt-private-key-q o1) (rsa-crt-private-key-q o2))
       (eqv? (rsa-crt-private-key-dP o1) (rsa-crt-private-key-dP o2))
       (eqv? (rsa-crt-private-key-dQ o1) (rsa-crt-private-key-dQ o2))
       (eqv? (rsa-crt-private-key-qP o1) (rsa-crt-private-key-qP o2))))

(define-method write-object ((o <rsa-crt-private-key>) (p <port>))
  (let-values (((out e) (open-string-output-port)))
    (format out "#<rsa-crt-private-key~%")
    (format out "            modulus: ~x~%" (rsa-private-key-modulus o))
    (format out "    public exponent: ~x~%"
	    (rsa-crt-private-key-public-exponent o))
    (format out "   private exponent: ~x~%"
	    (rsa-private-key-private-exponent o))
    (format out "           prime P: ~x~%" (rsa-crt-private-key-p o))
    (format out "           prime Q: ~x~%" (rsa-crt-private-key-q o))
    (format out "  prime exponent P: ~x~%" (rsa-crt-private-key-dP o))
    (format out "  prime exponent Q: ~x~%" (rsa-crt-private-key-dQ o))
    (format out "   crt coefficient: ~x~%" (rsa-crt-private-key-qP o))
    (display #\> out)
    (display (e) p)))

(define-method generate-key-pair ((m (eql *key:rsa*))
				  :key (size 2048)
				       (prng (secure-random-generator *prng:chacha20*))
				  :allow-other-keys)
  (rsa-generate-key-pair size prng #x10001))

(define-method generate-public-key ((n (eql *key:rsa*)) m e)
  (make-rsa-public-key m e))
(define-method generate-private-key ((n (eql *key:rsa*)) m e . rest)
  (apply rsa-generate-private-key m e rest))

(define (rsa-generate-private-key m private-exponent
				  :key (e :public-exponent #f) (p #f) (q #f)
				  :allow-other-keys rest)
  (if (and e p q)
      (apply make-rsa-crt-private-key m e private-exponent p q rest)
      (make-rsa-private-key m private-exponent)))

(define (rsa-generate-key-pair size prng e)
  (define (create-key-pair n e d p q)
    (let ((private (make-rsa-crt-private-key n e d p q))
	  (public (make-rsa-public-key n e)))
      (make-key-pair private public)))

  (define (rsa-random-prime check)
    (let loop ((p (generate-random-prime (/ size 16) prng)))
      (if (and (or (not check) (not (= p check))) (= 1 (gcd (- p 1) e)))
	  p
	  (loop (generate-random-prime (/ size 16) prng)))))
  (when (< size (*rsa-min-keysize*))
    (assertion-violation 'rsa-generate-key-pair
			 "Generating key size is too small"))
  (unless (probable-prime? e)
    (assertion-violation 'rsa-generate-key-pair
			 "exponent is not prime number" e))

  (let* ((p (rsa-random-prime #f))
	 (q (rsa-random-prime p))
	 (n (* p q))
	 (phi (* (- p 1) (- q 1))))
    (let ((d (mod-inverse e phi)))
      (create-key-pair n e d p q))))

;;; Export and import
;; misc
(define *rsa-oid* "1.2.840.113549.1.1.1")
(define *rsa-key-oids*
  `(,*rsa-oid*
    "1.2.840.113549.1.1.2" 
    "1.2.840.113549.1.1.3" 
    "1.2.840.113549.1.1.4" 
    "1.2.840.113549.1.1.5" 
    "1.2.840.113549.1.1.7" 
    "1.2.840.113549.1.1.10"
    "1.2.840.113549.1.1.11"
    "1.2.840.113549.1.1.12"
    "2.5.8.1.1"))
(define-method oid->key-operation ((oid (member *rsa-key-oids*))) *key:rsa*)
(define-method key->oid ((k <rsa-key>)) *rsa-oid*)
;; We use RSAPublicKey for default
;;
;; RSAPublicKey ::= SEQUENCE {
;;     modulus           INTEGER,  -- n
;;     publicExponent    INTEGER   -- e
;; }
;;
(define (rsa-import-raw-public-key (public der-sequence?))
  (let ((objects (list-queue-list (asn1-collection-elements public))))
    (unless (= 2 (length objects))
      (assertion-violation 'rsa-import-public-key
			   "Invalid format of RSAPublicKey" public))
    (unless (for-all der-integer? objects)
      (assertion-violation 'rsa-import-public-key
			   "Invalid format of RSAPublicKey" public))
    (make-rsa-public-key (der-integer->integer (car objects))
			 (der-integer->integer (cadr objects)))))

(define (check-rsa-aid aid)
  (unless (der-sequence? aid)
    (assertion-violation 'rsa-import-spki-public-key
			 "Invalid format of AlgorithmIdentifier" aid))
  (let ((oid (asn1-collection-ref aid 0)))
    (unless (der-object-identifier? oid)
      (assertion-violation 'rsa-import-spki-public-key
			   "Invalid format of AlgorithmIdentifier" aid))
    (unless (eq? *key:rsa*
		 (oid->key-operation (der-object-identifier->oid-string oid)))
      (assertion-violation 'rsa-import-spki-public-key
			   "Invalid format of AlgorithmIdentifier" aid))))
;; SubjectPublicKeyInfo {ALGORITHM: IOSet} ::= SEQUENCE {
;;      algorithm        AlgorithmIdentifier {{IOSet}},
;;      subjectPublicKey BIT STRING
;; }
(define (rsa-import-spki-public-key (public der-sequence?))
  (check-rsa-aid (asn1-collection-ref public 0))
  (let ((bit-string (asn1-collection-ref public 1)))
    (unless (der-bit-string? bit-string)
      (assertion-violation 'rsa-import-spki-public-key
			   "Invalid format of SubjectPublicKeyInfo" public))
    (rsa-import-raw-public-key (bytevector->asn1-object
				(der-bit-string->bytevector bit-string)))))
(define-method import-public-key ((m (eql *key:rsa*)) (in <bytevector>) . opts)
  (apply import-public-key m (open-bytevector-input-port in) opts))
(define-method import-public-key ((m (eql *key:rsa*)) (in <port>) . opts)
  (apply import-public-key m (read-asn1-object in) opts))
(define-method import-public-key ((m (eql *key:rsa*)) (in <der-sequence>)
				  :optional (format (public-key-format raw)))
  (case format
    ((raw) (rsa-import-raw-public-key in))
    ((subject-public-key-info) (rsa-import-spki-public-key in))
    (else (assertion-violation 'import-public-key "Unknown public key format"
			       format))))

(define (rsa-export-raw-public-key (key rsa-public-key?))
  (asn1-encodable->bytevector
   (der-sequence (integer->der-integer (rsa-public-key-modulus key))
		 (integer->der-integer (rsa-public-key-exponent key)))))

(define (rsa-export-spki-public-key (key rsa-public-key?))
  (let ((raw (rsa-export-raw-public-key key)))
    (asn1-encodable->bytevector
     (der-sequence
      (der-sequence (oid-string->der-object-identifier *rsa-oid*))
      (bytevector->der-bit-string raw)))))
  
(define-method export-public-key ((key <rsa-public-key>) . opts)
  (apply export-public-key *key:rsa* key opts))
(define-method export-public-key ((m (eql *key:rsa*)) (key <rsa-public-key>)
				  :optional (format (public-key-format raw)))
  (case format
    ((raw) (rsa-export-raw-public-key key))
    ((subject-public-key-info) (rsa-export-spki-public-key key))
    (else (assertion-violation 'export-public-key "Unknown public key format"
			       format))))

;; RSAPrivateKey ::= SEQUENCE {
;;     version           Version,
;;     modulus           INTEGER,  -- n
;;     publicExponent    INTEGER,  -- e
;;     privateExponent   INTEGER,  -- d
;;     prime1            INTEGER,  -- p
;;     prime2            INTEGER,  -- q
;;     exponent1         INTEGER,  -- d mod (p-1)
;;     exponent2         INTEGER,  -- d mod (q-1)
;;     coefficient       INTEGER,  -- (inverse of q) mod p
;;     otherPrimeInfos   OtherPrimeInfos OPTIONAL -- We do not support this.
;; }
;; Version ::= INTEGER { two-prime(0), multi(1) }
;;    (CONSTRAINED BY
;;    {-- version must be multi if otherPrimeInfos present --})
(define (rsa-import-private-key (private der-sequence?))
  (define (check-der-integer v)
    (unless (der-integer? v)
      (assertion-violation 'rsa-import-private-key
			   "Invalid RSAPrivateKey format" private))
    (der-integer->uinteger v))
  ;; lazy length validation :D
  (let-values (((v m e pe p q dP dQ qP . other)
		(apply values (list-queue-list
			       (asn1-collection-elements private)))))
    (unless (zero? (check-der-integer v))
      (assertion-violation 'rsa-import-private-key
			   "otherPrimeInfos are not supported"))
    (make-rsa-crt-private-key (check-der-integer m)
			      (check-der-integer e)
			      (check-der-integer pe)
			      (check-der-integer p)
			      (check-der-integer q)
			      :dP (check-der-integer dP)
			      :dQ (check-der-integer dQ)
			      :qP (check-der-integer qP))))

(define-method import-private-key ((m (eql *key:rsa*)) (in <bytevector>) . opts)
  (apply import-private-key m (open-bytevector-input-port in) opts))
(define-method import-private-key ((m (eql *key:rsa*)) (in <port>) . opts)
  (apply import-private-key m (read-asn1-object in) opts))
(define-method import-private-key ((m (eql *key:rsa*)) (in <der-sequence>)
				   . opts)
  (rsa-import-private-key in))

(define (rsa-export-private-key (key rsa-crt-private-key?))
  (asn1-encodable->bytevector
   (der-sequence
    (integer->der-integer 0)
    (integer->der-integer (rsa-private-key-modulus key))
    (integer->der-integer (rsa-crt-private-key-public-exponent key))
    (integer->der-integer (rsa-private-key-private-exponent key))
    (integer->der-integer (rsa-crt-private-key-p key))
    (integer->der-integer (rsa-crt-private-key-q key))
    (integer->der-integer (rsa-crt-private-key-dP key))
    (integer->der-integer (rsa-crt-private-key-dQ key))
    (integer->der-integer (rsa-crt-private-key-qP key)))))
(define-method export-private-key ((key <rsa-crt-private-key>) . opts)
  (apply export-private-key *key:rsa* key opts))
(define-method export-private-key ((m (eql *key:rsa*))
				   (key <rsa-crt-private-key>) . opts)
  (rsa-export-private-key key))

)
