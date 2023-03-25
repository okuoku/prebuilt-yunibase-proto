;; -*- mode: scheme; coding: utf-8; -*-
(library (encoding decoder)
    (export lookup-decoder
	    decode)
    (import (rnrs)
	    (only (sagittarius) format)
	    (sagittarius control)
	    (encoding sjis)
	    (encoding euc-jp))

  (define *supported-encoding*
    `((iso-8859-1 . ,(latin-1-codec))
      (us-ascii   . ,(latin-1-codec)) ;; should this be other codec?
      (sjis       . ,(sjis-codec))
      (shift_jis  . ,(sjis-codec))
      (euc-jp     . ,(euc-jp-codec))
      (latin-1    . ,(latin-1-codec))
      (utf-8      . ,(utf-8-codec))
      (utf-16     . ,(utf-16-codec))))

  (define (lookup-decoder charset)
    (cond ((and (string? charset)
		(assq (string->symbol (string-downcase charset))
		      *supported-encoding*))
	   => cdr)
	  ((and (symbol? charset)
		(assq charset *supported-encoding*))
	   => cdr)
	  (else
	   (assertion-violation 'lookup-decoder
				"Given charset is not supported"
				charset))))

  ;; decoder is just a codec
  (define (decode decoder bytes :optional (eol-style 'lf))
    (let* ((tr (make-transcoder decoder eol-style))
	   (in (open-bytevector-input-port bytes tr)))
      (get-string-all in)))
)