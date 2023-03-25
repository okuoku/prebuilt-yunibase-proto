;; -*- scheme -*-
(library (rnrs arithmetic flonums (6))
    (export flonum?
	    real->flonum
	    fl=?
	    fl<?
	    fl>?
	    fl<=?
	    fl>=?
	    flinteger?
	    flzero?
	    flpositive?
	    flnegative?
	    flodd?
	    fleven?
	    flfinite?
	    flinfinite?
	    flnan?
	    flmax
	    flmin
	    fl+
	    fl*
	    fl-
	    fl/
	    fldiv-and-mod
	    fldiv
	    flmod
	    fldiv0-and-mod0
	    fldiv0
	    flmod0
	    flnumerator
	    fldenominator
	    flfloor
	    flceiling
	    fltruncate
	    flround
	    flexp
	    flexpt
	    fllog
	    flsin
	    flcos
	    fltan
	    flasin
	    flacos
	    flatan
	    flabs
	    flsqrt
	    fixnum->flonum
	    &no-infinities make-no-infinities-violation no-infinities-violation?
	    &no-nans make-no-nans-violation no-nans-violation?)
    (import (core)
	    (core arithmetic)
	    (core conditions)
	    (sagittarius flonums))

  (define-condition-type &no-infinities
    &implementation-restriction
    make-no-infinities-violation
    no-infinities-violation?)

  (define-condition-type &no-nans
    &implementation-restriction
    make-no-nans-violation no-nans-violation?)

) ; [end]
;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; End:
