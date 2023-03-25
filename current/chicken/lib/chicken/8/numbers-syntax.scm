;;;; numbers-syntax.scm
;;
;; Compiler support for extended number literal syntax (an ugly hack)
;;
;
; Copyright (c) 2012-2013 The CHICKEN Team
; All rights reserved.
;
; Redistribution and use in source and binary forms, with or without
; modification, are permitted provided that the following conditions
; are met:
; 1. Redistributions of source code must retain the above copyright
;    notice, this list of conditions and the following disclaimer.
; 2. Redistributions in binary form must reproduce the above copyright
;    notice, this list of conditions and the following disclaimer in the
;    documentation and/or other materials provided with the distribution.
; 3. The name of the authors may not be used to endorse or promote products
;    derived from this software without specific prior written permission.
;
; THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND ANY EXPRESS OR
; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
; IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY DIRECT, INDIRECT,
; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(use numbers)

;; By overriding this we get rid of "illegal atomic form" when compiling.
;; We rely on basic struct and bytevector serialization for non-core numbers.
(let ((core-compiler-constant? ##compiler#constant?))
  (set! ##compiler#constant?
    (lambda (x)
      (or (number? x)    ; This uses the "extended" number? predicate
          (core-compiler-constant? x)))))

;; XXX TODO: Maybe override ##compiler#collapsable-literal? too?

;; Unfortunately this is unportable.  The resulting C code only works on
;; the platform on which the Scheme code was translated to C.
;;
;; So if we detect that the user has accidentally done this, punt.
(set! ##compiler#postponed-initforms
      (append
       `((unless (and (eq? (##sys#fudge 3) ,(##sys#fudge 3)) ; 64 bits?
                      (eq? (machine-byte-order) ',(machine-byte-order)))
           (error ,(sprintf (string-append
                             "This program or one of its libraries was "
                             "compiled using extended numbers syntax, "
                             "but on a ~A bits ~A machine (~A), which is "
                             "a different architecture from yours. "
                             "This is currently unsupported.")
                            (if (##sys#fudge 3) 64 32)
                            (machine-byte-order) (machine-type)))))
       ##compiler#postponed-initforms))
