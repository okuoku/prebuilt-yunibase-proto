;;; -*- mode: scheme; coding: utf-8; -*-
;;;
;;; win32/gui/button.scm - Win32 GUI button
;;;  
;;;   Copyright (c) 2015  Takashi Kato  <ktakashi@ymail.com>
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

(library (win32 gui button)
    (export <win32-button> win32-button?
	    make-win32-button
	    )
    (import (rnrs)
	    (clos user)
	    (win32 user)
	    (win32 defs)
	    (win32 kernel)
	    (win32 common-control)
	    (win32 gui api)
	    (sagittarius ffi)
	    (sagittarius object)
	    (sagittarius control))

(define *win32-default-button-class-name* "sagittarius-default-button-class")

(define-class <win32-button> (<win32-component>) ())
(define-method initialize ((b <win32-button>) initargs)
  (call-next-method)
  (let ((style (bitwise-ior (~ b 'style) BS_NOTIFY)))
    (unless (slot-bound? b 'class-name)
      (set! (~ b 'class-name) *win32-default-button-class-name*))
    (set! (~ b 'style) style)
    #;(when (zero? (~ b 'window-style))
      (set! (~ b 'window-style) WS_EX_STATICEDGE)))
  b)

(define (make-win32-button . opt) (apply make <win32-button> opt))
(define (win32-button? o) (is-a? o <win32-button>))

(define-method win32-translate-notification ((b <win32-button>) code)
  (cond ((= code BN_CLICKED) 'click)
	((= code BN_DOUBLECLICKED) 'double-click)
	((= code BN_SETFOCUS) 'focus)
	((= code BN_KILLFOCUS) 'blur)
	(else code)))

(inherit-window-class WC_BUTTON *win32-default-button-class-name* WM_NCCREATE)

)
