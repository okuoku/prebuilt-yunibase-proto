;;; -*- mode: scheme; coding: utf-8; -*-
;;;
;;; win32/gui/edit - Win32 GUI edit control
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

(library (win32 gui edit)
    (export <win32-edit>
	    <win32-text-edit> win32-text-edit?
	    make-win32-text-edit 
	    
	    <win32-multi-text-edit> win32-multi-text-edit?
	    make-win32-multi-text-edit
	    win32-draw-edit

	    win32-edit-set-font ;; external api
	    win32-edit-set-text!
	    win32-edit-append-text!
	    win32-edit-get-text
	    win32-edit-update-font ;; internal api
	    )
    (import (rnrs)
	    (sagittarius)
	    (sagittarius ffi)
	    (win32 user)
	    (win32 common-control)
	    (win32 defs)
	    (win32 gdi)
	    (win32 richedit)
	    (win32 gui api)
	    (clos user)
	    (sagittarius object))

(define *win32-default-edit-class-name* "sagittarius-default-edit-class")
(define-class <win32-edit> (<win32-component>) 
  ;; edit value
  ((value :init-keyword :value :init-value "")
   (font  :init-keyword :font
	  :init-value (get-stock-object SYSTEM_FIXED_FONT))
   (text-color :init-keyword :text-color
	       :init-value (get-stock-object BLACK_BRUSH))
   (background-color :init-keyword :background-color 
		     :init-value (get-stock-object WHITE_BRUSH))
   ;; private slots
   font-height 
   font-width
   ))
(define-method initialize ((t <win32-edit>) initargs)
  (call-next-method)
  (unless (slot-bound? t 'class-name)
    (set! (~ t 'class-name) *win32-default-edit-class-name*))
  (set! (~ t 'name) (~ t 'value)) ;; this would the initial text
  t)

(define-method win32-create ((t <win32-edit>))
  (call-next-method)
  (win32-edit-update-font t))

(define (win32-edit-update-font t)
  ;; setup font size here
  (let* ((tm (allocate-c-struct TEXTMETRIC))
	 (hdc (get-dc (~ t 'hwnd)))
	 (old (select-object hdc  (~ t 'font))))
    (get-text-metrics hdc tm)
    (set! (~ t 'font-height) (c-struct-ref tm TEXTMETRIC 'tmHeight))
    (set! (~ t 'font-width) (c-struct-ref tm TEXTMETRIC 'tmAveCharWidth))
    (select-object hdc old)
    (release-dc (~ t 'hwnd) hdc)))

(define (win32-edit-set-font e font :optional (redraw? #t))
  (set! (~ e 'font) font)
  (win32-edit-update-font e) ;; now update font info
  (send-message (~ e 'hwnd) WM_SETFONT font (if redraw? 1 0)))

(define (win32-edit-set-text! e text)
  (set! (~ e 'value) text)
  (set-window-text (~ e 'hwnd) text))

(define (win32-edit-get-text e)
  (let ((len (get-window-text-length (~ e 'hwnd))))
    (if (zero? len)
	(begin (set! (~ e 'value) "") "")
	(let* ((len+1 (+ len 1)) ;; add null...
	       ;; it's  wchar_t
	       (p (allocate-pointer (* len+1 size-of-wchar_t))))
	  (get-window-text (~ e 'hwnd) p len+1)
	  (let ((s (wchar-pointer->string p)))
	    (set! (~ e 'value) s)
	    s)))))


;; as the final goal, we provide all edit control from win32 api. however 
;; for now it's only what we need.

(define-class <win32-text-edit> (<win32-edit>) ())
(define (win32-text-edit? o) (is-a? o <win32-text-edit>))
(define (make-win32-text-edit . opt) (apply make <win32-text-edit> opt))
(define-method initialize ((t <win32-text-edit>) initargs)
  (call-next-method)
  ;; TODO correct?
  (set! (~ t 'style) (bitwise-ior (~ t 'style) ES_AUTOHSCROLL))
  t)
(define-method win32-edit-append-text! ((e <win32-text-edit>) text)
  (define hwnd (~ e 'hwnd))
  ;; TODO should we?
  ;; (set! (~ e 'value) (string-append (~ e 'value text)))
  (let ((len (get-window-text-length hwnd)))
    (send-message hwnd EM_SETSEL len (integer->pointer len))
    (send-message hwnd EM_REPLACESEL 0
		  (string->utf16 (string-append text "\x0;")
				 (endianness native)))))

(define-class <win32-multi-text-edit> (<win32-text-edit>) ())
(define (win32-multi-text-edit? o) (is-a? o <win32-multi-text-edit>))
(define (make-win32-multi-text-edit . opt) 
  (apply make <win32-multi-text-edit> opt))

(define-method initialize ((t <win32-multi-text-edit>) initargs)
  (call-next-method)
  ;; TODO correct?
  (set! (~ t 'style) (bitwise-ior (~ t 'style)
				  ES_MULTILINE
				  ES_WANTRETURN
				  WS_VSCROLL
				  ES_AUTOVSCROLL))
  t)

(define-method win32-edit-append-text! ((e <win32-multi-text-edit>) text)
  (define hwnd (~ e 'hwnd))
  (call-next-method)
  (send-message hwnd WM_VSCROLL SB_BOTTOM null-pointer))

(define-method win32-before-drawing ((t <win32-edit>) hdc)
  (win32-draw-edit t hdc))

(define (win32-draw-edit edit hdc) 
  (select-object hdc (~ edit 'font))
  (set-bk-mode hdc TRANSPARENT)
  )

(inherit-window-class MSFTEDIT_CLASS *win32-default-edit-class-name*
		      WM_NCCREATE)
)
