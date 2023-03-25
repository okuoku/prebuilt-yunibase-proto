;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; text/yaml/tags.scm - YAML default tags
;;;
;;;   Copyright (c) 2018  Takashi Kato  <ktakashi@ymail.com>
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

;; Reference
;; - Language-Independent Types for YAML™ Version 1.1
;;   http://yaml.org/type/
#!nounbound
(library (text yaml tags)
    (export +yaml-tag:binary+
	    +yaml-tag:bool+
	    +yaml-tag:float+
	    +yaml-tag:int+
	    +yaml-tag:merge+
	    +yaml-tag:null+
	    +yaml-tag:str+
	    +yaml-tag:timestamp+
	    +yaml-tag:value+
	    +yaml-tag:yaml+

	    +yaml-tag:map+
	    +yaml-tag:omap+
	    +yaml-tag:pairs+
	    +yaml-tag:set+
	    +yaml-tag:seq+

	    +yaml-tag-prefix+

	    +yaml-regexp:bool+
	    +yaml-regexp:float+
	    +yaml-regexp:int+
	    +yaml-regexp:null+
	    +yaml-regexp:timestamp+
	    +yaml-regexp:yaml+
	    )
    (import (only (rnrs) quote)
	    (only (sagittarius) define-constant))

(define-constant +yaml-tag-prefix+    "tag:yaml.org,2002:")
(define-constant +yaml-tag:binary+    "tag:yaml.org,2002:binary")
(define-constant +yaml-tag:bool+      "tag:yaml.org,2002:bool")
(define-constant +yaml-tag:float+     "tag:yaml.org,2002:float")
(define-constant +yaml-tag:int+       "tag:yaml.org,2002:int")
(define-constant +yaml-tag:merge+     "tag:yaml.org,2002:merge")
(define-constant +yaml-tag:null+      "tag:yaml.org,2002:null")
(define-constant +yaml-tag:str+       "tag:yaml.org,2002:str")
(define-constant +yaml-tag:timestamp+ "tag:yaml.org,2002:timestamp")
(define-constant +yaml-tag:value+     "tag:yaml.org,2002:value")
(define-constant +yaml-tag:yaml+      "tag:yaml.org,2002:yaml")
(define-constant +yaml-tag:map+       "tag:yaml.org,2002:map")
(define-constant +yaml-tag:omap+      "tag:yaml.org,2002:omap")
(define-constant +yaml-tag:pairs+     "tag:yaml.org,2002:pairs")
(define-constant +yaml-tag:set+       "tag:yaml.org,2002:set")
(define-constant +yaml-tag:seq+       "tag:yaml.org,2002:seq")

(define-constant +yaml-regexp:bool+
  '(: bos
      (or "yes" "Yes" "YES" "no" "No" "NO"
	  "true" "True" "TRUE" "false" "False" "FALSE"
	  "on" "On" "ON" "off" "Off" "OFF")
      eol))
(define-constant +yaml-regexp:float+
  '(or (: (? ("-+"))
	  (? (: (/ "09") (* ("_9876543210")))) "."
	  (: (/ "09") (* ("_9876543210")))
	  (? (: ("eE") ("-+") (: (+ (/ "09"))))))
       (: (? ("-+")) (/ "09") (* ("_9876543210"))
	  (: (: ":" (? ("543210")) (/ "09"))
	     (* (: ":" (? ("543210")) (/ "09")))) "."
	     (* ("_9876543210")))
       (: (? ("-+")) "." (or "inf" "Inf" "INF"))
       (: "." ($ (or "nan" "NaN" "NAN")))))
(define-constant +yaml-regexp:int+
  '(or (: (? ("-+")) (/ "19") (* ("_9876543210"))
	  (: ":" (? (/ "05")) (/ "09")
	     (* (: ":" (? (/ "05")) (/ "09")))))
       (: (? ("-+")) "0b" (+ ("_10")))
       (: (? ("-+")) "0x" (+ (or #\_ xdigit)))
       (: (? ("-+")) "0" (+ ("_76543210")))
       (: (? ("-+")) (or #\0 (: (/ "19") (* ("_9876543210")))))))
(define-constant +yaml-regexp:null+
  '(: bos (or #\~ (: "null") (: "Null") (: "NULL") (:)) eol))
(define-constant +yaml-regexp:timestamp+
  '(or (: (= 4 (/ "09")) "-"  (** 1 2(/ "09")) "-" (** 1 2(/ "09"))
	  (or ("tT") (+ (" \t")))
	  (** 1 2 (/ "09")) ":" (= 2 (/ "09")) ":" (= 2 (/ "09"))
	  (? (: "." (* (/ "09"))))
	  (? (: (* (" \t"))
		(or (: ("-+") (/ "09") (? (/ "09"))
		       (? (: ":" (= 2 (/ "09")))))
		    (:  "Z")))))
       (: (= 4 (/ "09")) "-" (= 2 (/ "09")) "-" (= 2 (/ "09")))))
(define-constant +yaml-regexp:yaml+ '(or #\! #\& #\*))
)
