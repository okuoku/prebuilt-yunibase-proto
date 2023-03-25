;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; peg/derived.scm - PEG syntax sugers et al
;;;  
;;;   Copyright (c) 2017  Takashi Kato  <ktakashi@ymail.com>
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
(library (peg derived)
    (export $bind $do $let* $let $optional $repeat
	    $parameterize $lazy $guard
	    $if $when $unless $cond else
	    $peek-match
	    $eqv?)
    (import (rnrs)
	    (rnrs r5rs)
	    (core inline)
	    (peg primitives)
	    (srfi :39 parameters))

(define ($$bind p f)
  (lambda (l)
    (let-values (((s v nl) (p l)))
      (if (parse-success? s)
	  ((f v) nl)
	  (values s v l)))))

(define-syntax $bind
  (lambda (x)
    (syntax-case x ()
      ((_ p? f?)
       #'(let ((p p?) (f f?))
	   (lambda (l)
	     (let-values (((s v nl) (p l)))
	       (if (parse-success? s)
		   ((f v) nl)
		   (values s v l))))))
      (k (identifier? #'k) #'$$bind))))

;; $do clause ... body
;;   clause := (var parser)
;;          |  (parser)
;;          |  parser
(define-syntax $do
  (syntax-rules ()
    ((_ body) body)
    ((_ (var parser) clause rest ...)
     ($bind parser (lambda (var) ($do clause rest ...))))
    ((_ (parser) clause rest ...)
     ($bind parser (lambda (_) ($do clause rest ...))))
    ((_ parser clause rest ...)
     ($bind parser (lambda (_) ($do clause rest ...))))))

;; $let* (bind ...) body
;;   bind := (var parser)
;;        |  (parser)
;;        |  parser
;; almost the same as $do but more explicit (I think)
(define-syntax $let*
  (syntax-rules ()
    ((_ () body0 body* ...) ($seq body0 body* ...))
    ((_ ((var parser) bind* ...) body ...)
     ($bind parser (lambda (var) ($let* (bind* ...) body ...))))
    ((_ ((parser) bind* ...) body  ...)
     ($bind parser (lambda (_) ($let* (bind* ...) body ...))))
    ((_ (parser bind* ...) body ...)
     ($bind parser (lambda (_) ($let* (bind* ...) body ...))))))

;; $let (bind ...) body
;;   bind := (var parser)
;;        |  (parser)
;;        |  parser
;; analogy of let* but better performance
(define-syntax $let
  (syntax-rules ()
    ((_ () body0 body* ...) ($seq body0 body* ...)) ;; short cut
    ((_ (bind bind* ...) body* ...)
     ($let "collect" (bind bind* ...) (body* ...) ()))

    ((_ "collect" ((var parser) bind* ...) body ((v t p) ...))
     ($let "collect" (bind* ...) body ((v t p) ... (var tmp parser))))

    ((_ "collect" ((parser) bind* ...) body ((v t p) ...))
     ($let "collect" (bind* ...) body ((v t p) ... (var tmp parser))))

    ((_ "collect" (parser bind* ...) body ((v t p) ...))
     ($let "collect" (bind* ...) body ((v t p) ... (var tmp parser))))

    ((_ "collect" () (body0 body* ...) ((v t p) ...))
     (let ((t p) ...) ;; bind parser
       ($let* ((v t) ...) body0 body* ...)))))

(define ($$optional parser . maybe-fallback)
  (define fallback (if (null? maybe-fallback) #f (car maybe-fallback)))
  ($do (r ($many parser 0 1))
       ($return (if (null? r) fallback (car r)))))
(define-syntax $optional
  (lambda (x)
    (syntax-case x ()
      ((_ parser) #'($optional parser #f))
      ((_ parser fallback)
       #'($do (r ($many parser 0 1))
	      ($return (if (null? r) fallback (car r)))))
      (k (identifier? #'k) #'$$optional))))
       
(define-inline ($repeat parser n) ($many parser n n))

(define-syntax $parameterize
  (syntax-rules ()
    ((_ ((p c) ...) parser0 parser* ...)
     (let ((e ($seq parser0 parser* ...)))
       (lambda (l)
	 (parameterize ((p c) ...)
	   (e l)))))))
(define-syntax $lazy
  (syntax-rules ()
    ((_ parser)
     (let ((p (delay parser)))
       (lambda (l) ((force parser) l))))))

(define-syntax $guard
  (syntax-rules ()
    ((_ (e (pred clause) ...) body)
     (let ((parser body))
       (lambda (l)
	 (guard (e (pred (clause l)) ...)
	   (parser l)))))))

(define-syntax $if
  (syntax-rules ()
    ((_ pred consequence alternative)
     (let ((c consequence)
	   (a alternative))
       (lambda (l)
	 (if pred
	     (c l)
	     (a l)))))))
(define-syntax $when
  (syntax-rules ()
    ((_ pred body)
     ($if pred body ($expect 'pred)))))
(define-syntax $unless
  (syntax-rules ()
    ((_ pred body)
     ($when (not pred) body))))

(define-syntax $cond
  (syntax-rules (else)
    ((_ "emit" ((pred val) ...))
     (lambda (input)
       (cond (pred (val input)) ...)))
    ((_ "collect" (p&c ...) ()) ($cond "emit" (p&c ...)))
    ((_ "collect" (p&c ...) ((else alternative)))
     (let ((tmp alternative))
       ($cond "emit" (p&c ... (else tmp)))))
    ((_ "collect" (p&c ...) ((pred consequence) rest ...))
     (let ((tmp consequence))
       ($cond "collect" (p&c ... (pred tmp)) (rest ...))))
    ((_  clause ...)
     ($cond "collect" () (clause ...)))))

(define-syntax $peek-match
  (syntax-rules ()
    ((_ parser consequence alternative)
     (let ((peek ($peek parser)))
       (lambda (input)
	 (let-values (((s v n) (peek input)))
	   (if (parse-success? s)
	       (consequence input)
	       (alternative input))))))))

(define-inline ($eqv? v) ($satisfy (lambda (c) (eqv? c v)) v))
)
