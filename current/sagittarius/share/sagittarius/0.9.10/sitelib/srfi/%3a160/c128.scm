;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; srfi/%3a160/c128.scm - Homogeneous numeric vector datatypes (c128)
;;;  
;;;   Copyright (c) 2020  Takashi Kato  <ktakashi@ymail.com>
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
(library (srfi :160 c128)
    (export make-c128vector c128vector
	    c128vector-unfold c128vector-unfold-right
	    c128vector-copy c128vector-reverse-copy 
	    c128vector-append c128vector-concatenate
	    c128vector-append-subvectors
	    ;; Predicates 
	    c128? c128vector? c128vector-empty? c128vector=
	    ;; Selectors
	    c128vector-ref c128vector-length
	    ;; Iteration 
	    c128vector-take c128vector-take-right
	    c128vector-drop c128vector-drop-right
	    c128vector-segment
	    c128vector-fold c128vector-fold-right
	    c128vector-map c128vector-map! c128vector-for-each
	    c128vector-count c128vector-cumulate
	    ;; Searching 
	    c128vector-take-while c128vector-take-while-right
	    c128vector-drop-while c128vector-drop-while-right
	    c128vector-index c128vector-index-right c128vector-skip c128vector-skip-right 
	    c128vector-any c128vector-every c128vector-partition
	    c128vector-filter c128vector-remove
	    ;; Mutators 
	    c128vector-set! c128vector-swap! c128vector-fill! c128vector-reverse!
	    c128vector-copy! c128vector-reverse-copy!
	    c128vector-unfold! c128vector-unfold-right!
	    ;; Conversion 
	    c128vector->list list->c128vector
	    reverse-c128vector->list reverse-list->c128vector
	    c128vector->vector vector->c128vector
	    ;; Misc
	    make-c128vector-generator c128vector-comparator write-c128vector)
    (import (rename (rnrs) (exact inexact->exact))
	    (srfi :128)
	    (srfi :160 base))

;;;; From example implementation
;;; Copyright (c) John Cowan 2018.
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;; 
;;; The above copyright notice and this permission notice (including
;;; the next paragraph) shall be included in all copies or substantial
;;; portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;; OTHER DEALINGS IN THE SOFTWARE.

;; make-c128vector defined in (srfi 160 base)

;; c128vector defined in (srfi 160 base)

(define (c128vector-unfold f len seed)
  (let ((v (make-c128vector len)))
    (let loop ((i 0) (state seed))
      (unless (= i len)
        (let-values (((value newstate) (f i state)))
          (c128vector-set! v i value)
          (loop (+ i 1) newstate))))
    v))

(define (c128vector-unfold-right f len seed)
  (let ((v (make-c128vector len)))
    (let loop ((i (- len 1)) (state seed))
      (unless (= i -1)
        (let-values (((value newstate) (f i state)))
          (c128vector-set! v i value)
          (loop (- i 1) newstate))))
    v))

(define c128vector-copy
  (case-lambda
    ((vec) (c128vector-copy* vec 0 (c128vector-length vec)))
    ((vec start) (c128vector-copy* vec start (c128vector-length vec)))
    ((vec start end) (c128vector-copy* vec start end))))

(define (c128vector-copy* vec start end)
  (let ((v (make-c128vector (- end start))))
    (c128vector-copy! v 0 vec start end)
    v))

(define (c128vector-copy! to at from start end)
  (let loop ((at at) (i start))
    (unless (= i end)
      (c128vector-set! to at (c128vector-ref from i))
      (loop (+ at 1) (+ i 1)))))

(define c128vector-reverse-copy
  (case-lambda
    ((vec) (c128vector-reverse-copy* vec 0 (c128vector-length vec)))
    ((vec start) (c128vector-reverse-copy* vec start (c128vector-length vec)))
    ((vec start end) (c128vector-reverse-copy* vec start end))))

(define (c128vector-reverse-copy* vec start end)
  (let ((v (make-c128vector (- end start))))
    (c128vector-reverse-copy! v 0 vec start end)
    v))

(define (c128vector-reverse-copy! to at from start end)
  (let loop ((at at) (i (- end 1)))
    (unless (< i start)
      (c128vector-set! to at (c128vector-ref from i))
      (loop (+ at 1) (- i 1)))))

(define (c128vector-append . vecs)
  (c128vector-concatenate vecs))

(define (c128vector-concatenate vecs)
  (let ((v (make-c128vector (len-sum vecs))))
    (let loop ((vecs vecs) (at 0))
      (unless (null? vecs)
        (let ((vec (car vecs)))
          (c128vector-copy! v at vec 0 (c128vector-length vec))
          (loop (cdr vecs) (+ at (c128vector-length vec)))))
    v)))

(define (len-sum vecs)
  (if (null? vecs)
    0
    (+ (c128vector-length (car vecs))
       (len-sum (cdr vecs)))))

(define (c128vector-append-subvectors . args)
  (let ((v (make-c128vector (len-subsum args))))
    (let loop ((args args) (at 0))
      (unless (null? args)
        (let ((vec (car args))
              (start (cadr args))
              (end (caddr args)))
          (c128vector-copy! v at vec start end)
          (loop (cdddr args) (+ at (- end start))))))
    v))

(define (len-subsum vecs)
  (if (null? vecs)
    0
    (+ (- (caddr vecs) (cadr vecs))
       (len-subsum (cdddr vecs)))))

;; c128? defined in (srfi 160 base)

;; c128vector? defined in (srfi 160 base)

(define (c128vector-empty? vec)
  (zero? (c128vector-length vec)))

(define (c128vector= . vecs)
  (c128vector=* (car vecs) (cadr vecs) (cddr vecs)))

(define (c128vector=* vec1 vec2 vecs)
  (if (null? vecs)
    (and
      (c128dyadic-vecs= vec1 0 (c128vector-length vec1)
                          vec2 0 (c128vector-length vec2))
      (if (null? vecs)
        #t
        (c128vector=* vec2 (car vecs) (cdr vecs))))))

(define (c128dyadic-vecs= vec1 start1 end1 vec2 start2 end2)
  (cond
    ((not (= end1 end2)) #f)
    ((not (< start1 end1)) #t)
    ((let ((elt1 (c128vector-ref vec1 start1))
           (elt2 (c128vector-ref vec2 start2)))
      (= elt1 elt2))
     (c128dyadic-vecs= vec1 (+ start1 1) end1
                         vec2 (+ start2 1) end2))
    (else #f)))

;; c128vector-ref defined in (srfi 160 base)

;; c128vector-length defined in (srfi 160 base)

(define (c128vector-take vec n)
  (let ((v (make-c128vector n)))
    (c128vector-copy! v 0 vec 0 n)
    v))

(define (c128vector-take-right vec n)
  (let ((v (make-c128vector n))
        (len (c128vector-length vec)))
    (c128vector-copy! v 0 vec (- len n) len)
    v))

(define (c128vector-drop vec n)
 (let* ((len (c128vector-length vec))
        (vlen (- len n))
        (v (make-c128vector vlen)))
    (c128vector-copy! v 0 vec n len)
    v))

(define (c128vector-drop-right vec n)
  (let* ((len (c128vector-length vec))
         (rlen (- len n))
         (v (make-c128vector rlen)))
    (c128vector-copy! v 0 vec 0 rlen)
    v))

(define (c128vector-segment vec n)
  (let loop ((r '()) (i 0) (remain (c128vector-length vec)))
    (if (<= remain 0)
      (reverse r)
      (let ((size (min n remain)))
        (loop
          (cons (c128vector-copy vec i (+ i size)) r)
          (+ i size)
          (- remain size))))))

;; aux. procedure
(define (%c128vectors-ref vecs i)
  (map (lambda (v) (c128vector-ref v i)) vecs))

(define (c128vector-fold kons knil vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (c128vector-length vec)))
      (let loop ((r knil) (i 0))
        (if (= i len)
          r
          (loop (kons r (c128vector-ref vec i)) (+ i 1)))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map c128vector-length vecs))))
      (let loop ((r knil) (i 0))
        (if (= i len)
          r
          (loop (apply kons r (%c128vectors-ref vecs i))
                (+ i 1)))))))

(define (c128vector-fold-right kons knil vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (c128vector-length vec)))
      (let loop ((r knil) (i (- (c128vector-length vec) 1)))
        (if (negative? i)
          r
          (loop (kons (c128vector-ref vec i) r) (- i 1)))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map c128vector-length vecs))))
      (let loop ((r knil) (i (- len 1)))
        (if (negative? i)
          r
          (loop (apply kons r (%c128vectors-ref vecs i))
                (- i 1)))))))

(define (c128vector-map f vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let* ((len (c128vector-length vec))
           (v (make-c128vector len)))
      (let loop ((i 0))
        (unless (= i len)
          (c128vector-set! v i (f (c128vector-ref vec i)))
          (loop (+ i 1))))
      v)
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map c128vector-length vecs)))
           (v (make-c128vector len)))
      (let loop ((i 0))
        (unless (= i len)
          (c128vector-set! v i (apply f (%c128vectors-ref vecs i)))
          (loop (+ i 1))))
      v)))
    

(define (c128vector-map! f vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (c128vector-length vec)))
      (let loop ((i 0))
        (unless (= i len)
          (c128vector-set! vec i (f (c128vector-ref vec i)))
          (loop (+ i 1)))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map c128vector-length vecs))))
      (let loop ((i 0))
        (unless (= i len)
          (c128vector-set! vec i (apply f (%c128vectors-ref vecs i)))
          (loop (+ i 1)))))))

(define (c128vector-for-each f vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (c128vector-length vec)))
      (let loop ((i 0))
        (unless (= i len)
          (f (c128vector-ref vec i))
          (loop (+ i 1)))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map c128vector-length vecs))))
      (let loop ((i 0))
        (unless (= i len)
          (apply f (%c128vectors-ref vecs i))
          (loop (+ i 1)))))))

(define (c128vector-count pred vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (c128vector-length vec)))
      (let loop ((i 0) (r 0))
        (cond
         ((= i (c128vector-length vec)) r)
         ((pred (c128vector-ref vec i)) (loop (+ i 1) (+ r 1)))
         (else (loop (+ i 1) r)))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map c128vector-length vecs))))
      (let loop ((i 0) (r 0))
        (cond
         ((= i len) r)
         ((apply pred (%c128vectors-ref vecs i)) (loop (+ i 1) (+ r 1)))
         (else (loop (+ i 1) r)))))))

(define (c128vector-cumulate f knil vec)
  (let* ((len (c128vector-length vec))
         (v (make-c128vector len)))
    (let loop ((r knil) (i 0))
      (unless (= i len)
        (let ((next (f r (c128vector-ref vec i))))
          (c128vector-set! v i next)
          (loop next (+ i 1)))))
    v))

(define (c128vector-foreach f vec)
  (let ((len (c128vector-length vec)))
    (let loop ((i 0))
      (unless (= i len)
        (f (c128vector-ref vec i))
        (loop (+ i 1))))))

(define (c128vector-take-while pred vec)
  (let* ((len (c128vector-length vec))
         (idx (c128vector-skip pred vec))
         (idx* (if idx idx len)))
    (c128vector-copy vec 0 idx*)))

(define (c128vector-take-while-right pred vec)
  (let* ((len (c128vector-length vec))
         (idx (c128vector-skip-right pred vec))
         (idx* (if idx (+ idx 1) 0)))
    (c128vector-copy vec idx* len)))

(define (c128vector-drop-while pred vec)
  (let* ((len (c128vector-length vec))
         (idx (c128vector-skip pred vec))
         (idx* (if idx idx len)))
    (c128vector-copy vec idx* len)))

(define (c128vector-drop-while-right pred vec)
  (let* ((len (c128vector-length vec))
         (idx (c128vector-skip-right pred vec))
         (idx* (if idx idx -1)))
    (c128vector-copy vec 0 (+ 1 idx*))))

(define (c128vector-index pred vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (c128vector-length vec)))
      (let loop ((i 0))
        (cond
         ((= i len) #f)
         ((pred (c128vector-ref vec i)) i)
         (else (loop (+ i 1))))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map c128vector-length vecs))))
      (let loop ((i 0))
        (cond
         ((= i len) #f)
         ((apply pred (%c128vectors-ref vecs i)) i)
         (else (loop (+ i 1))))))))

(define (c128vector-index-right pred vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (c128vector-length vec)))
      (let loop ((i (- len 1)))
        (cond
         ((negative? i) #f)
         ((pred (c128vector-ref vec i)) i)
         (else (loop (- i 1))))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map c128vector-length vecs))))
      (let loop ((i (- len 1)))
        (cond
         ((negative? i) #f)
         ((apply pred (%c128vectors-ref vecs i)) i)
         (else (loop (- i 1))))))))

(define (c128vector-skip pred vec . vecs)
  (if (null? vecs)
    (c128vector-index (lambda (x) (not (pred x))) vec)
    (apply c128vector-index (lambda xs (not (apply pred xs))) vec vecs)))
     
(define (c128vector-skip-right pred vec . vecs)
  (if (null? vecs)
    (c128vector-index-right (lambda (x) (not (pred x))) vec)
    (apply c128vector-index-right (lambda xs (not (apply pred xs))) vec vecs)))

(define (c128vector-any pred vec . vecs)
  (if (null? vec)
    ;; fast path
    (let ((len (c128vector-length vec)))
      (let loop ((i 0))
        (cond
         ((= i len) #f)
         ((pred (c128vector-ref vec i)))  ;returns result of pred
         (else (loop (+ i 1))))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map c128vector-length vecs))))
      (let loop ((i 0))
        (cond
         ((= i len) #f)
         ((apply pred (%c128vectors-ref vecs i))) ;returns result of pred
         (else (loop (+ i 1))))))))

(define (c128vector-every pred vec . vecs)
  (if (null? vec)
    ;; fast path
    (let ((len (c128vector-length vec)))
      (let loop ((i 0) (last #t))
        (cond
         ((= i len) last)
         ((pred (c128vector-ref vec i)) => (lambda (r) (loop (+ i 1) r)))
         (else #f))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map c128vector-length vecs))))
      (let loop ((i 0) (last #t))
        (cond
         ((= i len) last)
         ((apply pred (%c128vectors-ref vecs i)) => (lambda (r) (loop (+ i 1) r)))
         (else #f))))))

(define (c128vector-partition pred vec)
  (let* ((len (c128vector-length vec))
         (cnt (c128vector-count pred vec))
         (r (make-c128vector len)))
    (let loop ((i 0) (yes 0) (no cnt))
      (cond
        ((= i len) r)
        ((pred (c128vector-ref vec i))
         (c128vector-set! r yes (c128vector-ref vec i))
         (loop (+ i 1) (+ yes 1) no))
        (else
         (c128vector-set! r no (c128vector-ref vec i))
         (loop (+ i 1) yes (+ no 1)))))))

(define (c128vector-filter pred vec)
  (let* ((len (c128vector-length vec))
         (cnt (c128vector-count pred vec))
         (r (make-c128vector cnt)))
    (let loop ((i 0) (j 0))
      (cond
        ((= i len) r)
        ((pred (c128vector-ref vec i))
         (c128vector-set! r j (c128vector-ref vec i))
         (loop (+ i 1) (+ j 1)))
        (else
         (loop (+ i 1) j))))))

(define (c128vector-remove pred vec)
  (c128vector-filter (lambda (x) (not (pred x))) vec))

;; c128vector-set! defined in (srfi 160 base)

(define (c128vector-swap! vec i j)
  (let ((ival (c128vector-ref vec i))
        (jval (c128vector-ref vec j)))
    (c128vector-set! vec i jval)
    (c128vector-set! vec j ival)))

(define c128vector-fill!
  (case-lambda
    ((vec fill) (c128vector-fill-some! vec fill 0 (c128vector-length vec)))
    ((vec fill start) (c128vector-fill-some! vec fill start (c128vector-length vec)))
    ((vec fill start end) (c128vector-fill-some! vec fill start end))))

(define (c128vector-fill-some! vec fill start end)
  (unless (= start end)
    (c128vector-set! vec start fill)
    (c128vector-fill-some! vec fill (+ start 1) end)))

(define c128vector-reverse!
  (case-lambda
    ((vec) (c128vector-reverse-some! vec 0 (c128vector-length vec)))
    ((vec start) (c128vector-reverse-some! vec start (c128vector-length vec)))
    ((vec start end) (c128vector-reverse-some! vec start end))))

(define (c128vector-reverse-some! vec start end)
  (let ((len (c128vector-length vec)))
    (let loop ((i 0)(j (- len 1)))
      (when (< i j)
        (c128vector-swap! vec i j)
        (loop (+ i 1) (- j 1))))))

(define (c128vector-unfold! f vec start end seed)
  (let loop ((i start) (seed seed))
    (when (< i end)
      (let-values (((elt seed) (f seed)))
        (c128vector-set! vec i elt)
        (loop (+ i 1) seed)))))

(define (c128vector-unfold-right! f vec start end seed)
  (let loop ((i (- end 1)) (seed seed))
    (when (>= i start)
      (let-values (((elt seed) (f seed)))
        (c128vector-set! vec i elt)
        (loop (- i 1) seed)))))

(define reverse-c128vector->list
  (case-lambda
    ((vec) (reverse-c128vector->list* vec 0 (c128vector-length vec)))
    ((vec start) (reverse-c128vector->list* vec start (c128vector-length vec)))
    ((vec start end) (reverse-c128vector->list* vec start end))))

(define (reverse-c128vector->list* vec start end)
  (let loop ((i start) (r '()))
    (if (= i end)
      r
      (loop (+ 1 i) (cons (c128vector-ref vec i) r)))))

(define (reverse-list->c128vector list)
  (let* ((len (length list))
         (r (make-c128vector len)))
    (let loop ((i 0) (list list))
      (cond
        ((= i len) r)
        (else
          (c128vector-set! r (- len i 1) (car list))
          (loop (+ i 1) (cdr list)))))))

(define c128vector->vector
  (case-lambda
    ((vec) (c128vector->vector* vec 0 (c128vector-length vec)))
    ((vec start) (c128vector->vector* vec start (c128vector-length vec)))
    ((vec start end) (c128vector->vector* vec start end))))

(define (c128vector->vector* vec start end)
  (let* ((len (- end start))
         (r (make-vector len)))
    (let loop ((i start) (o 0))
      (cond
        ((= i end) r)
        (else
          (vector-set! r o (c128vector-ref vec i))
          (loop (+ i 1) (+ o 1)))))))

(define vector->c128vector
  (case-lambda
    ((vec) (vector->c128vector* vec 0 (vector-length vec)))
    ((vec start) (vector->c128vector* vec start (vector-length vec)))
    ((vec start end) (vector->c128vector* vec start end))))

(define (vector->c128vector* vec start end)
  (let* ((len (- end start))
         (r (make-c128vector len)))
    (let loop ((i start) (o 0))
      (cond
        ((= i end) r)
        (else
          (c128vector-set! r o (vector-ref vec i))
          (loop (+ i 1) (+ o 1)))))))

(define make-c128vector-generator
  (case-lambda ((vec) (make-c128vector-generator vec 0 (c128vector-length vec)))
               ((vec start) (make-c128vector-generator vec start (c128vector-length vec)))
               ((vec start end)
                (lambda () (if (>= start end)
                             (eof-object)
                             (let ((next (c128vector-ref vec start)))
                              (set! start (+ start 1))
                              next))))))

(define write-c128vector
  (case-lambda
    ((vec) (write-c128vector* vec (current-output-port)))
    ((vec port) (write-c128vector* vec port))))


(define (write-c128vector* vec port)
  (display "#c128(" port)  ; c128-expansion is blind, so will expand this too
  (let ((last (- (c128vector-length vec) 1)))
    (let loop ((i 0))
      (cond
        ((= i last)
         (write (c128vector-ref vec i) port)
         (display ")" port))
        (else
          (write (c128vector-ref vec i) port)
          (display " " port)
          (loop (+ i 1)))))))

(define (c128vector< vec1 vec2)
  (let ((len1 (c128vector-length vec1))
        (len2 (c128vector-length vec2)))
    (cond
      ((< len1 len2)
       #t)
      ((> len1 len2)
       #f)
      (else
       (let loop ((i 0))
         (cond
           ((= i len1)
            #f)
           ((< (c128vector-ref vec1 i) (c128vector-ref vec2 i))
            #t)
           ((> (c128vector-ref vec1 i) (c128vector-ref vec2 i))
            #f)
           (else
             (loop (+ i 1)))))))))

(define (c128vector-hash vec)
  (let ((len (min 256 (c128vector-length vec))))
    (let loop ((i 0) (r 0))
      (if (= i len)
        (abs (floor (real-part (inexact->exact r))))
        (loop (+ i 1) (+ r (c128vector-ref vec i)))))))

(define c128vector-comparator
  (make-comparator c128vector? c128vector= c128vector< c128vector-hash))

)
