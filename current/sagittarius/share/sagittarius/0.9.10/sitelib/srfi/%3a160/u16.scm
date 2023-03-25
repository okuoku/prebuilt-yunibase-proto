;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; srfi/%3a160/u16.scm - Homogeneous numeric vector datatypes (u16)
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
(library (srfi :160 u16)
    (export make-u16vector u16vector
	    u16vector-unfold u16vector-unfold-right
	    u16vector-copy u16vector-reverse-copy 
	    u16vector-append u16vector-concatenate
	    u16vector-append-subvectors
	    ;; Predicates 
	    u16? u16vector? u16vector-empty? u16vector=
	    ;; Selectors
	    u16vector-ref u16vector-length
	    ;; Iteration 
	    u16vector-take u16vector-take-right
	    u16vector-drop u16vector-drop-right
	    u16vector-segment
	    u16vector-fold u16vector-fold-right
	    u16vector-map u16vector-map! u16vector-for-each
	    u16vector-count u16vector-cumulate
	    ;; Searching 
	    u16vector-take-while u16vector-take-while-right
	    u16vector-drop-while u16vector-drop-while-right
	    u16vector-index u16vector-index-right u16vector-skip u16vector-skip-right 
	    u16vector-any u16vector-every u16vector-partition
	    u16vector-filter u16vector-remove
	    ;; Mutators 
	    u16vector-set! u16vector-swap! u16vector-fill! u16vector-reverse!
	    u16vector-copy! u16vector-reverse-copy!
	    u16vector-unfold! u16vector-unfold-right!
	    ;; Conversion 
	    u16vector->list list->u16vector
	    reverse-u16vector->list reverse-list->u16vector
	    u16vector->vector vector->u16vector
	    ;; Misc
	    make-u16vector-generator u16vector-comparator write-u16vector)
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

;; make-u16vector defined in (srfi 160 base)

;; u16vector defined in (srfi 160 base)

(define (u16vector-unfold f len seed)
  (let ((v (make-u16vector len)))
    (let loop ((i 0) (state seed))
      (unless (= i len)
        (let-values (((value newstate) (f i state)))
          (u16vector-set! v i value)
          (loop (+ i 1) newstate))))
    v))

(define (u16vector-unfold-right f len seed)
  (let ((v (make-u16vector len)))
    (let loop ((i (- len 1)) (state seed))
      (unless (= i -1)
        (let-values (((value newstate) (f i state)))
          (u16vector-set! v i value)
          (loop (- i 1) newstate))))
    v))

(define u16vector-copy
  (case-lambda
    ((vec) (u16vector-copy* vec 0 (u16vector-length vec)))
    ((vec start) (u16vector-copy* vec start (u16vector-length vec)))
    ((vec start end) (u16vector-copy* vec start end))))

(define (u16vector-copy* vec start end)
  (let ((v (make-u16vector (- end start))))
    (u16vector-copy! v 0 vec start end)
    v))

(define (u16vector-copy! to at from start end)
  (let loop ((at at) (i start))
    (unless (= i end)
      (u16vector-set! to at (u16vector-ref from i))
      (loop (+ at 1) (+ i 1)))))

(define u16vector-reverse-copy
  (case-lambda
    ((vec) (u16vector-reverse-copy* vec 0 (u16vector-length vec)))
    ((vec start) (u16vector-reverse-copy* vec start (u16vector-length vec)))
    ((vec start end) (u16vector-reverse-copy* vec start end))))

(define (u16vector-reverse-copy* vec start end)
  (let ((v (make-u16vector (- end start))))
    (u16vector-reverse-copy! v 0 vec start end)
    v))

(define (u16vector-reverse-copy! to at from start end)
  (let loop ((at at) (i (- end 1)))
    (unless (< i start)
      (u16vector-set! to at (u16vector-ref from i))
      (loop (+ at 1) (- i 1)))))

(define (u16vector-append . vecs)
  (u16vector-concatenate vecs))

(define (u16vector-concatenate vecs)
  (let ((v (make-u16vector (len-sum vecs))))
    (let loop ((vecs vecs) (at 0))
      (unless (null? vecs)
        (let ((vec (car vecs)))
          (u16vector-copy! v at vec 0 (u16vector-length vec))
          (loop (cdr vecs) (+ at (u16vector-length vec)))))
    v)))

(define (len-sum vecs)
  (if (null? vecs)
    0
    (+ (u16vector-length (car vecs))
       (len-sum (cdr vecs)))))

(define (u16vector-append-subvectors . args)
  (let ((v (make-u16vector (len-subsum args))))
    (let loop ((args args) (at 0))
      (unless (null? args)
        (let ((vec (car args))
              (start (cadr args))
              (end (caddr args)))
          (u16vector-copy! v at vec start end)
          (loop (cdddr args) (+ at (- end start))))))
    v))

(define (len-subsum vecs)
  (if (null? vecs)
    0
    (+ (- (caddr vecs) (cadr vecs))
       (len-subsum (cdddr vecs)))))

;; u16? defined in (srfi 160 base)

;; u16vector? defined in (srfi 160 base)

(define (u16vector-empty? vec)
  (zero? (u16vector-length vec)))

(define (u16vector= . vecs)
  (u16vector=* (car vecs) (cadr vecs) (cddr vecs)))

(define (u16vector=* vec1 vec2 vecs)
  (if (null? vecs)
    (and
      (u16dyadic-vecs= vec1 0 (u16vector-length vec1)
                          vec2 0 (u16vector-length vec2))
      (if (null? vecs)
        #t
        (u16vector=* vec2 (car vecs) (cdr vecs))))))

(define (u16dyadic-vecs= vec1 start1 end1 vec2 start2 end2)
  (cond
    ((not (= end1 end2)) #f)
    ((not (< start1 end1)) #t)
    ((let ((elt1 (u16vector-ref vec1 start1))
           (elt2 (u16vector-ref vec2 start2)))
      (= elt1 elt2))
     (u16dyadic-vecs= vec1 (+ start1 1) end1
                         vec2 (+ start2 1) end2))
    (else #f)))

;; u16vector-ref defined in (srfi 160 base)

;; u16vector-length defined in (srfi 160 base)

(define (u16vector-take vec n)
  (let ((v (make-u16vector n)))
    (u16vector-copy! v 0 vec 0 n)
    v))

(define (u16vector-take-right vec n)
  (let ((v (make-u16vector n))
        (len (u16vector-length vec)))
    (u16vector-copy! v 0 vec (- len n) len)
    v))

(define (u16vector-drop vec n)
 (let* ((len (u16vector-length vec))
        (vlen (- len n))
        (v (make-u16vector vlen)))
    (u16vector-copy! v 0 vec n len)
    v))

(define (u16vector-drop-right vec n)
  (let* ((len (u16vector-length vec))
         (rlen (- len n))
         (v (make-u16vector rlen)))
    (u16vector-copy! v 0 vec 0 rlen)
    v))

(define (u16vector-segment vec n)
  (let loop ((r '()) (i 0) (remain (u16vector-length vec)))
    (if (<= remain 0)
      (reverse r)
      (let ((size (min n remain)))
        (loop
          (cons (u16vector-copy vec i (+ i size)) r)
          (+ i size)
          (- remain size))))))

;; aux. procedure
(define (%u16vectors-ref vecs i)
  (map (lambda (v) (u16vector-ref v i)) vecs))

(define (u16vector-fold kons knil vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (u16vector-length vec)))
      (let loop ((r knil) (i 0))
        (if (= i len)
          r
          (loop (kons r (u16vector-ref vec i)) (+ i 1)))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map u16vector-length vecs))))
      (let loop ((r knil) (i 0))
        (if (= i len)
          r
          (loop (apply kons r (%u16vectors-ref vecs i))
                (+ i 1)))))))

(define (u16vector-fold-right kons knil vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (u16vector-length vec)))
      (let loop ((r knil) (i (- (u16vector-length vec) 1)))
        (if (negative? i)
          r
          (loop (kons (u16vector-ref vec i) r) (- i 1)))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map u16vector-length vecs))))
      (let loop ((r knil) (i (- len 1)))
        (if (negative? i)
          r
          (loop (apply kons r (%u16vectors-ref vecs i))
                (- i 1)))))))

(define (u16vector-map f vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let* ((len (u16vector-length vec))
           (v (make-u16vector len)))
      (let loop ((i 0))
        (unless (= i len)
          (u16vector-set! v i (f (u16vector-ref vec i)))
          (loop (+ i 1))))
      v)
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map u16vector-length vecs)))
           (v (make-u16vector len)))
      (let loop ((i 0))
        (unless (= i len)
          (u16vector-set! v i (apply f (%u16vectors-ref vecs i)))
          (loop (+ i 1))))
      v)))
    

(define (u16vector-map! f vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (u16vector-length vec)))
      (let loop ((i 0))
        (unless (= i len)
          (u16vector-set! vec i (f (u16vector-ref vec i)))
          (loop (+ i 1)))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map u16vector-length vecs))))
      (let loop ((i 0))
        (unless (= i len)
          (u16vector-set! vec i (apply f (%u16vectors-ref vecs i)))
          (loop (+ i 1)))))))

(define (u16vector-for-each f vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (u16vector-length vec)))
      (let loop ((i 0))
        (unless (= i len)
          (f (u16vector-ref vec i))
          (loop (+ i 1)))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map u16vector-length vecs))))
      (let loop ((i 0))
        (unless (= i len)
          (apply f (%u16vectors-ref vecs i))
          (loop (+ i 1)))))))

(define (u16vector-count pred vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (u16vector-length vec)))
      (let loop ((i 0) (r 0))
        (cond
         ((= i (u16vector-length vec)) r)
         ((pred (u16vector-ref vec i)) (loop (+ i 1) (+ r 1)))
         (else (loop (+ i 1) r)))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map u16vector-length vecs))))
      (let loop ((i 0) (r 0))
        (cond
         ((= i len) r)
         ((apply pred (%u16vectors-ref vecs i)) (loop (+ i 1) (+ r 1)))
         (else (loop (+ i 1) r)))))))

(define (u16vector-cumulate f knil vec)
  (let* ((len (u16vector-length vec))
         (v (make-u16vector len)))
    (let loop ((r knil) (i 0))
      (unless (= i len)
        (let ((next (f r (u16vector-ref vec i))))
          (u16vector-set! v i next)
          (loop next (+ i 1)))))
    v))

(define (u16vector-foreach f vec)
  (let ((len (u16vector-length vec)))
    (let loop ((i 0))
      (unless (= i len)
        (f (u16vector-ref vec i))
        (loop (+ i 1))))))

(define (u16vector-take-while pred vec)
  (let* ((len (u16vector-length vec))
         (idx (u16vector-skip pred vec))
         (idx* (if idx idx len)))
    (u16vector-copy vec 0 idx*)))

(define (u16vector-take-while-right pred vec)
  (let* ((len (u16vector-length vec))
         (idx (u16vector-skip-right pred vec))
         (idx* (if idx (+ idx 1) 0)))
    (u16vector-copy vec idx* len)))

(define (u16vector-drop-while pred vec)
  (let* ((len (u16vector-length vec))
         (idx (u16vector-skip pred vec))
         (idx* (if idx idx len)))
    (u16vector-copy vec idx* len)))

(define (u16vector-drop-while-right pred vec)
  (let* ((len (u16vector-length vec))
         (idx (u16vector-skip-right pred vec))
         (idx* (if idx idx -1)))
    (u16vector-copy vec 0 (+ 1 idx*))))

(define (u16vector-index pred vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (u16vector-length vec)))
      (let loop ((i 0))
        (cond
         ((= i len) #f)
         ((pred (u16vector-ref vec i)) i)
         (else (loop (+ i 1))))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map u16vector-length vecs))))
      (let loop ((i 0))
        (cond
         ((= i len) #f)
         ((apply pred (%u16vectors-ref vecs i)) i)
         (else (loop (+ i 1))))))))

(define (u16vector-index-right pred vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (u16vector-length vec)))
      (let loop ((i (- len 1)))
        (cond
         ((negative? i) #f)
         ((pred (u16vector-ref vec i)) i)
         (else (loop (- i 1))))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map u16vector-length vecs))))
      (let loop ((i (- len 1)))
        (cond
         ((negative? i) #f)
         ((apply pred (%u16vectors-ref vecs i)) i)
         (else (loop (- i 1))))))))

(define (u16vector-skip pred vec . vecs)
  (if (null? vecs)
    (u16vector-index (lambda (x) (not (pred x))) vec)
    (apply u16vector-index (lambda xs (not (apply pred xs))) vec vecs)))
     
(define (u16vector-skip-right pred vec . vecs)
  (if (null? vecs)
    (u16vector-index-right (lambda (x) (not (pred x))) vec)
    (apply u16vector-index-right (lambda xs (not (apply pred xs))) vec vecs)))

(define (u16vector-any pred vec . vecs)
  (if (null? vec)
    ;; fast path
    (let ((len (u16vector-length vec)))
      (let loop ((i 0))
        (cond
         ((= i len) #f)
         ((pred (u16vector-ref vec i)))  ;returns result of pred
         (else (loop (+ i 1))))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map u16vector-length vecs))))
      (let loop ((i 0))
        (cond
         ((= i len) #f)
         ((apply pred (%u16vectors-ref vecs i))) ;returns result of pred
         (else (loop (+ i 1))))))))

(define (u16vector-every pred vec . vecs)
  (if (null? vec)
    ;; fast path
    (let ((len (u16vector-length vec)))
      (let loop ((i 0) (last #t))
        (cond
         ((= i len) last)
         ((pred (u16vector-ref vec i)) => (lambda (r) (loop (+ i 1) r)))
         (else #f))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map u16vector-length vecs))))
      (let loop ((i 0) (last #t))
        (cond
         ((= i len) last)
         ((apply pred (%u16vectors-ref vecs i)) => (lambda (r) (loop (+ i 1) r)))
         (else #f))))))

(define (u16vector-partition pred vec)
  (let* ((len (u16vector-length vec))
         (cnt (u16vector-count pred vec))
         (r (make-u16vector len)))
    (let loop ((i 0) (yes 0) (no cnt))
      (cond
        ((= i len) r)
        ((pred (u16vector-ref vec i))
         (u16vector-set! r yes (u16vector-ref vec i))
         (loop (+ i 1) (+ yes 1) no))
        (else
         (u16vector-set! r no (u16vector-ref vec i))
         (loop (+ i 1) yes (+ no 1)))))))

(define (u16vector-filter pred vec)
  (let* ((len (u16vector-length vec))
         (cnt (u16vector-count pred vec))
         (r (make-u16vector cnt)))
    (let loop ((i 0) (j 0))
      (cond
        ((= i len) r)
        ((pred (u16vector-ref vec i))
         (u16vector-set! r j (u16vector-ref vec i))
         (loop (+ i 1) (+ j 1)))
        (else
         (loop (+ i 1) j))))))

(define (u16vector-remove pred vec)
  (u16vector-filter (lambda (x) (not (pred x))) vec))

;; u16vector-set! defined in (srfi 160 base)

(define (u16vector-swap! vec i j)
  (let ((ival (u16vector-ref vec i))
        (jval (u16vector-ref vec j)))
    (u16vector-set! vec i jval)
    (u16vector-set! vec j ival)))

(define u16vector-fill!
  (case-lambda
    ((vec fill) (u16vector-fill-some! vec fill 0 (u16vector-length vec)))
    ((vec fill start) (u16vector-fill-some! vec fill start (u16vector-length vec)))
    ((vec fill start end) (u16vector-fill-some! vec fill start end))))

(define (u16vector-fill-some! vec fill start end)
  (unless (= start end)
    (u16vector-set! vec start fill)
    (u16vector-fill-some! vec fill (+ start 1) end)))

(define u16vector-reverse!
  (case-lambda
    ((vec) (u16vector-reverse-some! vec 0 (u16vector-length vec)))
    ((vec start) (u16vector-reverse-some! vec start (u16vector-length vec)))
    ((vec start end) (u16vector-reverse-some! vec start end))))

(define (u16vector-reverse-some! vec start end)
  (let ((len (u16vector-length vec)))
    (let loop ((i 0)(j (- len 1)))
      (when (< i j)
        (u16vector-swap! vec i j)
        (loop (+ i 1) (- j 1))))))

(define (u16vector-unfold! f vec start end seed)
  (let loop ((i start) (seed seed))
    (when (< i end)
      (let-values (((elt seed) (f seed)))
        (u16vector-set! vec i elt)
        (loop (+ i 1) seed)))))

(define (u16vector-unfold-right! f vec start end seed)
  (let loop ((i (- end 1)) (seed seed))
    (when (>= i start)
      (let-values (((elt seed) (f seed)))
        (u16vector-set! vec i elt)
        (loop (- i 1) seed)))))

(define reverse-u16vector->list
  (case-lambda
    ((vec) (reverse-u16vector->list* vec 0 (u16vector-length vec)))
    ((vec start) (reverse-u16vector->list* vec start (u16vector-length vec)))
    ((vec start end) (reverse-u16vector->list* vec start end))))

(define (reverse-u16vector->list* vec start end)
  (let loop ((i start) (r '()))
    (if (= i end)
      r
      (loop (+ 1 i) (cons (u16vector-ref vec i) r)))))

(define (reverse-list->u16vector list)
  (let* ((len (length list))
         (r (make-u16vector len)))
    (let loop ((i 0) (list list))
      (cond
        ((= i len) r)
        (else
          (u16vector-set! r (- len i 1) (car list))
          (loop (+ i 1) (cdr list)))))))

(define u16vector->vector
  (case-lambda
    ((vec) (u16vector->vector* vec 0 (u16vector-length vec)))
    ((vec start) (u16vector->vector* vec start (u16vector-length vec)))
    ((vec start end) (u16vector->vector* vec start end))))

(define (u16vector->vector* vec start end)
  (let* ((len (- end start))
         (r (make-vector len)))
    (let loop ((i start) (o 0))
      (cond
        ((= i end) r)
        (else
          (vector-set! r o (u16vector-ref vec i))
          (loop (+ i 1) (+ o 1)))))))

(define vector->u16vector
  (case-lambda
    ((vec) (vector->u16vector* vec 0 (vector-length vec)))
    ((vec start) (vector->u16vector* vec start (vector-length vec)))
    ((vec start end) (vector->u16vector* vec start end))))

(define (vector->u16vector* vec start end)
  (let* ((len (- end start))
         (r (make-u16vector len)))
    (let loop ((i start) (o 0))
      (cond
        ((= i end) r)
        (else
          (u16vector-set! r o (vector-ref vec i))
          (loop (+ i 1) (+ o 1)))))))

(define make-u16vector-generator
  (case-lambda ((vec) (make-u16vector-generator vec 0 (u16vector-length vec)))
               ((vec start) (make-u16vector-generator vec start (u16vector-length vec)))
               ((vec start end)
                (lambda () (if (>= start end)
                             (eof-object)
                             (let ((next (u16vector-ref vec start)))
                              (set! start (+ start 1))
                              next))))))

(define write-u16vector
  (case-lambda
    ((vec) (write-u16vector* vec (current-output-port)))
    ((vec port) (write-u16vector* vec port))))


(define (write-u16vector* vec port)
  (display "#u16(" port)  ; u16-expansion is blind, so will expand this too
  (let ((last (- (u16vector-length vec) 1)))
    (let loop ((i 0))
      (cond
        ((= i last)
         (write (u16vector-ref vec i) port)
         (display ")" port))
        (else
          (write (u16vector-ref vec i) port)
          (display " " port)
          (loop (+ i 1)))))))

(define (u16vector< vec1 vec2)
  (let ((len1 (u16vector-length vec1))
        (len2 (u16vector-length vec2)))
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
           ((< (u16vector-ref vec1 i) (u16vector-ref vec2 i))
            #t)
           ((> (u16vector-ref vec1 i) (u16vector-ref vec2 i))
            #f)
           (else
             (loop (+ i 1)))))))))

(define (u16vector-hash vec)
  (let ((len (min 256 (u16vector-length vec))))
    (let loop ((i 0) (r 0))
      (if (= i len)
        (abs (floor (real-part (inexact->exact r))))
        (loop (+ i 1) (+ r (u16vector-ref vec i)))))))

(define u16vector-comparator
  (make-comparator u16vector? u16vector= u16vector< u16vector-hash))

)
