;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; srfi/%3a160/s8.scm - Homogeneous numeric vector datatypes (s8)
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
(library (srfi :160 s8)
    (export make-s8vector s8vector
	    s8vector-unfold s8vector-unfold-right
	    s8vector-copy s8vector-reverse-copy 
	    s8vector-append s8vector-concatenate
	    s8vector-append-subvectors
	    ;; Predicates 
	    s8? s8vector? s8vector-empty? s8vector=
	    ;; Selectors
	    s8vector-ref s8vector-length
	    ;; Iteration 
	    s8vector-take s8vector-take-right
	    s8vector-drop s8vector-drop-right
	    s8vector-segment
	    s8vector-fold s8vector-fold-right
	    s8vector-map s8vector-map! s8vector-for-each
	    s8vector-count s8vector-cumulate
	    ;; Searching 
	    s8vector-take-while s8vector-take-while-right
	    s8vector-drop-while s8vector-drop-while-right
	    s8vector-index s8vector-index-right s8vector-skip s8vector-skip-right 
	    s8vector-any s8vector-every s8vector-partition
	    s8vector-filter s8vector-remove
	    ;; Mutators 
	    s8vector-set! s8vector-swap! s8vector-fill! s8vector-reverse!
	    s8vector-copy! s8vector-reverse-copy!
	    s8vector-unfold! s8vector-unfold-right!
	    ;; Conversion 
	    s8vector->list list->s8vector
	    reverse-s8vector->list reverse-list->s8vector
	    s8vector->vector vector->s8vector
	    ;; Misc
	    make-s8vector-generator s8vector-comparator write-s8vector)
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

;; make-s8vector defined in (srfi 160 base)

;; s8vector defined in (srfi 160 base)

(define (s8vector-unfold f len seed)
  (let ((v (make-s8vector len)))
    (let loop ((i 0) (state seed))
      (unless (= i len)
        (let-values (((value newstate) (f i state)))
          (s8vector-set! v i value)
          (loop (+ i 1) newstate))))
    v))

(define (s8vector-unfold-right f len seed)
  (let ((v (make-s8vector len)))
    (let loop ((i (- len 1)) (state seed))
      (unless (= i -1)
        (let-values (((value newstate) (f i state)))
          (s8vector-set! v i value)
          (loop (- i 1) newstate))))
    v))

(define s8vector-copy
  (case-lambda
    ((vec) (s8vector-copy* vec 0 (s8vector-length vec)))
    ((vec start) (s8vector-copy* vec start (s8vector-length vec)))
    ((vec start end) (s8vector-copy* vec start end))))

(define (s8vector-copy* vec start end)
  (let ((v (make-s8vector (- end start))))
    (s8vector-copy! v 0 vec start end)
    v))

(define (s8vector-copy! to at from start end)
  (let loop ((at at) (i start))
    (unless (= i end)
      (s8vector-set! to at (s8vector-ref from i))
      (loop (+ at 1) (+ i 1)))))

(define s8vector-reverse-copy
  (case-lambda
    ((vec) (s8vector-reverse-copy* vec 0 (s8vector-length vec)))
    ((vec start) (s8vector-reverse-copy* vec start (s8vector-length vec)))
    ((vec start end) (s8vector-reverse-copy* vec start end))))

(define (s8vector-reverse-copy* vec start end)
  (let ((v (make-s8vector (- end start))))
    (s8vector-reverse-copy! v 0 vec start end)
    v))

(define (s8vector-reverse-copy! to at from start end)
  (let loop ((at at) (i (- end 1)))
    (unless (< i start)
      (s8vector-set! to at (s8vector-ref from i))
      (loop (+ at 1) (- i 1)))))

(define (s8vector-append . vecs)
  (s8vector-concatenate vecs))

(define (s8vector-concatenate vecs)
  (let ((v (make-s8vector (len-sum vecs))))
    (let loop ((vecs vecs) (at 0))
      (unless (null? vecs)
        (let ((vec (car vecs)))
          (s8vector-copy! v at vec 0 (s8vector-length vec))
          (loop (cdr vecs) (+ at (s8vector-length vec)))))
    v)))

(define (len-sum vecs)
  (if (null? vecs)
    0
    (+ (s8vector-length (car vecs))
       (len-sum (cdr vecs)))))

(define (s8vector-append-subvectors . args)
  (let ((v (make-s8vector (len-subsum args))))
    (let loop ((args args) (at 0))
      (unless (null? args)
        (let ((vec (car args))
              (start (cadr args))
              (end (caddr args)))
          (s8vector-copy! v at vec start end)
          (loop (cdddr args) (+ at (- end start))))))
    v))

(define (len-subsum vecs)
  (if (null? vecs)
    0
    (+ (- (caddr vecs) (cadr vecs))
       (len-subsum (cdddr vecs)))))

;; s8? defined in (srfi 160 base)

;; s8vector? defined in (srfi 160 base)

(define (s8vector-empty? vec)
  (zero? (s8vector-length vec)))

(define (s8vector= . vecs)
  (s8vector=* (car vecs) (cadr vecs) (cddr vecs)))

(define (s8vector=* vec1 vec2 vecs)
  (if (null? vecs)
    (and
      (s8dyadic-vecs= vec1 0 (s8vector-length vec1)
                          vec2 0 (s8vector-length vec2))
      (if (null? vecs)
        #t
        (s8vector=* vec2 (car vecs) (cdr vecs))))))

(define (s8dyadic-vecs= vec1 start1 end1 vec2 start2 end2)
  (cond
    ((not (= end1 end2)) #f)
    ((not (< start1 end1)) #t)
    ((let ((elt1 (s8vector-ref vec1 start1))
           (elt2 (s8vector-ref vec2 start2)))
      (= elt1 elt2))
     (s8dyadic-vecs= vec1 (+ start1 1) end1
                         vec2 (+ start2 1) end2))
    (else #f)))

;; s8vector-ref defined in (srfi 160 base)

;; s8vector-length defined in (srfi 160 base)

(define (s8vector-take vec n)
  (let ((v (make-s8vector n)))
    (s8vector-copy! v 0 vec 0 n)
    v))

(define (s8vector-take-right vec n)
  (let ((v (make-s8vector n))
        (len (s8vector-length vec)))
    (s8vector-copy! v 0 vec (- len n) len)
    v))

(define (s8vector-drop vec n)
 (let* ((len (s8vector-length vec))
        (vlen (- len n))
        (v (make-s8vector vlen)))
    (s8vector-copy! v 0 vec n len)
    v))

(define (s8vector-drop-right vec n)
  (let* ((len (s8vector-length vec))
         (rlen (- len n))
         (v (make-s8vector rlen)))
    (s8vector-copy! v 0 vec 0 rlen)
    v))

(define (s8vector-segment vec n)
  (let loop ((r '()) (i 0) (remain (s8vector-length vec)))
    (if (<= remain 0)
      (reverse r)
      (let ((size (min n remain)))
        (loop
          (cons (s8vector-copy vec i (+ i size)) r)
          (+ i size)
          (- remain size))))))

;; aux. procedure
(define (%s8vectors-ref vecs i)
  (map (lambda (v) (s8vector-ref v i)) vecs))

(define (s8vector-fold kons knil vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (s8vector-length vec)))
      (let loop ((r knil) (i 0))
        (if (= i len)
          r
          (loop (kons r (s8vector-ref vec i)) (+ i 1)))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map s8vector-length vecs))))
      (let loop ((r knil) (i 0))
        (if (= i len)
          r
          (loop (apply kons r (%s8vectors-ref vecs i))
                (+ i 1)))))))

(define (s8vector-fold-right kons knil vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (s8vector-length vec)))
      (let loop ((r knil) (i (- (s8vector-length vec) 1)))
        (if (negative? i)
          r
          (loop (kons (s8vector-ref vec i) r) (- i 1)))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map s8vector-length vecs))))
      (let loop ((r knil) (i (- len 1)))
        (if (negative? i)
          r
          (loop (apply kons r (%s8vectors-ref vecs i))
                (- i 1)))))))

(define (s8vector-map f vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let* ((len (s8vector-length vec))
           (v (make-s8vector len)))
      (let loop ((i 0))
        (unless (= i len)
          (s8vector-set! v i (f (s8vector-ref vec i)))
          (loop (+ i 1))))
      v)
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map s8vector-length vecs)))
           (v (make-s8vector len)))
      (let loop ((i 0))
        (unless (= i len)
          (s8vector-set! v i (apply f (%s8vectors-ref vecs i)))
          (loop (+ i 1))))
      v)))
    

(define (s8vector-map! f vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (s8vector-length vec)))
      (let loop ((i 0))
        (unless (= i len)
          (s8vector-set! vec i (f (s8vector-ref vec i)))
          (loop (+ i 1)))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map s8vector-length vecs))))
      (let loop ((i 0))
        (unless (= i len)
          (s8vector-set! vec i (apply f (%s8vectors-ref vecs i)))
          (loop (+ i 1)))))))

(define (s8vector-for-each f vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (s8vector-length vec)))
      (let loop ((i 0))
        (unless (= i len)
          (f (s8vector-ref vec i))
          (loop (+ i 1)))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map s8vector-length vecs))))
      (let loop ((i 0))
        (unless (= i len)
          (apply f (%s8vectors-ref vecs i))
          (loop (+ i 1)))))))

(define (s8vector-count pred vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (s8vector-length vec)))
      (let loop ((i 0) (r 0))
        (cond
         ((= i (s8vector-length vec)) r)
         ((pred (s8vector-ref vec i)) (loop (+ i 1) (+ r 1)))
         (else (loop (+ i 1) r)))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map s8vector-length vecs))))
      (let loop ((i 0) (r 0))
        (cond
         ((= i len) r)
         ((apply pred (%s8vectors-ref vecs i)) (loop (+ i 1) (+ r 1)))
         (else (loop (+ i 1) r)))))))

(define (s8vector-cumulate f knil vec)
  (let* ((len (s8vector-length vec))
         (v (make-s8vector len)))
    (let loop ((r knil) (i 0))
      (unless (= i len)
        (let ((next (f r (s8vector-ref vec i))))
          (s8vector-set! v i next)
          (loop next (+ i 1)))))
    v))

(define (s8vector-foreach f vec)
  (let ((len (s8vector-length vec)))
    (let loop ((i 0))
      (unless (= i len)
        (f (s8vector-ref vec i))
        (loop (+ i 1))))))

(define (s8vector-take-while pred vec)
  (let* ((len (s8vector-length vec))
         (idx (s8vector-skip pred vec))
         (idx* (if idx idx len)))
    (s8vector-copy vec 0 idx*)))

(define (s8vector-take-while-right pred vec)
  (let* ((len (s8vector-length vec))
         (idx (s8vector-skip-right pred vec))
         (idx* (if idx (+ idx 1) 0)))
    (s8vector-copy vec idx* len)))

(define (s8vector-drop-while pred vec)
  (let* ((len (s8vector-length vec))
         (idx (s8vector-skip pred vec))
         (idx* (if idx idx len)))
    (s8vector-copy vec idx* len)))

(define (s8vector-drop-while-right pred vec)
  (let* ((len (s8vector-length vec))
         (idx (s8vector-skip-right pred vec))
         (idx* (if idx idx -1)))
    (s8vector-copy vec 0 (+ 1 idx*))))

(define (s8vector-index pred vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (s8vector-length vec)))
      (let loop ((i 0))
        (cond
         ((= i len) #f)
         ((pred (s8vector-ref vec i)) i)
         (else (loop (+ i 1))))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map s8vector-length vecs))))
      (let loop ((i 0))
        (cond
         ((= i len) #f)
         ((apply pred (%s8vectors-ref vecs i)) i)
         (else (loop (+ i 1))))))))

(define (s8vector-index-right pred vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (s8vector-length vec)))
      (let loop ((i (- len 1)))
        (cond
         ((negative? i) #f)
         ((pred (s8vector-ref vec i)) i)
         (else (loop (- i 1))))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map s8vector-length vecs))))
      (let loop ((i (- len 1)))
        (cond
         ((negative? i) #f)
         ((apply pred (%s8vectors-ref vecs i)) i)
         (else (loop (- i 1))))))))

(define (s8vector-skip pred vec . vecs)
  (if (null? vecs)
    (s8vector-index (lambda (x) (not (pred x))) vec)
    (apply s8vector-index (lambda xs (not (apply pred xs))) vec vecs)))
     
(define (s8vector-skip-right pred vec . vecs)
  (if (null? vecs)
    (s8vector-index-right (lambda (x) (not (pred x))) vec)
    (apply s8vector-index-right (lambda xs (not (apply pred xs))) vec vecs)))

(define (s8vector-any pred vec . vecs)
  (if (null? vec)
    ;; fast path
    (let ((len (s8vector-length vec)))
      (let loop ((i 0))
        (cond
         ((= i len) #f)
         ((pred (s8vector-ref vec i)))  ;returns result of pred
         (else (loop (+ i 1))))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map s8vector-length vecs))))
      (let loop ((i 0))
        (cond
         ((= i len) #f)
         ((apply pred (%s8vectors-ref vecs i))) ;returns result of pred
         (else (loop (+ i 1))))))))

(define (s8vector-every pred vec . vecs)
  (if (null? vec)
    ;; fast path
    (let ((len (s8vector-length vec)))
      (let loop ((i 0) (last #t))
        (cond
         ((= i len) last)
         ((pred (s8vector-ref vec i)) => (lambda (r) (loop (+ i 1) r)))
         (else #f))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map s8vector-length vecs))))
      (let loop ((i 0) (last #t))
        (cond
         ((= i len) last)
         ((apply pred (%s8vectors-ref vecs i)) => (lambda (r) (loop (+ i 1) r)))
         (else #f))))))

(define (s8vector-partition pred vec)
  (let* ((len (s8vector-length vec))
         (cnt (s8vector-count pred vec))
         (r (make-s8vector len)))
    (let loop ((i 0) (yes 0) (no cnt))
      (cond
        ((= i len) r)
        ((pred (s8vector-ref vec i))
         (s8vector-set! r yes (s8vector-ref vec i))
         (loop (+ i 1) (+ yes 1) no))
        (else
         (s8vector-set! r no (s8vector-ref vec i))
         (loop (+ i 1) yes (+ no 1)))))))

(define (s8vector-filter pred vec)
  (let* ((len (s8vector-length vec))
         (cnt (s8vector-count pred vec))
         (r (make-s8vector cnt)))
    (let loop ((i 0) (j 0))
      (cond
        ((= i len) r)
        ((pred (s8vector-ref vec i))
         (s8vector-set! r j (s8vector-ref vec i))
         (loop (+ i 1) (+ j 1)))
        (else
         (loop (+ i 1) j))))))

(define (s8vector-remove pred vec)
  (s8vector-filter (lambda (x) (not (pred x))) vec))

;; s8vector-set! defined in (srfi 160 base)

(define (s8vector-swap! vec i j)
  (let ((ival (s8vector-ref vec i))
        (jval (s8vector-ref vec j)))
    (s8vector-set! vec i jval)
    (s8vector-set! vec j ival)))

(define s8vector-fill!
  (case-lambda
    ((vec fill) (s8vector-fill-some! vec fill 0 (s8vector-length vec)))
    ((vec fill start) (s8vector-fill-some! vec fill start (s8vector-length vec)))
    ((vec fill start end) (s8vector-fill-some! vec fill start end))))

(define (s8vector-fill-some! vec fill start end)
  (unless (= start end)
    (s8vector-set! vec start fill)
    (s8vector-fill-some! vec fill (+ start 1) end)))

(define s8vector-reverse!
  (case-lambda
    ((vec) (s8vector-reverse-some! vec 0 (s8vector-length vec)))
    ((vec start) (s8vector-reverse-some! vec start (s8vector-length vec)))
    ((vec start end) (s8vector-reverse-some! vec start end))))

(define (s8vector-reverse-some! vec start end)
  (let ((len (s8vector-length vec)))
    (let loop ((i 0)(j (- len 1)))
      (when (< i j)
        (s8vector-swap! vec i j)
        (loop (+ i 1) (- j 1))))))

(define (s8vector-unfold! f vec start end seed)
  (let loop ((i start) (seed seed))
    (when (< i end)
      (let-values (((elt seed) (f seed)))
        (s8vector-set! vec i elt)
        (loop (+ i 1) seed)))))

(define (s8vector-unfold-right! f vec start end seed)
  (let loop ((i (- end 1)) (seed seed))
    (when (>= i start)
      (let-values (((elt seed) (f seed)))
        (s8vector-set! vec i elt)
        (loop (- i 1) seed)))))

(define reverse-s8vector->list
  (case-lambda
    ((vec) (reverse-s8vector->list* vec 0 (s8vector-length vec)))
    ((vec start) (reverse-s8vector->list* vec start (s8vector-length vec)))
    ((vec start end) (reverse-s8vector->list* vec start end))))

(define (reverse-s8vector->list* vec start end)
  (let loop ((i start) (r '()))
    (if (= i end)
      r
      (loop (+ 1 i) (cons (s8vector-ref vec i) r)))))

(define (reverse-list->s8vector list)
  (let* ((len (length list))
         (r (make-s8vector len)))
    (let loop ((i 0) (list list))
      (cond
        ((= i len) r)
        (else
          (s8vector-set! r (- len i 1) (car list))
          (loop (+ i 1) (cdr list)))))))

(define s8vector->vector
  (case-lambda
    ((vec) (s8vector->vector* vec 0 (s8vector-length vec)))
    ((vec start) (s8vector->vector* vec start (s8vector-length vec)))
    ((vec start end) (s8vector->vector* vec start end))))

(define (s8vector->vector* vec start end)
  (let* ((len (- end start))
         (r (make-vector len)))
    (let loop ((i start) (o 0))
      (cond
        ((= i end) r)
        (else
          (vector-set! r o (s8vector-ref vec i))
          (loop (+ i 1) (+ o 1)))))))

(define vector->s8vector
  (case-lambda
    ((vec) (vector->s8vector* vec 0 (vector-length vec)))
    ((vec start) (vector->s8vector* vec start (vector-length vec)))
    ((vec start end) (vector->s8vector* vec start end))))

(define (vector->s8vector* vec start end)
  (let* ((len (- end start))
         (r (make-s8vector len)))
    (let loop ((i start) (o 0))
      (cond
        ((= i end) r)
        (else
          (s8vector-set! r o (vector-ref vec i))
          (loop (+ i 1) (+ o 1)))))))

(define make-s8vector-generator
  (case-lambda ((vec) (make-s8vector-generator vec 0 (s8vector-length vec)))
               ((vec start) (make-s8vector-generator vec start (s8vector-length vec)))
               ((vec start end)
                (lambda () (if (>= start end)
                             (eof-object)
                             (let ((next (s8vector-ref vec start)))
                              (set! start (+ start 1))
                              next))))))

(define write-s8vector
  (case-lambda
    ((vec) (write-s8vector* vec (current-output-port)))
    ((vec port) (write-s8vector* vec port))))


(define (write-s8vector* vec port)
  (display "#s8(" port)  ; s8-expansion is blind, so will expand this too
  (let ((last (- (s8vector-length vec) 1)))
    (let loop ((i 0))
      (cond
        ((= i last)
         (write (s8vector-ref vec i) port)
         (display ")" port))
        (else
          (write (s8vector-ref vec i) port)
          (display " " port)
          (loop (+ i 1)))))))

(define (s8vector< vec1 vec2)
  (let ((len1 (s8vector-length vec1))
        (len2 (s8vector-length vec2)))
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
           ((< (s8vector-ref vec1 i) (s8vector-ref vec2 i))
            #t)
           ((> (s8vector-ref vec1 i) (s8vector-ref vec2 i))
            #f)
           (else
             (loop (+ i 1)))))))))

(define (s8vector-hash vec)
  (let ((len (min 256 (s8vector-length vec))))
    (let loop ((i 0) (r 0))
      (if (= i len)
        (abs (floor (real-part (inexact->exact r))))
        (loop (+ i 1) (+ r (s8vector-ref vec i)))))))

(define s8vector-comparator
  (make-comparator s8vector? s8vector= s8vector< s8vector-hash))

)
