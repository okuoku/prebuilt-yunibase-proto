;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; srfi/%3a160/s32.scm - Homogeneous numeric vector datatypes (s32)
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
(library (srfi :160 s32)
    (export make-s32vector s32vector
	    s32vector-unfold s32vector-unfold-right
	    s32vector-copy s32vector-reverse-copy 
	    s32vector-append s32vector-concatenate
	    s32vector-append-subvectors
	    ;; Predicates 
	    s32? s32vector? s32vector-empty? s32vector=
	    ;; Selectors
	    s32vector-ref s32vector-length
	    ;; Iteration 
	    s32vector-take s32vector-take-right
	    s32vector-drop s32vector-drop-right
	    s32vector-segment
	    s32vector-fold s32vector-fold-right
	    s32vector-map s32vector-map! s32vector-for-each
	    s32vector-count s32vector-cumulate
	    ;; Searching 
	    s32vector-take-while s32vector-take-while-right
	    s32vector-drop-while s32vector-drop-while-right
	    s32vector-index s32vector-index-right s32vector-skip s32vector-skip-right 
	    s32vector-any s32vector-every s32vector-partition
	    s32vector-filter s32vector-remove
	    ;; Mutators 
	    s32vector-set! s32vector-swap! s32vector-fill! s32vector-reverse!
	    s32vector-copy! s32vector-reverse-copy!
	    s32vector-unfold! s32vector-unfold-right!
	    ;; Conversion 
	    s32vector->list list->s32vector
	    reverse-s32vector->list reverse-list->s32vector
	    s32vector->vector vector->s32vector
	    ;; Misc
	    make-s32vector-generator s32vector-comparator write-s32vector)
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

;; make-s32vector defined in (srfi 160 base)

;; s32vector defined in (srfi 160 base)

(define (s32vector-unfold f len seed)
  (let ((v (make-s32vector len)))
    (let loop ((i 0) (state seed))
      (unless (= i len)
        (let-values (((value newstate) (f i state)))
          (s32vector-set! v i value)
          (loop (+ i 1) newstate))))
    v))

(define (s32vector-unfold-right f len seed)
  (let ((v (make-s32vector len)))
    (let loop ((i (- len 1)) (state seed))
      (unless (= i -1)
        (let-values (((value newstate) (f i state)))
          (s32vector-set! v i value)
          (loop (- i 1) newstate))))
    v))

(define s32vector-copy
  (case-lambda
    ((vec) (s32vector-copy* vec 0 (s32vector-length vec)))
    ((vec start) (s32vector-copy* vec start (s32vector-length vec)))
    ((vec start end) (s32vector-copy* vec start end))))

(define (s32vector-copy* vec start end)
  (let ((v (make-s32vector (- end start))))
    (s32vector-copy! v 0 vec start end)
    v))

(define (s32vector-copy! to at from start end)
  (let loop ((at at) (i start))
    (unless (= i end)
      (s32vector-set! to at (s32vector-ref from i))
      (loop (+ at 1) (+ i 1)))))

(define s32vector-reverse-copy
  (case-lambda
    ((vec) (s32vector-reverse-copy* vec 0 (s32vector-length vec)))
    ((vec start) (s32vector-reverse-copy* vec start (s32vector-length vec)))
    ((vec start end) (s32vector-reverse-copy* vec start end))))

(define (s32vector-reverse-copy* vec start end)
  (let ((v (make-s32vector (- end start))))
    (s32vector-reverse-copy! v 0 vec start end)
    v))

(define (s32vector-reverse-copy! to at from start end)
  (let loop ((at at) (i (- end 1)))
    (unless (< i start)
      (s32vector-set! to at (s32vector-ref from i))
      (loop (+ at 1) (- i 1)))))

(define (s32vector-append . vecs)
  (s32vector-concatenate vecs))

(define (s32vector-concatenate vecs)
  (let ((v (make-s32vector (len-sum vecs))))
    (let loop ((vecs vecs) (at 0))
      (unless (null? vecs)
        (let ((vec (car vecs)))
          (s32vector-copy! v at vec 0 (s32vector-length vec))
          (loop (cdr vecs) (+ at (s32vector-length vec)))))
    v)))

(define (len-sum vecs)
  (if (null? vecs)
    0
    (+ (s32vector-length (car vecs))
       (len-sum (cdr vecs)))))

(define (s32vector-append-subvectors . args)
  (let ((v (make-s32vector (len-subsum args))))
    (let loop ((args args) (at 0))
      (unless (null? args)
        (let ((vec (car args))
              (start (cadr args))
              (end (caddr args)))
          (s32vector-copy! v at vec start end)
          (loop (cdddr args) (+ at (- end start))))))
    v))

(define (len-subsum vecs)
  (if (null? vecs)
    0
    (+ (- (caddr vecs) (cadr vecs))
       (len-subsum (cdddr vecs)))))

;; s32? defined in (srfi 160 base)

;; s32vector? defined in (srfi 160 base)

(define (s32vector-empty? vec)
  (zero? (s32vector-length vec)))

(define (s32vector= . vecs)
  (s32vector=* (car vecs) (cadr vecs) (cddr vecs)))

(define (s32vector=* vec1 vec2 vecs)
  (if (null? vecs)
    (and
      (s32dyadic-vecs= vec1 0 (s32vector-length vec1)
                          vec2 0 (s32vector-length vec2))
      (if (null? vecs)
        #t
        (s32vector=* vec2 (car vecs) (cdr vecs))))))

(define (s32dyadic-vecs= vec1 start1 end1 vec2 start2 end2)
  (cond
    ((not (= end1 end2)) #f)
    ((not (< start1 end1)) #t)
    ((let ((elt1 (s32vector-ref vec1 start1))
           (elt2 (s32vector-ref vec2 start2)))
      (= elt1 elt2))
     (s32dyadic-vecs= vec1 (+ start1 1) end1
                         vec2 (+ start2 1) end2))
    (else #f)))

;; s32vector-ref defined in (srfi 160 base)

;; s32vector-length defined in (srfi 160 base)

(define (s32vector-take vec n)
  (let ((v (make-s32vector n)))
    (s32vector-copy! v 0 vec 0 n)
    v))

(define (s32vector-take-right vec n)
  (let ((v (make-s32vector n))
        (len (s32vector-length vec)))
    (s32vector-copy! v 0 vec (- len n) len)
    v))

(define (s32vector-drop vec n)
 (let* ((len (s32vector-length vec))
        (vlen (- len n))
        (v (make-s32vector vlen)))
    (s32vector-copy! v 0 vec n len)
    v))

(define (s32vector-drop-right vec n)
  (let* ((len (s32vector-length vec))
         (rlen (- len n))
         (v (make-s32vector rlen)))
    (s32vector-copy! v 0 vec 0 rlen)
    v))

(define (s32vector-segment vec n)
  (let loop ((r '()) (i 0) (remain (s32vector-length vec)))
    (if (<= remain 0)
      (reverse r)
      (let ((size (min n remain)))
        (loop
          (cons (s32vector-copy vec i (+ i size)) r)
          (+ i size)
          (- remain size))))))

;; aux. procedure
(define (%s32vectors-ref vecs i)
  (map (lambda (v) (s32vector-ref v i)) vecs))

(define (s32vector-fold kons knil vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (s32vector-length vec)))
      (let loop ((r knil) (i 0))
        (if (= i len)
          r
          (loop (kons r (s32vector-ref vec i)) (+ i 1)))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map s32vector-length vecs))))
      (let loop ((r knil) (i 0))
        (if (= i len)
          r
          (loop (apply kons r (%s32vectors-ref vecs i))
                (+ i 1)))))))

(define (s32vector-fold-right kons knil vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (s32vector-length vec)))
      (let loop ((r knil) (i (- (s32vector-length vec) 1)))
        (if (negative? i)
          r
          (loop (kons (s32vector-ref vec i) r) (- i 1)))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map s32vector-length vecs))))
      (let loop ((r knil) (i (- len 1)))
        (if (negative? i)
          r
          (loop (apply kons r (%s32vectors-ref vecs i))
                (- i 1)))))))

(define (s32vector-map f vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let* ((len (s32vector-length vec))
           (v (make-s32vector len)))
      (let loop ((i 0))
        (unless (= i len)
          (s32vector-set! v i (f (s32vector-ref vec i)))
          (loop (+ i 1))))
      v)
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map s32vector-length vecs)))
           (v (make-s32vector len)))
      (let loop ((i 0))
        (unless (= i len)
          (s32vector-set! v i (apply f (%s32vectors-ref vecs i)))
          (loop (+ i 1))))
      v)))
    

(define (s32vector-map! f vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (s32vector-length vec)))
      (let loop ((i 0))
        (unless (= i len)
          (s32vector-set! vec i (f (s32vector-ref vec i)))
          (loop (+ i 1)))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map s32vector-length vecs))))
      (let loop ((i 0))
        (unless (= i len)
          (s32vector-set! vec i (apply f (%s32vectors-ref vecs i)))
          (loop (+ i 1)))))))

(define (s32vector-for-each f vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (s32vector-length vec)))
      (let loop ((i 0))
        (unless (= i len)
          (f (s32vector-ref vec i))
          (loop (+ i 1)))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map s32vector-length vecs))))
      (let loop ((i 0))
        (unless (= i len)
          (apply f (%s32vectors-ref vecs i))
          (loop (+ i 1)))))))

(define (s32vector-count pred vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (s32vector-length vec)))
      (let loop ((i 0) (r 0))
        (cond
         ((= i (s32vector-length vec)) r)
         ((pred (s32vector-ref vec i)) (loop (+ i 1) (+ r 1)))
         (else (loop (+ i 1) r)))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map s32vector-length vecs))))
      (let loop ((i 0) (r 0))
        (cond
         ((= i len) r)
         ((apply pred (%s32vectors-ref vecs i)) (loop (+ i 1) (+ r 1)))
         (else (loop (+ i 1) r)))))))

(define (s32vector-cumulate f knil vec)
  (let* ((len (s32vector-length vec))
         (v (make-s32vector len)))
    (let loop ((r knil) (i 0))
      (unless (= i len)
        (let ((next (f r (s32vector-ref vec i))))
          (s32vector-set! v i next)
          (loop next (+ i 1)))))
    v))

(define (s32vector-foreach f vec)
  (let ((len (s32vector-length vec)))
    (let loop ((i 0))
      (unless (= i len)
        (f (s32vector-ref vec i))
        (loop (+ i 1))))))

(define (s32vector-take-while pred vec)
  (let* ((len (s32vector-length vec))
         (idx (s32vector-skip pred vec))
         (idx* (if idx idx len)))
    (s32vector-copy vec 0 idx*)))

(define (s32vector-take-while-right pred vec)
  (let* ((len (s32vector-length vec))
         (idx (s32vector-skip-right pred vec))
         (idx* (if idx (+ idx 1) 0)))
    (s32vector-copy vec idx* len)))

(define (s32vector-drop-while pred vec)
  (let* ((len (s32vector-length vec))
         (idx (s32vector-skip pred vec))
         (idx* (if idx idx len)))
    (s32vector-copy vec idx* len)))

(define (s32vector-drop-while-right pred vec)
  (let* ((len (s32vector-length vec))
         (idx (s32vector-skip-right pred vec))
         (idx* (if idx idx -1)))
    (s32vector-copy vec 0 (+ 1 idx*))))

(define (s32vector-index pred vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (s32vector-length vec)))
      (let loop ((i 0))
        (cond
         ((= i len) #f)
         ((pred (s32vector-ref vec i)) i)
         (else (loop (+ i 1))))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map s32vector-length vecs))))
      (let loop ((i 0))
        (cond
         ((= i len) #f)
         ((apply pred (%s32vectors-ref vecs i)) i)
         (else (loop (+ i 1))))))))

(define (s32vector-index-right pred vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (s32vector-length vec)))
      (let loop ((i (- len 1)))
        (cond
         ((negative? i) #f)
         ((pred (s32vector-ref vec i)) i)
         (else (loop (- i 1))))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map s32vector-length vecs))))
      (let loop ((i (- len 1)))
        (cond
         ((negative? i) #f)
         ((apply pred (%s32vectors-ref vecs i)) i)
         (else (loop (- i 1))))))))

(define (s32vector-skip pred vec . vecs)
  (if (null? vecs)
    (s32vector-index (lambda (x) (not (pred x))) vec)
    (apply s32vector-index (lambda xs (not (apply pred xs))) vec vecs)))
     
(define (s32vector-skip-right pred vec . vecs)
  (if (null? vecs)
    (s32vector-index-right (lambda (x) (not (pred x))) vec)
    (apply s32vector-index-right (lambda xs (not (apply pred xs))) vec vecs)))

(define (s32vector-any pred vec . vecs)
  (if (null? vec)
    ;; fast path
    (let ((len (s32vector-length vec)))
      (let loop ((i 0))
        (cond
         ((= i len) #f)
         ((pred (s32vector-ref vec i)))  ;returns result of pred
         (else (loop (+ i 1))))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map s32vector-length vecs))))
      (let loop ((i 0))
        (cond
         ((= i len) #f)
         ((apply pred (%s32vectors-ref vecs i))) ;returns result of pred
         (else (loop (+ i 1))))))))

(define (s32vector-every pred vec . vecs)
  (if (null? vec)
    ;; fast path
    (let ((len (s32vector-length vec)))
      (let loop ((i 0) (last #t))
        (cond
         ((= i len) last)
         ((pred (s32vector-ref vec i)) => (lambda (r) (loop (+ i 1) r)))
         (else #f))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map s32vector-length vecs))))
      (let loop ((i 0) (last #t))
        (cond
         ((= i len) last)
         ((apply pred (%s32vectors-ref vecs i)) => (lambda (r) (loop (+ i 1) r)))
         (else #f))))))

(define (s32vector-partition pred vec)
  (let* ((len (s32vector-length vec))
         (cnt (s32vector-count pred vec))
         (r (make-s32vector len)))
    (let loop ((i 0) (yes 0) (no cnt))
      (cond
        ((= i len) r)
        ((pred (s32vector-ref vec i))
         (s32vector-set! r yes (s32vector-ref vec i))
         (loop (+ i 1) (+ yes 1) no))
        (else
         (s32vector-set! r no (s32vector-ref vec i))
         (loop (+ i 1) yes (+ no 1)))))))

(define (s32vector-filter pred vec)
  (let* ((len (s32vector-length vec))
         (cnt (s32vector-count pred vec))
         (r (make-s32vector cnt)))
    (let loop ((i 0) (j 0))
      (cond
        ((= i len) r)
        ((pred (s32vector-ref vec i))
         (s32vector-set! r j (s32vector-ref vec i))
         (loop (+ i 1) (+ j 1)))
        (else
         (loop (+ i 1) j))))))

(define (s32vector-remove pred vec)
  (s32vector-filter (lambda (x) (not (pred x))) vec))

;; s32vector-set! defined in (srfi 160 base)

(define (s32vector-swap! vec i j)
  (let ((ival (s32vector-ref vec i))
        (jval (s32vector-ref vec j)))
    (s32vector-set! vec i jval)
    (s32vector-set! vec j ival)))

(define s32vector-fill!
  (case-lambda
    ((vec fill) (s32vector-fill-some! vec fill 0 (s32vector-length vec)))
    ((vec fill start) (s32vector-fill-some! vec fill start (s32vector-length vec)))
    ((vec fill start end) (s32vector-fill-some! vec fill start end))))

(define (s32vector-fill-some! vec fill start end)
  (unless (= start end)
    (s32vector-set! vec start fill)
    (s32vector-fill-some! vec fill (+ start 1) end)))

(define s32vector-reverse!
  (case-lambda
    ((vec) (s32vector-reverse-some! vec 0 (s32vector-length vec)))
    ((vec start) (s32vector-reverse-some! vec start (s32vector-length vec)))
    ((vec start end) (s32vector-reverse-some! vec start end))))

(define (s32vector-reverse-some! vec start end)
  (let ((len (s32vector-length vec)))
    (let loop ((i 0)(j (- len 1)))
      (when (< i j)
        (s32vector-swap! vec i j)
        (loop (+ i 1) (- j 1))))))

(define (s32vector-unfold! f vec start end seed)
  (let loop ((i start) (seed seed))
    (when (< i end)
      (let-values (((elt seed) (f seed)))
        (s32vector-set! vec i elt)
        (loop (+ i 1) seed)))))

(define (s32vector-unfold-right! f vec start end seed)
  (let loop ((i (- end 1)) (seed seed))
    (when (>= i start)
      (let-values (((elt seed) (f seed)))
        (s32vector-set! vec i elt)
        (loop (- i 1) seed)))))

(define reverse-s32vector->list
  (case-lambda
    ((vec) (reverse-s32vector->list* vec 0 (s32vector-length vec)))
    ((vec start) (reverse-s32vector->list* vec start (s32vector-length vec)))
    ((vec start end) (reverse-s32vector->list* vec start end))))

(define (reverse-s32vector->list* vec start end)
  (let loop ((i start) (r '()))
    (if (= i end)
      r
      (loop (+ 1 i) (cons (s32vector-ref vec i) r)))))

(define (reverse-list->s32vector list)
  (let* ((len (length list))
         (r (make-s32vector len)))
    (let loop ((i 0) (list list))
      (cond
        ((= i len) r)
        (else
          (s32vector-set! r (- len i 1) (car list))
          (loop (+ i 1) (cdr list)))))))

(define s32vector->vector
  (case-lambda
    ((vec) (s32vector->vector* vec 0 (s32vector-length vec)))
    ((vec start) (s32vector->vector* vec start (s32vector-length vec)))
    ((vec start end) (s32vector->vector* vec start end))))

(define (s32vector->vector* vec start end)
  (let* ((len (- end start))
         (r (make-vector len)))
    (let loop ((i start) (o 0))
      (cond
        ((= i end) r)
        (else
          (vector-set! r o (s32vector-ref vec i))
          (loop (+ i 1) (+ o 1)))))))

(define vector->s32vector
  (case-lambda
    ((vec) (vector->s32vector* vec 0 (vector-length vec)))
    ((vec start) (vector->s32vector* vec start (vector-length vec)))
    ((vec start end) (vector->s32vector* vec start end))))

(define (vector->s32vector* vec start end)
  (let* ((len (- end start))
         (r (make-s32vector len)))
    (let loop ((i start) (o 0))
      (cond
        ((= i end) r)
        (else
          (s32vector-set! r o (vector-ref vec i))
          (loop (+ i 1) (+ o 1)))))))

(define make-s32vector-generator
  (case-lambda ((vec) (make-s32vector-generator vec 0 (s32vector-length vec)))
               ((vec start) (make-s32vector-generator vec start (s32vector-length vec)))
               ((vec start end)
                (lambda () (if (>= start end)
                             (eof-object)
                             (let ((next (s32vector-ref vec start)))
                              (set! start (+ start 1))
                              next))))))

(define write-s32vector
  (case-lambda
    ((vec) (write-s32vector* vec (current-output-port)))
    ((vec port) (write-s32vector* vec port))))


(define (write-s32vector* vec port)
  (display "#s32(" port)  ; s32-expansion is blind, so will expand this too
  (let ((last (- (s32vector-length vec) 1)))
    (let loop ((i 0))
      (cond
        ((= i last)
         (write (s32vector-ref vec i) port)
         (display ")" port))
        (else
          (write (s32vector-ref vec i) port)
          (display " " port)
          (loop (+ i 1)))))))

(define (s32vector< vec1 vec2)
  (let ((len1 (s32vector-length vec1))
        (len2 (s32vector-length vec2)))
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
           ((< (s32vector-ref vec1 i) (s32vector-ref vec2 i))
            #t)
           ((> (s32vector-ref vec1 i) (s32vector-ref vec2 i))
            #f)
           (else
             (loop (+ i 1)))))))))

(define (s32vector-hash vec)
  (let ((len (min 256 (s32vector-length vec))))
    (let loop ((i 0) (r 0))
      (if (= i len)
        (abs (floor (real-part (inexact->exact r))))
        (loop (+ i 1) (+ r (s32vector-ref vec i)))))))

(define s32vector-comparator
  (make-comparator s32vector? s32vector= s32vector< s32vector-hash))

)
