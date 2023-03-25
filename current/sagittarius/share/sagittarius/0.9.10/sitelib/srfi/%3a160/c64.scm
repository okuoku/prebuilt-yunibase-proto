;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; srfi/%3a160/c64.scm - Homogeneous numeric vector datatypes (c64)
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
(library (srfi :160 c64)
    (export make-c64vector c64vector
	    c64vector-unfold c64vector-unfold-right
	    c64vector-copy c64vector-reverse-copy 
	    c64vector-append c64vector-concatenate
	    c64vector-append-subvectors
	    ;; Predicates 
	    c64? c64vector? c64vector-empty? c64vector=
	    ;; Selectors
	    c64vector-ref c64vector-length
	    ;; Iteration 
	    c64vector-take c64vector-take-right
	    c64vector-drop c64vector-drop-right
	    c64vector-segment
	    c64vector-fold c64vector-fold-right
	    c64vector-map c64vector-map! c64vector-for-each
	    c64vector-count c64vector-cumulate
	    ;; Searching 
	    c64vector-take-while c64vector-take-while-right
	    c64vector-drop-while c64vector-drop-while-right
	    c64vector-index c64vector-index-right c64vector-skip c64vector-skip-right 
	    c64vector-any c64vector-every c64vector-partition
	    c64vector-filter c64vector-remove
	    ;; Mutators 
	    c64vector-set! c64vector-swap! c64vector-fill! c64vector-reverse!
	    c64vector-copy! c64vector-reverse-copy!
	    c64vector-unfold! c64vector-unfold-right!
	    ;; Conversion 
	    c64vector->list list->c64vector
	    reverse-c64vector->list reverse-list->c64vector
	    c64vector->vector vector->c64vector
	    ;; Misc
	    make-c64vector-generator c64vector-comparator write-c64vector)
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

;; make-c64vector defined in (srfi 160 base)

;; c64vector defined in (srfi 160 base)

(define (c64vector-unfold f len seed)
  (let ((v (make-c64vector len)))
    (let loop ((i 0) (state seed))
      (unless (= i len)
        (let-values (((value newstate) (f i state)))
          (c64vector-set! v i value)
          (loop (+ i 1) newstate))))
    v))

(define (c64vector-unfold-right f len seed)
  (let ((v (make-c64vector len)))
    (let loop ((i (- len 1)) (state seed))
      (unless (= i -1)
        (let-values (((value newstate) (f i state)))
          (c64vector-set! v i value)
          (loop (- i 1) newstate))))
    v))

(define c64vector-copy
  (case-lambda
    ((vec) (c64vector-copy* vec 0 (c64vector-length vec)))
    ((vec start) (c64vector-copy* vec start (c64vector-length vec)))
    ((vec start end) (c64vector-copy* vec start end))))

(define (c64vector-copy* vec start end)
  (let ((v (make-c64vector (- end start))))
    (c64vector-copy! v 0 vec start end)
    v))

(define (c64vector-copy! to at from start end)
  (let loop ((at at) (i start))
    (unless (= i end)
      (c64vector-set! to at (c64vector-ref from i))
      (loop (+ at 1) (+ i 1)))))

(define c64vector-reverse-copy
  (case-lambda
    ((vec) (c64vector-reverse-copy* vec 0 (c64vector-length vec)))
    ((vec start) (c64vector-reverse-copy* vec start (c64vector-length vec)))
    ((vec start end) (c64vector-reverse-copy* vec start end))))

(define (c64vector-reverse-copy* vec start end)
  (let ((v (make-c64vector (- end start))))
    (c64vector-reverse-copy! v 0 vec start end)
    v))

(define (c64vector-reverse-copy! to at from start end)
  (let loop ((at at) (i (- end 1)))
    (unless (< i start)
      (c64vector-set! to at (c64vector-ref from i))
      (loop (+ at 1) (- i 1)))))

(define (c64vector-append . vecs)
  (c64vector-concatenate vecs))

(define (c64vector-concatenate vecs)
  (let ((v (make-c64vector (len-sum vecs))))
    (let loop ((vecs vecs) (at 0))
      (unless (null? vecs)
        (let ((vec (car vecs)))
          (c64vector-copy! v at vec 0 (c64vector-length vec))
          (loop (cdr vecs) (+ at (c64vector-length vec)))))
    v)))

(define (len-sum vecs)
  (if (null? vecs)
    0
    (+ (c64vector-length (car vecs))
       (len-sum (cdr vecs)))))

(define (c64vector-append-subvectors . args)
  (let ((v (make-c64vector (len-subsum args))))
    (let loop ((args args) (at 0))
      (unless (null? args)
        (let ((vec (car args))
              (start (cadr args))
              (end (caddr args)))
          (c64vector-copy! v at vec start end)
          (loop (cdddr args) (+ at (- end start))))))
    v))

(define (len-subsum vecs)
  (if (null? vecs)
    0
    (+ (- (caddr vecs) (cadr vecs))
       (len-subsum (cdddr vecs)))))

;; c64? defined in (srfi 160 base)

;; c64vector? defined in (srfi 160 base)

(define (c64vector-empty? vec)
  (zero? (c64vector-length vec)))

(define (c64vector= . vecs)
  (c64vector=* (car vecs) (cadr vecs) (cddr vecs)))

(define (c64vector=* vec1 vec2 vecs)
  (if (null? vecs)
    (and
      (c64dyadic-vecs= vec1 0 (c64vector-length vec1)
                          vec2 0 (c64vector-length vec2))
      (if (null? vecs)
        #t
        (c64vector=* vec2 (car vecs) (cdr vecs))))))

(define (c64dyadic-vecs= vec1 start1 end1 vec2 start2 end2)
  (cond
    ((not (= end1 end2)) #f)
    ((not (< start1 end1)) #t)
    ((let ((elt1 (c64vector-ref vec1 start1))
           (elt2 (c64vector-ref vec2 start2)))
      (= elt1 elt2))
     (c64dyadic-vecs= vec1 (+ start1 1) end1
                         vec2 (+ start2 1) end2))
    (else #f)))

;; c64vector-ref defined in (srfi 160 base)

;; c64vector-length defined in (srfi 160 base)

(define (c64vector-take vec n)
  (let ((v (make-c64vector n)))
    (c64vector-copy! v 0 vec 0 n)
    v))

(define (c64vector-take-right vec n)
  (let ((v (make-c64vector n))
        (len (c64vector-length vec)))
    (c64vector-copy! v 0 vec (- len n) len)
    v))

(define (c64vector-drop vec n)
 (let* ((len (c64vector-length vec))
        (vlen (- len n))
        (v (make-c64vector vlen)))
    (c64vector-copy! v 0 vec n len)
    v))

(define (c64vector-drop-right vec n)
  (let* ((len (c64vector-length vec))
         (rlen (- len n))
         (v (make-c64vector rlen)))
    (c64vector-copy! v 0 vec 0 rlen)
    v))

(define (c64vector-segment vec n)
  (let loop ((r '()) (i 0) (remain (c64vector-length vec)))
    (if (<= remain 0)
      (reverse r)
      (let ((size (min n remain)))
        (loop
          (cons (c64vector-copy vec i (+ i size)) r)
          (+ i size)
          (- remain size))))))

;; aux. procedure
(define (%c64vectors-ref vecs i)
  (map (lambda (v) (c64vector-ref v i)) vecs))

(define (c64vector-fold kons knil vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (c64vector-length vec)))
      (let loop ((r knil) (i 0))
        (if (= i len)
          r
          (loop (kons r (c64vector-ref vec i)) (+ i 1)))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map c64vector-length vecs))))
      (let loop ((r knil) (i 0))
        (if (= i len)
          r
          (loop (apply kons r (%c64vectors-ref vecs i))
                (+ i 1)))))))

(define (c64vector-fold-right kons knil vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (c64vector-length vec)))
      (let loop ((r knil) (i (- (c64vector-length vec) 1)))
        (if (negative? i)
          r
          (loop (kons (c64vector-ref vec i) r) (- i 1)))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map c64vector-length vecs))))
      (let loop ((r knil) (i (- len 1)))
        (if (negative? i)
          r
          (loop (apply kons r (%c64vectors-ref vecs i))
                (- i 1)))))))

(define (c64vector-map f vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let* ((len (c64vector-length vec))
           (v (make-c64vector len)))
      (let loop ((i 0))
        (unless (= i len)
          (c64vector-set! v i (f (c64vector-ref vec i)))
          (loop (+ i 1))))
      v)
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map c64vector-length vecs)))
           (v (make-c64vector len)))
      (let loop ((i 0))
        (unless (= i len)
          (c64vector-set! v i (apply f (%c64vectors-ref vecs i)))
          (loop (+ i 1))))
      v)))
    

(define (c64vector-map! f vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (c64vector-length vec)))
      (let loop ((i 0))
        (unless (= i len)
          (c64vector-set! vec i (f (c64vector-ref vec i)))
          (loop (+ i 1)))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map c64vector-length vecs))))
      (let loop ((i 0))
        (unless (= i len)
          (c64vector-set! vec i (apply f (%c64vectors-ref vecs i)))
          (loop (+ i 1)))))))

(define (c64vector-for-each f vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (c64vector-length vec)))
      (let loop ((i 0))
        (unless (= i len)
          (f (c64vector-ref vec i))
          (loop (+ i 1)))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map c64vector-length vecs))))
      (let loop ((i 0))
        (unless (= i len)
          (apply f (%c64vectors-ref vecs i))
          (loop (+ i 1)))))))

(define (c64vector-count pred vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (c64vector-length vec)))
      (let loop ((i 0) (r 0))
        (cond
         ((= i (c64vector-length vec)) r)
         ((pred (c64vector-ref vec i)) (loop (+ i 1) (+ r 1)))
         (else (loop (+ i 1) r)))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map c64vector-length vecs))))
      (let loop ((i 0) (r 0))
        (cond
         ((= i len) r)
         ((apply pred (%c64vectors-ref vecs i)) (loop (+ i 1) (+ r 1)))
         (else (loop (+ i 1) r)))))))

(define (c64vector-cumulate f knil vec)
  (let* ((len (c64vector-length vec))
         (v (make-c64vector len)))
    (let loop ((r knil) (i 0))
      (unless (= i len)
        (let ((next (f r (c64vector-ref vec i))))
          (c64vector-set! v i next)
          (loop next (+ i 1)))))
    v))

(define (c64vector-foreach f vec)
  (let ((len (c64vector-length vec)))
    (let loop ((i 0))
      (unless (= i len)
        (f (c64vector-ref vec i))
        (loop (+ i 1))))))

(define (c64vector-take-while pred vec)
  (let* ((len (c64vector-length vec))
         (idx (c64vector-skip pred vec))
         (idx* (if idx idx len)))
    (c64vector-copy vec 0 idx*)))

(define (c64vector-take-while-right pred vec)
  (let* ((len (c64vector-length vec))
         (idx (c64vector-skip-right pred vec))
         (idx* (if idx (+ idx 1) 0)))
    (c64vector-copy vec idx* len)))

(define (c64vector-drop-while pred vec)
  (let* ((len (c64vector-length vec))
         (idx (c64vector-skip pred vec))
         (idx* (if idx idx len)))
    (c64vector-copy vec idx* len)))

(define (c64vector-drop-while-right pred vec)
  (let* ((len (c64vector-length vec))
         (idx (c64vector-skip-right pred vec))
         (idx* (if idx idx -1)))
    (c64vector-copy vec 0 (+ 1 idx*))))

(define (c64vector-index pred vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (c64vector-length vec)))
      (let loop ((i 0))
        (cond
         ((= i len) #f)
         ((pred (c64vector-ref vec i)) i)
         (else (loop (+ i 1))))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map c64vector-length vecs))))
      (let loop ((i 0))
        (cond
         ((= i len) #f)
         ((apply pred (%c64vectors-ref vecs i)) i)
         (else (loop (+ i 1))))))))

(define (c64vector-index-right pred vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (c64vector-length vec)))
      (let loop ((i (- len 1)))
        (cond
         ((negative? i) #f)
         ((pred (c64vector-ref vec i)) i)
         (else (loop (- i 1))))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map c64vector-length vecs))))
      (let loop ((i (- len 1)))
        (cond
         ((negative? i) #f)
         ((apply pred (%c64vectors-ref vecs i)) i)
         (else (loop (- i 1))))))))

(define (c64vector-skip pred vec . vecs)
  (if (null? vecs)
    (c64vector-index (lambda (x) (not (pred x))) vec)
    (apply c64vector-index (lambda xs (not (apply pred xs))) vec vecs)))
     
(define (c64vector-skip-right pred vec . vecs)
  (if (null? vecs)
    (c64vector-index-right (lambda (x) (not (pred x))) vec)
    (apply c64vector-index-right (lambda xs (not (apply pred xs))) vec vecs)))

(define (c64vector-any pred vec . vecs)
  (if (null? vec)
    ;; fast path
    (let ((len (c64vector-length vec)))
      (let loop ((i 0))
        (cond
         ((= i len) #f)
         ((pred (c64vector-ref vec i)))  ;returns result of pred
         (else (loop (+ i 1))))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map c64vector-length vecs))))
      (let loop ((i 0))
        (cond
         ((= i len) #f)
         ((apply pred (%c64vectors-ref vecs i))) ;returns result of pred
         (else (loop (+ i 1))))))))

(define (c64vector-every pred vec . vecs)
  (if (null? vec)
    ;; fast path
    (let ((len (c64vector-length vec)))
      (let loop ((i 0) (last #t))
        (cond
         ((= i len) last)
         ((pred (c64vector-ref vec i)) => (lambda (r) (loop (+ i 1) r)))
         (else #f))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map c64vector-length vecs))))
      (let loop ((i 0) (last #t))
        (cond
         ((= i len) last)
         ((apply pred (%c64vectors-ref vecs i)) => (lambda (r) (loop (+ i 1) r)))
         (else #f))))))

(define (c64vector-partition pred vec)
  (let* ((len (c64vector-length vec))
         (cnt (c64vector-count pred vec))
         (r (make-c64vector len)))
    (let loop ((i 0) (yes 0) (no cnt))
      (cond
        ((= i len) r)
        ((pred (c64vector-ref vec i))
         (c64vector-set! r yes (c64vector-ref vec i))
         (loop (+ i 1) (+ yes 1) no))
        (else
         (c64vector-set! r no (c64vector-ref vec i))
         (loop (+ i 1) yes (+ no 1)))))))

(define (c64vector-filter pred vec)
  (let* ((len (c64vector-length vec))
         (cnt (c64vector-count pred vec))
         (r (make-c64vector cnt)))
    (let loop ((i 0) (j 0))
      (cond
        ((= i len) r)
        ((pred (c64vector-ref vec i))
         (c64vector-set! r j (c64vector-ref vec i))
         (loop (+ i 1) (+ j 1)))
        (else
         (loop (+ i 1) j))))))

(define (c64vector-remove pred vec)
  (c64vector-filter (lambda (x) (not (pred x))) vec))

;; c64vector-set! defined in (srfi 160 base)

(define (c64vector-swap! vec i j)
  (let ((ival (c64vector-ref vec i))
        (jval (c64vector-ref vec j)))
    (c64vector-set! vec i jval)
    (c64vector-set! vec j ival)))

(define c64vector-fill!
  (case-lambda
    ((vec fill) (c64vector-fill-some! vec fill 0 (c64vector-length vec)))
    ((vec fill start) (c64vector-fill-some! vec fill start (c64vector-length vec)))
    ((vec fill start end) (c64vector-fill-some! vec fill start end))))

(define (c64vector-fill-some! vec fill start end)
  (unless (= start end)
    (c64vector-set! vec start fill)
    (c64vector-fill-some! vec fill (+ start 1) end)))

(define c64vector-reverse!
  (case-lambda
    ((vec) (c64vector-reverse-some! vec 0 (c64vector-length vec)))
    ((vec start) (c64vector-reverse-some! vec start (c64vector-length vec)))
    ((vec start end) (c64vector-reverse-some! vec start end))))

(define (c64vector-reverse-some! vec start end)
  (let ((len (c64vector-length vec)))
    (let loop ((i 0)(j (- len 1)))
      (when (< i j)
        (c64vector-swap! vec i j)
        (loop (+ i 1) (- j 1))))))

(define (c64vector-unfold! f vec start end seed)
  (let loop ((i start) (seed seed))
    (when (< i end)
      (let-values (((elt seed) (f seed)))
        (c64vector-set! vec i elt)
        (loop (+ i 1) seed)))))

(define (c64vector-unfold-right! f vec start end seed)
  (let loop ((i (- end 1)) (seed seed))
    (when (>= i start)
      (let-values (((elt seed) (f seed)))
        (c64vector-set! vec i elt)
        (loop (- i 1) seed)))))

(define reverse-c64vector->list
  (case-lambda
    ((vec) (reverse-c64vector->list* vec 0 (c64vector-length vec)))
    ((vec start) (reverse-c64vector->list* vec start (c64vector-length vec)))
    ((vec start end) (reverse-c64vector->list* vec start end))))

(define (reverse-c64vector->list* vec start end)
  (let loop ((i start) (r '()))
    (if (= i end)
      r
      (loop (+ 1 i) (cons (c64vector-ref vec i) r)))))

(define (reverse-list->c64vector list)
  (let* ((len (length list))
         (r (make-c64vector len)))
    (let loop ((i 0) (list list))
      (cond
        ((= i len) r)
        (else
          (c64vector-set! r (- len i 1) (car list))
          (loop (+ i 1) (cdr list)))))))

(define c64vector->vector
  (case-lambda
    ((vec) (c64vector->vector* vec 0 (c64vector-length vec)))
    ((vec start) (c64vector->vector* vec start (c64vector-length vec)))
    ((vec start end) (c64vector->vector* vec start end))))

(define (c64vector->vector* vec start end)
  (let* ((len (- end start))
         (r (make-vector len)))
    (let loop ((i start) (o 0))
      (cond
        ((= i end) r)
        (else
          (vector-set! r o (c64vector-ref vec i))
          (loop (+ i 1) (+ o 1)))))))

(define vector->c64vector
  (case-lambda
    ((vec) (vector->c64vector* vec 0 (vector-length vec)))
    ((vec start) (vector->c64vector* vec start (vector-length vec)))
    ((vec start end) (vector->c64vector* vec start end))))

(define (vector->c64vector* vec start end)
  (let* ((len (- end start))
         (r (make-c64vector len)))
    (let loop ((i start) (o 0))
      (cond
        ((= i end) r)
        (else
          (c64vector-set! r o (vector-ref vec i))
          (loop (+ i 1) (+ o 1)))))))

(define make-c64vector-generator
  (case-lambda ((vec) (make-c64vector-generator vec 0 (c64vector-length vec)))
               ((vec start) (make-c64vector-generator vec start (c64vector-length vec)))
               ((vec start end)
                (lambda () (if (>= start end)
                             (eof-object)
                             (let ((next (c64vector-ref vec start)))
                              (set! start (+ start 1))
                              next))))))

(define write-c64vector
  (case-lambda
    ((vec) (write-c64vector* vec (current-output-port)))
    ((vec port) (write-c64vector* vec port))))


(define (write-c64vector* vec port)
  (display "#c64(" port)  ; c64-expansion is blind, so will expand this too
  (let ((last (- (c64vector-length vec) 1)))
    (let loop ((i 0))
      (cond
        ((= i last)
         (write (c64vector-ref vec i) port)
         (display ")" port))
        (else
          (write (c64vector-ref vec i) port)
          (display " " port)
          (loop (+ i 1)))))))

(define (c64vector< vec1 vec2)
  (let ((len1 (c64vector-length vec1))
        (len2 (c64vector-length vec2)))
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
           ((< (c64vector-ref vec1 i) (c64vector-ref vec2 i))
            #t)
           ((> (c64vector-ref vec1 i) (c64vector-ref vec2 i))
            #f)
           (else
             (loop (+ i 1)))))))))

(define (c64vector-hash vec)
  (let ((len (min 256 (c64vector-length vec))))
    (let loop ((i 0) (r 0))
      (if (= i len)
        (abs (floor (real-part (inexact->exact r))))
        (loop (+ i 1) (+ r (c64vector-ref vec i)))))))

(define c64vector-comparator
  (make-comparator c64vector? c64vector= c64vector< c64vector-hash))

)
