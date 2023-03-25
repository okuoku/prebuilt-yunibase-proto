;; -*- scheme -*-
#!core
(library (pp)
    (export pretty-print pretty-print-to-string
	    pp
	    pretty-print/ss
	    pp/ss)
    (import (core)
	    (core base)
	    (sagittarius)
	    ;; don't want to load rnrs or syntax layer libraries
	    (sagittarius regex impl)
	    (sagittarius misc))

  (define-syntax if-let1
    (er-macro-transformer
     (lambda (f r c)
       (let ((var (cadr f)) (expr (caddr f))
	     (then (cadddr f)) (els (if (not (null? (cddddr f)))
					(list (car (cddddr f)))
					'())))
	 `(let ((,var ,expr))
	    (if ,var ,then ,@els))))))

	 

; File: "pp.scm"   (c) 1991, Marc Feeley

; 'generic-write' is a procedure that transforms a Scheme data value (or
; Scheme program expression) into its textual representation.  The interface
; to the procedure is sufficiently general to easily implement other useful
; formatting procedures such as pretty printing, output to a string and
; truncated output.
;
; Parameters:
;
;   OBJ       Scheme data value to transform.
;   DISPLAY?  Boolean, controls whether characters and strings are quoted.
;   WIDTH     Extended boolean, selects format:
;               #f = single line format
;               integer > 0 = pretty-print (value = max nb of chars per line)
;   OUTPUT    Procedure of 1 argument of string type, called repeatedly
;               with successive substrings of the textual representation.
;               This procedure can return #f to stop the transformation.
;
; The value returned by 'generic-write' is undefined.
;
; Examples:
;
;   (write obj)   = (generic-write obj #f #f display-string)
;   (display obj) = (generic-write obj #t #f display-string)
;
; where display-string = (lambda (s) (for-each write-char (string->list s)) #t)
(define (generic-write obj display? width output :optional (shared #f))
  (define (read-macro? l)
    (define (length1? l) (and (pair? l) (null? (cdr l))))
    (let ((head (car l)) (tail (cdr l)))
      (case head
	((quote quasiquote unquote unquote-splicing) (length1? tail))
	(else #f))))

  (define (read-macro-body l)
    (cadr l))

  (define (read-macro-prefix l)
    (let ((head (car l)) (tail (cdr l)))
      (case head
        ((quote)            "'")
        ((quasiquote)       "`")
        ((unquote)          ",")
        ((unquote-splicing) ",@"))))
  
  (define (out str col)
    (and col (output str) (+ col (string-length str))))

  (define (cycle? obj col)
    (and-let* (( shared )
	       (e (hashtable-ref (cdr shared) obj #f)))
      (cond ((not e) #f)
	    ((number? e) (out (format "#~d#" e) col))
	    (else
	     (let ((id (car shared)))
	       (hashtable-set! (cdr shared) obj id)
	       (set-car! shared (+ id 1))
	       (out (format "#~d=" id) col)
	       #f)))))

  (define (wr obj col)
    ;; TODO handle special case here
    (define (wr-expr expr col)
      (if (read-macro? expr)
        (wr (read-macro-body expr) (out (read-macro-prefix expr) col))
        (wr-lst expr col)))

    (define (wr-lst l col)
      (if (pair? l)
        (let loop ((l (cdr l)) (col (wr (car l) (out "(" col))))
          (and col
               (cond ((and shared (hashtable-ref (cdr shared) l #f))
		      => (lambda (e)
			   (out ")" (wr e (out " . " col)))))
		     ((pair? l) (loop (cdr l) (wr (car l) (out " " col))))
                     ((null? l) (out ")" col))
                     (else      (out ")" (wr l (out " . " col)))))))
        (out "()" col)))

    (cond ((pair? obj)
	   (if-let1 ncol (cycle? obj col)
	      ncol
	      (wr-expr obj col)))
          ((null? obj)        (wr-lst obj col))
          ((vector? obj)
	   (if-let1 ncol (cycle? obj col)
	      ncol
	      (wr-lst (vector->list obj) (out "#" col))))
          ((boolean? obj)     (out (if obj "#t" "#f") col))
          ((number? obj)      (out (number->string obj) col))
	  ;; for Sagittarius
          ;;((symbol? obj)      (out (format "~s" obj) col))
	  ;;((identifier? obj)  (out (format "~s" obj) col))
          ;;((procedure? obj)   (out "#<procedure>" col))
          ((string? obj)      (if display?
				  (out obj col)
				  (let loop ((i 0) (j 0) (col (out "\"" col)))
				    (if (and col (< j (string-length obj)))
					(let ((c (string-ref obj j)))
					  (if (or (char=? c #\\)
						  (char=? c #\"))
					      (loop j
						    (+ j 1)
						    (out "\\"
							 (out (substring obj i j)
							      col)))
					      (loop i (+ j 1) col)))
					(out "\""
					     (out (substring obj i j) col))))))
          ((char? obj)        (if display?
				  (out (make-string 1 obj) col)
				  (out (case obj
					 ((#\space)   "space")
					 ((#\newline) "newline")
					 (else        (make-string 1 obj)))
				       (out "#\\" col))))
	  ((char-set? obj)    (out (char-set->regex-string obj) col))
	  ;; Sagittarius we don't need this
          ;;((input-port? obj)  (out "#<input-port>" col))
	  ;;((output-port? obj) (out "#<output-port>" col))
          ;;((eof-object? obj)  (out "#<eof-object>" col))
	  ;; Sagittarius #<unknown> -> obj
          (else               (out (format "~s" obj) col))))
 
  (define (pp obj col)

    (define (spaces n col)
      (if (> n 0)
        (if (> n 7)
          (spaces (- n 8) (out "        " col))
          (out (substring "        " 0 n) col))
        col))

    (define (indent to col)
      (and col
           (if (< to col)
             (and (out (make-string 1 #\newline) col) (spaces to 0))
             (spaces (- to col) col))))

    ;; TODO handle shared object here as well...
    (define (pr obj col extra pp-pair)
       ; may have to split on multiple lines
      (cond ((or (pair? obj) (vector? obj))
	     (let ((result '())
		   (left (min (+ (- (- width col) extra) 1) max-expr-width)))
	       (generic-write obj display? #f
			      (lambda (str)
				(set! result (cons str result))
				(set! left (- left (string-length str)))
				(> left 0))
			      shared)
	       (if (> left 0) ; all can be printed on one line
		   (out (reverse-string-append result) col)
		   (if (pair? obj)
		       (pp-pair obj col extra)
		       (pp-list (vector->list obj)
				(out "#" col) extra pp-expr)))))
	    (else (wr obj col))))

    (define (pp-expr expr col extra)
      (if (read-macro? expr)
        (pr (read-macro-body expr)
            (out (read-macro-prefix expr) col)
            extra
            pp-expr)
        (let ((head (car expr)))
          (if (symbol? head)
            (let ((proc (style head)))
              (if proc
                (proc expr col extra)
                (if (> (string-length (symbol->string head))
                       max-call-head-width)
                  (pp-general expr col extra #f #f #f pp-expr)
                  (pp-call expr col extra pp-expr))))
            (pp-list expr col extra pp-expr)))))

    ; (head item1
    ;       item2
    ;       item3)
    (define (pp-call expr col extra pp-item)
      (let ((col* (wr (car expr) (out "(" col))))
        (and col
             (pp-down (cdr expr) col* (+ col* 1) extra pp-item))))

    ; (item1
    ;  item2
    ;  item3)
    (define (pp-list l col extra pp-item)
      (let ((col (out "(" col)))
        (pp-down l col col extra pp-item)))

    (define (pp-down l col1 col2 extra pp-item)
      (let loop ((l l) (col col1))
        (and col
             (cond ((pair? l)
                    (let ((rest (cdr l)))
                      (let ((extra (if (null? rest) (+ extra 1) 0)))
                        (loop rest
                              (pr (car l) (indent col2 col) extra pp-item)))))
                   ((null? l)
                    (out ")" col))
                   (else
                    (out ")"
                         (pr l
                             (indent col2 (out "." (indent col2 col)))
                             (+ extra 1)
                             pp-item)))))))

    (define (pp-general expr col extra named? pp-1 pp-2 pp-3)

      (define (tail1 rest col1 col2 col3)
        (if (and pp-1 (pair? rest))
	    (let* ((val1 (car rest))
		   (rest (cdr rest))
		   (extra (if (null? rest) (+ extra 1) 0)))
	      (tail2 rest col1 (pr val1 (indent col3 col2) extra pp-1) col3))
	    (tail2 rest col1 col2 col3)))

      (define (tail2 rest col1 col2 col3)
        (if (and pp-2 (pair? rest))
	    (let* ((val1 (car rest))
		   (rest (cdr rest))
		   (extra (if (null? rest) (+ extra 1) 0)))
	      (tail3 rest col1 (pr val1 (indent col3 col2) extra pp-2)))
	    (tail3 rest col1 col2)))

      (define (tail3 rest col1 col2)
        (pp-down rest col2 col1 extra pp-3))

      (let* ((head (car expr))
             (rest (cdr expr))
             (col* (wr head (out "(" col))))
        (if (and named? (pair? rest))
	    (let* ((name (car rest))
		   (rest (cdr rest))
		   (col** (wr name (out " " col*))))
	      (tail1 rest (+ col indent-general) col** (+ col** 1)))
	    (tail1 rest (+ col indent-general) col* (+ col* 1)))))

    (define (pp-expr-list l col extra)
      (pp-list l col extra pp-expr))

    (define (pp-lambda expr col extra)
      (pp-general expr col extra #f pp-expr-list #f pp-expr))

    (define (pp-if expr col extra)
      (pp-general expr col extra #f pp-expr #f pp-expr))

    (define (pp-cond expr col extra)
      (pp-call expr col extra pp-expr-list))

    (define (pp-case expr col extra)
      (pp-general expr col extra #f pp-expr #f pp-expr-list))

    (define (pp-and expr col extra)
      (pp-call expr col extra pp-expr))

    (define (pp-let expr col extra)
      (let* ((rest (cdr expr))
             (named? (and (pair? rest) (symbol? (car rest)))))
        (pp-general expr col extra named? pp-expr-list #f pp-expr)))

    (define (pp-begin expr col extra)
      (pp-general expr col extra #f #f #f pp-expr))

    (define (pp-do expr col extra)
      (pp-general expr col extra #f pp-expr-list pp-expr-list pp-expr))

    (define indent-general 2)

    (define max-call-head-width 5)

    (define max-expr-width 50)

    (define (style head)
      (case head
        ((lambda let* letrec define) pp-lambda)
        ((if set!)                   pp-if)
        ((cond)                      pp-cond)
        ((case)                      pp-case)
        ((and or)                    pp-and)
        ((let)                       pp-let)
        ((begin)                     pp-begin)
        ((do)                        pp-do)
        (else                        #f)))
    (pr obj col 0 pp-expr))

  (define (walk! obj seen)
    (define mark (undefined))
    (define (register!? obj)
      (let ((r (hashtable-ref seen obj mark)))
	(if (undefined? r)
	    (begin (hashtable-set! seen obj #f) #f)
	    (begin (hashtable-set! seen obj #t) #t))))
    (cond ((pair? obj)
	   (unless (register!? obj)
	     (when (or (pair? (car obj)) (vector? (car obj)))
	       (walk! (car obj)  seen))
	     (walk! (cdr obj) seen)))
	  ((vector? obj)
	   (unless (register!? obj)
	     (do ((len (vector-length obj))
		  (i 0 (+ i 1)))
		 ((= i len))
	       (let ((e (vector-ref obj i)))
		 (when (or (pair? e) (vector? e)) (walk! e seen))))))))
  (when (and shared (not (car shared)))
    (set-car! shared 0)
    (walk! obj (cdr shared)))
  (if width
      (out (make-string 1 #\newline) (pp obj 0))
      (wr obj 0)))

(define (reverse-string-append l)

  (define (rev-string-append l i)
    (if (pair? l)
	(let* ((str (car l))
	       (len (string-length str))
	       (result (rev-string-append (cdr l) (+ i len))))
	  (let loop ((j 0) (k (- (- (string-length result) i) len)))
	    (if (< j len)
		(begin
		  (string-set! result k (string-ref str j))
		  (loop (+ j 1) (+ k 1)))
		result)))
	(make-string i)))

  (rev-string-append l 0))

(define (object->string obj)
  (let ((result '()))
    (generic-write obj #f #f (lambda (str) (set! result (cons str result)) #t))
    (reverse-string-append result)))

(define (object->limited-string obj limit)
  (let ((result '()) (left limit))
    (generic-write obj #f #f
		   (lambda (str)
		     (let ((len (string-length str)))
		       (if (> len left)
			   (begin
			     (set! result (cons (substring str 0 left) result))
			     (set! left 0)
			     #f)
			   (begin
			     (set! result (cons str result))
			     (set! left (- left len))
			     #t)))))
    (reverse-string-append result)))

(define (pretty-print obj . opt)
  (let ((port (if (pair? opt) (car opt) (current-output-port))))
    (generic-write obj #f (output-port-width port)
		   (lambda (s) (display s port) #t))))

(define (pretty-print-to-string obj)
  (let ((result '()))
    (generic-write obj #f (output-port-width (current-output-port))
		   (lambda (str) (set! result (cons str result)) #t))
    (reverse-string-append result)))

(define pp pretty-print)

(define (pretty-print/ss obj . opt)
  (let ((port (if (pair? opt) (car opt) (current-output-port))))
    (generic-write obj #f (output-port-width port)
		   (lambda (s) (display s port) #t)
		   (cons #f (make-eq-hashtable)))))

(define pp/ss pretty-print/ss)
)
