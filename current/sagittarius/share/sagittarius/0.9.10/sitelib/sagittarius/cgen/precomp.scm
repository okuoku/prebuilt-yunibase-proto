;;; -*- mode: scheme; coding: utf-8; -*-
;;;
;;; sagittarius/cgen/precomp.scm - Scheme->C translator
;;;  
;;;   Copyright (c) 2010-2013  Takashi Kato  <ktakashi@ymail.com>
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

;; the precomp library is basically different implementation of
;; boot/tools/code2c2.scm. In near future, we want to remove the
;; vm.scm to generate compiled C code.

;; 
#!read-macro=sagittarius/regex
(library (sagittarius cgen precomp)
    (export cgen-precompile cgen-precompile-file
	    (rename (default-name-generator cgen-default-name-generator)
		    (+replace-prefix+ +cgen-replace-prefix+))
	    <cgen-precomp-unit>
	    (rename (encode-library-name cgen-encode-library-name)
		    (decode-library-name cgen-decode-library-name))
	    *cgen-show-warning*
	    ;; for backward compatibility
	    default-name-generator
	    +replace-prefix+
	    )
    (import (rnrs)
	    (rnrs eval)
	    (clos user)
	    (sagittarius)
	    (sagittarius clos)
	    (sagittarius object)
	    (sagittarius control)
	    (sagittarius regex)
	    (sagittarius vm)
	    (sagittarius vm debug)
	    (sagittarius compiler)
	    (sagittarius cgen stub)
	    (sagittarius cgen unit)
	    (sagittarius cgen literal)
	    (match)
	    (core base) ;; for print
	    (srfi :1 lists)
	    (srfi :13 strings)
	    (srfi :26 cut)
	    (srfi :39 parameters)
	    (util file)
	    (util list))

  (define *cgen-show-warning* (make-parameter #t))
  ;; internal parameter
  (define *cgen-macro-emit-phase* (make-parameter #f))
  (define *cgen-cyclic-objects* (make-parameter #f))

  ;; if library name starts this then the library will be replaced
  ;; to current library.
  ;; mostly for hygine macro so make sure the compiling library
  ;; can refer it
  (define-constant +replace-prefix+ "~precomp.")
  (define-constant replace-pattern #/~precomp\./)

  (define-class <cgen-precomp-unit> (<cgen-stub-unit>)
    ((library :init-keyword :library)
     (toplevel :init-keyword :toplevel :init-form (gensym))))

  ;; for future though
  (define (read-with-source in) (read in :source-info? #t))

  (define (default-name-generator filename library-name)
    (let1 safe-name (library-name->string library-name)
      (values (or (and filename
		       (let1 base (path-basename (path-sans-extension filename))
			 (string-append base ".c")))
		  (string-append safe-name ".c"))
	      safe-name)))

  (define (cgen-precompile-file file . options)
    (apply cgen-precompile (call-with-input-file file read-with-source)
	   :in-file file
	   options))

  (define (cgen-precompile form
			   :key (name-generator default-name-generator) 
			   (in-file #f)
			   (predef-syms '())
			   (compiler compile)
			   (unit-class <cgen-precomp-unit>)
			   (need-macro? #f)
			   :rest retry)
    (define (get-port in-file)
      (and in-file
	   (open-file-input-port in-file (file-options no-fail)
				 (buffer-mode block) (native-transcoder))))
    (define (library-keyword? l) (memq l '(library define-library)))
    (unless (pair? form) (error 'cgen-precompile "form must be a list" form))
    (match form
      (((? library-keyword? l) name rest ...)
       (let-values (((out-file initialiser) (name-generator in-file name)))
	 (let1 safe-name (encode-library-name name)
	   (parameterize ((cgen-current-unit
			   (get-unit unit-class in-file initialiser
				     out-file predef-syms))
			  ;; to handle include properly
			  (*current-loading-port* (get-port in-file)))
	     ;; should be handled in get-unit but i'm lazy...
	     (set! (~ (cgen-current-unit) 'library) safe-name)
	     (do-it safe-name `(,l ,safe-name ,@rest) compiler
		    need-macro?)))))
      (_ (error 'cgen-precompile "invalid form" form))))

    
  (define (do-it safe-name library-form compiler need-macro?)
    (emit-toplevel-executor safe-name
     (compile-form library-form compiler)
     need-macro?)
    (cgen-emit-c (cgen-current-unit)))

  (define (emit-macro lib)
    (define (literalise m)
      (let1 ml (cgen-literal m)
	(cgen-init (format "    Sg_InsertBinding(SG_LIBRARY(~a), ~a, ~a);~%"
			   (cgen-cexpr (cgen-literal lib))
			   (cgen-cexpr (cgen-literal (macro-name m)))
			   (cgen-cexpr ml)))))
    (define (collect-macro lib)
      (define (gloc-macro? g)
	(and-let* ((m (gloc-ref g))
		   ( (macro? m) ))
	  m))
      (let1 table (library-table lib)
	(filter-map gloc-macro? (hashtable-values-list table))))
    (let1 macros (collect-macro lib)
      (unless (null? macros)
	(let* ((core-macro (gensym))
	       (gloc (gensym))
	       (unit (cgen-current-unit))
	       (prologue (~ unit 'init-prologue)))
	  (set! (~ unit 'init-prologue)
		(string-append
		 prologue
		 (format "  SgObject ~a = Sg_FindLibrary(SG_INTERN(~s), FALSE);~%"
			 core-macro "(core macro)")
		 (format "  SgObject ~a = Sg_FindBinding(~a, SG_INTERN(~s), SG_UNDEF);~%"
			 gloc core-macro "macro-transform")
		 (format "  SgObject macro_transform = SG_GLOC_GET(SG_GLOC(~a));~%"
			 gloc))))
	(parameterize ((*cgen-macro-emit-phase* #t))
	  (for-each literalise macros)))))
  
  (define (emit-toplevel-executor name topcb need-macro?)
    (define unit (cgen-current-unit))
    (define (strip-expand imported)
      (define (strip-it import)
	(define (check import)
	  (cond ((assq 'for (cdr import)) =>
		 (lambda (slot)
		   (let ((phases (delete-duplicates (cdr slot))))
		     (cond ((and (null? (cdr phases))
				 (member (car phases) '(ran (meta 0))))
			    imported)
			   (else #f)))))
		(else import)))
	(if (null? (cdr import))
	    import
	    (check import)))
      (filter-map strip-it imported))
    (cgen-body (format "static SgCodeBuilder *~a = "
		       (slot-ref unit 'toplevel)))
    (cgen-body 
     (format "   SG_CODE_BUILDER(~a);" (cgen-cexpr topcb)))
    (let1 library (find-library name #f) ;; get the library
      ;; emit imports
      (for-each (lambda (i)
		  (let1 t (library-name (car i))
		    (cond ((null? (cdr i))
			   (cgen-init
			    (format "  Sg_ImportLibrary(~a, ~a);~%"
				    (cgen-cexpr (cgen-literal library))
				    (cgen-cexpr (cgen-literal t)))))
			  (else
			   (cgen-init
			    (format "  Sg_ImportLibraryFullSpec(~a, ~a, ~a);~%"
				    (cgen-cexpr (cgen-literal library))
				    (cgen-cexpr (cgen-literal t))
				    (cgen-cexpr (cgen-literal (cdr i)))))))))
		(strip-expand (reverse (library-imported library))))
      (cgen-init (format "  Sg_LibraryExportedSet(~a, ~a);~%"
			 (cgen-cexpr (cgen-literal library))
			 (cgen-cexpr (cgen-literal (library-exported library)))))
      (cgen-init (format "  Sg_VM()->currentLibrary = ~a;" 
			 (cgen-cexpr (cgen-literal library))))
      ;; Macro creation and execution need to be here after importing
      ;; all dependency.
      (when need-macro? (emit-macro library))
      (cgen-init (format "  Sg_VMExecute(SG_OBJ(~a));"
			 (slot-ref unit 'toplevel)))
      (cgen-init "  Sg_VM()->currentLibrary = save;")))

  ;; use '.' joined library form...
  ;; it's legal on Sagittarius for historical reason
  ;; but we can use it anyway...
  (define (encode-library-name name)
    (if (and (pair? name) (null? (cdr name)))
	(string->symbol (format "~s." (car name)))
	(string->symbol
	 (string-join (map (lambda (s) (format "~s" s)) name) "."))))

  (define (decode-library-name name)
    (define (encoded? name)
      (string-contains (symbol->string name) "."))
    (if (encoded? name)
	(map string->symbol (string-split (symbol->string name)  #/\./))
	name))

  ;; To maximise the compiler optimisation we need to compile it
  ;; with library form
  (define (construct-safe-library name exports imports toplevels)
    `(library ,name
	 (export ,@exports)
	 (import ,@imports)
       ,@toplevels))
  
  (define (compile-form form compiler)
    (let1 cb (compiler form (environment
			     '(only (sagittarius) library define-library)))
      (cgen-literal cb)))

  (define (get-unit unit-class in-file initfun-name out.c predef-syms)
    (let* ((base (path-sans-extension (path-basename out.c)))
	   (safe-name (regex-replace-all #/[-+]/ base "_")))
      (rlet1 u 
	  (make unit-class
	    :name base :c-name-prefix safe-name
	    :preamble 
	    `(,(format "/* Generated automatically from ~a. DO NOT EDIT! */"
		       (or in-file in-file "<anonymous>")))
	    :init-prologue 
	    (format "void Sg__Init_~a() {~%  \
                       SgObject save = Sg_VM()->currentLibrary;~%"
		    initfun-name))
	(parameterize ((cgen-current-unit u))
	  (for-each cgen-define predef-syms)
	  (cgen-include "<sagittarius.h>")
	  ;; For precompiled source, we need private structures
	  ;; I don't expect users to do precompile...
	  (cgen-include "<sagittarius/private.h>")))))
  
  (define-cgen-literal <cgen-scheme-code> <code-builder>
    ((code-name :init-keyword :code-name)
     (code-vector-c-name :init-keyword :code-vector-c-name)
     (literals  :init-keyword :literals))
    (make (value)
      (define (fixup cb-name)
	(if (pair? cb-name)
	    (string->symbol (format "~a" cb-name))
	    cb-name))
      (let* ((cv (code-builder->vector value))
	     (lv (extract-literals cv))
	     (cvn (allocate-code-vector cv lv (code-builder-name value)))
	     (code-name (cgen-literal (fixup (code-builder-name value)))))
	(define (init-thunk) 
	  (format #t "    SG_STATIC_CODE_BUILDER( /* ~a */~%"
		  (cgen-safe-comment (code-builder-name value)))
	  (format #t "      (SgWord *)~a, ~a, ~a, ~a, ~a, ~a,"
		  cvn (if (cgen-literal-static? code-name)
			  (cgen-cexpr code-name)
			  "SG_FALSE")
		  (code-builder-argc value) (code-builder-optional value)
		  (code-builder-freec value) (code-builder-maxstack value))
	  (format #t " ~a)" (vector-length cv)))
	(let1 c-name (cgen-allocate-static-datum 'runtime 'SgCodeBuilder
						 init-thunk)
	  (make <cgen-scheme-code> :value value
		:c-name c-name
		:code-vector-c-name cvn
		:code-name code-name
		:literals lv))))
    (init (self)
      (unless (cgen-literal-static? (~ self 'code-name))
	(print "  SG_CODE_BUILDER(" (~ self 'c-name) ")->name = "
	       (cgen-cexpr (~ self 'code-name)) ";"
	       "/* " (cgen-safe-comment (code-builder-name (~ self 'value)))
	       " */"))
      (fill-code self))
    (static (self) #t))

  (define (extract-literals code)
    (let* ((len (vector-length code))
	   (lits (make-vector len #f)))
      (do ((i 0 (+ i 1)))
	  ((= i len) lits)
	(let1 insn (vector-ref code i)
	  ;; insn-info only needs one byte
	  ;; To avoid 64 bit environment problem, this is needed...
	  (cond-expand
	   (64bit (set! insn (bitwise-and insn #xFF)))
	   (else #t))
	  (let-values (((name&insn iv argc src? label?) (insn-info insn)))
	    (unless (zero? argc) ;; always 0 or 1
	      (vector-set! lits (+ i 1)
			   (cgen-literal (vector-ref code (+ i 1)))))
	    (set! i (+ i argc)))))))
  
  (define (allocate-code-vector cv lv full-name)
    (define (alloc-word initval)
      (cgen-allocate-static-datum 'runtime 'SgWord initval))
    (define (loop cv lv count first-cexpr)
      (if (= count (vector-length cv))
	  first-cexpr
	  (let1 insn (vector-ref cv count)
	    ;; Raw insn can exceed 32 bit range on 64 bit environment
	    ;; however instruction value can only be in range
	    ;; #x-7ffff - #x7ffff (20 bits) (see compiler.scm)
	    ;; So for 64 bit environment we mask it to 32 bits
	    (cond-expand
	     (64bit (set! insn (bitwise-and insn #xFFFFFFFF)))
	     (else #t))
	    (let-values (((name&insn iv argc src? label?) (insn-info insn)))
	      (let* ((insn-name (car name&insn))
		     (name-info (if first-cexpr
				    ""
				    (format "/* ~a */"
					    (cgen-safe-comment full-name))))
		     (insn-cexpr
		      (alloc-word
		       (if (> insn #x80000000)
			   (format "~a-0x~8,'0x   /* ~3d ~a */"
				   name-info (- #x100000000 insn) count
				   (cgen-safe-comment insn-name))
			   (format "~a0x~8,'0x    /* ~3d ~a */"
				   name-info insn count
				   (cgen-safe-comment insn-name)))))
		     (first-cexpr (or first-cexpr insn-cexpr)))
		(cond (label? 
		       (alloc-word (format "SG_WORD(~a)" 
					   (vector-ref cv (+ count 1))))
		       (loop cv lv (+ count 2) first-cexpr))
		      ((not (zero? argc))
		       (let ((v (vector-ref lv (+ count 1)))
			     (c (cgen-safe-comment 
				 (format/ss "~a" (vector-ref cv (+ count 1))))))
			 (alloc-word
			  (if (cgen-literal-static? v)
			      (format "SG_WORD(~a) /* ~a */" (cgen-cexpr v) c)
			      (format "SG_WORD(SG_UNDEF) /* ~a */" c)))
			 (loop cv lv (+ count 2) first-cexpr)))
		      (else 
		       (loop cv lv (+ count 1) first-cexpr))))))))
    (loop cv lv 0 #f))
			     
  (define (fill-code cb)
    (let ((cvn (~ cb 'code-vector-c-name))
	  (lv  (~ cb 'literals)))
      (do ((len (vector-length lv)) (i 0 (+ i 1)))
	  ((= len i))
	(let1 lit (vector-ref lv i)
	  (when (and lit (not (cgen-literal-static? lit)))
	    (format #t "  ((SgWord*)~a)[~a] = SG_WORD(~a);~%"
		    cvn i (cgen-cexpr lit)))))))

  (define (library-name->string lib)
    (define str (regex-replace-all #/[^a-zA-Z0-9_]/ (format "~s" lib)
		   (lambda (m)
		     (let ((c (string-ref (m 0) 0)))
		       (cond ((or (char=? c #\() (char=? c #\))) " ")
			     ((char=? c #\space) "_")
			     ((char=? c #\-) "_")
			     (else (number->string (char->integer c) 16)))))))
    (string-trim-both
       (string-map (lambda (c) (if (char=? c #\space) #\_ c)) str)
       #\_))

  (define-cgen-literal <cgen-scheme-identifier> <identifier>
    ((name :init-keyword :name)
     (envs :init-keyword :envs)
     (identity :init-keyword :identity)
     (pending :init-keyword :pending)
     (library :init-keyword :library)) ;; name
    (make (value)
      (define (strip-envs envs)
	(define (strip-frame frame)
	  ;; (type . ((var . val) ...))
	  (for-each (lambda (rib)
		      (and-let* ((v (cdr rib))
				 ( (vector? v) )
				 ( (eq? (vector-ref v 0) 'lvar) ))
			(vector-set! v 2 '()))) (cdr frame)))
	(define (strip-env env)
	  (for-each strip-frame env))
	;; now we only need the unique ness of lvar, thus we can
	;; strip off init value
	;; FIXME this is too much depending on the compiler internal
	(for-each strip-env envs)
	envs)
	
      (when (and (*cgen-show-warning*) (not (*cgen-macro-emit-phase*)))
	;; FIXME this warning message is a bit oversight since 
	;;       identifiers are now compared by it's identity
	(unless (or (null? (id-envs value))
		    ;; FIXME comparing this is a bit too much depending on
		    ;;       the structure of compiler environment.
		    (and (null? (cdr (id-envs value)))
			 (equal? (car (id-envs value)) '((4)))))
	  (format (current-error-port) 
		  "*WARNING* identifier '~a' in ~a contains environment~%    \
                   assume the identifier can be resolved in the \
                   target library. ~,,,,20s~%"
		  (id-name value) (library-name (id-library value))
		  (id-envs value))))
      (let1 cname (cgen-allocate-static-datum)
	(cgen-init (format "  ~a = Sg_MakeRawIdentifier(NULL, NULL, NULL, NULL, 0);~%"
			   cname))
	(let ((libname (symbol->string (library-name (id-library value))))
	      (envs (if (*cgen-macro-emit-phase*)
			(cgen-literal (strip-envs (id-envs value)))
			'()))
	      (identity (if (*cgen-macro-emit-phase*)
			    (cgen-literal (id-identity value))
			    #f))
	      (pending (if (*cgen-macro-emit-phase*)
			   (pending-identifier? value)
			   #f)))
	  (make <cgen-scheme-identifier> :value value
		:c-name cname
		:name (cgen-literal (id-name value))
		:envs envs
		:identity identity
		:pending pending
		:library (cgen-literal 
			  (if (not (replace-pattern libname))
			      (id-library value)
			      (find-library (~ (cgen-current-unit) 'library)
					    #f)))))))
    (init (self)
      (let ((name (cgen-cexpr (~ self 'name)))
	    (value (~ self 'value))
	    (cname (~ self 'c-name))
	    (envs (~ self 'envs))
	    (identity (~ self 'identity))
	    (pending? (~ self 'pending)))
	(format #t "  SG_INIT_IDENTIFIER(~a,SG_SYMBOL(~a),~a,~a,SG_LIBRARY(~a),~a); /* ~a */~%"
		cname
		name
		(if (null? envs) 'SG_NIL (cgen-cexpr envs))
		(if identity (cgen-cexpr identity) 'SG_FALSE)
		(cgen-cexpr (~ self 'library))
		(if pending? 'TRUE 'FALSE)
		(cgen-safe-comment (id-name value)))))
    (static (self) #f))
  
  (define-cgen-literal <cgen-scheme-gloc> <gloc>
    ((name :init-keyword :name)
     (library :init-keyword :library)) ;; name
    (make (value)
      (let1 libname (symbol->string (library-name (gloc-library value)))
	(make <cgen-scheme-gloc> :value value
	      :c-name (cgen-allocate-static-datum)
	      :name (cgen-literal (gloc-name value))
	      :library (cgen-literal 
			(if (not (replace-pattern libname))
			    (gloc-library value)
			    (find-library (~ (cgen-current-unit) 'library)
					  #f))))))
    (init (self)
	  (let ((name (cgen-cexpr (~ self 'name)))
		(cname (~ self 'c-name)))
	    (format #t "  ~a = Sg_MakeGlobalIdentifier(SG_SYMBOL(~a), SG_LIBRARY(~a));~%"
		    cname name (cgen-cexpr (~ self 'library)))))
    (static (self) #f))

  (define-cgen-literal <cgen-scheme-library> <library>
    ((name :init-keyword :name))
    (make (value)
      (make <cgen-scheme-library> :value value
	    :c-name (cgen-allocate-static-datum)
	    :name (cgen-literal 
		   (string->symbol 
		    (format "~a" (decode-library-name (library-name value)))))))
    (init (self)
      (let ((name (cgen-cexpr (~ self 'name)))
	    (cname (~ self 'c-name)))
	(format #t "  ~a = Sg_FindLibrary(SG_SYMBOL(~a), TRUE);~%"
		cname name)))
    (static (self) #f))

  (define (last-pair* l)
    (let loop ((l l) (r '()))
      (cond ((null? l) (values (reverse! r) '()))
	    ((not (pair? l)) (values (reverse! r) l))
	    (else (loop (cdr l) (cons (car l) r))))))

  (define-class <shared> ()
    ((object :init-keyword :object)
     literal))
  (define-method write-object ((o <shared>) out)
    (format/ss out "#<shared ~s (~a)>"
	       (slot-ref o 'object) (slot-bound? o 'literal)))
  (define (make-shared v) (make <shared> :object v))
  (define (shared? v) (is-a? v <shared>))
  (define (get-literal v)
    (cond ((hashtable-ref (*cgen-cyclic-objects*) v))
	  ((and (not (macro? v)) (not (identifier? v))) (cgen-literal v))
	  (else
	   (let1 s (make-shared v)
	     (hashtable-set! (*cgen-cyclic-objects*) v s)
	     (let1 cl (cgen-literal v)
	       (set! (~ s 'literal) cl)
	       cl)))))
  (define (get-shared v)
    (if (and (shared? v) (slot-bound? v 'literal))
	(~ v 'literal)
	v))
  ;; FIXME ugly
  (define-method cgen-cexpr ((o <shared>)) (cgen-cexpr (~ o 'literal)))
  
  (define-cgen-literal <cgen-scheme-pair> <pair>
    ((values-list :init-keyword :values-list))
    (make (value)
      (define (make-values-list c-name value)
	(let loop ((l value) (r '()))
	  (cond ((null? l) (reverse! r))
		;; #0=(a . #0#) case
		((and (not (pair? l)) (eq? l value))
		 (cons (reverse! r) c-name))
		((not (pair? l)) (append (reverse! r) (get-literal l)))
		;; #0=(a #0# b) case
		((eq? (car l) value) (loop (cdr l) (cons c-name r)))
		(else (loop (cdr l) (cons (get-literal (car l)) r))))))
      (let1 c-name (cgen-allocate-static-datum)
	(make <cgen-scheme-pair> :value value
	      :c-name c-name
	      :values-list
	      (parameterize ((*cgen-cyclic-objects* (or (*cgen-cyclic-objects*)
							(make-eq-hashtable))))
		(make-values-list c-name value)))))
    (init (self)
      (let ((values (~ self 'values-list))
	    (cname (~ self 'c-name))
	    (h     (gensym))
	    (t     (gensym)))
	(print "  do {")
	(format #t "    /* ~a */ ~%"
		(cgen-safe-comment (format/ss "~s" (~ self'value))))
	(format #t "    SgObject ~a = SG_NIL, ~a = SG_NIL;~%" h t)
	(let-values (((l last) (last-pair* values))
		     ((v vlast) (last-pair* (~ self 'value))))
	  (define (unwrap shared)
	    (if (shared? shared)
		(~ shared 'object)
		shared))
	  (for-each (lambda (v o)
		      (let1 n (cond ((eq? v cname) h)
				    ((string? v) v)
				    (else (cgen-cexpr v)))
			(format/ss #t "    SG_APPEND1(~a, ~a, ~a); /* ~a */ ~%"
				   h t n
				   (cgen-safe-comment (format/ss "~s" o)))))
		    l v)
	  (unless (null? last)
	    (let1 n (cond ((eq? last cname) t)
			  ((string? last) last)
			  (else (cgen-cexpr last)))
	      (format #t "    SG_SET_CDR(~a, ~a); /* ~a */~%"
		      t n (cgen-safe-comment (format/ss "~s" (unwrap last)))))))
	(format #t "    ~a = ~a;~%" cname h)
	(print "  } while (0);")))
    (static (self) #f))

(define-cgen-literal <cgen-scheme-vector> <vector>
  ((values-vec :init-keyword :values-vec))
  (make (value)
    (define (make-values-vector c-name value)
      (define len (vector-length value))
      (define r (make-vector len))
      (do ((i 0 (+ i 1)))
	  ((= i len) r)
	(let1 v (vector-ref value i)
	  (if (eq? v value)
	      (vector-set! r i c-name)
	      (vector-set! r i (get-literal v))))))
    (let1 cname (cgen-allocate-static-datum)
      (make <cgen-scheme-vector> :value value
	    :c-name cname
	    :values-vec
	    (parameterize ((*cgen-cyclic-objects* (or (*cgen-cyclic-objects*)
						      (make-eq-hashtable))))
	      (make-values-vector cname value)))))
  (init (self)
	(let* ((values (~ self 'values-vec))
	       (vec    (~ self 'value))
	       (cname (~ self 'c-name))
	       (s     (vector-length values)))
	  (print "  do {")
	  (format #t "    ~a = Sg_MakeVector(~a, SG_FALSE);~%" cname s)
	  (do ((i 0 (+ i 1)))
	      ((= i s))
	    (let* ((v (vector-ref values i))
		   (n (cond ((eq? v cname) cname)
			    ((string? v) v)
			    (else (cgen-cexpr v)))))
	      (format #t "    SG_VECTOR_ELEMENT(~a, ~a) = ~a; /* ~a */~%"
		      cname i n
		      (cgen-safe-comment (format/ss "~s" (vector-ref vec i))))))
	  (print "  } while (0);")))
  (static (self) #f))

(define-cgen-literal <cgen-scheme-regex> <pattern>
  ((pattern :init-keyword :pattern))
  (make (value)
    (make <cgen-scheme-regex> :value value
	  :c-name (cgen-allocate-static-datum)
	  :pattern (cgen-literal (regex-pattern value))))
  (init (self)
	(let ((pattern (~ self 'value))
	      (lite (~ self 'pattern))
	      (cname (~ self 'c-name)))
	  (format #t "  ~a = Sg_CompileRegex(~a, ~a, FALSE); /* ~s */~%" cname
		  (cgen-cexpr lite)
		  (regex-flags pattern)
		  (regex-pattern pattern))))
  (static (self) #f))

(define-cgen-literal <cgen-scheme-char-set> <char-set>
  ((ranges :init-keyword :ranges))
  (make (value)
    (make <cgen-scheme-char-set> :value value
	  :c-name (cgen-allocate-static-datum)
	  :ranges (%char-set-ranges value)))
  (init (self)
	(let ((cs (~ self 'value))
	      (ranges (~ self 'ranges))
	      (cname (~ self 'c-name)))
	  (format #t "  ~a = Sg_MakeEmptyCharSet(); /* ~a */~%" cname cs)
	  (dolist (range ranges)
	    (format #t "  Sg_CharSetAddRange(~a, ~a, ~a);~%"
		    cname
		    (car range)
		    (cdr range)))))
  (static (self) #f))

(define-cgen-literal <cgen-scheme-macro> <macro>
  ((name :init-keyword :name)
   (env :init-keyword :env)
   (code :init-keyword :code))
  (make (value)
    (let1 cname  (cgen-allocate-static-datum)
      (cgen-init (format "  ~a = Sg_MakeMacro(NULL, NULL, NULL, NULL, NULL);~%"
			 cname))
      (let ((mn (cgen-literal (macro-name value)))
	    (me (cgen-literal (macro-env value)))
	    ;; maybe for future...
	    ;;(mt (cgen-literal (macro-transformer macro)))
	    (mc (cgen-literal (macro-compiled-code value))))
	(make <cgen-scheme-macro> :value value
	      :c-name cname
	      :name mn
	      :env me
	      :code mc))))
  (init (self)
	(let ((cname (~ self 'c-name))
	      (macro (~ self 'value))
	      (mn (~ self 'name))
	      (me (~ self 'env))
	      (mc (~ self 'code)))
	  (format #t "  SG_INIT_MACRO(~a, ~a, SG_CLOSURE(~a), Sg_VMExecute(~a), ~a, SG_CODE_BUILDER(~a)); /* ~a */~%"
		  cname
		  (cgen-cexpr mn)
		  ;; for now...
		  "macro_transform" ;;(cgen-cexpr mt)
		  (cgen-cexpr mc)
		  (cgen-cexpr me)
		  (cgen-cexpr mc)
		  (cgen-safe-comment (format "~s" (macro-name macro)))))))

)
