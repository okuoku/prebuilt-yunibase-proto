;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; text/xml/dom/nodes.scm - DOM nodes
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

;; reference
;;  - https://dom.spec.whatwg.org/
;; mainly section 4
;; we don't implement shadow tree for now
#!nounbound
(library (text xml dom nodes)
    (export node-list? node-list-length node-list:item make-node-list
	    list->node-list node-list-sub-list node-list-append
	    node-list-for-each node-list->list

	    +element-node+ +attribute-node+ +text-node+
	    +cdata-section-node+ +entity-reference-node+
	    +entity-node+ +processing-instruction-node+
	    +comment-node+ +document-node+
	    +document-type-node+ +document-fragment-node+ +notation-node+

	    node? node-node-type node-node-name node-base-uri
	    node-connected? node-owner-document
	    node-parent-node node-parent-element
	    node-child-nodes node-first-child
	    node-last-child node-previous-sibling node-next-sibling
	    node-node-value node-text-content
	    node:has-child-nodes?
	    node:get-root-node node:normalize! node:clone-node
	    node:equal-node? node:same-node? node:compare-document-position
	    node:contains? node:lookup-prefix node:lookup-namespace-uri
	    node:default-namespace node:insert-before! node:append-child!
	    node:replace-child! node:remove-child!

	    document-type? document-type-name document-type-public-id
	    document-type-system-id

	    entity? entity-public-id entity-system-id
	    entity-entity-value ;; non dom
	    entity-notation-name entity-input-encoding
	    entity-xml-encoding entity-xml-version

	    namespace-aware namespace-aware?
	    namespace-aware-namespace-uri namespace-aware-local-name
	    namespace-aware-prefix

	    element? element-namespace-uri element-prefix
	    element-local-name element-tag-name element-id
	    element-class-name element-class-list
	    ;; element-slot
	    element-attributes
	    ;; element-shadow-root ;; not supported yet
	    element:has-attributes? element:get-attribute-names
	    element:get-attribute element:get-attribute-ns
	    element:set-attribute! element:set-attribute-ns!
	    element:remove-attribute! element:remove-attribute-ns!
	    element:has-attribute? element:has-attribute-ns?
	    element:get-attribute-node element:get-attribute-node-ns
	    element:set-attribute-node! element:set-attribute-node-ns!
	    element:remove-attribute-node!
	    ;; element:attach-shadow! ;; not supported yet
	    element:closest element:matches?
	    element:get-elements-by-tag-name element:get-elements-by-tag-name-ns
	    element:get-elements-by-class-name
	    element:namespace-nodes ;; non dom

	    attr? attr-namespace-uri attr-prefix attr-local-name
	    attr-name attr-value attr-owner-element attr-specified?
	    attr-value-set!

	    named-node-map? named-node-map-length
	    named-node-map:item named-node-map:get-named-item
	    named-node-map:get-named-item-ns
	    named-node-map:set-named-item! named-node-map:set-named-item-ns!
	    named-node-map:remove-named-item!
	    named-node-map:remove-named-item-ns!
	    named-node-map->node-list
	    named-node-map:fold
	    named-node-map:contains?

	    entity-reference?
	    character-data? text? cdata-section? comment?
	    processing-instruction?
	    character-data-data character-data-data-set!
	    character-data-length
	    character-data:substring-data
	    character-data:append-data!
	    character-data:insert-data!
	    character-data:delete-data!
	    character-data:replace-data!
	    text-whole-text
	    
	    processing-instruction-target

	    (rename (make-document make-xml-document))
	    document? document-url document-document-uri document-origin
	    document-compat-mode document-character-set document-charset
	    document-input-encoding document-content-type
	    document-doctype document-document-element document-xml-standalone?
	    document-xml-version
	    document:get-elements-by-tag-name
	    document:get-elements-by-tag-name-ns
	    document:get-elements-by-class-name
	    document:get-element-by-id
	    document:create-element document:create-element-ns
	    document:create-document-fragment document:create-text-node
	    document:create-cdata-section document:create-comment
	    document:create-processing-instruction
	    document:create-document-type ;; non-dom
	    document:import-node document:adopt-node
	    document:create-attribute document:create-attribute-ns
	    document:create-event document:create-range
	    document:create-node-iterator document:create-tree-walker
	    document-character-set-set! document-content-type-set!
	    document-doctype-set! document-document-element-set!
	    document-xml-standalone?-set! document-xml-version-set!
	    document:create-entity-reference

	    tree-walker? tree-walker-what-to-show tree-walker-root
	    tree-walker-current-node tree-walker-filter
	    tree-walker:parent-node
	    tree-walker:first-child
	    tree-walker:last-child
	    tree-walker:next-sibling
	    tree-walker:previous-sibling
	    tree-walker:next-node
	    tree-walker:previous-node
	    +node-filter-show-all+
	    +node-filter-show-element+
	    +node-filter-show-attribute+
	    +node-filter-show-text+
	    +node-filter-show-cdata-section+
	    +node-filter-show-entity-reference+
	    +node-filter-show-entity+
	    +node-filter-show-processing-instruction+
	    +node-filter-show-comment+
	    +node-filter-show-document+
	    +node-filter-show-document-type+
	    +node-filter-show-document-fragment+
	    +node-filter-show-notation+
	    +node-filter-filter-accept+
	    +node-filter-filter-reject+
	    +node-filter-filter-skip+

	    node-iterator?
	    node-iterator-root node-iterator-reference-node
	    node-iterator-pointer-before-reference-node
	    node-iterator-what-to-show node-iterator-filter
	    node-iterator:next-node node-iterator:previous-node
	    node-iterator:detach
	    
	    ;; internal use only
	    make-document-type
	    document:create-element-qname
	    document:create-attribute-qname
	    document:create-entity/value
	    document:create-entity/public-id
	    document:create-entity/system-id
	    document:create-element-type

	    document-type-entities
	    document-type-elements
	    node-source node-source-set!
	    char-ref-text? document:create-char-ref-text
	    element-type? element-type-name element-type-spec
	    node-children
	    +element-type-node+
	    element-type-attlist element-type-attlist-append!

	    attdef? make-attdef
	    attdef-name attdef-required? attdef-implied? attdef-fixed?
	    attdef-att-type attdef-att-value
	    +attdef-node+

	    namespace? namespace-prefix namespace-uri namespace-parent
	    +namespace-node+
	    )
    (import (rnrs)
	    (sagittarius) ;; for define-constant
	    (sagittarius treemap)
	    (srfi :1 lists)
	    (srfi :13 strings)
	    (srfi :117 list-queues)
	    (text xml dom events))

;;; NodeList
(define-record-type node-list
  (fields length items)
  (protocol
   (lambda (p)
     (lambda (queue)
       (cond ((list-queue? queue)
	      (p (list-queue-length queue)
		 (list->vector (list-queue-list queue))))
	     ((vector? queue)
	      ;; we share the vector...
	      (p (vector-length queue) queue)))))))
(define (node-list:item nl index) (vector-ref (node-list-items nl) index))
(define (list->node-list l) (make-node-list (make-list-queue l)))
(define (node-list-sub-list nl start . maybe-end)
  (define end (if (not (null? maybe-end)) (car maybe-end) (node-list-length nl)))
  (make-node-list (vector-copy (node-list-items nl) start end)))
(define (node-list-append nl . nl*)
  (define queue (make-list-queue (vector->list (node-list-items nl))))
  (do ((nl* nl* (cdr nl*)))
      ((null? nl*) (make-node-list queue))
    (vector-for-each (lambda (n) (list-queue-add-back! queue n))
		     (node-list-items (car nl*)))))

(define (node-list-for-each proc vec . more)
  (if (null? more)
      (let ((len (node-list-length vec)))
	(let loop ((i 0))
	  (unless (= i len)
	    (proc (node-list:item vec i))
	    (loop (+ i 1)))))
      (let* ((vecs (cons vec more))
	     (len  (apply min (map node-list-length vecs))))
	(let loop ((i 0))
	  (unless (= i len)
	    (apply proc (map (lambda (v) (node-list:item v i)) vecs))
	    (loop (+ i 1)))))))

(define (node-list->list node-list)
  (vector->list (node-list-items node-list)))


;;; Node
(define-constant +element-node+                1)
(define-constant +attribute-node+              2)
(define-constant +text-node+                   3)
(define-constant +cdata-section-node+          4)
(define-constant +entity-reference-node+       5) ;; historical
(define-constant +entity-node+                 6) ;; historical
(define-constant +processing-instruction-node+ 7)
(define-constant +comment-node+                8)
(define-constant +document-node+               9)
(define-constant +document-type-node+          10)
(define-constant +document-fragment-node+      11)
(define-constant +notation-node+               12) ;; historical

(define-constant +element-type-node+           101) ;; non dom
(define-constant +namespace-node+              102) ;; non dom
(define-constant +attdef-node+                 103) ;; non dom

(define-constant +document-position-disconnected+ #x01)
(define-constant +document-position-preceding+    #x02)
(define-constant +document-position-following+    #x04)
(define-constant +document-position-contains+     #x08)
(define-constant +document-position-contained-by+ #x10)
(define-constant +document-position-implementation-specifix+ #x20)

(define-record-type node
  (parent <event-target>)
  (fields node-type                  ;; unsigned short
	  node-name                  ;; DOMString
	  base-uri                   ;; USVString
	  connected?                 ;; boolean
	  (mutable owner-document)   ;; Document?
	  (mutable parent-node)	     ;; Node?
	  (mutable parent-element)   ;; Element?
	  children
	  ;; TODO can we merge them?
	  (mutable node-value)	 ;; DOMString?
	  (mutable text-content) ;; DOMString?
	  ;; not dom internal use
	  (mutable source)
	  mutation-event-listeners ;; queue
	  )
  (protocol (lambda (n)
	      (lambda (node-type :key (node-name #f)
				      (base-uri #f)
				      (connected? #f)
				      (owner-document #f)
				      (parent-node #f)
				      (parent-element #f)
				      (child-nodes (list-queue))
				      (node-value #f)
				      (text-content #f))
		((n)
		 node-type
		 node-name
		 base-uri
		 connected?
		 owner-document
		 parent-node
		 parent-element
		 child-nodes
		 node-value
		 text-content
		 #f
		 (list-queue))))))
(define (node-child-nodes node)
  (make-node-list (node-children node)))
(define (node-first-child node)
  (let ((children (node-children node)))
    (and (not (list-queue-empty? children))
	 (list-queue-front children))))
(define (node-last-child node)
  (let ((children (node-children node)))
    (and (not (list-queue-empty? children))
	 (list-queue-back children))))

(define (node:has-child-nodes? node)
  (not (list-queue-empty? (node-children node))))
;; non dom
(define (node:invoke-mutation-event this type node)
  (define listeners (node-mutation-event-listeners this))
  (list-queue-for-each (lambda (listener) (listener this type node)) listeners))

;; at this moment, we do stupidly
(define (node-previous-sibling node)
  (let ((parent (node-parent-node node)))
    (and parent
	 (let loop ((previous #f)
		    (children (list-queue-list (node-children parent))))
	   (cond ((null? children) #f)
		 ((eq? (car children) node) previous)
		 (else (loop (car children) (cdr children))))))))
(define (node-next-sibling node)
  (let ((parent (node-parent-node node)))
    (and parent
	 (let loop ((children (list-queue-list (node-children parent))))
	   (cond ((memq node children) =>
		  (lambda (l)
		    (and (not (null? (cdr l))) (cadr l))))
		 (else #f))))))

(define (node:get-root-node node :optional (options #f)) )
(define (node:normalize! node))
(define (node:clone-node node :optional (deep #f)))
(define (node:equal-node? node1 node2) #f)
(define (node:same-node? node1 node2) (eq? node1 node2))
(define (node:compare-document-position node other) 0)
(define (node:contains? node other))
(define (node:lookup-prefix node namespace))
(define (node:lookup-namespace-uri node prefix))
(define (node:default-namespace node namespace))
;; mutations
(define (find-index lst e)
  (let loop ((lst lst) (i 0))
    (cond ((null? lst) #f)
	  ((eq? (car lst) e) i)
	  (else (loop (cdr lst) (+ i 1))))))

(define (node:insert-before! node node0 child) 
  (node-parent-node-set! child node)
  (let ((children (node-children node)))
    (cond ((find-index (list-queue-list children) node0) =>
	   (lambda (index)
	     (if (zero? index) 
		 (list-queue-add-front! children child)
		 ;; TODO do the destructive way
		 (let-values (((head tail) 
			       (split-at (list-queue-list children) index)))
		   (list-queue-set-list! children `(,@head ,child ,@tail))))))
	  (else (list-queue-add-back! (node-children node) child)))
    (node:invoke-mutation-event node 'insert child)))

(define (node:append-child! node child)
  (node-parent-node-set! child node)
  (list-queue-add-back! (node-children node) child)
  (node:invoke-mutation-event node 'insert child))
(define (node:replace-child! node node0 child)
  (node-parent-node-set! child node)
  (list-queue-map! 
   (lambda (n) 
     (if (eq? n node0)
	 (begin 
	   (node:invoke-mutation-event node 'remove node0)
	   child)
	 n))
   (node-children node))
  (node:invoke-mutation-event node 'insert child))

(define (node:remove-child! node child)
  (define children (node-children node))
  (cond ((find-index (list-queue-list children) child) =>
	 (lambda (index)
	   (cond ((zero? index) (list-queue-remove-front! children))
		 ((= index (- (list-queue-length children)))
		  (list-queue-remove-back! children))
		 ;; TODO do the destructive way
		 (else
		  (let-values (((head tail) 
				(split-at (list-queue-list children) index)))
		    (list-queue-set-list! children `(,@head ,@(cdr tail))))))
	   (node:invoke-mutation-event node 'remove child)))))

;; tree-walker
(define-record-type tree-walker
  (fields root
	  what-to-show
	  filter
	  (mutable current-node))
  (protocol (lambda (p)
	      (lambda (root what-to-show filter)
		(p root what-to-show filter root)))))
(define (tree-walker:parent-node tw)
  (let ((node (tree-walker-current-node tw))
	(root   (tree-walker-root tw)))
    (and node (eq? root node)
	 (let ((parent (node-parent-node node))
	       (filter (tree-walker-filter tw)))
	   (and (accepted? (filter parent))
		parent)))))
(define (tree-walker:traverse-children tw type)
  (define filter (tree-walker-filter tw))
  (define root (tree-walker-root tw))
  (define current-node (tree-walker-current-node tw))
  (define what-to-show (tree-walker-what-to-show tw))
  (define (get-child node)
    (if (eq? type 'first)
	(node-first-child node)
	(node-last-child node)))
  (define (get-sibling node)
    (if (eq? type 'first)
	(node-next-sibling node)
	(node-previous-sibling node)))
  (define (get-parent node)
    (let ((parent (node-parent-node node)))
      (and (not (eq? parent root))
	   (not (eq? parent current-node))
	   parent)))
  (let loop ((node (get-child current-node)))
    (and node
	 (let ((v (filter what-to-show node)))
	   (cond ((accepted? v)
		  (tree-walker-current-node-set! tw node)
		  node)
		 ((skipped? v) (loop (get-child node)))
		 ((get-sibling node) => loop)
		 ((get-parent node) => loop)
		 (else #f))))))

(define (tree-walker:first-child tw) (tree-walker:traverse-children tw 'first))
(define (tree-walker:last-child tw) (tree-walker:traverse-children tw 'last))

(define (tree-walker:traverse-sibling tw type)
  (define filter (tree-walker-filter tw))
  (define root (tree-walker-root tw))
  (define current-node (tree-walker-current-node tw))
  (define what-to-show (tree-walker-what-to-show tw))
  (define (get-sibling node)
    (if (eq? type 'next)
	(node-next-sibling node)
	(node-previous-sibling node)))
  (define (get-child node)
    (if (eq? type 'next)
	(node-first-child node)
	(node-last-child node)))
  (and (not (eq? current-node root))
       (let loop ((node current-node))
	 (let* ((new-node (get-sibling current-node))
		(v (filter what-to-show new-node)))
	   (if new-node
	       (cond ((accepted? v)
		      (tree-walker-current-node-set! tw new-node)
		      new-node)
		     ;; I think this is weird but this is how it's specified...
		     ((rejected? v) (loop (get-child new-node)))
		     ((skipped? v) (loop (get-sibling new-node)))
		     (else #f))
	       (if node
		   ;; I think this is weird...
		   (let ((parent (node-parent-node node)))
		     (and parent (not (eq? parent root))
			  (let ((v (filter what-to-show parent)))
			    (and (rejected? v)
				 (loop parent)))))))))))

(define (tree-walker:next-sibling tw) (tree-walker:traverse-sibling tw 'next))
(define (tree-walker:previous-sibling tw)
  (tree-walker:traverse-sibling tw 'previous))

(define (tree-walker:previous-node tw)
  (define filter (tree-walker-filter tw))
  (define root (tree-walker-root tw))
  (define current-node (tree-walker-current-node tw))
  (define what-to-show (tree-walker-what-to-show tw))
  (define (find-child node result) ;; 2.3
    (if (or (rejected? result) (not (node:has-child-nodes? node)))
	(values node result)
	(let ((child (node-last-child node)))
	  (find-child child (filter what-to-show child)))))
  (define (find-sibling sibling) ;; 2.2
    (or (and sibling
	     (let-values (((node result)
			   (find-child sibling ;; <- node
				       (filter what-to-show sibling))))
	       (if (accepted? result)
		   (values node #t) ;; 2.4
		   (find-sibling (node-previous-sibling node)))))
	(values sibling #f)))
  (define (return node)
    (tree-walker-current-node-set! tw node)
    node)
  (let loop ((node current-node)) ;; 1
    (and node (not (eq? node root))
	 (let-values (((node found?)
		       (find-sibling (node-previous-sibling node))))
	   (cond (found? (return node))
		 ((or (not node) (eq? node root) #f))
		 (else
		  (let ((node (node-parent-node node)))
		    (cond ((accepted? (filter what-to-show node))
			   (return node))
			  (else (loop node))))))))))

(define (tree-walker:next-node tw)
  (define filter (tree-walker-filter tw))
  (define root (tree-walker-root tw))
  (define current-node (tree-walker-current-node tw))
  (define what-to-show (tree-walker-what-to-show tw))
  (define (find-child node result)
    (cond ((rejected? result) (values node result #f))
	  ((node-first-child node) =>
	   (lambda (child)
	     (let ((result (filter what-to-show child)))
	       (if (accepted? result)
		   (values child result #t)
		   (find-child child result)))))
	  (else (values node result #f))))
  (define (find-sibling temporary)
    (and (not (eq? temporary root))
	 (let ((sibling (node-next-sibling temporary)))
	   (or sibling
	       (find-sibling (node-parent-node temporary))))))
  (let loop ((node current-node)
	     (result +node-filter-filter-accept+))
    (let-values (((node result found?) (find-child node result)))
      (cond (found?
	     (tree-walker-current-node-set! tw node)
	     node)
	    (else
	     (let ((node (find-sibling node)))
	       (and node
		    (let ((result (filter what-to-show node)))
		      (cond ((accepted? result)
			     (tree-walker-current-node-set! tw node)
			     node)
			    (else (loop node result)))))))))))

(define-record-type node-iterator
  (fields root
	  (mutable reference-node)
	  (mutable pointer-before-reference-node)
	  what-to-show
	  filter)
  (protocol (lambda (p)
	      (lambda (root what-to-show filter)
		(p root root #t what-to-show filter)))))

(define (node-iterator:traverse ni next?)
  (define root (node-iterator-root ni))
  (define filter (node-iterator-filter ni))
  (define what-to-show (node-iterator-what-to-show ni))

  (define (next-node before-node node)
    (define (find-parent-sibling parent)
      (and parent (not (eq? parent root))
	   (or (node-next-sibling parent)
	       (find-parent-sibling (node-parent-node parent)))))
    (cond ((not node) (values root before-node))
	  (before-node (values node #f))
	  (else
	   (values 
	    (cond ((node:has-child-nodes? node) (node-first-child node))
		  ((eq? node root) #f)
		  (else (let ((sib (node-next-sibling node)))
			  (or sib
			      (find-parent-sibling (node-parent-node node))))))
	    before-node))))
  (define (previous-node before-node node)
    (define (find-child-node node)
      (if (node:has-child-nodes? node)
	  (find-child-node (node-last-child node))
	  node))
    (if before-node
	(values 
	 (and node (not (eq? node root))
	      (let ((sib (node-previous-sibling node)))
		(or (and  sib (find-child-node sib))
		    (node-parent-node node))))
	 before-node)
	(values node #t)))
  
  (define (return node before-node)
    (node-iterator-reference-node-set! ni node)
    (node-iterator-pointer-before-reference-node-set! ni before-node)
    node)
  (let loop ((node (node-iterator-reference-node ni))
	     (before-node (node-iterator-pointer-before-reference-node ni)))
    (let-values (((node before-node) (if next?
					 (next-node before-node node)
					 (previous-node before-node node))))
      (and node
	   (if (accepted? (filter what-to-show node))
	       (return node (not next?))
	       (loop node before-node))))))

(define (node-iterator:next-node ni) (node-iterator:traverse ni #t))
(define (node-iterator:previous-node ni) (node-iterator:traverse ni #f))
;; noop
(define (node-iterator:detach ni))

(define (accepted? v) (eqv? v +node-filter-filter-accept+))
(define (rejected? v) (eqv? v +node-filter-filter-reject+))
(define (skipped? v)  (eqv? v +node-filter-filter-skip+))

(define-constant +node-filter-filter-accept+ 1)
(define-constant +node-filter-filter-reject+ 2)
(define-constant +node-filter-filter-skip+   3)
(define node-filter-values
  `(,+node-filter-filter-accept+
    ,+node-filter-filter-reject+
    ,+node-filter-filter-skip+))

(define-constant +node-filter-show-all+                    #xFFFFFFFF)
(define-constant +node-filter-show-element+                #x01)
(define-constant +node-filter-show-attribute+              #x02)
(define-constant +node-filter-show-text+                   #x04)
(define-constant +node-filter-show-cdata-section+          #x08)
(define-constant +node-filter-show-entity-reference+       #x10)
(define-constant +node-filter-show-entity+                 #x20)
(define-constant +node-filter-show-processing-instruction+ #x40)
(define-constant +node-filter-show-comment+                #x80)
(define-constant +node-filter-show-document+               #x100)
(define-constant +node-filter-show-document-type+          #x200)
(define-constant +node-filter-show-document-fragment+      #x400)
(define-constant +node-filter-show-notation+               #x800)


(define-record-type entity
  (parent node)
  (fields (mutable public-id)
	  (mutable system-id)
	  (mutable notation-name)
	  (mutable entity-value)   ;; for inline entity
	  (mutable input-encoding) ;; internal subset (the same as document)
	  (mutable xml-encoding)   ;; external file encoding (or #f)
	  (mutable xml-version)    ;; external file version (or #f)
	  )
  (protocol (lambda (n)
	      (lambda (document name)
		((n +entity-node+ :owner-document document :node-name name)
		 #f ;; public id
		 #f ;; system id
		 #f ;; notation name
		 #f
		 (document-input-encoding document) ;; input-encoding
		 (document-charset document)	    ;; probably not right
		 (document-xml-version document))))))

(define-record-type element-type
  (parent node)
  (fields spec
	  att-list)
  (protocol (lambda (n)
	      (lambda (name spec)
		((n +element-type-node+ :node-name name) spec (list-queue))))))
(define element-type-name node-node-name)
(define (element-type-attlist et)
  (make-node-list (element-type-att-list et)))
(define (element-type-attlist-append! et at)
  (unless (attdef? at)
    (assertion-violation 'element-type-attlist-append! "attdef required" at))
  (list-queue-add-back! (element-type-att-list et) at))

(define-record-type attdef
  (parent node)
  (fields att-type  ;; CDATA etc.
	  decl-type ;; #REQUIRED, #IMPLIED or #FIXED
	  	    ;; (required, implied, fixed or #f)
	  att-value ;; default value if there
	  )
  (protocol (lambda (n)
	      (lambda (name att-type decl-type att-value)
		((n +attdef-node+ :node-name name)
		 att-type decl-type att-value)))))
(define attdef-name node-node-name)
(define (attdef-required? attdef) (eq? 'required (attdef-decl-type attdef)))
(define (attdef-implied? attdef)  (eq? 'implied (attdef-decl-type attdef)))
(define (attdef-fixed? attdef)    (eq? 'fixed (attdef-decl-type attdef)))

;;; Non DOM Namespace (needed for XPath Data Model)
(define-record-type namespace
  (parent node)
  (fields prefix
	  uri)
  (protocol (lambda (n)
	      (lambda (prefix uri element)
		((n +namespace-node+ :node-name prefix :parent-node element)
		 prefix uri)))))
(define namespace-parent node-parent-node)

(define-record-type namespace-aware
  (parent node)
  (fields namespace-uri	;; DOMString?
	  prefix	;; DOMString?
	  local-name)	;; DOMString?
  (protocol (lambda (p)
	      (lambda (node-type node-name namespace-uri prefix local-name . opt)
		((apply p node-type :node-name node-name opt)
		 namespace-uri prefix local-name)))))

;;; Element
(define-record-type element
  (parent namespace-aware)
  (fields (mutable class-list)	;; DOMTokenList
	  attributes ;; NamedNodeMap (hashtable)
	  shadow-root	;; ShadowRoot? (not supported)
	  )
  (protocol
   (lambda (n)
     (define (->tagname prefix localname)
       (if (or (not prefix) (zero? (string-length prefix)))
	   localname
	   (string-append prefix ":" localname)))
     (define (attr-compare aa ab)
       (define (compare-string a b)
	 (cond ((string<? a b) -1)
	       ((string>? a b) 1)
	       (else 0)))
       (unless (eq? (attr-owner-element aa) (attr-owner-element ab))
	 (assertion-violation 'element:named-node-map
			      "invalid element is set to the attribute"
			      (attr-name aa) (attr-name ab)))
       ;; order:
       ;;  1 xmlns
       ;;  2 alphabetical
       (cond ((and (string=? (attr-namespace-uri aa) (attr-namespace-uri ab))
		   (string=? (attr-name aa) (attr-name ab))
		   0))
	     ((and (string-prefix? "xmlns" (attr-name aa))
		   (string-prefix? "xmlns" (attr-name ab)))
	      (compare-string (attr-name aa) (attr-name ab)))
	     ((string-prefix? "xmlns" (attr-name aa)) -1) 
	     ((string-prefix? "xmlns" (attr-name ab)) 1)
	     (else
	      ;; the primary key is uri, not prefix
	      ;; See Note of https://www.w3.org/TR/xml-c14n/#Example-SETags
	      (let ((r (compare-string (attr-namespace-uri aa)
				       (attr-namespace-uri ab))))
		(if (zero? r)
		    (compare-string (attr-local-name aa) (attr-local-name ab))
		    r)))))
     
     (lambda (namespace-uri prefix local-name)
       (let ((e ((n +element-node+
		    (->tagname prefix local-name)
		    namespace-uri prefix local-name)
		 '()
		 (make-named-node-map attr-compare)
		 #f ;; shadow-root later
		 )))
	 (list-queue-add-back! (node-mutation-event-listeners e)
			       element-namespace-normalizer)
	 e)))))
(define (normalize-namespace element child)
  (define (remove-xmlns! child)
    (define (->xmlns child)
      (let ((prefix (element-prefix child)))
	(if (or (not prefix) (zero? (string-length prefix)))
	    "xmlns"
	    (string-append "xmlns:" prefix))))
    (element:remove-attribute! child (->xmlns child)))
  (and (equal? (element-namespace-uri element) (element-namespace-uri child))
       (equal? (element-prefix element) (element-prefix child))
       (remove-xmlns! child)))

(define element-namespace-uri namespace-aware-namespace-uri)
(define element-prefix        namespace-aware-prefix)
(define element-local-name    namespace-aware-local-name)

(define (element-namespace-normalizer element event target)
  (when (element? target)
    (case event
      ((insert)
       (when (node? target) (node-parent-element-set! target element))
       (normalize-namespace element target))
      ((remove)
       (node-parent-node-set! target #f)
       (node-parent-element-set! target #f)))))

(define (element-tag-name element) (node-node-name element))
(define (element-id element)
  (cond ((element:get-attribute element "id"))
	(else #f)))
(define (element-class-name element)
  (cond ((element:get-attribute element "class"))
	(else #f)))
(define (element:has-attributes? element)
  (not (zero? (named-node-map-length (element-attributes element)))))
(define (element:get-attribute-names element)
  (named-node-map:fold (element-attributes element) '()
		       (lambda (key-attr value-attr r)
			 (cons (attr-name key-attr) r))))

(define (element:get-attribute element qualified-name)
  (cond ((element:get-attribute-node element qualified-name) => attr-value)
	(else #f)))
(define (element:get-attribute-ns element namespace local-name)
  (cond ((element:get-attribute-node-ns element namespace local-name)
	 => attr-value)
	(else #f)))
(define (element:set-attribute! element qualified-name value)
  (define doc (node-owner-document element))
  (unless (string? value)
    (assertion-violation 'element:set-attribute! "XML attribute must be string"
			 value))
  (let ((attr (document:create-attribute doc qualified-name)))
    (attr-value-set! attr value)
    (element:set-attribute-node! element attr)))

(define (element:set-attribute-ns! element namespace qualified-name value)
  (define doc (node-owner-document element))
  (unless (string? value)
    (assertion-violation 'element:set-attribute-ns!
			 "XML attribute must be string" value))
  (let-values (((prefix local-name) (split-qualified-name qualified-name)))
    (let ((attr (document:create-attribute-ns doc namespace qualified-name))
	  (xmlns (make-xmlns-attr doc prefix namespace)))
      (attr-value-set! attr value)
      (element:set-attribute-node-ns! element xmlns)
      (element:set-attribute-node-ns! element attr))))
(define (element:remove-attribute! element qualified-name)
  (cond ((element:get-attribute-node element qualified-name) =>
	 (lambda (attr) (element:remove-attribute-node! element attr)))))
(define (element:remove-attribute-ns! element namespace local-name)
  (cond ((element:get-attribute-node-ns element namespace local-name) =>
	 (lambda (attr) (element:remove-attribute-node! element attr)))))
(define (element:has-attribute? element qualified-name)
  ;; lazy
  (and (element:get-attribute-node element qualified-name) #t))
(define (element:has-attribute-ns? element namespace local-name)
  ;; lazy
  (and (element:get-attribute-node-ns element namespace local-name) #t))

(define (element:get-attribute-node element qualified-name)
  (cond ((named-node-map:get-named-item (element-attributes element)
					qualified-name))
	(else #f)))
(define (element:get-attribute-node-ns element namespace local-name)
  (cond ((named-node-map:get-named-item-ns (element-attributes element)
					   namespace local-name))
	(else #f)))
(define (element:set-attribute-node! element attr)
  ;; first owner incase of duplicated attribute (e.g. xmlns:bla)
  (attr-owner-element-set! attr element)
  (named-node-map:set-named-item! (element-attributes element) attr)
  ;; qualified name?
  (when (equal? "class" (attr-qualified-name attr))
    (element-class-list-set! element (string-tokenize (attr-value attr)))))
(define (element:set-attribute-node-ns! element attr)
  (element:set-attribute-node! element attr))

(define (element:remove-attribute-node! element attr)
  (named-node-map:remove-item! (element-attributes element) attr))

(define (element:attach-shadow! element init)
  (assertion-violation 'element:attach-shadow! "not supported"))

(define (element:closest element selector) #f)
(define (element:matches? element selector) #f)

;; TODO maybe move to somewhere else?
(define (split-qualified-name qualified-name)
  (cond ((string-index qualified-name #\:) =>
	 (lambda (index)
	   ;; On Sagittarius it doesn't mean anything but
	   ;; may mean in the future so use substring/shared here
	   (values (substring/shared qualified-name 0 index)
		   (substring/shared qualified-name (+ index 1)))))
	(else (values #f qualified-name))))

(define (node-iterator->node-list tw)
  (let ((queue (list-queue)))
    (do ((n (node-iterator:next-node tw) (node-iterator:next-node tw)))
	((not n) (make-node-list queue))
      (list-queue-add-back! queue n))))

(define (element:get-elements-by-tag-name element qualified-name)
  (let-values (((prefix local-name) (split-qualified-name qualified-name)))
    (define (local-name-filter node)
      (if (string=? local-name (element-local-name node))
	  +node-filter-filter-accept+
	  +node-filter-filter-skip+))
    (define (qualified-name-filter node)
      (if (and (equal? prefix (element-prefix node))
	       (string=? local-name (element-local-name node)))
	  +node-filter-filter-accept+
	  +node-filter-filter-skip+))
    (let ((ni (document:create-node-iterator (node-owner-document element)
					     element
					     +node-filter-show-element+
					     (if prefix
						 qualified-name-filter
						 local-name-filter))))
      (node-iterator->node-list ni))))

(define (element:get-elements-by-tag-name-ns element namespace local-name)
  (define (filter node)
    (if (and (equal? namespace (element-namespace-uri node))
	     (string=? local-name (element-local-name node)))
	+node-filter-filter-accept+
	+node-filter-filter-skip+))
  (let ((ni (document:create-node-iterator (node-owner-document element)
					   element
					   +node-filter-show-element+
					   filter)))
    (node-iterator->node-list ni)))

(define (element:get-elements-by-class-name element class-name)
  (define (filter node)
    (if (member class-name (element-class-list node))
	+node-filter-filter-accept+
	+node-filter-filter-skip+))
  (let ((ni (document:create-node-iterator (node-owner-document element)
					   element
					   +node-filter-show-element+
					   filter)))
    (node-iterator->node-list ni)))

;; we need to return all [in-scope namespaces]. This means we need to
;; get all xmlns attributes plus parent elements' xmlns
;; 
;; ref: http://courses.ischool.berkeley.edu/i290-14/s05/lecture-3/slide16.xhtml
(define (element:namespace-nodes e)
  (define ns-set (list-queue))
  (define seen (make-hashtable string-hash string=?))
  (define (add-namespace ns-set ns)
    (unless (hashtable-contains? seen (namespace-prefix ns))
      (list-queue-add-back! ns-set ns)
      (hashtable-set! seen (namespace-prefix ns) ns)))
  ;; we don't keep namespace node separately, in case this element is
  ;; appended to the other node or so. so we compute each time
  (define (do-collect e ns-set)
    (let ((attrs (list-sort string<
			    (filter (lambda (n) (string-prefix? "xmlns" n))
				    (element:get-attribute-names e)))))
      (for-each (lambda (a)
		  (let ((prefix (if (= (string-length a) 5)
				    "" ;; xmlns
				    (substring a 6 (string-length a)))))
		    (add-namespace ns-set
				   (make-namespace prefix
						   (element:get-attribute e a)
						   e)))) attrs)))
      
  (define (collect-namespace n ns-set)
    (cond ((document? n)
	   ;; add xml and xmlns, implicit ones
	   (let ((xml (make-namespace "xml"
				      "http://www.w3.org/XML/1998/namespace" n))
		 (xmlns (make-namespace "xmlns"
					"http://www.w3.org/2000/xmlns/" n)))
	     (add-namespace ns-set xml)
	     (add-namespace ns-set xmlns)))
	  ((element? n)
	   (do-collect n ns-set)
	   (collect-namespace (node-parent-node n) ns-set))))
  (collect-namespace e ns-set)
  (make-node-list ns-set))

;;; Attr
(define-record-type attr
  (parent namespace-aware)
  (fields (mutable owner-element) ;; Element
	  specified?	;; boolean (useless always returns true)
	  )
  (protocol (lambda (n)
	      (define (->qualified-name prefix local-name)
		(cond ((zero? (string-length prefix)) local-name)
		      (else (string-append prefix ":" local-name))))
	      (lambda (namespace-uri prefix local-name)
		((n +attribute-node+
		    (->qualified-name prefix local-name)
		    namespace-uri prefix local-name
		    :node-value ""
		    :text-content "") #f #t)))))
(define attr-name node-node-name)
(define attr-value node-node-value)
(define (attr-value-set! attr value)
  (node-node-value-set! attr value)
  (node-text-content-set! attr value))
(define attr-qualified-name node-node-name)
(define attr-namespace-uri namespace-aware-namespace-uri)
(define attr-prefix        namespace-aware-prefix)
(define attr-local-name    namespace-aware-local-name)

;;; NamedNodeMap
;; Internally, it's a treemap using as a set.
(define-record-type named-node-map
  (fields values)
  (protocol (lambda (p)
	      (lambda (compare)
		(p (make-rb-treemap compare))))))
(define (named-node-map-length map) (treemap-size (named-node-map-values map)))
(define (named-node-map:item map index)
  (treemap-find/index (lambda (i k) (= i index)) (named-node-map-values map)))

(define (named-node-map:get-named-item map qualified-name)
  (treemap-find (lambda (attr)
		  (string=? (node-node-name attr) qualified-name))
		(named-node-map-values map)))

(define (named-node-map:get-named-item-ns map namespace local-name)
  (treemap-find (lambda (v)
		  (if (namespace-aware? v)
		      (and (equal? namespace (namespace-aware-namespace-uri v))
			   (equal? local-name (namespace-aware-local-name v)))
		      ;; well, for now
		      (string=? (node-node-name v)
				(string-append namespace ":" local-name))))
		(named-node-map-values map)))

(define (named-node-map:set-named-item! map item)
  (treemap-set! (named-node-map-values map) item item))

(define (named-node-map:set-named-item-ns! map item)
  ;; TODO maybe check namespace-uri?
  (treemap-set! (named-node-map-values map) attr item))
(define (named-node-map:remove-named-item! map qualified-name)
  (cond ((named-node-map:get-named-item map qualified-name) =>
	 (lambda (attr) (named-node-map:remove-item! map attr)))))
(define (named-node-map:remove-named-item-ns! map namespace local-name)
  (cond ((named-node-map:get-named-item-ns map namespace local-name) =>
	 (lambda (attr) (named-node-map:remove-item! map attr)))))

;; non dom for convenience
(define (named-node-map:remove-item! map attr)
  (treemap-delete! (named-node-map-values map) attr))
(define (named-node-map:fold map seed proc)
  (treemap-fold proc (named-node-map-values map) seed))
(define (named-node-map:contains? map name)
  (treemap-contains? (named-node-map-values map) name))

(define (named-node-map->node-list map)
  (make-node-list (treemap-values (named-node-map-values map))))

;; deprecated but we support this.
(define-record-type entity-reference
  (parent node)
  (protocol (lambda (n)
	      (lambda (name)
		((n +entity-reference-node+ :node-name name))))))

;;; DocumentType
(define-record-type document-type
  (parent node)
  (fields public-id ;; DOMString
	  system-id ;; DOMString
	  entities  ;; NamedNodeMap (from DOM 3)
	  elements  ;; NamedNodeMap (need this?)
	  notations ;; NamedNodeMap (from DOM 3)
	  attlists  ;; NamedNodeMap (non DOM)
	  )
  (protocol (lambda (n)
	      (define (proc< v) -1)
	      (define (proc= v) 0)
	      (define (proc> v) 1)
	      (define (name-compare a b)
		(string-compare (node-node-name a)
				(node-node-name b) proc< proc= proc>))
	      (lambda (name public-id system-id)
		((n +document-type-node+ :node-name name)
		 public-id system-id
		 (make-named-node-map name-compare)
		 (make-named-node-map name-compare)
		 (make-named-node-map name-compare)
		 (make-named-node-map name-compare))))))

(define (document-type-name dt) (node-node-name dt))
;;; CharacterData
(define-record-type character-data
  (parent node)
  (protocol (lambda (n)
	      (lambda (type data . args)
		((apply n type :text-content data :node-value data args))))))
(define (character-data-data cd) (node-text-content cd))
(define (character-data-data-set! cd data)
  (node-text-content-set! cd data)
  (node-node-value-set! cd data))
(define (character-data-length cd) (string-length (character-data-data cd)))
(define (character-data:substring-data cd offset count)
  (when (negative? offset)
    (assertion-violation 'character-data:substring-data
			 "Offset can't be negative" offset))
  (let* ((data (character-data-data cd))
	 (len (string-length data)))
    (cond ((> offset len)
	   (assertion-violation 'character-data:substring-data
				"Offset is bigeer than data length" offset))
	  ((or (> count (- len offset)) (negative? count)) data)
	  (else (substring data offset (+ offset count))))))
(define (character-data:append-data! cd data)
  (let ((o (character-data-data cd)))
    (character-data-data-set! cd (string-append o data))))
(define (character-data:insert-data! cd offset data)
  (let* ((o (character-data-data cd))
	 (olen (string-length o))
	 (dlen (string-length data)))
    (cond ((<= offset 0)
	   (character-data-data-set! cd (string-append data o)))
	  ((>= offset olen)
	   (assertion-violation 'character-data:insert-data!
				"Offset is greater than length"
				`(offset ,offset)
				`(length ,olen)))
	  (else
	   (let* ((s (make-string (+ olen dlen)))
		  (in (open-string-input-port o)))
	     (get-string-n! in s 0 offset)
	     (get-string-n! (open-string-input-port data) s offset dlen)
	     (get-string-n! in s (+ offset dlen) (- olen offset))
	     (character-data-data-set! cd s))))))
(define (character-data:delete-data! cd offset count)
  (when (negative? offset)
    (assertion-violation 'character-data:delete-data!
			 "Offset can't be negative" offset))
  (let* ((d (character-data-data cd))
	 (len (string-length d)))
    (when (> (+ offset count) len)
      (assertion-violation 'character-data:delete-data!
			   "Too many count" count))
    (when (> offset len)
      (assertion-violation 'character-data:delete-data!
			   "Out of range" offset))
    (character-data-data-set! cd
      (string-append (substring d 0 offset)
		     (substring d (+ offset count) len)))))
(define (character-data:replace-data! cd offset count data)
    (when (negative? offset)
    (assertion-violation 'character-data:replace-data!
			 "Offset can't be negative" offset))
  (let* ((d (character-data-data cd))
	 (len (string-length d)))
    (when (> (+ offset count) len)
      (assertion-violation 'character-data:replace-data!
			   "Too many count" count))
    (when (> offset len)
      (assertion-violation 'character-data:replace-data!
			   "Out of range" offset))
    (character-data-data-set! cd
     (string-append (substring d 0 offset)
		    data
		    (substring d (+ offset count) len)))))

;; Text
(define-record-type text
  (parent character-data)
  (protocol (lambda (n)
	      (lambda (:key (data "")
			    (type +text-node+) ;; for CDATA...
			    )
		((n type data :node-name "#text"))))))
(define (text-whole-text text) (character-data-data text))

(define (text:split-text text offset))

(define-record-type cdata-section
  (parent text)
  (protocol (lambda (n)
	      (lambda (:key (data ""))
		((n :data data :type +cdata-section-node+))))))

;; non dom
(define-record-type char-ref-text
  (parent text)
  (protocol (lambda (n)
	      (lambda (data source)
		(let ((node ((n :data data))))
		  (node-source-set! node source)
		  node)))))

(define-record-type processing-instruction
  (parent character-data)
  (protocol (lambda (n)
	      (lambda (name data)
		((n +processing-instruction-node+ data :node-name name))))))
(define (processing-instruction-target pi) (node-node-name pi))

(define-record-type comment
  (parent character-data)
  (protocol (lambda (n)
	      (lambda (data)
		((n +comment-node+ data :node-name "#comment"))))))

;;; Document
(define-record-type document
  (parent node)
  (fields ;; url	  ;; USVString
	  ;; document-uri ;; USVString
	  ;; we use above the same as baseURI of the node
	  ;; origin       ;; USVString (not used)
	  compat-mode  ;; DOMString (always BackCompat)
	  (mutable character-set) ;; DOMString
	  ;; merged into character-set
	  ;; charset	;; DOMString historical
	  ;; input-encoding ;; DOMString historical
	  (mutable content-type)	 ;; DOMString (default text/xml)
	  (mutable doctype)	 ;; DocumentType?
	  (mutable document-element) ;;Element?
	  (mutable xml-standalone?)
	  (mutable xml-version)
	  )
  (protocol
   (lambda (n)
     (define (make url)
       (let ((r ((n +document-node+ :node-name "#document" :base-uri url)
		 "BackCompat" "UTF-8" "text/xml" #f #f #f "1.0")))
	 (list-queue-add-back! (node-mutation-event-listeners r)
			       document-normalizer)
	 r))
     (case-lambda
      (() (make #f))
      ((url) (make url))))))

(define (handle-insertion document target)
  (cond ((element? target)
	 (document-document-element-set! document target))
	((document-type? target)
	 (document-doctype-set! document target))))

(define (document-normalizer document event target)
  (case event
    ((insert) (handle-insertion document target))
    ((remove)
     (node-parent-node-set! target #f)
     (node-parent-element-set! target #f))))

(define document-url node-base-uri)
(define document-document-uri node-base-uri)
(define (document-origin document) #f)
(define document-charset document-character-set)
(define document-input-encoding document-character-set)

(define (document:get-elements-by-tag-name document qualified-name)
  (let ((element (document-document-element document)))
    (element:get-elements-by-tag-name element qualified-name)))
(define (document:get-elements-by-tag-name-ns document namespace local-name)
  (let ((element (document-document-element document)))
    (element:get-elements-by-tag-name-ns element namespace local-name)))
(define (document:get-elements-by-class-name document class-name)
  (let ((element (document-document-element document)))
    (element:get-elements-by-class-name element class-name)))
(define (document:get-element-by-id document id)
  (define (id-filter node)
    (if (equal? (element-id node) id)
	+node-filter-filter-accept+
	+node-filter-filter-skip+))
  ;; This implemnetation traverses entire tree, so not really
  ;; efficient as it can be O(1) if we use identifier storage.
  ;; (which puts a lot more complexity though).
  (let ((root-element (document-document-element document)))
    (if (equal? (element-id root-element) id)
	root-element
	(let ((tw (document:create-tree-walker document
		   (document-document-element document)
		   +node-filter-show-element+ id-filter)))
	  (tree-walker:next-node tw)))))

(define (document:create-element document local-name :optional (option #f))
  (let ((node (make-element #f #f local-name)))
    (node-owner-document-set! node document)
    node))

;; helper
(define (make-xmlns-attr document prefix namespace)
  (let ((xmlns (document:create-attribute-qname
		document "http://www.w3.org/2000/xmlns/"
		(if prefix "xmlns" "")
		(or prefix "xmlns"))))
    (attr-value-set! xmlns namespace)
    xmlns))
(define (document:create-element-ns document namespace qualified-name
				    :optional (option #f))
  (let-values (((prefix local-name) (split-qualified-name qualified-name)))
    (let ((node (make-element namespace (or prefix "") local-name))
	  (xmlns (make-xmlns-attr document prefix namespace)))
      (node-owner-document-set! node document)
      (element:set-attribute-node! node xmlns)
      node)))

;; TBD
(define (document:create-document-fragment document))

(define (document:create-text-node document data)
  (let ((node (make-text :data data)))
    (node-owner-document-set! node document)
    node))
(define (document:create-cdata-section document data)
  (let ((node (make-cdata-section :data data)))
    (node-owner-document-set! node document)
    node))

(define (document:create-comment document data)
  (let ((node (make-comment data)))
    (node-owner-document-set! node document)
    node))
(define (document:create-processing-instruction document target data)
  (let ((node (make-processing-instruction target data)))
    (node-owner-document-set! node document)
    node))
(define (document:create-entity-reference document name)
  (let ((node (make-entity-reference name)))
    (node-owner-document-set! node document)
    node))

;; non-dom factory
(define (document:create-document-type document name public-id system-id)
  (let ((node (make-document-type name public-id system-id)))
    (node-owner-document-set! node document)
    node))
(define (document:create-element-qname document namespace prefix local-part
					    :optional (option #f))
  (let ((node (make-element namespace prefix local-part)))
    (node-owner-document-set! node document)
    node))
(define (document:create-char-ref-text document s source)
  (let ((node (make-char-ref-text s source)))
    (node-owner-document-set! node document)
    node))
;; entity decl
(define (document:create-entity/value document name value)
  (let ((entity (make-entity document name)))
    (entity-entity-value-set! entity value)
    entity))

(define (document:create-entity/public-id document name
					  public-id system-id
					  :optional (notation #f))
  (let ((entity (make-entity document name)))
    (entity-public-id-set! entity public-id)
    (entity-system-id-set! entity system-id)
    (when notation (entity-notation-name-set! entity notation))
    entity))
(define (document:create-entity/system-id document name
					  id :optional (notation #f))
  (let ((entity (make-entity document name)))
    (entity-system-id-set! entity id)
    (when notation (entity-notation-name-set! entity notation))
    entity))
(define (document:create-element-type document name spec)
  (let ((type (make-element-type name spec)))
    (node-owner-document-set! type document)
    type))
;; TODO pe entities

(define (document:import-node document node :optional (deep #f)))
(define (document:adopt-node document node))

(define (document:create-attribute document local-name)
  (let ((node (make-attr "" "" local-name)))
    (node-owner-document-set! node document)
    node))
(define (document:create-attribute-ns document namespace qualified-name)
  (let-values (((prefix local-name) (split-qualified-name qualified-name)))
    (let ((node (make-attr namespace (or prefix "") local-name)))
      (node-owner-document-set! node document)
      node)))

(define (document:create-attribute-qname document namespace prefix local-part)
  (let ((node (make-attr (or namespace
			     ;; a bit ugly but xml is sort of default namespace
			     (and (equal? prefix "xml")
				  "http://www.w3.org/XML/1998/namespace")
			     "")
			 prefix local-part)))
    (node-owner-document-set! node document)
    node))

(define (document:create-event document interface))
(define (document:create-range document))

(define (wrap-filter filter)
  (let ((active #f))
    (lambda (what-to-show node)
      (define (check-bit node tw)
	(let ((nth (- (node-node-type node) 1)))
	  (bitwise-bit-set? what-to-show nth)))
      ;; preprocess
      (when active
	(assertion-violation 'tree-filter "filter process is active"))
      (if (check-bit node what-to-show)
	  (or (and filter
		   (dynamic-wind
		       (lambda () (set! active #t))
		       (lambda ()
			 (let ((r (filter node)))
			   (if (memv r node-filter-values)
			       r
			       ;; default accepted :)
			       +node-filter-filter-accept+)))
		       (lambda () (set! active #f))))
	      +node-filter-filter-accept+)
	  +node-filter-filter-skip+))))
(define (document:create-node-iterator document root
	   :optional (what-to-show +node-filter-show-all+)
		     (filter #f))
  (make-node-iterator root what-to-show (wrap-filter filter)))

(define (document:create-tree-walker document root
	   :optional (what-to-show +node-filter-show-all+)
		     (filter #f))
  (make-tree-walker root what-to-show (wrap-filter filter)))

)
