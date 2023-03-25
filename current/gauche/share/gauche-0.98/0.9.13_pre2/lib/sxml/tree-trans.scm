;;;
;;; sxml.tree-trans - SXML utility
;;;
;;;   This file is mechanically translated for Gauche from
;;;   Oleg Kiselyov's SXML-tree-trans.scm, v 1.5.
;;;   Public domain.
;;;

(define-module sxml.tree-trans
  (use text.parse)
  (use sxml.adaptor)
  (export SRV:send-reply
          post-order
          pre-post-order
          foldts
          replace-range))
(select-module sxml.tree-trans)

;;; Generated from "SXML-tree-trans.scm"
(define (SRV:send-reply . fragments) (let loop ((fragments fragments) (result #f)) (cond ((null? fragments) result) ((not (car fragments)) (loop (cdr fragments) result)) ((null? (car fragments)) (loop (cdr fragments) result)) ((eq? #t (car fragments)) (loop (cdr fragments) #t)) ((pair? (car fragments)) (loop (cdr fragments) (loop (car fragments) result))) ((procedure? (car fragments)) ((car fragments)) (loop (cdr fragments) #t)) (else (display (car fragments)) (loop (cdr fragments) #t)))))
(define (pre-post-order tree bindings) (let* ((default-binding (assq '*default* bindings)) (text-binding (or (assq '*text* bindings) default-binding)) (text-handler (and text-binding (if (procedure? (cdr text-binding)) (cdr text-binding) (cddr text-binding))))) (let loop ((tree tree)) (cond ((null? tree) '()) ((not (pair? tree)) (let ((trigger '*text*)) (if text-handler (text-handler trigger tree) (error "Unknown binding for " trigger " and no default")))) ((not (symbol? (car tree))) (map loop tree)) (else (let* ((trigger (car tree)) (binding (or (assq trigger bindings) default-binding))) (cond ((not binding) (error "Unknown binding for " trigger " and no default")) ((not (pair? (cdr binding))) (apply (cdr binding) trigger (map loop (cdr tree)))) ((eq? '*preorder* (cadr binding)) (apply (cddr binding) tree)) ((eq? '*macro* (cadr binding)) (loop (apply (cddr binding) tree))) (else (apply (cddr binding) trigger (pre-post-order (cdr tree) (append (cadr binding) bindings)))))))))))
(define post-order pre-post-order)
(define (foldts fdown fup fhere seed tree) (cond ((null? tree) seed) ((not (pair? tree)) (fhere seed tree)) (else (let loop ((kid-seed (fdown seed tree)) (kids (cdr tree))) (if (null? kids) (fup seed kid-seed tree) (loop (foldts fdown fup fhere kid-seed (car kids)) (cdr kids)))))))
(define (replace-range beg-pred end-pred forest) (define (loop forest keep? new-forest) (if (null? forest) (values (reverse new-forest) keep?) (let ((node (car forest))) (if keep? (cond ((beg-pred node) => (lambda (repl-branches) (loop (cdr forest) #f (append (reverse repl-branches) new-forest)))) ((not (pair? node)) (loop (cdr forest) keep? (cons node new-forest))) (else (let*-values (((node?) (symbol? (car node))) ((new-kids keep?) (loop (if node? (cdr node) node) #t '()))) (loop (cdr forest) keep? (cons (if node? (cons (car node) new-kids) new-kids) new-forest))))) (cond ((end-pred node) => (lambda (repl-branches) (loop (append repl-branches (cdr forest)) #t new-forest))) ((not (pair? node)) (loop (cdr forest) keep? new-forest)) (else (let*-values (((node?) (symbol? (car node))) ((new-kids keep?) (loop (if node? (cdr node) node) #f '()))) (loop (cdr forest) keep? (if (or keep? (pair? new-kids)) (cons (if node? (cons (car node) new-kids) new-kids) new-forest) new-forest))))))))) (let*-values (((new-forest keep?) (loop forest #t '()))) new-forest))

;; Local variables:
;; mode: scheme
;; end:
