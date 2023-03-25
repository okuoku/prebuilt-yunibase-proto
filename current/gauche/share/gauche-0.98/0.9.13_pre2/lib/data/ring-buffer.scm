;; generated automatically.  DO NOT EDIT
#!no-fold-case
(define-module data.ring-buffer (use gauche.sequence) (use gauche.uvector) (use gauche.record) (use scheme.vector) (use util.match) (export make-ring-buffer make-overflow-doubler ring-buffer? ring-buffer-empty? ring-buffer-full? ring-buffer-num-entries ring-buffer-capacity ring-buffer-room ring-buffer-front ring-buffer-back ring-buffer-add-front! ring-buffer-add-back! ring-buffer-remove-front! ring-buffer-remove-back! ring-buffer-ref ring-buffer-set! ring-buffer-insert-all! ring-buffer-clear! ring-buffer->xsubvectors ring-buffer->xvector ring-buffer->xvector!))
(select-module data.ring-buffer)
(dynamic-load "data--ring-buffer")
