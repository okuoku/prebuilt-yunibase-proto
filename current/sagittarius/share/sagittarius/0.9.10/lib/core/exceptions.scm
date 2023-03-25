;; -*- scheme -*-
(library (core exceptions)
    (export with-exception-handler
	    guard
	    raise
	    raise-continuable
	    else =>)
    (import (core)
	    (core errors)
	    (sagittarius))

  (define (with-exception-handler handler thunk)
    (let* ((old (current-exception-handlers))
	   (new (lambda (c)
		  (let ((save (current-exception-handlers)))
		   (dynamic-wind
		       (lambda () (current-exception-handlers old))
		       (lambda () (handler c))
		       (lambda () (current-exception-handlers save)))))))
      (dynamic-wind
	  (lambda () (current-exception-handlers (cons new old)))
	  thunk
	  (lambda () (current-exception-handlers old)))))
  
  (define-syntax guard
    (lambda (x)
      (syntax-case x (else)
	((_ (var clause ... (else e1 e2 ...)) b1 b2 ...)
	 #'((call/cc
	     (lambda (guard-k)
	       (lambda ()
		 (with-exception-handler
		  (lambda (condition)
		    (guard-k
		     (lambda ()
		       (let ((var condition))
			 (cond clause ... 
			       (else e1 e2 ...))))))
		  (lambda () b1 b2 ...)))))))
	((_ (var clause ...) b1 b2 ...)
	 #'((call/cc
	     (lambda (guard-k)
	       (lambda ()
		 (with-exception-handler
		  (lambda (condition)
		    ((call/cc
		      (lambda (handler-k)
			(guard-k
			 (lambda ()
			   (let ((var condition))
			     (cond clause ...
				   (else 
				    (handler-k 
				     (lambda () 
				       (raise-continuable condition))))))))))))
		  (lambda () b1 b2 ...))))))))))

)
