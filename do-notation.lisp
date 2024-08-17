;;;; do-notation.lisp
;;;; Copyright (c) 2013 Robert Smith

(in-package #:cl-algebraic-data-type)

(defmacro do-notation (bind-function expressions &body body)
  "Given a BIND-FUNCTION and all the bind expressions, evaluate BODY."
  `(progn
     ,@(reduce (lambda (acc do-binding)
		 (append `((,bind-function (lambda (,(car do-binding)) ,@acc)
					   ,@(cdr do-binding)))))
	       (reverse expressions)
	       :initial-value body)))
