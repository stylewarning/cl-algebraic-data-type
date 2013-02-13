;;;; utilities.lisp
;;;; Copyright (c) 2013 Robert Smith

(in-package #:cl-algebraic-data-type)

(defvar *constructors* (make-hash-table))

(defun get-constructors (adt)
  (gethash adt *constructors*))

(defun set-constructors (adt constructors)
  (setf (gethash adt *constructors*)
        constructors))

(defmacro define-constant (name value &optional doc)
  `(defconstant ,name (if (boundp ',name)
                          (symbol-value ',name)
                          ,value)
     ,@(when doc (list doc))))

(defun wild? (s)
  (and (symbolp s)
       (string= "_" (symbol-name s))))

(defun ensure-list (x)
  (if (listp x)
      x
      (list x)))

(defun ensure-car (x)
  (if (consp x)
      (car x)
      x))

(defun internal (s)
  (intern (format nil "%~A" s)))

(defun unwrap-singletons (list)
  (mapcar #'(lambda (x)
              (if (and (listp x)
                       (= 1 (length x)))
                  (first x)
                  x))
          list))

(defun gen-names (n)
  (loop :for i :below n
        :collect (make-symbol (format nil "%~D" i))))

(defun field (name n)
  (intern (format nil "~A%~D" name n)
          (symbol-package name)))
