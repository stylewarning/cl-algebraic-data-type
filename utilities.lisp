;;;; utilities.lisp
;;;;
;;;; Copyright (c) 2013-2019 Robert Smith

(in-package #:cl-algebraic-data-type)

(global-vars:define-global-var *constructors* (make-hash-table :test 'eq))

(defstruct (algebraic-data-type (:constructor nil)
                                (:copier      nil)
                                (:predicate   nil))
  "Abstract type for all algebraic data types, primarily used to identify such types."
  ;; no slots
  )

(defun algebraic-data-type-p (type)
  "Is TYPE a known algebraic data type?"
  ;; XXX: Can we always rely on this? Sometimes the second value is
  ;; NIL, for example in CCL, but it's not always NIL.
  (values (subtypep type 'algebraic-data-type)))

(defun algebraic-data-value-p (value)
  "Is the value VALUE that of some algebraic data type?"
  (typep value 'algebraic-data-type))

(defun get-constructors (adt)
  "Get the constructors and their arity for the adt ADT. Two values will be returned:

    1. If the ADT exists, then a list of pairs

           (CONSTRUCTOR-SYMBOL ARITY).

       If the ARITY is zero, then the CONSTRUCTOR-SYMBOL is a value as opposed to a function.

    2. T if the ADT exists, NIL otherwise. This mimics the behavior of GETHASH.
"
  (multiple-value-bind (ctors exists?) (gethash adt *constructors*)
    (if (and (algebraic-data-type-p adt)
             exists?)
        (values (mapcar #'copy-list ctors) t)
        (values nil nil))))

(defun set-constructors (adt constructors)
  (setf (gethash adt *constructors*)
        constructors))

(defun wild? (s)
  (and (symbolp s)
       (string= "_" s)))

(defun ensure-list (x)
  (if (listp x)
      x
      (list x)))

(defun ensure-car (x)
  (if (consp x)
      (car x)
      x))

(defun internal (s)
  (alexandria:format-symbol ':cl-algebraic-data-type "%~A" s))

(defun unwrap-singletons (list)
  (mapcar (lambda (x)
            (if (and (listp x)
                     (= 1 (length x)))
                (first x)
                x))
          list))

(defun gen-names (n)
  (loop :for i :below n
        :collect (internal i)))

(defun field (name n)
  (alexandria:format-symbol (symbol-package name) "~A%~D" name n))

(defmacro define-constant (name value documentation)
  `(alexandria:define-constant ,name ,value
     :test 'equalp :documentation ,documentation))

(defun unsplice (x)
  (and x (list x)))

(defun property-list-p (x &rest keys)
  ;; ensure that KEYS exist
  (and (listp x)
       (evenp (length x))
       (loop :for (key val) :on x :by #'cddr
             ;; Property lists can actually have any symbols, but for
             ;; our use, they'll be required to be keywords.
             :always (keywordp key)
             :do (setf keys (remove key keys)))
       (null keys)))
