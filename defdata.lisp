;;;; cl-algebraic-data-type.lisp
;;;; Copyright (c) 2012-2013 Robert Smith

(in-package #:cl-algebraic-data-type)

(defmacro defdata (adt-name &body constructors)
  "Define a new ADT. ADT-name has the following grammar:

ADT-NAME := <symbol>
          | (<symbol>)
          | (<symbol> :MUTABLE)

There is no difference between specifying it as a symbol or as a
singleton list. Specifying :MUTABLE will make DEFDATA mutable,
allowing the use of SET-DATA.

Constructors is a list of clauses with the following grammar:

<clause> := <symbol>
          | (<symbol> <type-specifier>*)

Each clause defines a constructor for the ADT. Nullary constructors
will define constants and all other constructors will define
functions."
  ;; Do some extreme sanity checking.
  (assert
   (or
    ;; Must be a symbol, or...
    (symbolp adt-name)
    (and
     ;; a list that is...
     (listp adt-name)
     ;; non-empty
     (not (null adt-name))
     (or
      ;; whose length is at 1, or
      (= 1 (length adt-name))
      ;; whose length is 2 and whose second element is :MUTABLE.
      (and (= 2 (length adt-name))
           (eql :mutable (second adt-name)))
      ;; whose first element is a symbol (the name of the adt)
      (symbolp (first adt-name)))))
   (adt-name)
   "ADT-NAME must either be a symbol, a singleton list with the ADT ~
      name as a symbol, or a list of two elements with the ADT name ~
      and :MUTABLE respectively. Given ~S."
   adt-name)
  
  (let ((adt-name (ensure-car adt-name))
        (mutable? (and (listp adt-name)
                       (member :mutable adt-name)
                       t)))
    ;; Add constructors to the database.
    (set-constructors adt-name
                      (mapcar #'ensure-car constructors))
    
    ;; Define everything.
    `(progn
       ;; Define the data type.
       (defstruct (,adt-name (:constructor nil)))
       
       ;; Define each of the field constructors.
       ,@(loop :for ctor :in (unwrap-singletons constructors)
               :collect
               (etypecase ctor
                 ;; Nullary constructor
                 (symbol `(progn
                            (defstruct (,ctor
                                        (:include ,adt-name)
                                        (:constructor ,(internal ctor))))
                            #+sbcl (declaim (sb-ext:freeze-type ,ctor))
                            (define-constant ,ctor (,(internal ctor)))
                            (fmakunbound ',(internal ctor))))
                 
                 ;; N-ary constructors
                 (list (let* ((ctor-name (first ctor))
                              (field-types (rest ctor))
                              (field-names (gen-names (length field-types))))
                         `(progn
                            (defstruct (,ctor-name
                                        (:include ,adt-name)
                                        (:constructor ,ctor-name (,@field-names))
                                        (:conc-name ,ctor-name))
                              ,@(mapcar #'(lambda (name type)
                                            `(,name (error "Unspecified field.")
                                                    :type ,type
                                                    ,@(if mutable?
                                                          nil
                                                          '(:read-only t))))
                                 field-names
                                 field-types))
                            #+sbcl (declaim (sb-ext:freeze-type ,ctor-name)))))))
       #+sbcl (declaim (sb-ext:freeze-type ,adt-name))
       ;; Return the type name
       ',adt-name)))

;; Setter
(defmacro set-data (obj (name &rest new-values))
  (let ((once (gensym "ONCE")))
    `(let ((,once ,obj))
       (psetf
        ,@(loop :for i :from 0
                :for x :in new-values
                :when (not (wild? x))
                  :append (list `(,(field name i) ,once)
                                x))))))

;; Destructuring
(defmacro with-data ((name &rest vars) obj &body body)
  (let* ((once (gensym "ONCE-"))
         (bindings (loop :for i :from 0
                         :for v :in vars
                         :when (not (wild? v))
                           :collect `(,v (,(field name i)
                                          ,once)))))
    `(let ((,once ,obj))
       (declare (ignorable ,once))
       (let (,@bindings)
         ,@body))))


