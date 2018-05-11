;;;; cl-algebraic-data-type.lisp
;;;;
;;;; Copyright (c) 2012-2018 Robert Smith

(in-package #:cl-algebraic-data-type)

(defvar *print-adt-readably* t
  "Print preceding #. when printing ADT values.")

(defmacro defdata (adt-name &body constructors)
  "Define a new ADT. ADT-name has the following grammar:

ADT-NAME := <symbol>
          | (<symbol> <options>*)

There is no difference between specifying it as a symbol or as a
singleton list. There are two possible options, specified as a
property list:

    * :MUTABLE {T, NIL} (default: NIL): Specifies whether the fields
      of the type are mutable, allowing the use of SET-DATA.

    * :INCLUDE <adt-type>: Specifies whether another defined ADT
      should be inherited.

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
     ;; whose first element is a symbol (the name of the adt)
     (symbolp (first adt-name))
     ;; and the rest is a property list
     (property-list-p (rest adt-name))))
   (adt-name)
   "ADT-NAME must either be a symbol, a singleton list with the ADT ~
      name as a symbol, or a list of two elements with the ADT name ~
      and :MUTABLE respectively. Given ~S."
   adt-name)

  (let ((adt-name (ensure-car adt-name))
        (mutable? (and (listp adt-name)
                       (getf (rest adt-name) ':mutable)
                       t))
        (include (or (and (listp adt-name)
                          (getf (rest adt-name) ':include))
                     'algebraic-data-type))
        (object (gensym "OBJECT-"))
        (stream (gensym "STREAM-"))
        (depth (gensym "DEPTH-"))
        (env (gensym "ENVIRONMENT-"))
        (documentation
          (when (stringp (car constructors))
            (pop constructors))))

    ;; Sanity check that INCLUDE is a proper algebraic data type.
    (assert (algebraic-data-type-p include)
            ()
            "When defining the algebraic data type ~S, the :INCLUDE option ~
             was specified with ~S, which is not another algebraic data ~
             type."
            adt-name
            include)

    ;; Add constructors and their arity to the database.
    (flet ((constructor-and-arity (ctor)
             (if (listp ctor)
                 (list (first ctor) (length (rest ctor)))
                 (list ctor 0))))
      (set-constructors adt-name (append (get-constructors include)
                                         (mapcar #'constructor-and-arity constructors))))

    (flet ((make-printer (name &optional (nfields 0))
             "Make a printer function for the structs."
             `(lambda (,object ,stream ,depth)
                (declare (ignore ,depth)
                         (ignorable ,object))
                ;; We don't check *PRINT-READABLY* because these can
                ;; be readably printed always, provided the arguments
                ;; can be printed readably.
                ;;
                ;; However, we do respect a convenience option
                ;; *PRINT-ADT-READABLY* in case someone finds the
                ;; printing of the #. to be annoying.
                (when *print-adt-readably*
                  (write-string "#." ,stream))
                ,(when (plusp nfields)
                   `(write-char #\( ,stream))
                (prin1 ',name ,stream)
                ,@(when (plusp nfields)
                    (loop :for i :below nfields
                          :append (list
                                   `(write-char #\Space ,stream)
                                   `(prin1 (,(field name i) ,object)
                                           ,stream))))
                ,(when (plusp nfields)
                   `(write-char #\) ,stream)))))
      ;; Define everything.
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         ;; Define the data type.
         (defstruct (,adt-name (:constructor nil)
                               (:copier nil)
                               (:predicate nil)
                               (:include ,include))
           ,@(unsplice documentation))

         ;; Define each of the field constructors.
         ,@(loop :for ctor :in (unwrap-singletons constructors)
                 :collect
                 (etypecase ctor
                   ;; Nullary constructor
                   (symbol `(progn
                              (eval-when (:compile-toplevel :load-toplevel :execute)
                                (defstruct
                                    (,ctor
                                     (:include ,adt-name)
                                     (:constructor ,(internal ctor))
                                     (:copier nil)
                                     (:predicate nil)
                                     (:print-function ,(make-printer ctor)))))
                              #+sbcl (declaim (sb-ext:freeze-type ,ctor))
                              (defmethod make-load-form ((,object ,ctor) &optional ,env)
                                (make-load-form-saving-slots ,object
                                                             :slot-names nil
                                                             :environment ,env))
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
                                          (:conc-name ,ctor-name)
                                          (:copier nil)
                                          (:predicate nil)
                                          (:print-function
                                           ,(make-printer ctor-name
                                                          (length field-names))))
                                ,@(mapcar (lambda (name type)
                                            `(,name (error "Unspecified field.")
                                                    :type ,type
                                                    ,@(if mutable?
                                                          nil
                                                          '(:read-only t))))
                                   field-names
                                   field-types))
                              #+sbcl (declaim (sb-ext:freeze-type ,ctor-name))
                              (defmethod make-load-form ((,object ,ctor-name) &optional ,env)
                                (make-load-form-saving-slots ,object
                                                             :slot-names ',field-names
                                                             :environment ,env)))))))
         ;; Return the type name
         ',adt-name))))

(defmacro set-data (obj (name &rest new-values))
  "Mutate the fields of the ADT value OBJ whose constructor is NAME
and whose updated values are NEW-VALUES based on order. If the symbol
'_' is used as a value, that field is not updated. Trailing '_' may be
omitted."
  (let ((once (gensym "ONCE")))
    `(let ((,once ,obj))
       (psetf
        ,@(loop :for i :from 0
                :for x :in new-values
                :when (not (wild? x))
                  :append (list `(,(field name i) ,once)
                                x))))))

(defmacro with-data ((name &rest vars) obj &body body)
  "Destructure the ADT value OBJ, whose constructor is NAME. VARS must
be symbol which will be bound, or they must be the symbol '_', which
means the value will not be bound."
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
