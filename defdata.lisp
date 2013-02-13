;;;; cl-algebraic-data-type.lisp
;;;; Copyright (c) 2012-2013 Robert Smith

(in-package #:cl-algebraic-data-type)

(defmacro defdata (adt-name &body constructors)
  
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
                                                  :type ,type))
                               field-names
                               field-types))
                          #+sbcl (declaim (sb-ext:freeze-type ,ctor-name)))))))
     #+sbcl (declaim (sb-ext:freeze-type ,adt-name))
     ;; Return the type name
     ',adt-name))

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


