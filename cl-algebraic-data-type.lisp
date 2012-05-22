;;;; cl-algebraic-data-type.lisp
;;;; Copyright (c) 2012 Robert Smith

(in-package #:cl-algebraic-data-type)

#+#:EXAMPLE
(progn
  (defadt liste
      knil
      (kons (kar t)
            (kdr liste)))
  
  ;; turns into something like...
  
  (defstruct (liste (:constructor nil)))
  
  (defstruct (knil (:include liste)
                   (:constructor %make-knil ())))
  
  (defconstant knil (%make-knil))
  
  (defstruct (kons (:include liste)
                   (:conc-name nil)
                   (:constructor kons (kar kdr)))
    (kar nil :type t)
    (kdr knil :type liste)))

(defmacro defadt (name &rest components)
  (check-type name symbol)
  (labels ((ctor (symb)
             (intern (concatenate 'string
                                  "%MAKE-"
                                  (symbol-name symb)))))
    `(progn
       ;; base structure; do not provide a constructor!
       (defstruct (,name (:constructor nil)))
       
       ;; component structures
       ,@(loop :for component :in components
               :append
               (if (symbolp component)
                   ;; Case of nullary constructor.
                   (list
                      ;; Generate the structure
                    `(defstruct (,component (:include ,name)
                                            (:constructor ,(ctor component)))
                       ;; Empty structure
                       )
                      
                      ;; Generate the constant
                    `(defconstant ,component (,(ctor component))))
                   
                   ;; Case of a complex constructor.
                   (list
                    (let ((component-name (first component))
                          (accessors (mapcar 'first (rest component))))
                      `(defstruct (,component-name (:include ,name)
                                                   (:conc-name nil)
                                                   (:constructor
                                                       ,component-name
                                                       ,accessors))
                         ,@accessors)))))
       
       ;; return the ADT's name, just as with DEFSTRUCT.
       ',name)))

