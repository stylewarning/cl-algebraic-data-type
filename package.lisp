;;;; package.lisp
;;;;
;;;; Copyright (c) 2012-2018 Robert Smith

(defpackage #:cl-algebraic-data-type
  (:documentation "A package for defining algebraic data types.")
  (:use #:cl)
  (:nicknames #:adt)
  (:export
   #:*print-adt-readably*               ; VARIABLE
   #:defdata                            ; MACRO
   #:set-data                           ; MACRO
   #:with-data                          ; MACRO
   #:match                              ; MACRO
   #:algebraic-data-type                ; TYPE
   #:algebraic-data-type-p              ; FUNCTION (PREDICATE)
   #:algebraic-data-value-p             ; FUNCTION (PREDICATE)
   #:get-constructors                   ; FUNCTION
   ))
