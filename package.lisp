;;;; package.lisp
;;;; Copyright (c) 2012 Robert Smith

(defpackage #:cl-algebraic-data-type
  (:use #:cl)
  (:nicknames #:adt)
  (:export
   #:defdata
   #:set-data
   #:with-data
   #:match
   #:algebraic-data-type
   #:algebraic-data-type-p
   #:get-constructors))

