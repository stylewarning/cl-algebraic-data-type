;;;; package.lisp
;;;; Copyright (c) 2012 Robert Smith

(defpackage #:cl-algebraic-data-type
  (:use #:cl)
  (:export
   #:base-adt                           ; type
   #:+adt-top+                          ; constant
   #:defadt                             ; macro
   ))

