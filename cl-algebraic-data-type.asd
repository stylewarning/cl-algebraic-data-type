;;;; cl-algebraic-data-type.asd
;;;;
;;;; Copyright (c) 2012-2018 Robert Smith

(asdf:defsystem #:cl-algebraic-data-type
  :description "A library for algebraic data types."
  :version "1.2.0"
  :author "Robert Smith <robert@stylewarning.com>"
  :license "BSD 3-clause"
  :serial t
  :components ((:static-file "LICENSE.txt")
               (:file "package")
               (:file "utilities")
               (:file "defdata")
               (:file "match")))
