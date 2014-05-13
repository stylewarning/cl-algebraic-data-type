;;;; cl-algebraic-data-type.asd
;;;; Copyright (c) 2012-2013 Robert Smith

(asdf:defsystem #:cl-algebraic-data-type
  :serial t
  :description "A library for algebraic data types."
  :version "1.1.0"
  :author "Robert Smith <quad@symbo1ics.com>"
  :license "BSD 3-clause"
  :components ((:file "package")
               (:file "utilities")
               (:file "defdata")
               (:file "match")))

