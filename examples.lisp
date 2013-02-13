;;;; defdata-examples.lisp
;;;;
;;;; Requires defdata.lisp.
;;;;
;;;; This errors with COMPILE-FILE on SBCL.

(in-package #:cl-algebraic-data-type)

;;; Maybe

(defdata maybe
  (just t)
  nothing)

(defun maybe-or-else (m else)
  (match maybe m
    ((just x) x)
    (nothing else)))

;;; Either

(defdata either
  (left t)
  (right t))


;;; Point

(defdata (point :mutable)
  (rect float float))

(defvar *origin* (rect 0.0 0.0))

(defun mirror-point! (pt)
  (with-data (rect x y) pt
    (set-data pt (rect y x))))
