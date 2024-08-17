;;;; defdata-examples.lisp


(in-package #:cl-algebraic-data-type)

;;; Maybe

(defdata maybe
  (just t)
  nothing)

(defun maybe-or-else (m else)
  (match maybe m
    ((just x) x)
    (nothing else)))

(defun bind-maybe (f m)
  (match maybe m
    ((just x) (funcall f x))
    (nothing nothing)))

(defun inc-maybe (x)
  (just (1+ x)))

(defun maybe->string (x)
  (just (write-to-string x)))

(string-equal "2" (do-notation bind-maybe
                      ((sum (inc-maybe 1))
                       (str (maybe->string sum)))
                    str))

(equal nothing (do-notation bind-maybe
                   ((sum nothing)
                    (str (maybe->string sum)))
                 (print str)))


;;; Either

(defdata either
  (left t)
  (right t))

(defun bind-either (f m)
  (match either m
    ((right x) (funcall f x))
    ((left b) (left b))))

(defun inc-either (x)
  (right (1+ x)))

(defun either->string (x)
  (right (write-to-string x)))

(string-equal "2" (do-notation bind-either
                      ((sum (inc-either 1))
                       (str (either->string sum)))
                    str))

(equalp (left 1) (do-notation bind-either
                     ((sum (left 1))
                      (str (either->string sum)))
                   (print str)))

;;; Point

(defdata (point :mutable t)
  (rect float float))

(defvar *origin* (rect 0.0 0.0))

(defun mirror-point! (pt)
  (with-data (rect x y) pt
    (set-data pt (rect y x))))
