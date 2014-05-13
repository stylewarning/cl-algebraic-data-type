;;;; match.lisp
;;;; Copyright (c) 2013 Robert Smith

(in-package #:cl-algebraic-data-type)

;; TODO: Optimize ETYPECASE to be a jump table.
(defmacro match (adt obj &body clauses)
  "Perform pattern matching on OBJ with (adt-type) ADT.

Each clause must have the following syntax:

<var> := <symbol> | '_'
<lhs> := '_'
       | (<symbol> <var>*)
<clause> := (<lhs> <lisp code>)

The symbol '_' denotes a wildcard, as well as a fallthough.

Note that pattern matching is only shallow (patterns are one-level
deep).
"
  (assert (symbolp adt)
          (adt)
          "MATCH requires a symbol for the first argument. Given ~S."
          adt)
  
  (let ((ctors (mapcar #'car (get-constructors adt)))
        (types (mapcar (lambda (clause)
                         (ensure-car (car clause)))
                       clauses))
        (once (gensym "OBJ-")))
    
    ;; Check for match exhaustiveness.
    (unless (some #'wild? types)
      (let ((diff (set-difference ctors types)))
        (when diff
          (warn "Non-exhaustive match. Missing cases:~{ ~S~}" diff))))
    
    ;; Generate the matching code.
    `(let ((,once ,obj))
       (check-type ,once ,adt)
       (etypecase ,once
         ,@(loop :for (bindings . body) :in clauses
                 :collect (let ((type (ensure-car bindings)))
                            (if (wild? type)
                                `(t ,@body)
                                `(,type 
                                  (with-data ,(ensure-list bindings)
                                             ,once
                                    ,@body)))))))))
