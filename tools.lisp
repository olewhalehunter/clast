;;;; -*- Mode: Lisp -*-

;;;; tools.lisp --
;;;; A few tools defined on the infrastructure just defined.
;;;; Some inspwction and debugging tools are kept in here.
;;;;
;;;; See file COPYING in main folder for licensing and copyright information.

(in-package "CLAST")

(defun print-element-ast (element
                          &optional
                          (out *standard-output*)
                          (level 0))
  "Prints the AST tree rooted at ELEMENT."
  (let ((element-string-rep
         (typecase element
           (form (as-string element))
           (t_lambda-list (string-downcase (type-of element)))
           (null "NIL")
           (list "list-of")
           (symbol (format nil "symbol ~A" element))
           (t (format nil "unhandled ~A" element))))
        )
    (format out "~VT ~A~%" (* level 4) element-string-rep)
    (dolist (sf (clast-element-subforms element) t)
      (print-element-ast sf out (1+ level)))
    ))


;;;; end of file -- tools.lisp --
