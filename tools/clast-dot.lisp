;;;; -*- Mode: Lisp -*-

;;;; clast-dot.lisp
;;;;
;;;; A few tools that make use of CL-DOT.
;;;;
;;;; See file COPYING in main folder for licensing and copyright information.

(in-package "CLAST")

;(require :cl-dot)

(eval-when (:load-toplevel :execute)
  (setf cl-dot:*dot-path* "/opt/local/bin/dot"
        cl-dot:*neato-path* "/opt/local/bin/neato"))


;;; CL-DOT methods to produce ASTs.

(defmethod cl-dot:graph-object-node ((graph (eql 'clast-ast)) (form form))
  (make-instance 'cl-dot:node
                 :attributes `(:label ,(format nil "~A\\n" (as-string form))
                               :shape :box)))


(defmethod cl-dot:graph-object-points-to ((graph (eql 'clast-ast)) (form form))
  ;; CL-DOT reverses the successor relation.
  (reverse (clast-element-subforms form)))


(defmethod cl-dot:graph-object-node ((graph (eql 'clast-ast)) (ll t_lambda-list))
  (make-instance 'cl-dot:node
                 :attributes '(; :label (format nil "~A \\n" (type-of ll))
                               :shape :box)))

(defmethod cl-dot:graph-object-points-to ((graph (eql 'clast-ast)) (ll t_lambda-list))
  ;; CL-DOT reverses the successor relation.
  (reverse (ll-vars ll)))


(defmethod cl-dot:graph-object-node ((graph (eql 'clast-ast)) (n null))
  (make-instance 'cl-dot:node
                 :attributes '(:label "NIL\\n"
                               :shape :box)))


(defmethod cl-dot:graph-object-node ((graph (eql 'clast-ast)) (l list))
  (make-instance 'cl-dot:node
                 :attributes '(:label "list-of\\n"
                               :shape :box)))


(defmethod cl-dot:graph-object-node ((graph (eql 'clast-ast)) (s symbol))
  (make-instance 'cl-dot:node
                 :attributes `(:label ,(format nil "~:[symbol~;keyword~] ~A\\n"
                                               (keywordp s)
                                               s)
                               :shape :box)))


(defmethod cl-dot:graph-object-node ((graph (eql 'clast-ast)) (s t))
  (make-instance 'cl-dot:node
                 :attributes `(:label ,(format nil "unhandled ~A \\n" s)
                               :color :red
                               :shape :box)))


;;; CL-DOT methods to produce ASTs.

(defmethod cl-dot:graph-object-node ((graph (eql 'clast-class-hierarchy)) (c class))
  (make-instance 'cl-dot:node
                 :attributes `(:label ,(format nil "~A" (class-name c))
                               :shape :box)))

(defmethod cl-dot:graph-object-points-to ((graph (eql 'clast-class-hierarchy)) (c class))
  (reverse (harlequin-common-lisp:class-direct-subclasses c)))



;;;; end of file -- clast-dot.lisp
