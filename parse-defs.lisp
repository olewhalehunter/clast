;;;; -*- Mode: Lisp -*-

;;;; parse-defs.lisp --
;;;; Parsing of "definition" constructs.

;;;; See the file COPYING for copyright and license information.

;;;; Portions of code lifted from IT.BESE.ARNESI and CL-WALKER.


(in-package "CLAST")


;;;;===========================================================================
;;;; Prologue.

;;;----------------------------------------------------------------------------
;;; defvar

(defmethod parse-form ((op (eql 'defvar)) form
                       &rest keys
                       &key
                       enclosing-form ; An EVAL-WHEN or some LET.
                       macroexpand
                       environment
                       &allow-other-keys)
  "Parsing of DEFVAR forms.

The return values include the augmented environment.
"
  (declare (ignore macroexpand))

  (destructuring-bind (defvar-kwd var-name
                                  &optional init-value doc-string)
      form
    (declare (ignore defvar-kwd))
    (values
     (make-instance 'defvar-form
                    :name var-name
                    :value (apply #'parse init-value keys)
                    :doc-string doc-string
                    
                    :top enclosing-form
                    :source form)
     (augment-environment environment
                          :variable (list var-name)
                          :declare (list `(special ,var-name))
                          )))
  )


;;;----------------------------------------------------------------------------
;;; defparameter

(defmethod parse-form ((op (eql 'defparameter)) form
                       &rest keys
                       &key
                       enclosing-form ; An EVAL-WHEN or some LET.
                       macroexpand
                       environment
                       &allow-other-keys)
  "Parsing of DEFPARAMETER forms.

The return values include the augmented environment.
"

  (declare (ignore macroexpand))

  (destructuring-bind (defpar-kwd var-name init-value
                                  &optional doc-string)
      form
    (declare (ignore defpar-kwd))
    (values
     (make-instance 'defparameter-form
                    :name var-name
                    :value (apply #'parse init-value keys)
                    :doc-string doc-string
                    
                    :top enclosing-form
                    :source form)
     (augment-environment environment
                          :variable (list var-name)
                          :declare (list `(special ,var-name))
                          )))
  )


;;;----------------------------------------------------------------------------
;;; defun

(defmethod parse-form ((op (eql 'defun)) form
                       &rest keys
                       &key
                       enclosing-form
                       environment
                       macroexpand
                       &allow-other-keys)
  (declare (ignore macroexpand))
  (destructuring-bind (defun-kwd f-name ll &body f-body)
      form
    (declare (ignore defun-kwd))
    (let* ((parsed-ll (parse-ll :ordinary ll))
           (ll-vars (ll-vars parsed-ll))
           (new-env
            (augment-environment environment
                                 :function (list f-name)
                                 ))
           (f-body-env
            (augment-environment new-env
                                 :variable ll-vars
                                 ))
           )
      (values
       (make-instance 'defun-form
                      :name f-name
                      :lambda-list parsed-ll
                      :top enclosing-form
                      :source form
                      :body-env f-body-env
                      :progn (apply #'parse `(block ,f-name
                                               ,@f-body)
                                    :environment f-body-env
                                    keys)
                      )
       new-env))))


;;;----------------------------------------------------------------------------
;;; defmacro

(defmethod parse-form ((op (eql 'defmacro)) form
                       &rest keys
                       &key
                       enclosing-form
                       environment
                       macroexpand
                       &allow-other-keys)
  (declare (ignore macroexpand))
  (destructuring-bind (defmacro-kwd m-name ll &body m-body)
      form
    (declare (ignore defmacro-kwd))
    (let* ((parsed-ll (parse-ll :macro ll))
           (ll-vars (ll-vars parsed-ll))
	   (m-def
	    (enclose (parse-macro m-name ll m-body environment)
		     environment))
           (new-env
	    (augment-environment environment
				 :macro (list (list m-name m-def))))
           (m-body-env
            (augment-environment new-env
                                 :variable ll-vars
                                 ))
           )
      (values
       (make-instance 'defmacro-form
                      :name m-name
                      :lambda-list parsed-ll
                      :top enclosing-form
                      :source form
                      :body-env m-body-env
                      :progn (apply #'parse `(block ,m-name
                                               ,@m-body)
                                    :environment m-body-env
                                    keys)
                      )
       new-env))))


;;;----------------------------------------------------------------------------
;;; defgeneric

(defmethod parse-form ((op (eql 'defgeneric)) form
                       &rest keys
                       &key
                       enclosing-form
                       environment
                       macroexpand
                       &allow-other-keys)
  (destructuring-bind (defgeneric-kwd gf-name ll &body opts-and-meths)
      form
    (declare (ignore defgeneric-kwd))
    (let* ((parsed-ll (parse-ll :generic-function ll))
           ;; (ll-vars (ll-vars parsed-ll))
           (gf-env
            (augment-environment environment
                                 :function (list gf-name)
                                 ))
           (options (remove :method opts-and-meths
                            :test #'eq
                            :key #'first))
           (methods (remove :method opts-and-meths
                            :test (complement #'eq)
                            :key #'first))
           (parsed-meths
            (mapcar (lambda (m)
                      (apply #'parse-form 'defmethod
			     (cons 'defmethod
				   (cons gf-name (rest m)))
                             :enclosing-form form
                             :environment gf-env
                             :macroexpand macroexpand
                             keys))
                    methods))
           )
      (values
       (make-instance 'defgeneric-form
                      :name gf-name
                      :lambda-list parsed-ll
                      :options options ; Unparsed.
                      :methods parsed-meths

                      :top enclosing-form
                      :source form
                      )
       gf-env))))


;;;----------------------------------------------------------------------------
;;; defmethod

(defmethod parse-form ((op (eql 'defmethod)) form
                       &rest keys
                       &key
                       enclosing-form
                       environment
                       macroexpand
                       &allow-other-keys)
  (declare (ignore macroexpand))

  (destructuring-bind (defmethod-kwd gf-name &rest method-spec)
      form
    (declare (ignore defmethod-kwd))
    (let* ((quals (loop for q in method-spec
                        when (listp q) do (loop-finish)
                        collect q ; Assuming only symbols as qualifiers
                        ))
           (ll-body (member-if #'listp method-spec))
           (ll (first ll-body))
           (meth-body (rest ll-body))

           (parsed-ll (parse-ll :specialized ll))
           (gf-env
            (if (function-information gf-name environment) ; The GF may already be there.
                environment
                (augment-environment environment
                                     :function (list gf-name)
                                     )))
           (m-body-env
            (augment-environment gf-env
                                 :variable (ll-vars parsed-ll)))
           )
      (values
       (make-instance 'defmethod-form
                      :name gf-name
                      :lambda-list parsed-ll
                      :qualifiers quals
                      :body-env m-body-env
                      :progn (apply #'parse `(block ,gf-name
                                               ,@meth-body)
                                    :environment m-body-env
                                    keys)

                      :top enclosing-form
                      :source form
                      )
       gf-env))))


;;;----------------------------------------------------------------------------
;;; define-compiler-macro-form
;;; Similar, with one difference to the DEFMACRO method.

(defmethod parse-form ((op (eql 'define-compiler-macro)) form
                       &rest keys
                       &key
                       enclosing-form
                       environment
                       macroexpand
                       &allow-other-keys)
  (declare (ignore macroexpand))
  (destructuring-bind (dcm-kwd m-name ll &body m-body)
      form
    (declare (ignore dcm-kwd))
    (let* ((parsed-ll (parse-ll :macro ll))
           (ll-vars (ll-vars parsed-ll))
           (new-env
            (if (function-information m-name environment)
                environment
		(let ((m-def
		       (enclose (parse-macro m-name ll m-body environment)
				environment)))
		  (augment-environment environment
				       :macro (list (list m-name m-def))
				       ))))
           (m-body-env
            (augment-environment new-env
                                 :variable ll-vars
                                 ))
           )
      (values
       (make-instance 'define-compiler-macro-form
                      :name m-name
                      :lambda-list parsed-ll
                      :top enclosing-form
                      :source form
                      :body-env m-body-env
                      :progn (apply #'parse `(block ,m-name
                                               ,@m-body)
                                    :environment m-body-env
                                    keys)
                      )
       new-env))))


;;;----------------------------------------------------------------------------
;;; define-modify-macro

(defmethod parse-form ((op (eql 'define-modify-macro)) form
                       &rest keys
                       &key
                       enclosing-form
                       environment
                       macroexpand
                       &allow-other-keys)
  (declare (ignore macroexpand))
  (destructuring-bind (dmm-kwd dmm-name ll fun &optional docstring)
      form
    (declare (ignore dmm-kwd))
    (let* ((parsed-ll (parse-ll :define-modify-macro ll))
           (ll-vars (ll-vars parsed-ll))
	   (ddm-def
	    (enclose (parse-macro m-name ll m-body environment)
		     environment))
           (new-env
            (augment-environment environment
                                 :macro (list (list dmm-name ddm-def))
                                 ))
           (m-body-env
            (augment-environment new-env
                                 :variable ll-vars
                                 ))
           )
      (declare (ignore m-body-env))
      (values
       (make-instance 'define-modifier-macro-form
                      :name dmm-name
                      :lambda-list parsed-ll
                      :function fun
                      :documentation docstring

                      :top enclosing-form
                      :source form
                      )
       new-env
       ))))

;;;; end of file -- parse-def.lisp
