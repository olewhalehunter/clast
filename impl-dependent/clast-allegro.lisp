;;;; -*- Mode: Lisp -*-

;;;; clast-allegro.lisp --
;;;; Code lifted from IT.BESE.ARNESI and CL-WALKER.
;;;;
;;;; Environment functions.
;;;; Either you have CLtL2 environment functions or your
;;;; implementation sucks.
;;;;
;;;; See file COPYING for copyright and license information.


(in-package "CLAST")

;;;; The Magnificent (yet neglected) 7.
;;;; CLtL2 environment manipulation manipulation functions.
;;;;
;;;; Franz redefined the order of the return values.  Hence this
;;;; implementation wrapper is needed.

(defmacro reorder-*information-values-portably (env-function-form)
  `(multiple-value-bind (binding-type
                         locative-cons
                         info
                         local-or-global-p)
       ,env-function-form
     (values binding-type
             local-or-global-p
             info
             locative-cons ; This will never be used in portable code,
                           ; but it is nice to provide it.
             )
     ))


(defun ensure-parsing-environment (&optional env)
  (sys:ensure-portable-walking-environment env))


(defmethod is-environment ((e sys::augmentable-environment)) t)


(defun get-implementation-env (env) ; This is needed for the internal API.
  env)


;;;; Morevoer, ACL needs a workaround to handle a "portable walking
;;;; environment.

(defvar *allegro-portable-parsing-env*
  ;; (sys:ensure-portable-walking-environment nil)
  (ensure-parsing-environment))


(defun variable-information (variable
                             &optional
                             (env *allegro-portable-parsing-env*))
  (reorder-*information-values-portably
   (sys:variable-information variable env t))
  )


(defun function-information (f
                             &optional
                             (env *allegro-portable-parsing-env*))
  (reorder-*information-values-portably
   (sys:function-information f env t t))
  )


(defun declaration-information (decl-name
                                &optional
                                (env *allegro-portable-parsing-env*))
  (reorder-*information-values-portably
   (sys:declaration-information decl-name env))
  )


(defun tag-information (tag-name
                        &optional
                        (env *allegro-portable-parsing-env*))
  (multiple-value-bind (tag-kwd locative decl)
      (sys:tag-information tag-name env)
    (values tag-kwd decl locative)))


(defun block-information (block-name
                          &optional
                          (env *allegro-portable-parsing-env*))
  (multiple-value-bind (block-kwd locative decl)
      (sys:block-information block-name env)
    (values block-kwd decl locative)))


(defun augment-environment (env &rest all-keys ; Just a utility variable.
                                &key
                                variable
                                symbol-macro
                                function
                                macro
                                declare
                                &allow-other-keys)
  (declare (ignore variable symbol-macro function macro declare))
  (apply #'sys:augment-environment
         (or env *allegro-portable-parsing-env*)
         all-keys)
  )


(defmacro define-declaration (decl-name lambda-list &body forms)
  `(sys:define-declaration ,decl-name ,lambda-list ,@forms)
  )


(defun parse-macro (name lambda-list body
                         &optional
                         (env *allegro-portable-parsing-env*))
  ;; (error "No implementation for CLtL2 PARSE-MACRO in Allegro CL.")
  (excl::defmacro-expander (list* name lambda-list body) env)
  )


(defun enclose (lambda-expression
                &optional
                (env *allegro-portable-parsing-env*))
  ;; (error "No implementation for CLtL2 ENCLOSE in Allegro CL.")
  (excl::make-lexical-closure lambda-expression nil env)
  )


;;;; end of file -- clast-allegro.lisp --
