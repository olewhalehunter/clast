;;;; -*- Mode: Lisp -*-

;;;; clast-sbcl.lisp --
;;;; Code lifted from IT.BESE.ARNESI and CL-WALKER.
;;;;
;;;; Environment functions.
;;;; Either you have CLtL2 environment functions or your
;;;; implementation sucks.
;;;;
;;;; See file COPYING for copyright and license information.


(in-package "CLAST")

(eval-when (:load-toplevel :compile-toplevel :execute)
  (require :sb-cltl2))

;;;; parsing-environment --
;;;; This is probably general enough to be moved "up" in 'env.lisp',
;;;; but, at the cost of having to maintain duplicated code in several
;;;; files, we prefer to keep it here, in order to keep all the code for a
;;;; given implementation together.

(defstruct (parsing-environment
            (:include env-wrapper)
            (:constructor
             %make-parsing-environment (&optional
                                        environment
                                        enclosing-env))
            )
  tags
  blocks
  enclosing-env
  )


(defmethod print-object ((pe parsing-environment) stream)
  (print-unreadable-object (pe stream :identity t)
    (write-string "CLAST Parsing Environment" stream)))


(defun ensure-parsing-environment (&optional env)
  (%make-parsing-environment env))


(defmethod is-environment ((e parsing-environment)) t)
(defmethod is-environment ((e sb-kernel:lexenv)) t)


(defvar *sbcl-parsing-env*
  (ensure-parsing-environment))


(declaim (inline get-implementation-env)
         (ftype (function ((or null
                               parsing-environment
                               sb-kernel:lexenv))
                          (or null sb-kernel:lexenv)
                          ;; T ; If we want to be less precise.
                          )
                get-implementation-env))


(defun get-implementation-env (env)
  (declare (type (or null
                     parsing-environment
                     sb-kernel:lexenv)))
  (etypecase env
    (null env)
    (sb-kernel:lexenv env)
    (parsing-environment (implementation-env env))))


(defun env-find-block (b-name env)
  (declare (type parsing-environment env))
  (labels ((env-find-block-1 (b-name env)
             (let ((b (member b-name (parsing-environment-blocks env)
                              :test #'eq)))
               (if b
                   b
                   (let ((next-pe (parsing-environment-enclosing-env env)))
                     (when next-pe
                       (env-find-block-1 b-name next-pe))))
               ))
           )
    (let ((block-info (env-find-block-1 b-name env)))
      (if block-info
          (values :block nil nil)
          (values nil nil nil)))))


(defun env-find-tag (t-name env)
  (declare (type parsing-environment env))
  (labels ((env-find-tag-1 (t-name env)
             (let ((tt (member t-name (parsing-environment-tags env)
                               :test #'eq)))
               (if tt
                   tt
                   (let ((next-pe (parsing-environment-enclosing-env env)))
                     (when next-pe
                       (env-find-tag-1 t-name next-pe))))
               ))
           )
    (let ((tag-info (env-find-tag-1 t-name env)))
      (if tag-info
          (values :tag nil nil)
          (values nil nil nil)))))


;;;; The Magnificent (yet neglected) 7.
;;;; CLtL2 environment manipulation manipulation functions.

(defun variable-information (variable &optional env)
  (sb-cltl2:variable-information variable env)
  )


(defun function-information (f &optional env)
  (sb-cltl2:function-information f env)
  )


(defun declaration-information (decl-name &optional env)
  (sb-cltl2:function-information decl-name env)
  )


(defun block-information (block-name
                          &optional
                          (env *lispworks-parsing-env*))
  (typecase env
    (parsing-environment
     (env-find-block block-name env))
    (t (values nil nil nil))) ; Just to reiterate...
  )


(defun tag-information (tag-name
                        &optional
                        (env *lispworks-parsing-env*))
  (typecase env
    (parsing-environment
     (env-find-tag tag-name env))
    (t (values nil nil nil))) ; Just to reiterate...
  )


(defun augment-environment (env &rest keys ; Just a utility variable.
                                &key
                                variable
                                symbol-macro
                                function
                                macro
                                declare)
  (declare (ignore variable symbol-macro function macro declare))
  (apply #'sb-cltl2:augment-environment env keys)
  )


(defmacro define-declaration (decl-name lambda-list &body forms)
  `(sb-cltl2:define-declaration ,decl-name ,lambda-list ,@forms)
  )


(defun parse-macro (name lambda-list body &optional env)
  (sb-cltl2:parse-macro name lambda-list body env)
  )


(defun enclose (lambda-expression &optional env)
  (sb-cltl2:enclose lambda-expression env)
  )


;;;; end of file -- clast-sbcl.lisp --
