;;;; -*- Mode: Lisp -*-

;;;; clast-lispworks.lisp --
;;;; Code lifted from IT.BESE.ARNESI and CL-WALKER.
;;;;
;;;; Environment functions.
;;;; Either you have CLtL2 environment functions or your
;;;; implementation sucks.

(in-package "CLAST")


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
(defmethod is-environment ((e sys::augmented-environment)) t)


(defvar *lispworks-parsing-env*
  (ensure-parsing-environment))


(declaim (inline get-implementation-env)
         (ftype (function ((or null
                               parsing-environment
                               sys::augmented-environment))
                          (or null sys::augmented-environment)
                          ;; T ; If we want to be less precise.
                          )
                get-implementation-env))


(defun get-implementation-env (env)
  (declare (type (or null
                     parsing-environment
                     sys::augmented-environment)))
  (etypecase env
    (null env)
    (sys::augmented-environment env)
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

(defun variable-information (variable
                             &optional
                             (env *lispworks-parsing-env*))
  (hcl:variable-information variable
                            (get-implementation-env env)))



(defun function-information (f
                             &optional
                             (env *lispworks-parsing-env*))
  (hcl:function-information f
                            (get-implementation-env env)))


(defun declaration-information (decl-name &optional (env *lispworks-parsing-env*))
  (hcl:declaration-information decl-name
                               (get-implementation-env env))
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
                                (tag () tag-supplied-p)
                                (block () block-supplied-p)
                                declare)
  ;; This function is hairy just because it wants to handle NULL
  ;; environments and the underlying implementation environments.
  ;; It could be simplified by assuming that ENV is always a
  ;; PARSING-ENVIRONMENT.

  (declare (ignore variable symbol-macro function macro declare)
           (type (or null
                     sys::augmented-environment
                     parsing-environment)))

  ;; First we augment the underlying environment and then we create a
  ;; new parsing environment, which will eventually be returned.

  (let* ((new-lw-env
          (typecase env
            ((or null sys::augmented-environment)
             (apply #'hcl:augment-environment env :allow-other-keys t keys))
            (parsing-environment
             (apply #'hcl:augment-environment
                    (get-implementation-env env)
                    :allow-other-keys t
                    keys))))

         (new-parsing-env
          (%make-parsing-environment new-lw-env
                                     (when (parsing-environment-p env)
                                       env)))
         )

    ;; The next operations need to be refined...
    (when tag-supplied-p
      (setf (parsing-environment-tags new-parsing-env)
            (copy-list tag)))

    (when block-supplied-p
      (setf (parsing-environment-blocks new-parsing-env)
            (copy-list block)))

    new-parsing-env
    ))


(defmacro define-declaration (decl-name lambda-list &body forms)
  `(hcl:define-declaration ,decl-name ,lambda-list ,@forms)
  )


;;; PARSE-MACRO and ENCLOSE
;;;
;;; We assume (as it is implied and/or "ordered" in CLtL2) that
;;; PARSE-MACRO and ENCLOSE don't play shenanigans with the environments;
;;; in particular, we assume that two functions have access *only* to
;;; the implementation environment and not the the "extended" parsing
;;; environment defined above.

(defun parse-macro (name lambda-list body &optional (env *lispworks-parsing-env*))
  (hcl:parse-macro name lambda-list body env)
  )


(defun enclose (lambda-expression &optional (env *lispworks-parsing-env*))
  (hcl:enclose lambda-expression env)
  )


;;;; end of file -- clast-lispworks.lisp --
