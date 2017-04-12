;;;; -*- Mode: Lisp -*-

;;;; walk.lisp --
;;;; Setup for walking (or "visiting" an AST).  Actually a MAP-REDUCE scheme.
;;;;
;;;; See file COPYING in main folder for licensing and copyright information.

(in-package "CLAST")


;; (deftype walking-direction () '(member :breadth :depth))


;;;;===========================================================================
;;;; Protocol


(defgeneric map-subforms (form func &key result-type &allow-other-keys)
  (:documentation "Maps the function FUNC over the sub-forms of FORM.

A sequence of type RESULT-TYPE is returned (as per MAP).  The mapping
is not recursive.  Only the 'sequence' of subforms is mapped over.
"))


(defgeneric walk (clast-element &rest keys
                                &key
                                key ; #'identity
                                result-type
                                map-fun
                                reduce-fun
                                initial-value
                                environment
                                op-first
                                &allow-other-keys)
  (:documentation "The 'visiting' engine used to traverse a form.

The WALK generic function methods recursively traverse the tree
corresponding to a form (i.e., CLAST-ELEMENT) using a map/reduce
scheme.

The function MAP-FUN is applied to each (sub)form and their respective
subforms are WALKed over.  WALK uses MAP-SUBFORMS internally,
therefore it generates sequences (of type RESULT-TYPE) as output. Once
the traversing of subforms is completed the function REDUCE-FUN is
applied, via REDUCE to the resulting sequence.
")
  )


;;;;===========================================================================
;;;; map-subforms methods.

(defmethod map-subforms ((ce form) func
                         &key
                         (result-type 'list)
                         &allow-other-keys)
  (map result-type func (clast-element-subforms ce)))


(defmethod map-subforms ((ce let-form) func
                         &key
                         (result-type 'list)
                         &allow-other-keys)
  (map result-type func
       (nconc (mapcar #'second (form-binds ce))
              (form-progn ce))))


(defmethod map-subforms ((ce let*-form) func
                         &key
                         (result-type 'list)
                         &allow-other-keys)
  (map result-type func
       (nconc (mapcar #'second (form-binds ce))
              (form-progn ce))))


(defmethod map-subforms ((ce loop-clause) func
                         &key
                         (result-type 'list)
                         &allow-other-keys)
  (map result-type func
       (loop-clause-subclauses ce))
  )

                         
;;;;===========================================================================
;;;; Generic walker infrastructure.

#|
(defmethod walk ((ce form)
                 &rest keys
                 &key
                 (result-type 'list)
                 (key #'identity)
                 ((:map mapfun) (lambda (e) (list (funcall key e))))
                 ((:reduce redfun) #'append)
                 (initial-value ())
                 environment
                 op-first ; boolean
                 &allow-other-keys)
  (declare (ignore environment))
  (let ((subforms-results
         (reduce redfun
                 (map-subforms ce mapfun :result-type result-type)
                 :initial-value initial-value))
        )
    (if op-first
        (concatenate result-type (funcall mapfun ce) subforms-results)
        (concatenate result-type subforms-results (funcall mapfun ce))
        )))
|#

(defmethod walk ((ce form)
                 &rest keys
                 &key
                 (result-type 'list)
                 (key #'identity)
                 ((:map mapfun) (lambda (e) (list (funcall key e))))
                 ((:reduce redfun) #'append)
                 (initial-value ())
                 environment
                 op-first ; boolean
                 &allow-other-keys)
  (declare (ignore environment))
  (let ((subforms-results
         (reduce redfun
                 (map-subforms ce
                               (lambda (f)
                                 (apply #'walk f keys))
                               :result-type result-type)
                 :initial-value initial-value))
        )
    (if op-first
        (concatenate result-type (funcall mapfun ce) subforms-results)
        (concatenate result-type subforms-results (funcall mapfun ce))
        )))


;;;;---------------------------------------------------------------------------
;;;; Queries...

(defun variables (form)
  "Returns all variables present in FORM.

Arguments and Values:

form   : a CLAST-ELEMENT.
result : a LIST of SYMBOLS."
  (walk form
        :map (lambda (e)
               (typecase e
                 (variable-ref (list (form-symbol e)))))
        ))


(defun bound-variables (form)
  "Returns all bound variables present in FORM.

Arguments and Values:

form   : a CLAST-ELEMENT.
result : a LIST of SYMBOLS."
  (walk form
        :map (lambda (e)
               (typecase e
                 (free-variable-ref ())
                 (variable-ref (list (form-symbol e)))))
        ))


(defun special-variables (form)
  "Returns all the special variables present in FORM.

Arguments and Values:

form   : a CLAST-ELEMENT.
result : a LIST of SYMBOLS."
  (walk form
        :map (lambda (e)
               (typecase e
                 (special-variable-ref (list (form-symbol e)))))
        ))


(defun free-variables (form) ; Ok.  All this work to be able to write
                             ; this function!!!!
  "Returns all the 'free' variables present in FORM.

Arguments and Values:

form   : a CLAST-ELEMENT.
result : a LIST of SYMBOLS."
  (walk form
        :map (lambda (e)
               (typecase e
                 (free-variable-ref (list (form-symbol e)))))
        ))


(defun functions (form)
  "Returns all the functions called in FORM.

Arguments and Values:

form   : a CLAST-ELEMENT.
result : a LIST of SYMBOLS."
  (walk form
        :map (lambda (e)
               (typecase e
                 (function-name-ref (list (form-symbol e)))))
        ))


(defun macros (form)
  "Returns all the macros called in FORM.

Arguments and Values:

form   : a CLAST-ELEMENT.
result : a LIST of SYMBOLS."
  (walk form
        :map (lambda (e)
               (typecase e
                 (macro-name-ref (list (form-symbol e)))))
        ))


(defun symbol-macros (form)
  "Returns all the symbol-macros present in FORM.

Arguments and Values:

form   : a CLAST-ELEMENT.
result : a LIST of SYMBOLS."
  (walk form
        :map (lambda (e)
               (typecase e
                 (symbol-macro-name-ref (list (form-symbol e)))))
        ))

;;;; end of file -- walk.lisp --
