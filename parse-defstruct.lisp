;;;; -*- Mode: Lisp -*-

;;;; parse-defstruct.lisp --
;;;; Parsing of "defstruct" constructs.

;;;; See the file COPYING for copyright and license information.

;;;; Portions of code lifted from IT.BESE.ARNESI and CL-WALKER.


(in-package "CLAST")


;;;;===========================================================================
;;;; Prologue.
;;;;
;;;; Parsing DEFSTRUCT is more complicated than parsing other
;;;; definition forms (DEFCLASS included); this is due to the fact
;;;; that DEFSTRUCT defines a lot of stuff automagically.
;;;;
;;;; As per LOOP two kinds of sub-forms are defined to deal with slots
;;;; and options.
;;;;
;;;; Note. Parsing of DEFSTRUCT options is done "one at a time".  One
;;;; could have collected, say, all the constructor options and parsed
;;;; them at once, but then the overall parsed DEFSTRUCT would have had
;;;; the parsed options reordered w.r.t. the source code.  This is
;;;; judged undesirable.  Alas, this requires some more
;;;; "post-processing" to deal with "defstruct wide" options effect.
;;;; E.g., whether or not declare a default constructor.

;;;----------------------------------------------------------------------------
;;; defstruct-subform, struct-option-subform, struct-slot-subform

(defclass defstruct-subform (form) ())


(defclass struct-slot-subform (defstruct-subform)
  ((name :reader struct-slot-subform-name
         :initarg :slot-name) ; Inherited; new reader and initarg added.
   (initform :reader struct-slot-subform-initform
             :initarg :slot-initform)
   (slot-type :reader struct-slot-subform-type
              :initarg :slot-type)
   (read-only :reader struct-slot-subform-read-only
              :initarg :slot-read-only)
   (other-options :reader struct-slot-subform-other-options
                  :initarg :other-options)
   )
  )


(defun is-struct-slot-subform (x)
  (typep x 'struct-slot-subform))


(defun struct-slot-subform-p (x)
  (is-struct-slot-subform x))


(defun make-struct-slot-form (slot-name
                              &optional
                              initform
                              &rest keys
                              &key
			      (type (make-instance
				     'type-specifier-form
				     :spec t))
			      (read-only nil)
                              &allow-other-keys
                              )
  (remf keys :type)
  (remf keys :read-only)
  (make-instance 'struct-slot-subform
         :slot-name slot-name
         :slot-initform initform
         :slot-read-only read-only
         :slot-type type
         :other-options keys))


(defmethod clast-element-subforms ((sos struct-slot-subform))
  (list (struct-slot-subform-name sos)
        (struct-slot-subform-initform sos)
        (struct-slot-subform-type sos)
        (struct-slot-subform-other-options sos)
        ))


(defclass struct-option-subform (defstruct-subform)
  ((name :reader struct-option-subform-name
         :initarg :option-name) ; Inherited; new reader and initarg added.
   (spec :reader struct-option-subform-spec
         :initarg :option-spec)
   )
  )


(defun is-struct-option-subform (x)
  (typep x 'struct-option-subform))


(defun struct-option-subform-p (x)
  (is-struct-option-subform x))


(defun make-struct-option-form (option-name &rest spec)
  (make-instance 'struct-option-subform
                 :option-name option-name
                 :option-spec spec))


(defmethod struct-option-subform-arg1 ((sos struct-option-subform))
  (first (struct-option-subform-spec sos)))


(defmethod struct-option-subform-args ((sos struct-option-subform))
  (rest (struct-option-subform-spec sos)))


(defgeneric struct-option-subform-bare-p (sos)
  (:method ((sos struct-option-subform))
   (null (struct-option-subform-spec sos)))
  (:documentation "Returns true if the option was 'bare'.

I.e, when the option did not have a 'main' argument; this is the case
for :CONSTRUCTOR, :COPIER and :PREDICATE.")
  )


(defmethod clast-element-subforms ((sos struct-option-subform))
  (if (struct-option-subform-bare-p sos)
      (list (struct-option-subform-name sos))
      (list* (struct-option-subform-name sos)
             (struct-option-subform-spec sos))))


;;;----------------------------------------------------------------------------
;;; defstruct-form

(defparameter *defstruct-options*
  '(:conc-name
    :constructor
    :copier
    :predicate
    :include
    :print-object
    :print-function
    :type
    :named))


(defun default-structure-fname (struct-name &rest names)
  (declare (type symbol struct-name))
  (intern
   (apply #'format nil "~A-~A" names)
   (symbol-package struct-name)))


(defun build-cons-key-arglist-types (parsed-slots)
  (loop for (slot-name nil . slot-keys) in parsed-slots
        collect
        (destructuring-bind (&key (type t) &allow-other-keys)
            slot-keys
          `(,(intern (string slot-name) "KEYWORD") ,type))
        into kvs
        finally (return (cons '&key kvs))
        )) ; THESE ARE TYPES!


(defun build-cons-key-arglist (parsed-slots)
  (loop for (slot-name initform) in parsed-slots
        collect `(,slot-name ,initform) ; ASSUME INITFORM UNPARSED (IT AIN'T SO).
        into kvs
        finally (return (cons '&key kvs))
        ))


(defun add-function-to-env (env fname args-decl return-type)
  (augment-environment
   env
   :function (list fname)
   :declare `((ftype (function ,args-decl ,return-type) ,fname))
   ))


;;;---------------------------------------------------------------------------
;;; parse-struct-option

(defgeneric parse-struct-option (opt-name
                                 opt
                                 struct-name
                                 env
                                 parsed-slots
                                 &key
                                 default-constructor-name
                                 default-copier-name
                                 default-predicate-name
                                 )
  (:method ((opt-name symbol)
            opt
            struct-name
            env
            parsed-slots
            &key
            default-constructor-name
            default-copier-name
            default-predicate-name
            )
   (declare (ignore parsed-slots
                    default-constructor-name
                    default-copier-name
                    default-predicate-name))
   (warn "Unrecognized or unimplemented parser for struct option ~S in structure ~S."
         opt-name
         struct-name)
   (values (apply #'make-struct-option-form opt-name opt)
           env)))


(defmethod parse-struct-option ((cons-kwd (eql :constructor))
                                cons-option
                                struct-name
                                env
                                parsed-slots
                                &key
                                (default-constructor-name
                                 (default-structure-fname
                                  struct-name
                                  'make
                                  struct-name))
                                default-copier-name
                                default-predicate-name
                                )
  (declare (ignore default-copier-name
                   default-predicate-name))

  (destructuring-bind (&optional (cname nil cname-supplied) arglist)
      cons-option
    (cond ((and cname-supplied (null cname))
           (values (make-struct-option-form :constructor
                                            (parse nil :environment env))
                   env)
           )

          ((and cname (null arglist))
           (values (make-struct-option-form :constructor cname)
                   (add-function-to-env env
                                        cname
                                        (build-cons-key-arglist parsed-slots)
                                        struct-name))
           )

          ((and cname arglist)
           (values (make-struct-option-form :constructor cname arglist)
                   (add-function-to-env env
                                        cname
                                        arglist
                                        struct-name))
           )

          ((null cname-supplied)
           (values (make-struct-option-form :constructor)
                   (add-function-to-env env
                                        default-constructor-name
                                        (build-cons-key-arglist parsed-slots)
                                        struct-name))
           )
          ))
  )


(defmethod parse-struct-option ((copier-kwd (eql :copier))
                                copier-option
                                struct-name
                                env
                                parsed-slots
                                &key
                                default-constructor-name
                                (default-copier-name
                                 (default-structure-fname
                                  struct-name
                                  'copy
                                  struct-name))
                                default-predicate-name
                                )
  (declare (ignore default-constructor-name
                   default-predicate-name))

  (destructuring-bind (&optional (copier-fname nil copier-supplied))
      copier-option
    (cond ((and copier-supplied copier-fname)
           (values (make-struct-option-form :copier copier-fname)
                   (add-function-to-env env
                                        copier-fname
                                        (list struct-name)
                                        struct-name))

           )
           
          ((and copier-supplied (null copier-fname))
           (values (make-struct-option-form :copier (parse nil :environment env))
                   env)
           )
          
          ((null copier-supplied)
           (values (make-struct-option-form :copier)
                   (add-function-to-env env
                                        default-copier-name
                                        (list struct-name)
                                        struct-name))
           )
          )))


(defmethod parse-struct-option ((predicate-kwd (eql :predicate))
                                predicate-option
                                struct-name
                                env
                                parsed-slots
                                &key
                                default-constructor-name
                                default-copier-name
                                (default-predicate-name
                                 (default-structure-fname
                                  struct-name
                                  struct-name
                                  'p))
                                )
  ;; Practically a duplicate of the :COPIER method.

  (declare (ignore default-constructor-name
                   default-copier-name))

  (destructuring-bind (&optional (predicate-fname nil predicate-supplied))
      predicate-option
    (cond ((and predicate-supplied predicate-fname)
           (values (make-struct-option-form :predicate predicate-fname)
                   (add-function-to-env env
                                        predicate-fname
                                        (list struct-name)
                                        'boolean))
           )

          ((and predicate-supplied (null predicate-fname))
           (values (make-struct-option-form :predicate (parse nil :environment env))
                   env)
           )

          ((null predicate-supplied)
           (values (make-struct-option-form :predicate default-predicate-name)
                   (add-function-to-env env
                                        default-predicate-name
                                        (list struct-name)
                                        'boolean))
           )
          )))


(defmethod parse-struct-option ((printer-opt-kwd (eql :print-function))
                                printer-option
                                struct-name
                                env
                                parsed-slots
                                &key
                                default-constructor-name
                                default-copier-name
                                default-predicate-name
                                )
  (declare (ignore default-constructor-name
                   default-copier-name
                   default-predicate-name))

  (destructuring-bind (&optional (printer-fname nil printer-supplied-p))
      printer-option
    (cond ((and printer-supplied-p printer-fname)
           (multiple-value-bind (fkind is-local f-decls)
               (function-information printer-fname env)
             (declare (ignore is-local f-decls))
             (if fkind
                 (values (make-struct-option-form :print-function printer-fname)
                         env)
                 (values (make-struct-option-form :print-function printer-fname)
                         (add-function-to-env env
                                              printer-fname
                                              (list struct-name 'stream '(integer 0))
                                              'null))))
           )

          ((and printer-supplied-p (null printer-fname))
           (values (make-struct-option-form :printer (parse nil :environment env))
                   env)
           )

          ((null printer-supplied-p)
           (values (make-struct-option-form :print-function) env))
          )
    ))


(defmethod parse-struct-option ((printer-opt-kwd (eql :print-object))
                                printer-option
                                struct-name
                                env
                                parsed-slots
                                &key
                                default-constructor-name
                                default-copier-name
                                default-predicate-name
                                )
  (declare (ignore default-constructor-name
                   default-copier-name
                   default-predicate-name))

  (destructuring-bind (&optional (printer-fname nil printer-supplied-p))
      printer-option
    (cond ((and printer-supplied-p printer-fname)
           (multiple-value-bind (fkind is-local f-decls)
               (function-information printer-fname env)
             (declare (ignore is-local f-decls))
             (if fkind
                 (values (make-struct-option-form :print-object printer-fname)
                         env)
                 (values (make-struct-option-form :print-object printer-fname)
                         (add-function-to-env env
                                              printer-fname
                                              (list struct-name 'stream)
                                              'null))))
           )

          ((and printer-supplied-p (null printer-fname))
           (values (make-struct-option-form :print-object (parse nil :environment env))
                   env)
           )

          ((null printer-supplied-p)
           (values (make-struct-option-form :print-object)
                   env))
          )
    ))


(defmethod parse-struct-option ((include-kwd (eql :include))
                                include-option
                                struct-name
                                env
                                parsed-slots
                                &key
                                default-constructor-name
                                default-copier-name
                                default-predicate-name
                                )
  (declare (ignore default-constructor-name
                   default-copier-name
                   default-predicate-name))

  (destructuring-bind (superstruct &rest slot-descriptions)
      include-option
    #|
    (warn "CLAST: uninplemented and incomplete parsing of DEFSTRUCT ~
                          :include options: slot modifiers still unimplemented.")
    |#
    (multiple-value-bind (parsed-slots parsed-slots-env)
        (parse-struct-slots struct-name struct-name slot-descriptions env ()) ; The () is wrong.
      (declare (ignore parsed-slots-env))
      (values (make-struct-option-form :include superstruct parsed-slots)
              env)
      )))


(defmethod parse-struct-option ((type-kwd (eql :type))
                                type-option
                                struct-name
                                env
                                parsed-slots
                                &key
                                default-constructor-name
                                default-copier-name
                                default-predicate-name
                                )
  (declare (ignore default-constructor-name
                   default-copier-name
                   default-predicate-name))
  (values (make-struct-option-form :type (first type-option)) env))


(defmethod parse-struct-option ((initial-offset-kwd (eql :initial-offset))
                                init-offset-option
                                struct-name
                                env
                                parsed-slots
                                &key
                                default-constructor-name
                                default-copier-name
                                default-predicate-name
                                )
  (declare (ignore default-constructor-name
                   default-copier-name
                   default-predicate-name))
  (values (make-struct-option-form :initial-offset
				   (first init-offset-option))
          env))


(defmethod parse-struct-option ((named-kwd (eql :named))
                                named-option
                                struct-name
                                env
                                parsed-slots
                                &key
                                default-constructor-name
                                default-copier-name
                                default-predicate-name
                                )
  (declare (ignore default-constructor-name
                   default-copier-name
                   default-predicate-name))
  (values (make-struct-option-form :named) env))


(defmethod parse-struct-option ((named-kwd (eql :conc-name))
                                conc-name-option
                                struct-name
                                env
                                parsed-slots
                                &key
                                default-constructor-name
                                default-copier-name
                                default-predicate-name
                                )
  (declare (ignore default-constructor-name
                   default-copier-name
                   default-predicate-name))
  (destructuring-bind (&optional
                       (cn
                        (intern (format nil "~A-" struct-name)
                                (symbol-package struct-name)) ; A bit pointless...
                        conc-name-supplied)
                       )
      conc-name-option
    (cond ((and conc-name-supplied cn)
           (values (make-struct-option-form :conc-name cn)
                   env))
          ((and conc-name-supplied (null cn))
           (values (make-struct-option-form :conc-name (parse nil :environment env))
                   env))
          )))


;;;---------------------------------------------------------------------------
;;; parse-struct-slots, parse-struct-slot

(defun parse-struct-slots (struct-name conc-name slots env keys)
  (let ((new-env env)
        (parsed-slots ())
        )
    (dolist (slot slots
                  (values (nreverse parsed-slots)
                          new-env))
      (multiple-value-bind (parsed-slot parsed-slot-env)
          (parse-struct-slot struct-name slot new-env keys)
        (push parsed-slot parsed-slots)
        
        ;; Add the slot reader.
        (setf new-env
              (add-function-to-env parsed-slot-env
                                   (default-structure-fname struct-name
                                                            conc-name
                                                            ;; (first parsed-slot)
                                                            (struct-slot-subform-name
                                                             parsed-slot)
                                                            )
                                   (list struct-name)
                                   (type-specifier-form-spec
				    (struct-slot-subform-type
				     parsed-slot))
                                   #|
                                   (if (getf (cddr parsed-slot) :type)
                                       (type-specifier-form-spec
                                        (getf (cddr parsed-slot) :type))
                                       T
                                       )|#
                                   ))
        ))))


(defun parse-struct-slot (struct-name slot env keys)
  (declare (type list slot)
           (ignore struct-name))
  (destructuring-bind (slot-name
                       &optional
                       (slot-initform nil slot-initform-supplied)
                       &rest slot-keys
                       &key
                       (type t type-key-supplied)
                       (read-only nil read-only-key-supplied)
                       &allow-other-keys ; Funky code...
                       )
      slot

    ;; Note the environment that parses initforms.
    ;; It is necessary beacuse of verbiage in the CLHS
    ;; about initforms as lambda list keyword
    ;; initializers.

    (let ((new-slot-keys (copy-list slot-keys))
          (parsed-initform nil)
          )
      (when slot-initform-supplied
        (setf parsed-initform
              (apply #'parse slot-initform
                     :environment env
                     keys))
        )
      (when type-key-supplied
        (setf (getf new-slot-keys :type)
              (make-instance 'type-specifier-form
                             :spec type
                             :source type
                             ;; :top ...
                             ))
        )
      (when read-only-key-supplied
        (setf (getf new-slot-keys :read-only)
              (apply #'parse read-only
                     :environment env
                     keys))
        )
      #|
      (values `(,slot-name
                ,.(when slot-initform-supplied
                    (list parsed-initform))
                ,.new-slot-keys)
              env)))|#
      (values (if slot-initform-supplied
                  (apply #'make-struct-slot-form
                         slot-name
                         parsed-initform
                         new-slot-keys)
                  (make-struct-slot-form slot-name))
              env)))
  )


;;;---------------------------------------------------------------------------
;;; parse-form defstruct

(defmethod parse-form ((op (eql 'defstruct)) form
                       &rest keys
                       &key
                       enclosing-form
                       environment
                       macroexpand
                       &allow-other-keys)

  (declare (ignore macroexpand))

  (destructuring-bind (ds-kwd name-n-options &rest spec)
      form
    (declare (ignore ds-kwd))
    (let* ((name-is-symbol (symbolp name-n-options))

           (name (typecase name-n-options
                   (symbol name-n-options)
                   (cons (first name-n-options))
                   (t (error 'ast-parse-error
                             :format-control "Illegal name and options in DEFSTRUCT: ~A."
                             :format-arguments (list name-n-options)))))

           (struct-name name)

           (options (typecase name-n-options
                      (cons (ensure-lists (rest name-n-options)))))

           (docstring (when (stringp (first spec))
                        (parse (first spec) :environment environment)))

           (slots (if docstring
                      (ensure-lists (rest spec))
                      (ensure-lists spec)))

           ;; Options unpacking.
           (default-conc-name
            (default-structure-fname struct-name
                                     struct-name
                                     ""))

           (conc-name (if name-is-symbol
                          default-conc-name
                          (let ((conc-name-opt
                                 (find :conc-name options :test #'eq :key #'first)))
                            (cond ((and conc-name-opt (second conc-name-opt))
                                   (second conc-name-opt))
                                  (conc-name-opt '||)
                                  (t default-conc-name))))
                      )

           (default-constructor-name
            (default-structure-fname struct-name
                                     'make
                                     struct-name))

           (default-copier-name
            (default-structure-fname struct-name
                                     'copy
                                     struct-name))

           (default-predicate-name
            (default-structure-fname struct-name
                                     struct-name
                                     'p))

  
           (parsed-opts ())
           (parsed-slots ())
           )

      (declare (type symbol
                     default-conc-name
                     default-constructor-name
                     default-copier-name
                     default-predicate-name
                     conc-name)
               )

      (labels ((parse-slot (slot env
                                 &aux
                                 (slot-name (first slot))
                                 (spec (rest slot)))
                 (declare (type list slot))
                 (destructuring-bind (slot-initform
                                      &rest slot-keys
                                      &key
                                      (type t)
                                      (read-only nil)
                                      &allow-other-keys ; Funky code...
                                      )
                     (or spec '(nil))
                   
                   (declare (ignore read-only))

                   ;; Note the environment that parses initforms.
                   ;; It is necessary beacuse of verbiage in the CLHS
                   ;; about initforms as lambda list keyword
                   ;; initializers.

                   (let* ((slot-accessor-name
                           (intern (format nil "~A~A" conc-name slot-name)
                                   (symbol-package slot-name)))
                          (parsed-initform
                           (apply #'parse slot-initform
                                  :environment env
                                  keys))
                          (new-env
                           (augment-environment
                            env
                            :function (list slot-accessor-name)
                            :declare (list `(ftype (function (,name) ,type)
                                                   ,slot-accessor-name))))
                          )
                     (values (list* slot-name parsed-initform slot-keys)
                             new-env)))
                 )

               (parse-slots (slots env &aux (new-env env))
                 (dolist (o slots new-env)
                   (multiple-value-bind (parsed-slot parsed-slot-env)
                       (parse-slot o new-env)
                     (push parsed-slot parsed-slots)
                     (setf new-env parsed-slot-env))))

               (select-opts (opt-kwd options)
                 (remove opt-kwd options :key #'first :test-not #'eq))

               (parse-options (options env)
                 (let ((cons-opts (select-opts :constructor options))
                       (copier-opts (select-opts :copier options))
                       (predicate-opts (select-opts :predicate options))
                       (type-opts (select-opts :type options))
                       (include-opts (select-opts :include options))
                       (print-function-opts (select-opts :print-function options))
                       (print-object-opts (select-opts :print-object options))
                       (init-offset-opts (select-opts :initial-offset options))
                       (new-env env)
                       )
                   (declare (ignore type-opts
                                    include-opts
                                    print-function-opts
                                    print-object-opts
                                    init-offset-opts
                                    ))

                   ;; Parse options in order.
                   (dolist (opt options)
                     (multiple-value-bind (parsed-opt parsed-opt-env)
                         (parse-struct-option (first opt)
                                              (rest opt)
                                              struct-name
                                              new-env
                                              parsed-slots
                                              :default-constructor-name default-constructor-name
                                              :default-copier-name default-copier-name
                                              :default-predicate-name default-predicate-name
                                              )
                       (push parsed-opt parsed-opts)
                       (setf new-env parsed-opt-env))
                     )

                   ;; Need to post-process options here, e.g., to
                   ;; declare default constructors.  Each call to the
                   ;; PARSE-STRUCT-OPTION function takes care of
                   ;; "present" options, but if the option is not
                   ;; present the appropriate default action (i.e.,
                   ;; the declaration of a function) must be performed.

                   (unless cons-opts
                     (setf new-env
                           (add-function-to-env new-env
                                                default-constructor-name
                                                (build-cons-key-arglist-types parsed-slots)
                                                struct-name))
                     )

                   (unless copier-opts
                     (setf new-env
                           (add-function-to-env new-env
                                                default-copier-name
                                                (list struct-name)
                                                struct-name))
                     )

                   (unless predicate-opts
                     (setf new-env
                           (add-function-to-env new-env
                                                default-predicate-name
                                                (list struct-name)
                                                'boolean))
                     )
                   
                   ;; PARSE-OPTIONS return values.
                   (values (nreverse parsed-opts) new-env))
                 )
               ) ; local functions.

        (multiple-value-bind (parsed-slots parsed-slot-env)
            (parse-struct-slots struct-name conc-name slots environment keys)
          (multiple-value-bind (parsed-opts opts-env)
              (parse-options options parsed-slot-env)

            (let* ((def-form
                    (make-instance 'defstruct-form
                                   :name name
                                   :options parsed-opts
                                   :slots (nreverse parsed-slots)

                                   :source form
                                   :top enclosing-form
                                   ;; :type T
                                   ))
                   )
              
              (values def-form opts-env)
              )))
        ))
    ))


(defmethod clast-element-subforms ((df defstruct-form))
  (list* (defstruct-form-name df)
         (defstruct-form-options df)
         (defstruct-form-slots df)))


;;;; end of file -- parse-defstruct.lisp
