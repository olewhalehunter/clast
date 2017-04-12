;;;; -*- Mode: Lisp -*-

;;;; clast-elements.lisp --
;;;; Some code lifted from IT.BESE.ARNESI and CL-WALKER.

;;;; See file COPYING in main folder for licensing and copyright information.

(in-package "CLAST")


;;;---------------------------------------------------------------------------
;;; CLAST-ELEMENT
;;;
;;; Top class just for convenience.

(defclass clast-element () ())

(defun is-clast-element (x)
  (typep x 'clast-element))


;;;---------------------------------------------------------------------------
;;; FORM
;;;
;;; Main class, other "top" classes are "mixins" (or "traits").
;;; The main "protocol" is also named after FORM.

(defclass form (clast-element)
  ((type :accessor form-type
         :initarg :type
         :initform t ; The declared or (possibly) inferred type of the form.
         )
   (top :accessor form-top
        :initarg :top
        :initform nil ; The 'enclosing form'; NIL for top-level forms.
        )
   (source :accessor form-source
           :initarg :source
           )
   )
  (:documentation "The FORM Class.

The top of the FORMs hierarchy."))

(defun is-form (x)
  (typep x 'form))

(defun form-p (x)
  (typep x 'form))


(defclass expansion-component (clast-element)
  ((expansion :accessor form-expansion
              :initarg :expansion))
  (:documentation "The EXPANSION-COMPONENT Class.

A mixin class used for all forms which may result in a macro
expansion; i.e., macro applications and symbol macros.")
  )


(defun expansion-component-p (x) (typep x 'expansion-component))


;;;---------------------------------------------------------------------------
;;; CONSTANT-FORM

(defclass constant-form (form)
  ((value :accessor form-value
          :initarg :value
          )
   )
  (:documentation "The CONSTANT-FORM Class.

The class of forms representing 'constants', e.g., literal numbers.
"))


(defun constant-form-p (x)
  (typep x 'constant-form))


;;;---------------------------------------------------------------------------
;;; BINDING-FORMs

(defclass binding-form (form)
  ((binds :accessor form-binds
          :initarg :binds
          :initform ()
          )
   )
  (:documentation "The BINDING-FORM Class.

The class representing all 'forms' that bind 'names'." 
   ))


(defun binding-form-p (x)
  (typep x 'binding-form))


(defclass vbinding-form (binding-form) ()
  (:documentation "The VBINDING-FORM Class.

The class representing all 'forms' that bind 'variables'." 
   ))


(defun vbinding-form-p (x)
  (typep x 'vbinding-form))


(defclass fbinding-form (binding-form) ()
  (:documentation "The FBINDING-FORM Class.

The class representing all 'forms' that bind 'functions'." 
   ))


(defun fbinding-form-p (x)
  (typep x 'fbinding-form))


;;;---------------------------------------------------------------------------
;;; IMPLICIT-PROGN
;;;
;;; A "mixin" class.

(defclass implicit-progn (clast-element)
  ((iprogn-forms :accessor form-progn
                 :accessor form-body
                 :initarg :progn
                 :initarg :body
                 :initform ())
   (body-env :accessor form-body-env
             :initarg :body-env
             :initform () #| (make-env) |#
             )
   )
  (:documentation "The IMPLICIT-PROGN CLass.

A mixin class that is used whenever a form contains an 'implicit progn'.")
  )


(defun implicit-progn-p (x)
  (typep x 'implicit-progn))


;;;---------------------------------------------------------------------------
;;; SYMBOL-REFs

(defclass symbol-ref (form)
  ((symbol :accessor form-symbol :initarg :symbol)
   (local :accessor form-local
          :initarg :local
          :initform nil
          )
   )
  (:documentation "The SYMBOL-REF Class.

The class of references to 'names' (be they variables, functions etc)."
   ))

(defun symbol-ref-p (x) (typep x 'symbol-ref))


(defclass variable-ref (symbol-ref) ()
 (:documentation "The VARIABLE-REF Class.

The class of references to 'variables'."
   ))

(defun variable-ref-p (x) (typep x 'variable-ref))


(defclass constant-ref (variable-ref constant-form) ()
  (:documentation "The CONSTANT-REF Class.

The class of references to 'constants'."
   ))

(defun constant-ref-p (x) (typep x 'constant-ref))


(defclass free-variable-ref (variable-ref) ()
  (:documentation "The FREE-VARIABLE-REF Class.

The class of references to 'variables' that are 'free' in a form."
   ))

(defun free-variable-ref-p (x) (typep x 'free-variable-ref))


(defclass special-variable-ref (variable-ref) ()
  (:documentation "The SPECIAL-VARIABLE-REF Class.

The class of references to 'special variables'."
   ))

(defun special-variable-ref-p (x) (typep x 'special-variable-ref))


(defclass symbol-macro-ref (symbol-ref expansion-component) ()
  (:documentation "The SYMBOL-MACRO-REF Class.

The class of references to 'symbol macros'."
   ))

(defun symbol-macro-ref-p (x) (typep x 'symbol-macro-ref))


(defclass function-name-ref (symbol-ref) ()
  (:documentation "The FUNCTION-NAME-REF Class.

The class of references to 'function' names."
   ))

(defun function-name-ref-p (x) (typep x 'function-name-ref))


(defclass macro-name-ref (symbol-ref) ()
  (:documentation "The MACRO-NAME-REF Class.

The class of references to 'macro' names."
   ))

(defun macro-name-ref-p (x) (typep x 'macro-name-ref))


(defclass block-name-ref (symbol-ref)
  ((symbol :accessor form-name
           :initarg :name))
  (:documentation "The BLOCK-NAME-REF Class.

The class of references to 'block' names.")
  )

(defun block-name-ref-p (x) (typep x 'block-name-ref))


(defclass go-tag (symbol-ref)
  ((symbol :accessor tag-name
           :initarg :tag)
   )
  (:documentation "The GO-TAG Class.

The instances of this represents references to GO tags.  I.e., they
are essetially SYMBOL-REFs that are found in TAGBODYs and in GO
expressions.")
  )

(defun go-tag-p (x) (typep x 'go-tag))


;;;---------------------------------------------------------------------------
;;; Applications

(defclass application (form)
  ((operator :accessor form-operator :initarg :operator)
   (args     :accessor form-args     :initarg :arguments))
  (:documentation "The APPLICATION Class.

The class representing all 'applications' in Common Lisp forms.")
  )

(defun application-p (x) (typep x 'application))


(defclass function-application (application) ()
  (:documentation "The FUNCTION-APPLICATION Class.

The class representing all 'regular' function applications in Common
Lisp forms."))

(defun function-application-p (x) (typep x 'function-application))


(defclass lambda-application (function-application) ()
  (:documentation "The LAMBDA-APPLICATION Class.

The class representing all anonymous LAMBDA function applications in
Common Lisp forms."))

(defun lambda-application-p (x) (typep x 'lambda-application))


(defclass functional-operator-application (function-application)
  ()
  (:documentation "The Functional Operator Application CLass.

This class represents functional applications where the operator is
non-standard with respect to the Common Lisp Standard; i.e.,
applications where the operator is not a symbol or a lambda expression."))

(defun functional-operator-application-p (x)
  (typep x 'functional-operator-application))


(defclass local-function-application (function-application) ())

(defun local-function-application-p (x) (typep x 'local-function-application))


(defclass macro-application (application expansion-component) ()
  (:documentation "The MACRO-APPLICATION Class.

The class representing all MACRO applications in Common Lisp forms.
The class also inherits the 'expansion' mixin."))

(defun macro-application-p (x) (typep x 'macro-application))


(defclass local-macro-application (macro-application) ())

(defun local-macro-application-p (x) (typep x 'local-macro-application))

;;;---------------------------------------------------------------------------
;;; Type specifiers

(defclass type-specifier-form (form)
  ((spec :accessor type-specifier-form-spec
         :initarg :spec
         )
   )
  (:documentation "The Type Specifier Form Class.

The class representing 'type'specifiers.  Note that the content of the
SPEC slot is always 'as is', i.e., not parsed.")
  )


(defun is-type-specifier-form (form)
  (typep form 'type-specifier-form))


(defun type-specifier-form-p (form)
  (is-type-specifier-form form))


;;;---------------------------------------------------------------------------
;;; Declarations

(defclass declaration-form (form)
  ((decls :accessor declaration-form-declarations
          :initarg :declarations
          )
   (new-env :accessor declaration-form-resulting-environment
            :accessor form-resulting-environment
            :initarg :resulting-environment)
   )
  (:documentation "The Declaration Form Class.

The class of all 'declarations' in Common Lisp.")
  )

(defun declaration-form-p (x) (typep x 'declaration-form))


(defclass declaration-specifier-form (form)
  ((identifier :accessor declaration-specifier-form-identifier
               :accessor declaration-specifier-identifier
               )
   )
  (:documentation "The DECLARATION-SPECIFIER-FORM Class.

The class of all 'declaration specifiers' in Common Lisp.")
  )

(defun declaration-specifier-form-p (x)
  (typep x 'declaration-specifier-form))


(defclass tf-declaration-specifier-form (declaration-specifier-form)
  ((type-spec :accessor declaration-type-spec
              :initarg :spec)
   (symbol-refs :accessor declaration-type-spec-symbols
                :initarg :symbols)
   )
  )

(defun tf-declaration-specifier-form-p (x)
  (typep x 'tf-declaration-specifier-form))


(defclass type-declaration-specifier-form (tf-declaration-specifier-form)
  ((identifier :initform 'type))
  (:documentation "The TYPE-DECLARATION-SPECIFIER-FORM Class.

The class of all 'type declaration specifiers' in Common Lisp.")
  )

(defun type-declaration-specifier-form-p (x)
  (typep x 'type-declaration-specifier-form))


(defclass ftype-declaration-specifier-form (tf-declaration-specifier-form)
  ((identifier :initform 'ftype))
  (:documentation "The FTYPE-DECLARATION-SPECIFIER-FORM Class.

The class of all 'ftype declaration specifiers' in Common Lisp.")
  )

(defun ftype-declaration-specifier-form-p (x)
  (typep x 'ftype-declaration-specifier-form))


(defclass id-declaration-specifier-form (declaration-specifier-form)
  ((identifier :initarg :id)
   )
  (:documentation "The ID-DECLARATION-SPECIFIER-FORM Class.

The class of all 'id declaration specifiers' in Common Lisp. E.g.,
OPTIMIZE declarations.")
  )

(defun id-declaration-specifier-form-p (x)
  (typep x 'id-declaration-specifier-form))


;;;---------------------------------------------------------------------------
;;; Declamations

(defclass declaim-form (declaration-form)
  ())


;;;;---------------------------------------------------------------------------
;;;; "Composite" forms.

(defclass block-form (form implicit-progn)
  ((name :accessor form-name
         :accessor block-name
         :initarg :name)
   )
  (:documentation "The BLOCK-FORM Class.

The class of forms that are Common Lisp BLOCKs.")
  )

(defun block-form-p (x) (typep x 'block-form))


(defclass catch-form (form implicit-progn)
  ((tag :accessor form-catch-tag
        :initarg :tag)
   )
  (:documentation "The CATCH-FORM Class.

The class of forms that are Common Lisp CATCHes.  The slot TAG
contains the catch TAG.")
  )

(defun catch-form-p (x) (typep x 'block-form))


(defclass throw-form (form)
  ((tag :accessor form-throw-tag
        :initarg :tag)
   (result :accessor form-result
           :initarg :result)
   )
  (:documentation "The THROW-FORM Class.")
  )

(defun throw-form-p (x) (typep x 'block-form))


(defclass eval-when-form (form implicit-progn)
  ((situations :accessor form-situations
               :initarg :situations)
   )
  (:documentation "The EVAL-WHEN-FORM CLass.")
  )

(defun eval-when-form-p (x) (typep x 'block-form))


;;;;---------------------------------------------------------------------------
;;;; Binding forms.

(defclass flet-form (fbinding-form implicit-progn)
  ()
  (:documentation "The FLET-FORM Class.")
  )

(defun flet-form-p (x) (typep x 'flet-form))


(defclass labels-form (fbinding-form implicit-progn)
  ()
  (:documentation "The LABELS-FORM Class.")
  )

(defun labels-form-p (x) (typep x 'labels-form))


(defclass macrolet-form (fbinding-form implicit-progn)
  ()
  (:documentation "The MACROLET-FORM Class.")
  )

(defun macrolet-form-p (x) (typep x 'macrolet-form))


(defclass symbol-macrolet-form (vbinding-form implicit-progn)
  ()
  (:documentation "The SYMBOL-MACROLET-FORM Class.")
  )

(defun symbol-macrolet-form-p (x) (typep x 'symbol-macrolet-form))


(defclass let-form (vbinding-form implicit-progn)
  ()
  (:documentation "The LET-FORM Class.")
  )

(defun let-form-p (x) (typep x 'let-form))


(defclass let*-form (vbinding-form implicit-progn)
  ()
  (:documentation "The LET*-FORM Class.")
  )

(defun let*-form-p (x) (typep x 'let*-form))


;;;---------------------------------------------------------------------------
;;; Function forms.

(defclass function-form (form)
  ((funct :accessor form-function
          :initarg :function
          :initarg :name)
   (type :initform 'function)
   )
  (:documentation "The FUNCTION-FORM Class.

The instances of this class represent the (FUNCTION <function-name>) forms.")
  )

(defun function-form-p (x) (typep x 'function-form))


(defclass lambda-form (function-form implicit-progn)
  ((lambda-list :accessor form-args
                :accessor form-lambda-list
                :initarg :args
                :initarg :lambda-list)
   (funct :initform 'lambda)
   )
  (:documentation "The LAMBDA-FORM Class.

The instances of this class represent LAMBDA espressions.")
  )

(defun lambda-form-p (x) (typep x 'lambda-form))


;;;---------------------------------------------------------------------------
;;; Flow control.

(defclass go-form (form)
  ((name :accessor form-name
         :accessor form-tag
         :initarg :name
         :initarg :tag)
   (enclosing-tagbody :accessor form-tagbody
                      :initarg :enclosing-tagbody)
   )
  (:documentation "The GO-FORM Class.

The instances of ths class are the forms (GO <tag>).")
  )

(defun go-form-p (x) (typep x 'go-form))


(defclass if-form (form)
  ((condition :accessor form-condition
              :initarg :condition)
   (then :accessor form-then
         :initarg :then)
   (else :accessor form-else
         :initarg :else)
   )
  (:documentation "The IF-FORM Class.

Basic conditional IF forms.")
  )

(defun if-form-p (x) (typep x 'if-form))


(defclass selection-form (form)
  ((clauses :accessor form-clauses
            :initarg :clauses
            )
   )
  (:documentation "The SELECTION-FORM Class.

Subclasses of SELECTION-FORM contain a list of 'clauses' (subclasses
of CLAUSE-FORM) representing the code associated to the form specific
syntax.")
  )

(defun selection-form-p (x) (typep x 'selection-form))


(defclass clause-form (form implicit-progn)
  ((selector-form :accessor form-selector
                  :initarg :selector
                  )
   )
  )


(defclass selector-form (selection-form)
  ((selector :initarg :selector
             :accessor selector-form-selection)
   )
  (:documentation "The SELECTOR-FORM CLass.

The superclass of classes representing 'forms with a selector', e.g.,
TYPECASE.")
  )


(defclass cond-form (selection-form)
  ()
  (:documentation "The COND-FORM Class.

A SELECTION-FORM where each 'clause' is interpreted in the 'cond' sense.")
  )

(defun cond-form-p (x) (typep x 'cond-form))


(defclass case-form (selector-form) ())

(defun case-form-p (x) (typep x 'case-form))


(defclass ccase-form (selector-form) ())

(defun ccase-form-p (x) (typep x 'ccase-form))


(defclass ecase-form (selector-form) ())

(defun ecase-form-p (x) (typep x 'ecase-form))


(defclass typecase-form (selector-form) ())

(defun typecase-form-p (x) (typep x 'typecase-form))


(defclass etypecase-form (selector-form) ())

(defun etypecase-form-p (x) (typep x 'etypecase-form))


;;;---------------------------------------------------------------------------
;;; Multiple values.

(defclass mvb-form (vbinding-form implicit-progn)
  ((values-form :accessor form-values-form
                :initarg :values-form)
   )
  (:documentation "The MVB-FORM CLass.

Representation for MULTIPLE-VALUE-BINDs forms.")
  )

(defun mvb-form-p (x) (typep x 'mvb-form))

(defun multiple-value-bind-form-p (x) (typep x 'mvb-form))


(defclass multiple-value-call-form (form implicit-progn)
  ;; Misnomer. 'implicit-progn' should be 'forms-sequence'.
  ((funct :accessor form-function
          :initarg :function)
   )
  (:documentation "The MULTIPLE-VALUE-CALL-FORM CLass.

Representation for MULTIPLE-VALUE-CALL forms.")
  )

(defun multiple-value-call-form-p (x) (typep x 'multiple-value-call-form))


(defclass multiple-value-prog1-form (form implicit-progn)
  ()
  (:documentation "The MULTIPLE-VALUE-PROG1-FORM CLass.

Representation for MULTIPLE-VALUE-PROG1 forms.")
  )

(defun multiple-value-prog1-form-p (x) (typep x 'multiple-value-prog1-form))


;;;---------------------------------------------------------------------------
;;; Other special operators.

(defclass load-time-value-form (form)
  ((ltv-form :accessor form-load-time-form
             :initarg :load-time-form)
   (read-only :accessor is-read-only
              :accessor read-only-p
              :initarg :read-only-p
              :initarg :is-read-only)
   )
  (:documentation "The LOAD-TIME-VALUE-FORM CLass.

Representation for LOAD-TIME-VALUE forms.")
  )

(defun load-time-value-form-p (x) (typep x 'load-time-value-form))


(defclass locally-form (form implicit-progn)
  ((decls :accessor form-declarations
          :initarg :declarations)
   )
  (:documentation "The LOCALLY-FORM Class.

Representation for LOCALLY forms.  Instances of this class contain a
list of declarations, accessible via FORMS-DECLARATIONS.")
  )

(defun locally-form-p (x) (typep x 'locally-form))


(defclass quote-form (constant-form)
  ()
  (:documentation "The QUOTE-FORM Class.

The class representing quoted expressions.")
  )

(defun quote-form-p (x) (typep x 'quoted-form))


(defclass return-from-form (form)
  ((name :accessor form-name
         :initarg :name)
   (result :accessor form-result
           :initarg :result)
   (enclosing-block :accessor form-enclosing-block
                    :accessor form-block
                    :initarg :block)
   )
  (:documentation "The RETURN-FROM-FORM CLass.

Instances representing RETURN-FROM expressions.  THe slots NAME and
RESULT represent the syntactic elements of th form, while the
ENCLOSING-BLOCK is a link to the actual form where NAME is named.

Notes:

The ENCLOSING-BLOCK slot is currently initialized in a possibly
improper way. Do not use.")
  )

(defun return-from-form-p (x) (typep x 'return-from-form))


(defclass the-form (form)
  ((type-cast :accessor form-type-cast
              :initarg :type)
   (form :accessor form-expr
         :initarg :expr)
   )
  (:documentation "The THE-FORM Class.

The class of instances of the 'the' form.  The accessors
FORM-TYPE-CAST and FORM-EXPR can be used to access the parts of this
class instances.")
  )

(defun the-form-p (x) (typep x 'the-form))


;;;---------------------------------------------------------------------------
;;; PROGx forms.

(defclass progn-form (form implicit-progn)
  ()
  (:documentation "The PROGN-FORM Class.

The instances of this class are (PROGN ...) forms.  It assumes
IMPLICIT-PROGN but it used for explicit PROGN forms.")
  )

(defun progn-form-p (x) (typep x 'progn-form))


(defclass progv-form (form implicit-progn)
  ((symbols :accessor form-symbols
            :initarg :symbols)
   (vals :accessor form-values
         :initarg :values)
   )
  (:documentation "The PROGV-FORM Class.")
  )

(defun progv-form-p (x) (typep x 'progv-form))


;;;---------------------------------------------------------------------------
;;; Assignment forms.

(defclass assignment-form (form)
  ((places :accessor form-places
           :initarg :places)
   (vals :accessor form-values
         :initarg :values)
   )
  (:documentation "The ASSIGNMENT-FORM CLass.

The superclass of all 'assignment' forms.")
  )

(defun assignment-form-p (x) (typep x 'assignment-form))


(defclass set-form (assignment-form) ()
  (:documentation "The SET-FORM Class."))

(defun set-form-p (x) (typep x 'set-form))


(defclass setq-form (assignment-form)
  ((places :initarg :symbols))
  (:documentation "The SET-FORM Class.

An assignment form with a different keyword initializer.")
  )

(defun setq-form-p (x) (typep x 'setq-form))


(defclass setf-form (assignment-form) ()
  (:documentation "The SETF-FORM Class.")
  )

(defun setf-form-p (x) (typep x 'setf-form))


;;;---------------------------------------------------------------------------
;;; tagbody and prog/prog*

(defclass tagbody-form (form)
  ((tagbody :accessor form-body
            :initarg :body
            )
   (tags :accessor form-tags
         :initarg :tags
         :initform ()
         )
   )
  (:documentation "The TAGBODY-FORM Class.

The class of instances of 'tagbody' forms.  The slot TAGBODY (accessed
via FORM-BODY) contains the list of subforms, including the 'go-tags'
that are parsed specially.  The 'go-tags' are held in the TAGS slot
and accessed via the FORM-TAGS accessor.")
  )

(defun tagbody-form-p (x) (typep x 'tagbody-form))


(defclass prog-form (vbinding-form tagbody-form)
  ((body-env :accessor form-body-env
             :initarg :body-env
             :initform () #| (make-env) |#
             )
   )
  (:documentation "The PROG-FORM Class.

The class of 'prog' forms.  Since it is also a VBINDING-FORM, it also
has an associated 'form environment', accessible via FORM-BODY-ENV.")
  )

(defun prog-form-p (x) (typep x 'prog-form))


(defclass prog*-form (prog-form)
  ()
  (:documentation "The PROG-FORM Class.

See Also:

PROG-FORM")
  )

(defun prog*-form-p (x) (typep x 'prog*-form))


;;;---------------------------------------------------------------------------
;;; Error handling forms.

(defclass unwind-protect-form (form)
  ((protected-form :accessor form-protected-form
                   :initarg :protected-form
                   )
   (cleanup-forms :accessor form-cleanup-forms
                  :initarg :cleanup-forms)
   )
  (:documentation "The UNWIND-PROTECT-FORM Class")
  )


(defclass error-clause-form (form implicit-progn) ; Should inherit for SELECTION-CLAUSE-FORM.
  ((datum :accessor form-datum
          :initarg :datum)

   (lambda-list :accessor form-lambda-list
                :accessor form-args
                :initarg :lambda-list
                :initarg :args)
   )
  (:documentation "The ERROR-CLAUSE-FORM Class.")
  )


(defclass condition-case-form (form)
  ((handled-form :accessor form-handled-form
                 :initarg :handled-form
                 )
   (clauses :accessor form-clauses
            :initarg :clauses
            )
   )
  (:documentation "The CONDITION-CLAUSE-FORM Class.")
  )


(defclass handler-case-form (condition-case-form) ()
  (:documentation "The HANDLER-CASE-FORM Class.")
  )


(defclass restart-case-form (condition-case-form) ()
  (:documentation "The RESTART-CASE-FORM Class.")
  )
  


(defclass handler-bind-form (fbinding-form implicit-progn) ()
  (:documentation "The HANDLER-BIND-FORM Class.")
  )

(defclass restart-bind-form (fbinding-form implicit-progn) ()
  (:documentation "The RESTART-BIND-FORM Class.")
  )


;;;;---------------------------------------------------------------------------
;;;; Definition forms.

(defclass function-definition-form (lambda-form)
  ()
  (:documentation "The FUNCTION-DEFINITION-FORM CLass.")
  )


(defclass macro-definition-form (function-definition-form)
  ((type :initform t))
  (:documentation "The MACRO-DEFINITION-FORM CLass.")
  )


(defclass definition-form (form)
  ((name :accessor definition-form-name
         :accessor form-name
         :initarg :name)
   )
  (:documentation "The DEFINITION-FORM CLass.

The superclass of all 'defining' forms.")
  )

(defun definition-form-p (x) (typep x 'definition-form))


(defclass definition-lambda-list-form (definition-form implicit-progn)
  ((lambda-list :accessor lambda-definition-form-lambda-list
                :initarg :lambda-list
                :initform ())
   )
  (:documentation "The DEFINITION-LAMBDA-LIST-FORM CLass.

The superclass of all 'defining' forms that have a 'lambda list'.")
  )

(defun definition-lambda-list-form-p (x) (typep x 'definition-lambda-list-form))


(defclass definition-code-form (definition-lambda-list-form)
  ()
  (:documentation "The DEFINITION-CODE-FORM CLass.

The superclass of all 'defining' forms that have a 'lambda list' and a body.")
  )

(defun definition-code-form-p (x) (typep x 'definition-lambda-list-form))


(defclass def-symbol-ref-form (definition-form)
  ((value :accessor form-value
          :initarg :value)
   (doc-string :accessor doc-string
               :initarg :doc-string
               :initform "")
   )
  (:documentation "The DEF-SYMBOL-REF-FORM Class.

The superclass of forms defining association to names.")
  )


(defclass defvar-form (def-symbol-ref-form)
  ((name :accessor defvar-form-name)
   (value :accessor defvar-form-value)
   (doc-string :accessor defvar-form-doc-string)
   )
  (:documentation "The DEFVAR-FORM Class.")
  )


(defclass defparameter-form (defvar-form)
  ((name :accessor defparameter-form-name)
   (value :accessor defparameter-form-value
          :initform (error "No initial value provided to DEFPARAMETER."))
   (doc-string :accessor defparameter-form-doc-string)
   )
  (:documentation "The DEFPARAMETER-FORM Class.")
  )


(defclass defconstant-form (definition-form)
  ((name :accessor defconstant-form-name)
   (value :accessor defconstant-form-value
          :initform (error "No initial value provided to DEFCONSTANT."))
   (doc-string :accessor defconstant-form-doc-string)
   )
  (:documentation "The DEFVAR-FORM Class.")
  )


(defclass defun-form (definition-code-form)
  ((name :accessor defun-form-name)
   (lambda-list :accessor defun-form-lambda-list)
   )
  (:documentation "The DEFUN-FORM Class")
  )


(defclass defmacro-form (definition-code-form)
  ((name :accessor defmacro-form-name)
   (lambda-list :accessor defmacro-form-lambda-list)
   )
  (:documentation "The DEFMACRO-FORM Class")
  )


(defclass defgeneric-form (definition-lambda-list-form)
  ((name :accessor defgeneric-form-name)
   (lambda-list :accessor defgeneric-form-lambda-list)
   (options :reader defgeneric-form-options
            :initarg :options
            :initform ()
            :type list)
   (methods :reader defgeneric-form-methods
            :initarg :methods
            :initform ()
            :type list)
   )
  (:documentation "The DEFGENERIC-FORM Class")
  )


(defclass defmethod-form (definition-code-form)
  ((name :accessor defgeneric-form-name
         :accessor defmethod-form-name)
   (lambda-list :accessor defmethod-form-lambda-list)
   (qualifiers :accessor defmethod-form-qualifiers
               :initarg :qualifiers
               :initform ())
   )
  (:documentation "The DEFMETHOD-FORM Class")
  )


(defclass define-compiler-macro-form (defmacro-form)
  ((name :accessor define-compiler-macro-form-name)))


(defclass define-modify-macro-form (definition-lambda-list-form)
  ((name :accessor define-modify-macro-form-name)
   (fun :accessor define-modify-macro-function
        :initarg :function
        :initform 'identity)
   (docstring :reader define-modify-macro-function-docstring
              :initarg :documentation
              :initform nil)
   )
  (:documentation "The DEFINE-MODIFY-MACRO-FORM Class.")
  )


(defun is-define-modify-macro-form (x)
  (typep x 'define-modify-macro-form))


(defun define-modify-macro-form-p (x)
  (typep x 'define-modify-macro-form))


(defclass defstruct-form (definition-form)
  ((name :accessor defstruct-form-name)
   (options :accessor defstruct-form-options
            :initarg :options)
   (slots :accessor defstruct-form-slots
          :initarg :slots)
   )
  (:documentation "The DEFSTRUCT-FORM Class.

Instances of the DEFSTRUCT-FORM class are created by parsing (via
PARSE) structure definition forms.  The parsing of structure
definition forms changes the environment, which is returned by PARSE
as second value, by adding the declarations of the functions --
constructors, copier, predicate -- that DEFSTRUCT normally
automatically defines.")
  )


(defclass defclass-form (definition-form)
  ((name :accessor defclass-form-name)
   (superclasses :accessor defclass-form-superclasses
                 :initarg :superclasses
                 :initform ())
   (slots :accessor defclass-form-slots
          :initarg :slots)
   (options :accessor defclass-form-options
            :initarg :options)
   )
  (:documentation "The DEFCLASS-FORM Class.")
  )


(defclass define-method-combination-form (definition-form)
  ((name :accessor define-method-combination-form-name))
  (:documentation "The DEFINE-METHOD-COMBINATION-FORM Class.")
  )


(defclass define-symbol-macro-form (definition-form)
  ((name :accessor define-symbol-macro-form-name))
  (:documentation "The DEFINE-SYMBOL-MACRO-FORM Class.")
  )


(defclass define-setf-expander-form (definition-form)
  ((name :accessor define-setf-expander-form-name))
  (:documentation "The DEFINE-SETF-EXPANDER-FORM Class.")
  )


(defclass defsetf-form (definition-form)
  ((name :accessor defsetf-form-name))
  (:documentation "The DEFSETF-FORM Class.")
  )


(defclass defpackage-form (definition-form)
  ((name :accessor defpackage-form-name))
  (:documentation "The DEFPACKAGE-FORM Class.")
  )


;;;;---------------------------------------------------------------------------
;;;; Iteration forms

(defclass iteration-form (form) ())

(defclass dovar-form (iteration-form
                      vbinding-form
                      implicit-progn)
  ((return-form :accessor return-form
                :initarg :return)
   )
  )


(defclass dolist-form (dovar-form) ())

(defclass dotimes-form (dovar-form) ())

(defclass do-loop-form (dovar-form)
  ((test :accessor form-test
         :initarg :test)
   )
  )


(defclass do-form (do-loop-form) ())

(defclass do*-form (do-loop-form) ())


(defclass simple-loop-form (iteration-form implicit-progn)
  ()
  (:documentation "The Simple LOOP Form Class.

The class that represents all 'simple' LOOP forms."))

(defun simple-loop-form-p (x) (typep x 'simple-loop-form))


(defclass loop-form (iteration-form vbinding-form)
  ((clauses :accessor loop-clauses
            :initarg :loop-clauses
            :initform ())
   (body-env :accessor form-body-env
             :initarg :body-env
             :initform ())
   )
  (:documentation "The LOOP Form Class.

The class that represents all (extended) LOOP forms.")
  )

(defun loop-form-p (x) (typep x 'loop-form))


;;;;---------------------------------------------------------------------------
;;;; Subform extraction.

(defgeneric clast-element-subforms (form)
  (:documentation "Returns a list of 'subforms' of a given FORM.

The methods of this generic form operate on the different kinds of AST
nodes that are of class FORM.  Other Common Lisp objects have NULL
subforms and LISTs are returned as they are.

Arguments and Values:

form : an instance of class FORM or LIST or a Common Lisp object.
result : a list of 'subforms' (or NIL).
"))


(defmethod clast-element-subforms ((ce constant-form)) ())


(defmethod clast-element-subforms ((ce symbol-ref)) ())


(defmethod clast-element-subforms ((a application))
  (cons (form-operator a)
        (form-args a)))


(defmethod clast-element-subforms ((a macro-application))
  (let ((expansion (form-expansion a)))
    (if expansion
        (list expansion)
        (cons (form-operator a)
              (form-args a)))))


(defmethod clast-element-subforms ((l let-form))
  (list* (form-binds l)
         (form-progn l)))


(defmethod clast-element-subforms ((d declaration-form))
  (declaration-form-declarations d))


(defmethod clast-element-subforms ((tb tagbody-form))
  (form-body tb))


(defmethod clast-element-subforms ((gf go-form))
  (list (form-tag gf)))


(defmethod clast-element-subforms ((b block-form))
  (list* (block-name b)
         (form-progn b)))


(defmethod clast-element-subforms ((b progn-form))
   (form-progn b))


(defmethod clast-element-subforms ((f binding-form))
  (list (form-binds f)
        (form-progn f)))


(defmethod clast-element-subforms ((fdf function-definition-form))
  (list (form-function fdf)
        (form-lambda-list fdf)
        (form-progn fdf)))


(defmethod clast-element-subforms ((df defun-form))
  (list (defun-form-name df)
        (defun-form-lambda-list df)
        (form-progn df)))


(defmethod clast-element-subforms ((df def-symbol-ref-form))
  (list (form-name df)
        (form-value df)
        (doc-string df)
        ))


(defmethod clast-element-subforms ((df dovar-form))
  (append (mapcar #'second (form-binds df))
          (list (return-form df)
                (form-body df))))


(defmethod clast-element-subforms ((df do-form))
  (list (form-binds df)
        (form-test df)
        (return-form df)
        (form-body df)))


(defmethod clast-element-subforms ((slf simple-loop-form))
  (form-progn slf))


(defmethod clast-element-subforms ((slf loop-form))
  (loop-clauses slf))


(defmethod clast-element-subforms ((ce list)) ce) ; Catch all method.


(defmethod clast-element-subforms ((ce t)) ()) ; Catch all method.


;;;; end of file -- clast-elements.lisp --
