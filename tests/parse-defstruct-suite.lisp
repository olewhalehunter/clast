;;;; -*- Mode: Lisp -*-

;;;; parse-defstruct-tests.lisp --

;;;; See file COPYING in main folder for licensing and copyright information.

(in-package :clast-tests)

(in-suite :parse-defstruct)

(test defstruct
  ;; GIVEN: a struct with one slot and one option
  (let ((input
	 '(defstruct (person :named)
	   (name "Matteo" :type string))))
    ;; WHEN: the struct is parsed
    (multiple-value-bind (element environment)
	(clast:parse input)
      (let ((name
	     (clast::defstruct-form-name element))
	    (options
	     (clast::defstruct-form-options element))
	    (slots
	     (clast::defstruct-form-slots element)))
      ;; THEN: a form of the right type is returned ...
      (is (eql 'clast:defstruct-form (type-of element)))
      ;; ... name, slots and options of the DEFSTRUCT are
      ;; recorded correctly
      (is (eql 'person name))
      (is (eql 1 (length options)))
      (is (eql 1 (length slots)))
      ;; ... and the environment is augmented with the appropriate
      ;; create, copy, type-check and slot accessor functions
      (is (eql :function
	       (function-information 'make-person environment)))
      (is (eql :function
	       (function-information 'copy-person environment)))
      (is (eql :function
	       (function-information 'person-p environment)))
      (is (eql :function
	       (function-information 'person-name environment)))
      )))


(test defstruct-slots
  ;; GIVEN: a struct with a slot
  (let ((input
	 '(defstruct person
	   (name
	    "Matteo"
	    :type string
	    :read-only t
	    :other 42))))
    ;; WHEN: the struct is parsed
    (multiple-value-bind (element environment)
	(clast:parse input)
      (declare (ignore environment))
      (let* ((slots
	      (clast::defstruct-form-slots element))
	     (slot
	      (first slots))
	     (name
	      (clast::struct-slot-subform-name slot))
	     (initform
	      (clast::struct-slot-subform-initform slot))
	     (type
	      (clast::struct-slot-subform-type slot))
	     (is-read-only
	      (clast::struct-slot-subform-read-only slot))
	     (other-options
	      (clast::struct-slot-subform-other-options slot)))
	;; THEN: the slot's name, init-form, type, read-only flag and
	;; etra options are recorded correctly
	(is (eql 'name name))
	(is (eql 'clast:constant-form (type-of initform)))
	(is (eql 'string (clast::type-specifier-form-spec type)))
	(is (eql t is-read-only))
	(is (equal '(:other 42) other-options))
	)))))


(test defstruct-options-conc-name
  ;; GIVEN: a struct definition that specifies a value for conc-name
  (let ((input
	 '(defstruct (person (:conc-name "PREFIX-"))
	   slot-name)))
    (MULTIPLE-value-bind (element environment)
	;; WHEN: the struct definition is parsed
	(clast:parse input)
      ;; THEN: accessor functions have the same name as their slots,
      ;; prepended with the specified conc-name
      (is (eql :function
	       (function-information 'prefix-slot-name environment)))
      )))


(test defstruct-options-conc-name-nil
  ;; GIVEN: a struct definition that specifies a nil conc-name
  (let ((input
	 '(defstruct (person (:conc-name nil))
	   slot-name)))
    (multiple-value-bind (element environment)
	;; WHEN: the struct definition is parsed
	(clast:parse input)
      ;; THEN: accessor functions have the same name as their slots
      (is (eql :function
	       (function-information 'slot-name environment)))
      )))


(test defstruct-options-constructor
  ;; GIVEN: a struct definition that specifies a constructor option
  (let ((input
	 '(defstruct (person
		      (:constructor constructor-name (param)))
	   slot-name)))
    (multiple-value-bind (element environment)
	;; WHEN: the struct definition is parsed
	(clast:parse input)
      ;; THEN: a constructor with the specified name and arguments is
      ;; add to the environment
      (is (eql :function
	       (function-information 'constructor-name environment)))
      )))


(test defstruct-options-constructor-multiple
  ;; GIVEN: a struct definition that specifies multiple constructor
  ;; options
  (let ((input
	 '(defstruct (person
		      (:constructor first-constructor (param))
		      (:constructor second-constructor (param)))
	   slot-name)))
    (multiple-value-bind (element environment)
	;; WHEN: the struct definition is parsed
	(clast:parse input)
      ;; THEN: a constructor with the specified name and arguments is
      ;; add to the environment
      (is (eql :function
	       (function-information 'first-constructor environment)))
      (is (eql :function
	       (function-information 'second-constructor environment)))
      )))


(test defstruct-options-constructor-nil
  ;; GIVEN: a struct definition that specifies that no constructor
  ;; should be generated
  (let ((input
	 '(defstruct (person (:constructor nil))
	   slot-name)))
    (multiple-value-bind (element environment)
	;; WHEN: the struct definition is parsed
	(clast:parse input)
      ;; THEN: no constructor for the struct is added to the
      ;; environment
      (is (eql nil (function-information 'make-person environment)))
      )))


(test defstruct-options-copier
  ;; GIVEN: a struct definition that specifies a name for the copier
  (let ((input
	 '(defstruct (person (:copier copier-name))
	   slot-name)))
    (multiple-value-bind (element environment)
	;; WHEN: the struct definition is parsed
	(clast:parse input)
      ;; THEN: a copier with the specied name is added to the
      ;; environment
      (is (eql :function
	       (function-information 'copier-name environment)))
      )))


(test defstruct-options-copier-nil
  ;; GIVEN: a struct definition that specifies that no copier should
  ;; be generated
  (let ((input
	 '(defstruct (person (:copier nil))
	   slot-name)))
    (multiple-value-bind (element environment)
	;; WHEN: the struct definition is parsed
	(clast:parse input)
      ;; THEN: no copier for the struct is added to the
      ;; environment
      (is (eql nil (function-information 'copy-person environment)))
      )))


(test defstruct-options-include
  (5am:fail "Parsing of include options from DEFSTRUCT forms is not ~
  yet implemented"))


(test defstruct-options-initial-offset
  ;; GIVEN: a struct definition that specifies a value for its
  ;; initial-offset option
  (let* ((input
	  '(defstruct (person
		       (:initial-offset 1)
		       (:type list))
	    name age))
	 ;; WHEN: the struct definition is parsed
	 (output
	  (clast:parse input))
	 (options
	  (clast::defstruct-form-options output)))
    ;; THEN: it records the two options correctly
    (is (eql 2 (length options)))
    ;; ... with appropriate names and value speciefied
    (let* ((type-option
    	    (first options))
    	   (type-option-name
    	    (clast::struct-option-subform-name type-option))
    	   (type-option-spec
    	    (clast::struct-option-subform-spec type-option)))  

      (is (eql :initial-offset type-option-name))
      (is (equal '(1) type-option-spec)))

    (let* ((initial-offset-option
    	    (second options))
    	   (initial-offset-option-name
    	    (clast::struct-option-subform-name initial-offset-option))
    	   (initial-offset-option-spec
    	    (clast::struct-option-subform-spec initial-offset-option)))

      (is (eql :type initial-offset-option-name))
      (is (equal '(list) initial-offset-option-spec)))
    ))


(test defstruct-options-named
  (let* ((input
	  '(defstruct (person (:type list) :named)))
	 (output
	  (clast:parse input))
	 (options
	  (clast::defstruct-form-options output))
	 (named-option
	  (second options)))

    (is (eql :named (clast::struct-option-subform-name named-option)))
    ))


(test defstruct-options-predicate
  ;; GIVEN: a struct definition that specifies a predicate name option
  (let ((input
	 '(defstruct (person (:predicate predicate-name)))))
    ;; WHEN: the struct definition is parsed
    (multiple-value-bind (element env)
	(clast:parse input)
      ;; THEN: a predicate with the appropriate name is added to the
      ;; environment
      (is (eql :function
	       (function-information 'predicate-name env)))
      ;; ...and the option is recorded correctly
      (let* ((options
	      (clast::defstruct-form-options element))
	     (predicate-option
	      (first options))
	     (predicate-option-name
	      (clast::struct-option-subform-name predicate-option))
	     (predicate-option-spec
	      (clast::struct-option-subform-spec predicate-option)))
      	(is (eql :predicate predicate-option-name))
	(is (equal '(predicate-name) predicate-option-spec))
	))))


;; TODO: Add PRINT-FUNCTION and PRINT-OBJECT options tests. The
;; following form is a suggested input for the PRINT-FUNCTION test
;; case.
;; 
;; '(defstruct (person
;; 	     (:print-function
;; 	      (lambda (p s k) nil))))


(test defstruct-options-type
  ;; GIVEN: a struct definition that specifies a value for its type
  ;; option
  (let ((input
	 '(defstruct (person (:type list)))))
    ;; WHEN the struct definition is parsed
    (multiple-value-bind (element environment)
	(clast:parse input)
      ;; TODO: this test should also verify that when using the
      ;; constructor for the struct an object of the appropriate type is
      ;; generated
      (declare (ignore environment))
      ;; THEN: the option is correctly recorded
      (let* ((options
	      (clast::defstruct-form-options element))
	     (type-option
	      (first options))
	     (type-option-name
	      (clast::struct-option-subform-name type-option))
	     (type-option-spec
	      (clast::struct-option-subform-spec type-option)))
	(is (eql :type type-option-name))
	(is (equal '(list) type-option-spec))
	))))


;;;; end of file -- parse-defstruct-tests.lisp --
