;;;; -*- Mode: Lisp -*-

;;;; parse-defclass-tests.lisp --

;;;; See file COPYING in main folder for licensing and copyright information.

(in-package :clast-tests)

(in-suite :parse-defclass)

(test type
  ;; GIVEN: a simple class definition
  (let* ((input
	  '(defclass person () ()))
	 ;; WHEN: the class is parse
	 (output
	  (clast:parse input)))
    ;; THEN: parsing returns a CLAST-ELEMENT instance of the
    ;; appropriate type
    (is (eql 'defclass-form (type-of output)))))


(test option-documentation
  ;; GIVEN: a class with a documentation class option
  (let* ((input
	  '(defclass person ()
	    ()
	    (:documentation "The PERSON class."))
	   )
	 ;; WHEN: the class is parsed
	 (output
	  (clast:parse input)))

    (let* ((class-option
	    (car (clast::defclass-form-options output)))
	   (class-option-name
	    (clast::class-option-subform-name class-option))
	   (class-option-value
	    (clast::class-option-subform-spec class-option)))
      ;; THEN: the presence and value of the documentation slot is
      ;; recorded correctly.
      (is (eql :documentation
	       class-option-name))
      (is (string= "The PERSON class."
		   (clast::form-value (first class-option-value))))
      )))


(test superclasses
  ;; GIVEN: a class that inherits from some superclass
  (let* ((input
	  '(defclass copier (printer scanner) ()))
	 ;; WHEN: the class is parsed
	 (output
	  (clast:parse input))
	 (superclasses
	  (clast::defclass-form-superclasses output)))
    ;; THEN: the superclasses are correctly recorded
    (is (equal '(printer scanner) superclasses))
    ))


(test simple-slot-options
  ;; GIVEN: a class with a slot that exposes a reader method
  (let ((input
	 '(defclass person ()
	   ((age
	     :allocation :class
	     :initarg :age
	     :initform 0
	     :type integer
	     :documentation "docstring"))
	   )))
    ;; WHEN: the class is parsed
    (multiple-value-bind (element environment)
	(clast:parse input)
      (declare (ignore environment))
      ;; THEN: All slot options are recorded correctly
      (let* ((slot
      	      (car (clast::defclass-form-slots element)))
      	     (slot-options
      	      (clast::class-slot-subform-options slot)))

	(is (eql 5 (length slot-options)))
	
	(dolist (slot-option slot-options)
	  ;; Since the slot-option value for the :initform case is a
	  ;; list of three element (:initform clast-element env), you
	  ;; cannot use DESTRUCTURING-BIND here.
	  (let ((name (first slot-option))
		(value (second slot-option)))
	    (case name
	      (:allocation
	       (is (eql :class value)))
	      (:initarg
	       (is (eql :age value)))
	      (:initform
	       (is (eql 'constant-form (type-of value))))
	      (:type
	       (is (eql 'integer value)))
	      (:documentation
	       (is (string= "docstring" value)))
	      (t (fail "Unexpected slot form found")))
	    ))))))


(test slot-option-reader
  ;; GIVEN: a class with a slot that exposes a reader method
  (let ((input
	 '(defclass person ()
	   ((age
	     :reader age))
	   )))
    ;; WHEN: the class is parsed
    (multiple-value-bind (element environment)
	(clast:parse input)
      ;; THEN: the presence of the slot option is recorded correctly
      ;; and the reader method is added to the environment
      (let* ((slot
      	      (car (clast::defclass-form-slots element)))
      	     (slot-option
      	      (car (clast::class-slot-subform-options slot))))
      	(is (eql :reader
		 (car slot-option))))
      (is (eql :function
	       (function-information 'age environment)))
      )))
      

;;;; end of file -- parse-defclass-tests.lisp --
