;;;; -*- Mode: Lisp -*-

;;;; parse-tests.lisp --

;;;; See file COPYING in main folder for licensing and copyright information.

(in-package :clast-tests)

(in-suite :parse-base)

(test number
  ;; GIVEN: a number
  (let* ((input 9)
	 ;; WHEN: the number is parsed
	 (output
	  (clast:parse input))
	 (top
	  (clast::form-top output))
	 (source
	  (clast::form-source output))
	 (type
	  (first (clast::form-type output))))
    ;; THEN: a CLAST-ELEMENT of appropriate type is returned and the
    ;; string value is recorded correctly
    (is (eql 'clast:constant-form (type-of output)))
    (is (eql 9 (clast::form-value output)))
    ;; .. as is top form, its source and its type
    (is (eql nil top))
    (is (eql 9 source))
    (is (eql 'integer type))
    ))


(test string
  ;; GIVEN: a string
  (let* ((input "something")
	 ;; WHEN: the string is parsed
	 (output
	  (clast:parse input))
	 (top
	  (clast::form-top output))
	 (source
	  (clast::form-source output))
	 (type
	  (first (clast::form-type output))))
    ;; THEN: a CLAST-ELEMENT of appropriate type is returned and the
    ;; string value is recorded correctly
    (is (eql 'clast:constant-form (type-of output)))
    (is (string= "something" (clast::form-value output)))
    ;; .. as is top form, its source and its type
    (is (eql nil top))
    (is (string= "something" source))
    (is (eql 'simple-base-string type))
    ))


(test lambda-application
  ;; GIVEN: a lambda application form
  (let* ((input '((lambda (x) x) 9))
	 ;; WHEN: the form is parsed
	 (output 
	  (clast:parse input))
	 (operator
	  (clast::form-operator output))
	 (args
	  (clast::form-args output))
	 (arg
	  (first args)))
    ;; THEN: an element of the correct type is returned
    (is (eql 'clast::lambda-application (type-of output)))
    ;; ... the list of arguments that are getting applied and the
    ;; lambda function are correctly recorded
    (is (eql 1 (length args)))
    (is (eql 'clast::constant-form (type-of arg)))
    (is (eql 'clast::lambda-form (type-of operator)))
    ))

;; TODO: Add FUNCTIONAL-OPERATOR-APPLICATION-FORM tests.

(test block-form
  ;; GIVEN: a block form
  (let* ((input
	  '(block name (+ 1 1)))
	 ;; WHEN: when the form is parsed 
	 (output
	  (clast:parse input))
	 (name
	  (clast::block-name output))
	 (body
	  (clast::form-body output)))
    ;; THEN: an element of the appropriate type is returned
    (is (eql 'clast::block-form (type-of output)))
    ;; ... and the name of the block and its body forms are correctly
    ;; recorded
    (is (eql 'name name))
    (is (eql 1 (length body)))
    ;; FIXME: Once the code for adding a lexical tag to the environment
    ;; you should also test that the tag is added correctly.
    ))


(test return-from
  ;; GIVEN: a block form that contains a RETURN-FROM invocation
  (let* ((input
	  '(block stop (return-from stop 9)))
	 ;; WHEN: the form is parsed
	 (output
	  (clast:parse input))
	 (body
	  (clast::form-body output))
	 (return-from-form
	  (first body))
	 (return-from-name
	  (clast::form-name return-from-form))
	 (return-from-result-form
	  (clast::form-result return-from-form))
	 (return-from-result-value
	  (clast::form-value return-from-result-form))
	 ;; TODO: Add enclosing block checking when the implementation
	 ;; for adding it is available.
	 ;; (return-from-enclosing-block
	 ;;  (clast::form-enclosing-block return-from-form))
	 )
    ;; THEN: the parsing reaches the return form and an element of the
    ;; appropriate type is returned
    (is (eql 'clast::return-from-form (type-of return-from-form)))
    ;; ... and the name of the block that the return-from form refers
    ;; to is recorded correctly along with the specified return value
    (is (eql 'stop return-from-name))
    (is (eql 9 return-from-result-value))
    ))


;; TODO: Add TAGBODY (form-body form-tags) and GO (form-name
;; enclosing-tagbody) tests after tags information handling in
;; environments is fixed. Here is a test case:
;;
;; (tagbody (go second)
;;  first (print "error")
;;  second (print "ok")))


;; TODO: Once parsing of THROW forms is added, define a test case for
;; both CATCH (form-catch-tag) and THROW. Here is a test a case:
;;
;; (catch 'dummy-tag 1 2 (throw 'dummy-tag 3) 4)


;; TODO: DECLARE is really heavy to test because it requires testing
;; of all possible declaration specifiers (SYMBOL TYPE FTYPE IGNORE
;; IGNORABLE OPTIMIZE INLINE NOT-INLINE SPECIAL DYNAMIC-EXTENT
;; DECLARATION)


(test progn
  ;; GIVEN: a progn form
  (let ((input '(progn
		 (defun id (x) x)
		 (id 9))))
    ;; WHEN: the form is parsed
    (multiple-value-bind (element environment)
	(clast:parse input)
      ;; THEN: an element of the appropriate type is returned
      (is (eql 'clast::progn-form (type-of element)))
      ;; FIXME: Both the environment that gets returned from a call to
      ;; PARSE and the body environment associated with the element
      ;; that gets returned do not get augmented with the definitions
      ;; that occur within the PROGN. This is wrong and should be
      ;; fixed.
      (is (eql :function (function-information 'id environment)))
      ;; ... the body of the form is correctly recorded together with
      ;; its associated environment
      (let ((body (clast::form-body element))
	    (body-env (clast::form-body-env element)))
	(is (eql 2 (length body)))
	(is (eql :function (function-information 'id body-env)))
	))))


;; TODO: Add PROGV when its implementation is complete


;; TODO: Deferring testing of PROG and PROG* because they
;; significantly complex.


(test eval-when
  ;; GIVEN: an eval-when form that declares a function and call PRINT
  (let* ((input
	  '(eval-when (:compile-toplevel :load-toplevel :execute)
	    (defun id (x) x)
	    (print "something")))
	 ;; WHEN: the form is parsed
	 (output
	  (clast:parse input))
	 (situations
	  (clast::form-situations output))
	 (body
	  (clast::form-body output))
	 (body-env
	  (clast::form-body-env output)))
    ;; THEN: an element of the appropriate type is returned
    (is (eql 'eval-when-form (type-of output)))
    ;; ... the form's situations list is correctly recorded
    (is (equal situations
     	       (list :compile-toplevel :load-toplevel :execute)))
    ;; ... as both forms in its body
    (is (eql 2 (length body)))
    ;; FIXME: The current implementation does not augment the body
    ;; environment while parsing its body forms.
    (is (eql :function (function-information 'id body-env)))
    ;; TODO: when a strategy for adding elements to the returned
    ;; environment is decided, update this test to reflect that.
    ))


(test declaim
  ;; GIVEN: a declaim form with two declarations
  (let ((input
	 '(declaim (special first) (special second))))
    ;; WHEN: the form is parsed
    (multiple-value-bind (element environment)
	(clast:parse input)
      ;; THEN: the returned environment records information regarding
      ;; both declarations
      (is (eql :special
	       (variable-information 'first environment)))
      (is (eql :special
	       (variable-information 'second environment)))
      (let* ((declarations
	      (clast::declaration-form-declarations element))
	     (declaration
	      (first declarations))
	     (specifier-identifier
	      (clast::declaration-specifier-identifier declaration))
	     ;; FIXME: The resulting environment slot is never
	     ;; initialized nor considered during parsing. This causes
	     ;; this test failure.
	     (resulting-environment
	      (clast::declaration-form-resulting-environment element)))
	;; ... both declarations are associated to the element
	(is (eql 2
		 (length declarations)))
	;; ... with the appropriate identifier
	(is (eql 'special
		 specifier-identifier))
	;; ... and the resulting environment associate with the
	;; returned element is correctly augmented
	(is (eql :special
		 (variable-information 'first resulting-environment)))
	(is (eql :special
		 (variable-information 'second resulting-environment)))
	))))


(test flet
  ;; GIVEN: a flet form that declares a local function and that, in
  ;; its body declares a top level function
  (let ((input
	 '(flet ((local-id (x) x))
	   (local-id 9)
	   (defun id (x) x))))
    ;; WHEN: the form is parsed
    (multiple-value-bind (element environment)
	(clast:parse input)
      ;; THEN: an element of the appropriate type should be returned
      (is (eql 'clast::flet-form (type-of element)))
      ;; ... the functions that were defined locally should not be in the
      ;; return environment (FIXME)
      (is (eql nil (function-information 'local-id environment)))
      ;; ... but progned definitions should
      (is (eql :function (function-information 'id environment)))
      (let ((binds
	     (clast::form-binds element))
	    (body
	     (clast::form-body element))
	    (body-env
	     (clast::form-body-env element)))
	;; ... body forms should be recorded correctly
	(is (eql 2 (length body)))
	;; ... as bindings, that should also be added to the body
	;; environment
	(is (eql :function (function-information 'local-id body-env)))
	))))

;; LABELS is equivalent to FLET except that the scope of the defined
;; function names for LABELS encompasses the function definitions
;; themselves as well as the body. Since this difference is not
;; recorded by the library fttb the test is the exact same.
(test labels
  (let ((input
	 '(labels ((local-id (x) x))
	   (local-id 9)
	   (defun id (x) x))))
    (multiple-value-bind (element environment)
	(clast:parse input)
      (is (eql 'clast::labels-form (type-of element)))
      ;; ... the functions that were defined locally should not be in the
      ;; return environment (FIXME)
      (is (eql nil (function-information 'local-id environment)))
      (is (eql :function (function-information 'id environment)))
      (let ((binds (clast::form-binds element))
	    (body (clast::form-body element))
	    (body-env (clast::form-body-env element)))
	(is (eql 2 (length body)))
	(is (eql :function (function-information 'local-id body-env)))
	))))


;; TODO: Add FUNCTION parsing tests once its implementation is
;; complete.


;; FIXME: the PARSE method that specializes on LAMBDA delegates work
;; to PARSE-LAMBDA-FORM, which does not return a tuple (CLAST-ELEMENT,
;; ENVIRONMENT) but just a CLAST-ELEMENT.
;; 
;; LAMBDA-FORM (form-lambda-list form-function body body-env)


(test if
  ;; GIVEN: an if form with two branches
  (let* ((input
	  '(if t 1 2))
	 ;; WHEN: the form is parsed
	 (output
	  (clast:parse input))
	 (condition
	  (clast::form-condition output))
	 (then
	  (clast::form-then output))
	 (else
	  (clast::form-else output)))
    ;; THEN: an element of the appropriate type is returned
    (is (eql 'clast::if-form (type-of output)))
    ;; ... and the if condition form is recorded correctly
    (is (eql 'clast::constant-ref (type-of condition)))
    ;; ... as both branches
    (is (eql 'clast::constant-form (type-of then)))
    (is (eql 'clast::constant-form (type-of else)))
    ))


(test cond
  ;; GIVEN: a cond form with two clauses, where the first clause
  ;; contains two subforms
  (let* ((input
	  '(cond
	    (t (print 1) (print 2))
	    (nil 1)))
	 ;; WHEN: the form is parsed
	 (output
	  (clast:parse input))
	 (clauses
	  (clast::form-clauses output))
	 (clause
	  (first clauses))
	 (clause-body
	  (clast::form-body clause))
	 (clause-selector
	  (clast::form-selector clause)))
    ;; THEN: an element of the appropriate type is returned
    (is (eql 'clast:cond-form (type-of output)))
    ;; ... and both clauses are recorded
    (is (eql 2 (length clause-body)))
    ;; ... with their selectors
    (is (eql 'clast::constant-ref (type-of clause-selector)))
    ))


(test case
  ;; GIVEN: a case form with three clauses and a constant as selection
  (let* ((input
	  '(case 4 (1 "hey") (2 "ho") (3 "letsgo")))
	 ;; WHEN: the form is parsed
	 (output
	  (clast:parse input))
	 (selection
	  (clast::selector-form-selection output))
	 (clauses
	  (clast::form-clauses output)))
    ;; THEN: an element of the appropriate type is returned
    (is (eql 'clast::case-form (type-of output)))
    ;;; ... the selection is recorded correctly
    (is (eql 'clast:constant-form (type-of selection)))
    ;; ... as all clauses
    (is (eql 3 (length clauses)))
    ))


(test let
  ;; GIVEN: a let form that declares two local variables and with
  ;; three body forms, where one consists of a function declaration
  (let ((input
	 '(let ((x 1) (y 2))
	   (defun id (x) x)
	   (print x)
	   (print y))))
    (multiple-value-bind (element environment)
	(clast:parse input)
      ;; THEN: an element of the appropriate type is returned and the
      ;; returned enviroment contains the function definition
      (is (eql 'clast:let-form (type-of element)))
      (is (eql :function (function-information 'id environment)))
      (let ((binds
	     (clast::form-binds element))
	    (body
	     (clast::form-body element))
	    (body-env
	     (clast::form-body-env element)))
	;; ... both local variables are recorded as its body forms
	(is (eql 2 (length binds))) 
	(is (eql 3 (length body)))
	;; ... and the body environment is correctle augmented with
	;; declarations of both local variables and the function
	;; definition
	(is (eql :lexical (variable-information 'x body-env)))
	(is (eql :lexical (variable-information 'y body-env)))
	;; FIXME: as with most other parsing functions declarations from
	;; implicitly progned forms are not tracked.
	(is (eql :function (function-information 'id body-env)))
	))))


(test macrolet
  ;; GIVEN: a macrolet form that defines a local id macro that gets
  ;; executed in its body.
  (let ((input '(macrolet ((id (x) x))
		 (id 9))))
    ;; WHEN: the form is parsed
    (multiple-value-bind (element environment)
	(clast:parse input)
      ;; THEN: an element of the appropriate type is returned and the
      (is (eql 'clast::macrolet-form (type-of element)))
      ;; TODO: Update this test once the parsing of progn forms is
      ;; corrected across all different parsing function in order to
      ;; support nested environment additions. Here you should check
      ;; for handling of those additions.
      (let* ((binds
	      (clast::form-binds element))
	     (bind
	      (first binds))
	     (body
	      (clast::form-body element))
	     (body-env
	      (clast::form-body-env element)))
	;; ... the form bindings are correctly recorded as its body
	;; forms
	(is (eql 'clast:macro-definition-form (type-of bind)))
	(is (eql 'clast:macro-application (type-of (first body))))
	;; ... and the body environment is correctly augmented
	(is (eql :macro (function-information 'id body-env)))
	;; ... add check for potential nested definitions here
	))))


(test multiple-value-bind
  ;; GIVEN: an mvb form that binds two variables and with two forms body
  (let ((input '(multiple-value-bind (first second)
		 (values 1 2)
		 (defun id (x) x)
		 (print first)
		 (print second))))
    ;; WHEN: the form is parsed
    (multiple-value-bind (element environment)
	(clast:parse input)
      ;; THEN: an element of the appropriate type is returned and the
      ;; environment is correctly augmented with definitions that
      ;; occur in the form's body
      (is (eql 'clast::mvb-form (type-of element)))
      (is (eql :function (function-information 'id environment)))
      (let ((binds
	     (clast::form-binds element))
	    (values-form
	     (clast::form-values-form element))
	    (body
	     (clast::form-body element))
	    (body-env
	     (clast::form-body-env element)))
	;; ... and both bindings and the values form are correctly
	;; recorded
	(is (equal '(first second) binds))
	(is (eql 'clast::function-application (type-of values-form)))
	;; ... as body forms and the body environment
	(is (eql 3 (length body)))
	(is (eql :function (function-information 'id body-env)))
	))))


;; TODO: Once implementation of SETQ and SETF parsing functions is
;; complete, add tests for them.


;; TODO: Once addition tags to the environment is fixed, add tests for
;; DOLIST, DOTIMES and DO.

;;;; end of file -- parse-tests.lisp --
