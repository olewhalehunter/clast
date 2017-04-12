;;;; -*- Mode: Lisp -*-

;;;; parse-loop-tests.lisp --

;;;; See file COPYING in main folder for licensing and copyright information.

(in-package :clast-tests)

(in-suite :parse-loop)

(test simple-loop
  ;; GIVEN: a simple loop that invokes a function and returns
  (let* ((input
	  '(loop (defun id (x) x) (return)))
	 (output
	  ;; WHEN: the loop is parsed
	  (clast:parse input)))
    ;; THEN: an element of the appropriate type is returned
    (is (eql 'clast:simple-loop-form (type-of output)))
    ;; ... and implicitly progned forms are recorded
    (is (eql 2 (length (clast::form-progn output))))
    ))

(test loop
  ;; GIVEN: a loop 
  (let ((input
	 '(loop for n from 1 to 3 collect n)))
    (multiple-value-bind (element env)
	;; WHEN: the loop is parsed
	(clast:parse input)
      ;; THEN: an element of the appropriate type is returned
      (is (eql 'clast:loop-form (type-of element)))
      (let ((body-env
	     (clast::form-body-env element))
	    (clauses
	     (clast::loop-clauses element)))
	;; ... the body environment is updated
	(is (eql :lexical (variable-information 'n body-env)))
	;; ... and clauses are recorded with name and subclauses
	(is (eql 2 (length clauses)))
	(let* ((for-clause
		(first clauses))
	       (collect-clause
		(second clauses))
	       (for-clause-name
		(clast::loop-clause-name for-clause))
	       (collect-clause-name
		(clast::loop-clause-name collect-clause))
	       (for-clause-subclauses
		(clast::loop-clause-subclauses for-clause))
	       (collect-clause-subclauses
		(clast::loop-clause-subclauses collect-clause)))
	  (is (eql :for for-clause-name))
	  (is (eql :collect collect-clause-name))
	  (is (eql 1 (length for-clause-subclauses)))
	  (is (eql 1 (length collect-clause-subclauses)))
	  )))))
  
;;;; end of file -- parse-loop-tests.lisp --
