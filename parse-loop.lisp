;;;; -*- Mode: Lisp -*-

;;;; parse-iteration.lisp --
;;;; Parsing of iteration constructs.

;;;; See the file COPYING for copyright and license information.

;;;; Portions of code lifted from IT.BESE.ARNESI and CL-WALKER.


(in-package "CLAST")


;;;;===========================================================================
;;;; Prologue.

(defparameter *loop-keywords*
  #(:named 

    :with
    :initially :finally
    :for :as

    :being
    :the

    :hash-key :hash-keys :hash-value :hash-values
    :using

    :do :doing
    :return

    :collect :collecting
    :nconc :nconcing

    :sum :summing
    :count :counting
    :maximize :maximizing
    :minimize :minimizing

    :when :if :unless
    :else
    :end

    :while :until :repeat
    
    :always :thereis :never

    :and

    :of-type
    :into

    ;; ... and some extra ones...

    :record :records
    :tuple :tuples

    :over

    )
  "Set of all the LOOP keywords, plus some extra ones.

The semi-standard SQL querying keywords (RECORD, RECORDS, TUPLE and
TUPLES) and other ones are included in the set."
  )


(defparameter *loop-arithmetic-clause-keywords*
  #(:from :downfrom
    :to :upto :below :above :downto
    :by
    ))


(defparameter *loop-accumulation-clause-keywords*
  #(:collect :collecting
    :nconc :nconcing

    :sum :summing
    :count :counting 
    :maximize :maximizing
    :minimize :minimizing))


(declaim (ftype (function (t) boolean)
                is-loop-keyword
                is-loop-arithmetic-keyword)
         (inline is-loop-keyword
                 is-loop-arithmetic-keyword))

(defun is-loop-keyword (form)
  (and (symbolp form)
       (not (null (find form *loop-keywords*
                        :test #'string-equal)))))


(defun is-loop-arithmetic-keyword (form)
  (and (symbolp form)
       (not (null (find form *loop-arithmetic-clause-keywords*
                        :test #'string-equal)))))


(defun is-loop-accumulation-keyword (form)
  (and (symbolp form)
       (not (null (find form *loop-accumulation-clause-keywords*
                        :test #'string-equal)))))


(declaim (ftype (function (t (or symbol string)) boolean) loop-kwd=)
         (inline loop-kwd=))

(defun loop-kwd= (loop-kwd kwd)
  (declare (type (or symbol string) kwd))
  (and (symbolp loop-kwd)
       (string-equal loop-kwd kwd)))


(declaim (ftype (function ((or symbol string)) keyword) as-loop-kwd)
         (inline as-loop-kwd))

(defun as-loop-kwd (kwd)
  (declare (type (or symbol string) kwd))
  (intern (string kwd) "KEYWORD"))


;;;---------------------------------------------------------------------------
;;; loop-clause

(defclass loop-clause (form) ; named, for, as, when, etc.
  ((name :reader loop-clause-name
         :reader form-name
         :type (or symbol string)
         :initarg :name
         )
   (subclauses :reader loop-clause-subclauses
               :accessor subclauses
               :type list
               :initarg :subclauses
               :initform ()
               )
   )
  ;; :sealed class.
  ;; No subclasses FTTB to avoid hyper-proliferation.

  (:documentation "The Loop Clause Class.

The class of all the LOOP clause forms.")
  )


(defmethod print-object ((lc loop-clause) stream)
  (print-unreadable-object (lc stream :identity t)
    (format stream "LOOP CLAUSE: ~A ~S"
            (loop-clause-name lc)
            (subclauses lc))))


(defun is-loop-clause (x)
  (typep x 'loop-clause))


(defun make-loop-clause (clause-name subclauses &optional source top)
  (make-instance 'loop-clause
                 :name clause-name
                 :subclauses subclauses
                 :top top
                 :source source
                 ))


(defmethod clast-element-subforms ((lc loop-clause))
  (cons (loop-clause-name lc) (subclauses lc)))


(defclass loop-subclause (loop-clause) ()) ; of-type, using, by etc.


(defun is-loop-subclause (x)
  (typep x 'loop-subclause))


(defun make-loop-subclause (clause-name subclauses &optional source top)
  (make-instance 'loop-subclause
                 :name clause-name
                 :subclauses subclauses
                 :top top
                 :source source))


(defun reverse-subclauses (lc)
  (declare (type loop-clause lc))
  (setf (slot-value lc 'subclauses)
        (nreverse  (slot-value lc 'subclauses)))
  lc)


;;;;===========================================================================
;;;; Protocol implementation.

;;;---------------------------------------------------------------------------
;;; parse-form LOOP

(defmethod parse-form ((op (eql 'loop)) form
                       &rest keys
                       &key
                       ;; enclosing-form
                       ;; macroexpand
                       ;; environment
                       &allow-other-keys)
  "The main entry method for the parsing of LOOP forms."
  (if (is-loop-keyword (second form)) ; Should be enough...
      (apply #'parse-extended-loop form keys)
      (apply #'parse-simple-loop form keys)
      ))


(defun parse-simple-loop (loop-form
                          &rest keys
                          &key
                          enclosing-form
                          macroexpand
                          environment
                          &allow-other-keys)
  (declare (ignore macroexpand))
  (values
   (make-instance 'simple-loop-form
                  :top enclosing-form
                  :source loop-form
                  :body-env environment
                  :progn (apply #'parse-form-seq
                                (rest loop-form)
                                keys))
   environment)
  )


(defun parse-extended-loop (loop-form
                            &rest keys
                            &key
                            enclosing-form
                            macroexpand
                            environment
                            &allow-other-keys
                            )
  (declare (ignore enclosing-form macroexpand))

  ;; Now we have a bit of kruft.
  ;; Accumulation variables, those appearing after INTO in
  ;; accumulation clauses should be treated "as WITH variables",
  ;; according to the ANSI spec.
  ;; Unfortunately, the ANSI spec is quite unclear about "where" such
  ;; WITH declarations should go.
  ;;
  ;; I just decide to find such variables and have them in the initial
  ;; environment.
  ;;
  ;; NB. The initial implementation below is wrong as it could "mask"
  ;; some variables in initialization forms, as it is equivalent to
  ;; sticking a number of WITH clauses at the beginning of the LOOP
  ;; form.
  ;; The way to fix it would be to have the accumulation variables
  ;; appear only in the environments used to parse THEN forms in
  ;; FOR/AS-EQUALS clauses and then in regular clauses.
  ;;
  ;; FTTB I keep the simple (and "wrong") implementation, trusting
  ;; that it will not result in too many mishaps, given current
  ;; programming styles.

  (let* ((acc-vars-n-types
          (collect-accumulation-vars loop-form))
         (loop-parsing-env
          (augment-environment
           environment
           :variable (mapcar #'second acc-vars-n-types)
           :declare acc-vars-n-types))
        )
    (start-loop-parsing (rest loop-form)
                        #| environment |#
                        loop-parsing-env
                        keys ())))


(defgeneric parse-loop-clause (clause-kwd
                               form ; (first form) == clause-kwd
                               clauses
                               env
                               &rest keys
                               &key
                               enclosing-form
                               macroexpand
                               environment
                               &allow-other-keys)
  (:documentation "Parses a single LOOP 'clause'.

The methods of the generic function dispatch on the first argument,
which is EQL-specialized on various LOOP keywords.  The FORM is
actually the rmaining LOOP form to be parsed.  CLAUSES are the LOOP
clauses parsed so far and ENV is the resulting environment.

The methods return three values

Arguments and Values:

clause-kwd : the LOOP keyword to be dispatched on.
form       : the part of the LOOP form to be parsed; (first form) == clause-kwd.
clauses    : the clauses parsed so far.
env        : the environment to be used while parsing.
keys       : a plist of keyword arguments (the next ones)
enclosing-form : the form enclosing the LOOP.
macroexpand    : if and how to macroexpand the various (sub)forms.
environment    : the environment to be used while parsing (same as env).
parsed-clause       : the clause just parsed.
remaining-loop-form : the rest of the LOOP form to be parsed.
new-env             : a possibly augmented env.
"
   )

  (:method ((clause-kwd symbol)
            form ; (first form) == clause-kwd
            clauses
            env
            &rest keys
            &key
            &allow-other-keys)
   (if (is-loop-keyword clause-kwd)
       (apply #'parse-loop-clause (as-loop-kwd clause-kwd) form clauses env keys)
       (error 'ast-parse-error
              :format-control "Illegal LOOP top level clause keyword ~A."
              :format-arguments (list clause-kwd))
       ))
  )


(defmacro advance (forms-place &optional (n 1))
  `(setf ,forms-place (nthcdr ,n ,forms-place)))


(defmacro next-form (forms-place &optional next-token)
  (if (and next-token (symbolp next-token))
      `(setf ,next-token (first ,forms-place))
      `(first ,forms-place)))


(defun start-loop-parsing (loop-form env keys clauses
                                     &aux
                                     (loop-kwd (first loop-form)))
  (if (is-loop-keyword loop-kwd)
      (multiple-value-bind (clause rest-loop-form env)
          (apply #'parse-loop-clause loop-kwd loop-form clauses env keys)
        (continue-loop-parsing rest-loop-form
                               (cons clause clauses)
                               env
                               keys))
      (error 'ast-parse-error
             :format-control "Unrcognized LOOP keyword ~A."
             :format-arguments (list loop-kwd)))
  )


(defun continue-loop-parsing (loop-forms
                              clauses
                              env
                              keys)
  (if (null loop-forms)
      (finish-loop-parsing clauses env keys)
      (let ((loop-kwd (next-form loop-forms)))
        (unless (symbolp loop-kwd)
          (error 'ast-parse-error
                 :format-control "Incorrect LOOP keyword ~S."
                 :format-arguments (list loop-kwd)
                 ))
        (multiple-value-bind (loop-clause rest-loop-form env)
            (apply #'parse-loop-clause
                   (intern (symbol-name loop-kwd) "KEYWORD")
                   loop-forms
                   clauses
                   env
                   keys)
          (continue-loop-parsing rest-loop-form
                                 (cons loop-clause clauses)
                                 env
                                 keys)
          ))))


(defun finish-loop-parsing (clauses env keys)
  (destructuring-bind (&key
                       environment
                       enclosing-form
                       macroexpand
                       &allow-other-keys)
      keys
    (declare (ignore macroexpand))
    (values
     (make-instance 'loop-form
                    :top enclosing-form
                    :loop-clauses (nreverse clauses)
                    :body-env env
                    )
     environment)))


;;;===========================================================================
;;; named-clause --

(defgeneric named-clause-p (x)
  (:method ((x loop-clause)) (eq :named (loop-clause-name x)))
  (:method ((x t)) nil))


(defgeneric named-clause-name (x)
  (:method ((x loop-clause)) (first (loop-clause-subclauses x))))


(defmethod parse-loop-clause ((loop-kwd (eql :named))
                              form
                              clauses
                              env
                              &rest keys
                              &key
                              enclosing-form
                              macroexpand
                              environment
                              &allow-other-keys)
  (declare (ignore macroexpand keys))
  (let ((loop-name (second form)))
    (unless (symbolp loop-name)
      (error 'ast-parse-error
             :format-control "Incorrect name for LOOP NAMED clause ~S."
             :format-arguments (list loop-name)
             ))
    (values ; (list loop-kwd loop-name)
     (make-loop-clause :named (list loop-name) enclosing-form)
     (cddr form)
     (augment-environment environment
                          :block (list loop-name)))))


;;;===========================================================================
;;; intially/finally clause --

(defmethod parse-loop-clause ((loop-kwd (eql :initially))
                              form
                              clauses
                              env
                              &rest keys
                              &key
                              enclosing-form
                              macroexpand
                              environment
                              &allow-other-keys)
  (declare (ignore enclosing-form environment macroexpand))
  (parse-loop-compound-forms-clause loop-kwd form clauses env keys))


(defmethod parse-loop-clause ((loop-kwd (eql :finally))
                              form
                              clauses
                              env
                              &rest keys
                              &key
                              enclosing-form
                              macroexpand
                              environment
                              &allow-other-keys)
  (declare (ignore enclosing-form environment macroexpand))
  (parse-loop-compound-forms-clause loop-kwd form clauses env keys))


;;;===========================================================================
;;; variable-clause --
;;; The <variable-clause>'s all deal with "variables" that are
;;; introduced in a LOOP form.  Therefore it is best to have a
;;; sub-element that represents such variables subclauses.

(defclass loop-var-subclause (loop-subclause)
  ((name :initarg :var
         :reader var-form
         :type (or symbol cons)
         )
   (of-type :initarg :of-type
            :initform t
            :reader loop-var-of-type-qualifier)
   )
  )


(defun is-loop-var-subclause (x)
  (typep x 'loop-var-subclause))


(defun make-loop-var-subclause (name of-type subclauses &optional top source)
  (make-instance 'loop-var-subclause
                 :var name
                 :of-type of-type
                 :subclauses subclauses
                 :top top
                 :source source))

(defmethod print-object ((lvc loop-var-subclause) stream)
  (print-unreadable-object (lvc stream :identity t)
    (format stream "LOOP VAR SUBCLAUSE: ~A ~S"
            (var-form lvc)
            (subclauses lvc))))

;;;---------------------------------------------------------------------------
;;; with clause --

(defgeneric with-clause-p (x)
  (:method ((x loop-clause)) (eq :with (loop-clause-name x)))
  (:method ((x t)) nil))


(defmethod parse-loop-clause ((loop-kwd (eql :with))
                              form
                              clauses
                              env
                              &rest keys
                              &key
                              enclosing-form
                              macroexpand
                              environment
                              &allow-other-keys)
  (declare (ignore enclosing-form macroexpand environment))
  (labels ((parse-var-spec (rest-form with-subclauses vars)
             (let ((var-spec (second rest-form))
                   (var-type t)
                   (var-form nil)
                   )
               (unless (or (symbolp var-spec)
                           (consp var-spec))
                 (error 'ast-parse-error
                        :format-control "Incorrect variable name in LOOP ~S."
                        :format-arguments (list var-spec)
                        ))
               (advance rest-form 2)

               (let ((next-token (next-form rest-form)))
                 (when (loop-kwd= next-token :of-type)
                   (setf var-type (second rest-form))
                   (advance rest-form 2)
                   (next-form rest-form next-token)
                   )

                 (setf vars
                       (nconc (associate-var-types var-spec var-type)
                              vars))

                 (when (loop-kwd= next-token '=)
                   (setf var-form (apply #'parse (second rest-form)
                                         :environment env
                                         keys))
                   (advance rest-form 2)
                   (next-form rest-form next-token)
                   )

                 (push (make-loop-var-subclause
                        var-spec
                        var-type
                        (and var-form
                             (list (make-loop-subclause ':=
                                                        (list var-form)))))
                       with-subclauses)

                 ;; One var done, let's see if there are other ones.

                 (if (loop-kwd= next-token 'and)
                     (parse-var-spec rest-form with-subclauses vars)
                     (let* ((variables (mapcan #'rest vars))
                            (declares (remove t vars :key #'first))
                            (ne (augment-environment env
                                                     :variable variables
                                                     :declare declares))
                            )
                       (values (make-loop-clause :with (nreverse with-subclauses))
                               rest-form
                               ne)
                       )))
               ))
           )
    (parse-var-spec form () ())))


;;;---------------------------------------------------------------------------
;;; for/as clause --
;;; The most complicated one...
;;; I could probably break it up in all its components, but I prefer
;;; to jumble it all together and use subfunctions.

(defmethod parse-loop-clause ((loop-kwd (eql :as))
                              form
                              clauses
                              env
                              &rest keys
                              &key
                              &allow-other-keys)
  (apply #'parse-loop-clause :for form clauses env keys))


(defmethod parse-loop-clause ((loop-kwd (eql :for))
                              form
                              clauses
                              env
                              &rest keys
                              &key
                              enclosing-form
                              macroexpand
                              environment
                              &allow-other-keys
                              &aux
                              (rest-form form)
                              (next-token loop-kwd) ; EQL (first form)
                              (subclause-vars ())
                              (subclause-var-types ())
                              )
  (declare (ignore enclosing-form macroexpand environment))

  ;; The variable FOR-AS-CLAUSE in each subfunction below is an
  ;; initially empty list that contains the actual FOR clauses joined
  ;; by AND.
  ;; I.e., FOR V1 ... AND V2 ... AND VK
  ;; Will eventually be kept in the variable as
  ;;
  ;;     (<VK ...> ... <V2 ...> <V1 ...>)
  ;;
  ;; Reversal of this list will happen in
  ;; CONTINUE-PARSE-FOR-AS-SUBCLAUSE.

  (labels ((parse-var-spec (for-as-kwd rest-form for-as-clause)
             (declare (type list for-as-clause))
             (let ((var (second rest-form))
                   (var-type t)
                   )


               (assert (member (as-loop-kwd for-as-kwd)
                               '(:for :as)
                               :test #'eq))

               ;; Ensure subclause-vars and subclause-var-types are
               ;; empty after an 'and'.
               (setf subclause-vars ()
                     subclause-var-types ())

               ;; Remember the clause's vars.
               ;; This could be done just before, but it is clearer to make
               ;; each step explicit.
               (setf subclause-vars 
                     (nconc (flatten var)
                            subclause-vars))

               (advance rest-form 2)
               (next-form rest-form next-token)

               (when (loop-kwd= next-token :of-type)
                 (setf var-type (second rest-form))
                 (advance rest-form 2)
                 (next-form rest-form next-token)
                 (setf subclause-var-types
                       (nconc (associate-var-types var var-type)
                              subclause-var-types)
                       )
                 )

               (push (make-loop-var-subclause var var-type ()) for-as-clause)

               (case (as-loop-kwd next-token)
                 ((:from :downfrom
                   :to :upto :below :above :downto
                   :by
                   )
                  (parse-arithmetic-subclause next-token
                                              rest-form
                                              for-as-clause))

                 ((:in :on)
                  (parse-list-subclause next-token
                                        rest-form
                                        for-as-clause))

                 (:=
                  (parse-equals-subclause next-token
                                          rest-form
                                          for-as-clause))

                 (:across
                  (parse-across-subclause next-token
                                          rest-form
                                          for-as-clause))

                 (:over ; Yep.  This would be nice...
                  (parse-over-subclause next-token
                                        rest-form
                                        for-as-clause))

                 (:being
                  (parse-being-subclause next-token
                                         rest-form
                                         for-as-clause))
                 (t (error 'ast-parse-error
                           :format-control "Unexpected LOOP keyword ~A."
                           :format-arguments (list next-token)))
                 )
               )
             )

           (continue-parse-for-as-subclause (rest-form
                                             next-token
                                             for-as-clause)
             (declare (type list for-as-clause))
             (if (loop-kwd= next-token :and)
                 (parse-var-spec :and rest-form for-as-clause)
                 (let ((ne (augment-environment env
                                                :variable subclause-vars
                                                :declare subclause-var-types))
                       )
                   (values (make-loop-clause :for
                                             (mapcar #'reverse-subclauses
                                                     (nreverse for-as-clause)))
                           rest-form
                           ne)
                   )))
           
           (parse-arithmetic-subclause (next-token rest-form for-as-clause)
             (declare (type list for-as-clause))
             (let ((value-form (second rest-form))
                   (var-clause (first for-as-clause))
                   )

               ;; We always have one artihmetic subclause.
               (push (make-loop-subclause
                      (as-loop-kwd next-token)
                      (list
                       (apply #'parse value-form :environment env keys)))
                     (subclauses var-clause))

               (advance rest-form 2)
               (next-form rest-form next-token)

               ;; We may have a second...
               (when (is-loop-arithmetic-keyword next-token)
                 (push (make-loop-subclause
                        next-token
                        (list
                         (apply #'parse (second rest-form) :environment env keys)))
                       (subclauses var-clause))

                 (advance rest-form 2)
                 (next-form rest-form next-token)
                 )

               ;; ... and possibly a third.
               (when (is-loop-arithmetic-keyword next-token)
                 (push (make-loop-subclause
                        next-token
                        (list
                         (apply #'parse (second rest-form) :environment env keys)))
                       (subclauses var-clause))

                 (advance rest-form 2)
                 (next-form rest-form next-token)
                 )

               (continue-parse-for-as-subclause rest-form
                                                next-token
                                                for-as-clause))
             )

           (parse-list-subclause (next-token rest-form for-as-clause)
             (declare (type list for-as-clause))
             (let ((value-form (second rest-form))
                   (var-clause (first for-as-clause))
                   )

               (push (make-loop-subclause
                      next-token
                      (list
                       (apply #'parse value-form :environment env keys)))
                     (subclauses var-clause))

               (advance rest-form 2)
               (next-form rest-form next-token)

               (when (loop-kwd= next-token :by)
                 (push (make-loop-subclause
                        :by
                        (list
                         (apply #'parse (second rest-form) :environment env keys)))
                       (subclauses var-clause))
                 (advance rest-form 2)
                 (next-form rest-form next-token)
                 )
               (continue-parse-for-as-subclause rest-form
                                                next-token
                                                for-as-clause))
             )

           (parse-equals-subclause (next-token rest-form for-as-clause)
             (declare (type list for-as-clause))
             (let ((value-form (second rest-form))
                   (var-clause (first for-as-clause))
                   )
               (push (make-loop-subclause
                      (as-loop-kwd next-token)
                      (list
                       (apply #'parse value-form :environment env keys)))
                     (subclauses var-clause))
               (advance rest-form 2)
               (next-form rest-form next-token)

               (when (loop-kwd= next-token :then)
                 (push (make-loop-subclause
                        :then
                        (list
                         (apply #'parse (second rest-form) :environment env keys)))
                       (subclauses var-clause))
                 (advance rest-form 2)
                 (next-form rest-form next-token)
                 )
               (continue-parse-for-as-subclause rest-form
                                                next-token
                                                for-as-clause))
             )

           (parse-across-subclause (next-token rest-form for-as-clause)
             (declare (type list for-as-clause))
             (let ((value-form (second rest-form))
                   (var-clause (first for-as-clause))
                   )

               (assert (loop-kwd= next-token :across))

               (push (make-loop-subclause
                      :across
                      (list
                       (apply #'parse value-form :environment env keys)))
                     (subclauses var-clause))
               (advance rest-form 2)
               (next-form rest-form next-token)
               (continue-parse-for-as-subclause rest-form
                                                next-token
                                                for-as-clause))
             )

           (parse-over-subclause (next-token rest-form for-as-clause)
             (parse-across-subclause next-token rest-form for-as-clause)
             )

           (parse-being-subclause (next-token rest-form for-as-clause)
             (declare (type list for-as-clause))
             (let ((value-form (second rest-form))
                   (var-clause (first for-as-clause))
                   )

               (assert (loop-kwd= next-token :being))

               ;; Pointless to add the :BEING subclause, but I do it nevertheless.
               (push (make-loop-subclause :being ())
                     (subclauses var-clause))

               ; (push next-token for-as-clause)

               (case (as-loop-kwd value-form) ; Just a misnomer carried over from above...
                 ((:each :the)
                  (advance rest-form 2)
                  (next-form rest-form next-token))
                 (t
                  (advance rest-form)
                  (next-form rest-form next-token))
                 )

               (case (as-loop-kwd next-token)
                 ((:hash-key :hash-keys :hash-value :hash-values)
                  (parse-hash-subclause next-token
                                        rest-form
                                        for-as-clause))

                 ((:symbol :symbols
                   :present-symbol :present-symbols
                   :external-symbol :external-symbols)
                  (parse-package-subclause next-token
                                           rest-form
                                           for-as-clause))

                 ((:record :records
                   :tuple :tuples)
                  (parse-sql-query-subclause next-token
                                             rest-form
                                             for-as-clause))
                 (t
                  (error 'ast-parse-error
                         :format-control "Unrecognized LOOP BEING keyword ~A."
                         :format-arguments (list next-token)))
                 )
               ))

           (parse-hash-subclause (next-token rest-form for-as-clause)
             (let ((var-form (second rest-form))
                   (var-clause (first for-as-clause))
                   )

               (assert (member (as-loop-kwd next-token)
                               '(:hash-key :hash-keys :hash-value :hash-values)
                               :test #'eq))

               (let ((hash-subclause (first (subclauses var-clause)))
                     (in-of-kwd var-form) ; Renaming...
                     (hash-kv-subclause
                      (make-loop-subclause (as-loop-kwd next-token) ()))
                     )
                 ;; (push next-token for-as-clause)

                 (push hash-kv-subclause
                       (subclauses hash-subclause))

                 (case (as-loop-kwd in-of-kwd)
                   ((:in :of)
                    (advance rest-form 2)
                    (next-form rest-form next-token)
                    )
                   (t
                    (error 'ast-parse-error
                           :format-control "Illigal LOOP BEING syntax at ~A."
                           :format-arguments (list in-of-kwd))))

                 (push (apply #'parse next-token :environment env keys)
                       (subclauses hash-kv-subclause))

                 (advance rest-form)
                 (next-form rest-form next-token)

                 (when (loop-kwd= next-token :using)
                   (let ((using-subclause
                          (make-loop-subclause :using ()))
                         )
                     (push using-subclause
                           (subclauses hash-subclause))
                     (advance rest-form)
                     (next-form rest-form next-token)
             
                     (handler-case
                         (destructuring-bind (htkv-kwd htkv-var)
                             next-token
                           (unless (or (loop-kwd= htkv-kwd :hash-value)
                                       (loop-kwd= htkv-kwd :hash-key))
                             (error 'ast-parse-error
                                    :format-control "Illegal LOOP HASH syntax ~S."
                                    :format-arguments (list next-token)))
                           
                           (push next-token (subclauses using-subclause))
                           (push htkv-var subclause-vars)
                           (advance rest-form)
                           (next-form rest-form next-token)
                           )
                       (ast-parse-error (ape)
                         ;; Re-signal.
                         (error ape))
                       (error (e)
                         (format *error-output* "Error: CLAST:~%")
                         (error e)))
                     ))
                 (reverse-subclauses hash-subclause)
                 (continue-parse-for-as-subclause rest-form
                                                  next-token
                                                  for-as-clause)
                 ))
             )

           (parse-package-subclause (next-token rest-form for-as-clause)
             (let ((var-form (second rest-form))
                   (var-clause (first for-as-clause))
                   )

               (assert (member (as-loop-kwd next-token)
                               '(:symbol :symbols
                                 :present-symbol :present-symbols
                                 :external-symbol :external-symbols)
                               :test #'eq))

               (let ((pkg-subclause (first (subclauses var-clause)))
                     (in-of-kwd var-form) ; Renaming...
                     (pkg-var-subclause
                      (make-loop-subclause (as-loop-kwd next-token) ()))
                     )
                 (push pkg-var-subclause
                       (subclauses pkg-subclause))

                 (case (as-loop-kwd in-of-kwd)
                   ((:in :of)
                    (advance rest-form 2)
                    (next-form rest-form next-token)
                    (push (apply #'parse next-token :environment env keys)
                          (subclauses pkg-var-subclause))
                    )
                   )

                 (advance rest-form)
                 (next-form rest-form next-token)

                 (continue-parse-for-as-subclause rest-form
                                                  next-token
                                                  for-as-clause))
               )
             )

           (parse-sql-query-subclause (next-token rest-form for-as-clause)
             (let ((var-form (second rest-form))
                   (var-clause (first for-as-clause))
                   )

               (assert (member (as-loop-kwd next-token)
                               '(:record :records
                                 :tuple :tuples)
                               :test #'eq))

               (let ((sql-subclause (first (subclauses var-clause)))
                     (in-of-kwd var-form) ; Renaming...
                     (sql-var-subclause
                      (make-loop-subclause (as-loop-kwd next-token) ()))
                     )

                 (push sql-var-subclause
                       (subclauses sql-subclause))

                 (case (as-loop-kwd in-of-kwd)
                   ((:in :of)
                    (advance rest-form 2)
                    (next-form rest-form next-token))
                   (t
                    (error 'ast-parse-error
                           :format-control "Illegal LOOP SQL syntax ~A."
                           :format-arguments (list (second rest-form)))
                    )
                   )

                 (push (apply #'parse next-token :environment env keys) ; Query expr.
                       (subclauses sql-var-subclause))

                 (advance rest-form)
                 (next-form rest-form next-token)

                 (when (loop-kwd= next-token :not-inside-transaction)
                   (let ((nit-subclause
                          (make-loop-subclause :not-inside-transaction
                                               (list
                                                (apply #'parse (second rest-form)
                                                       :environment env
                                                       keys))))
                         )
                     (push nit-subclause (subclauses sql-subclause))
                     (advance rest-form 2)
                     (next-form rest-form next-token)))

                 (when (loop-kwd= next-token :get-all)
                   (let ((ga-subclause
                          (make-loop-subclause :get-all
                                               (list
                                                (apply #'parse (second rest-form)
                                                       :environment env
                                                       keys))))
                         )
                     (push ga-subclause (subclauses sql-subclause))
                     (advance rest-form 2)
                     (next-form rest-form next-token)))

                 (reverse-subclauses sql-subclause)

                 (continue-parse-for-as-subclause rest-form next-token for-as-clause)
                 ))
             )
           )
    (parse-var-spec loop-kwd rest-form ())
    ))


;;;---------------------------------------------------------------------------
;;; main clauses


(defun parse-loop-compound-forms-clause (cfc-kwd form clauses env keys)
  (declare (ignore clauses))
  (labels ((parse-compound-forms (rest-forms compound-forms)
             (if (and rest-forms
                      (not (is-loop-keyword (first rest-forms))))
                 (parse-compound-forms (rest rest-forms)
                                       (cons (apply #'parse (first rest-forms)
                                                    :environment env
                                                    keys)
                                             compound-forms))
                 (values (make-loop-clause (as-loop-kwd cfc-kwd)
                                           (nreverse compound-forms))
                         rest-forms
                         env)))
           )
    (parse-compound-forms (rest form) ()))
  )


;;; unconditional clause --

(defmethod parse-loop-clause ((loop-kwd (eql :do))
                              form
                              clauses
                              env
                              &rest keys
                              &key
                              &allow-other-keys
                              )
  (parse-loop-compound-forms-clause loop-kwd form clauses env keys)
  )


(defmethod parse-loop-clause ((loop-kwd (eql :doing))
                              form
                              clauses
                              env
                              &rest keys
                              &key
                              &allow-other-keys
                              )
  (parse-loop-compound-forms-clause loop-kwd form clauses env keys)
  )


(defmethod parse-loop-clause ((loop-kwd (eql :return))
                              form
                              clauses
                              env
                              &rest keys
                              &key
                              enclosing-form
                              macroexpand
                              environment
                              &allow-other-keys
                              )
  (declare (ignore enclosing-form macroexpand environment))
  (if (is-loop-keyword (second form))
      (error 'ast-parse-error
             :format-control "LOOP keyword RETURN followed by ~
                              another LOOP keyword (~A)."
             :format-arguments (list (second form)))
      (let ((return-clause
             (make-loop-clause :return
                               (list (apply #'parse (second form) keys)))
             #|(list :return
                     (apply #'parse (second form) keys))|#
             )
            )
        (continue-loop-parsing (cddr form)
                               (cons return-clause clauses)
                               env
                               keys))
      ))


;;; accumulation clauses --


(defun parse-accumulation-clause (acc-kwd form clauses env keys)
  (declare (ignore clauses))
  (let* ((acc-clause (make-loop-clause (as-loop-kwd acc-kwd) ()))
         (rest-form form)
         (next-token (first rest-form))
         )
    (if (null (rest form))
        (error 'ast-parse-error
               :format-control "Missing accumulation form after ~A."
               :format-arguments (list acc-kwd))
        (let ((acc-form (second rest-form)))
          (cond ((is-loop-keyword acc-form)
                 (error 'ast-parse-error
                        :format-control "LOOP keyword ~A found after ~A."
                        :format-arguments (list acc-form acc-kwd)))
                (t
                 (push (apply #'parse acc-form 
                              :environment env
                              keys)
                       (subclauses acc-clause))
                 (advance rest-form 2)
                 (next-form rest-form next-token)
                 ))))

    (when (loop-kwd= next-token :into)
      ;; This makes things very tricky!
      ;; The variable should be available in the various environments
      ;; used for parsing the "stepping" functions.
      ;; Of course this calls for a different, delayed, parsing
      ;; routine; maybe based on saving "parsing thunks".

      (push (make-loop-subclause (as-loop-kwd next-token)
                                 (list (apply #'parse (second rest-form)
                                              :environment env
                                              keys)))
            (subclauses acc-clause))

      (advance rest-form 2)
      (next-form rest-form next-token)

      ;; Context dependency...
      (case (as-loop-kwd acc-kwd)
        ((:collect :collecting :nconc :nconcing)
         ;; No type-spec expected.
         (unless (or (null rest-form) ; We are done.
                     (and (is-loop-keyword next-token) ; No <type-spec>.
                          (not (eq (as-loop-kwd next-token) :of-type))))
           (error 'ast-parse-error
                  :format-control "Illegal LOOP syntax after list collection (~A)."
                  :format-arguments (list next-token)))
         )
        (otherwise ; Other collection schemes: sum, count, etc.
         (cond ((and rest-form (is-type-specifier next-token))
                ;; Should also check that it is NIL, T, FIXNUM, FLOAT.
                (push (make-loop-subclause :of-type
                                           (list (apply #'parse next-token
                                                        :environment env
                                                        keys)))
                      (subclauses acc-clause))
                (advance rest-form)
                (next-form rest-form next-token)
                )
               ((loop-kwd= next-token :of-type)
                (let ((ts (second rest-form)))
                  (unless (is-type-specifier ts)
                    (error 'ast-parse-error
                           :format-control "Illegal LOOP syntax: expected type specifier, ~
                                            found ~A."
                           :format-arguments (list ts)))
                  (push (make-loop-subclause :of-type
                                             (list (apply #'parse ts
                                                          :environment env
                                                          keys)))
                        (subclauses acc-clause))
                  (advance rest-form 2)
                  (next-form rest-form next-token)))
               ))
        ))

    (reverse-subclauses acc-clause)
    (values acc-clause
            rest-form
            env)))
      

(defmethod parse-loop-clause ((loop-kwd (eql :collect))
                              form
                              clauses
                              env
                              &rest keys
                              &key
                              &allow-other-keys
                              )
  (parse-accumulation-clause :collect form clauses env keys))


(defmethod parse-loop-clause ((loop-kwd (eql :collecting))
                              form
                              clauses
                              env
                              &rest keys
                              &key
                              &allow-other-keys
                              )
  (parse-accumulation-clause :collect form clauses env keys))


(defmethod parse-loop-clause ((loop-kwd (eql :append))
                              form
                              clauses
                              env
                              &rest keys
                              &key
                              &allow-other-keys
                              )
  (parse-accumulation-clause :append form clauses env keys))


(defmethod parse-loop-clause ((loop-kwd (eql :appending))
                              form
                              clauses
                              env
                              &rest keys
                              &key
                              &allow-other-keys
                              )
  (parse-accumulation-clause :appending form clauses env keys))


(defmethod parse-loop-clause ((loop-kwd (eql :nconc))
                              form
                              clauses
                              env
                              &rest keys
                              &key
                              &allow-other-keys
                              )
  (parse-accumulation-clause :nconc form clauses env keys))


(defmethod parse-loop-clause ((loop-kwd (eql :nconcing))
                              form
                              clauses
                              env
                              &rest keys
                              &key
                              &allow-other-keys
                              )
  (parse-accumulation-clause :nconcing form clauses env keys))


(defmethod parse-loop-clause ((loop-kwd (eql :count))
                              form
                              clauses
                              env
                              &rest keys
                              &key
                              &allow-other-keys
                              )
  (parse-accumulation-clause :count form clauses env keys))


(defmethod parse-loop-clause ((loop-kwd (eql :counting))
                              form
                              clauses
                              env
                              &rest keys
                              &key
                              &allow-other-keys
                              )
  (parse-accumulation-clause :counting form clauses env keys))


(defmethod parse-loop-clause ((loop-kwd (eql :sum))
                              form
                              clauses
                              env
                              &rest keys
                              &key
                              &allow-other-keys
                              )
  (parse-accumulation-clause :sum form clauses env keys))


(defmethod parse-loop-clause ((loop-kwd (eql :summing))
                              form
                              clauses
                              env
                              &rest keys
                              &key
                              &allow-other-keys
                              )
  (parse-accumulation-clause :summing form clauses env keys))


(defmethod parse-loop-clause ((loop-kwd (eql :maximize))
                              form
                              clauses
                              env
                              &rest keys
                              &key
                              &allow-other-keys
                              )
  (parse-accumulation-clause :maximize form clauses env keys))


(defmethod parse-loop-clause ((loop-kwd (eql :maximizing))
                              form
                              clauses
                              env
                              &rest keys
                              &key
                              &allow-other-keys
                              )
  (parse-accumulation-clause :maximizing form clauses env keys))


(defmethod parse-loop-clause ((loop-kwd (eql :minimize))
                              form
                              clauses
                              env
                              &rest keys
                              &key
                              &allow-other-keys
                              )
  (parse-accumulation-clause :minimize form clauses env keys))


(defmethod parse-loop-clause ((loop-kwd (eql :minimizing))
                              form
                              clauses
                              env
                              &rest keys
                              &key
                              &allow-other-keys
                              )
  (parse-accumulation-clause :minimizing form clauses env keys))


;;; conditional clause --
;;; COND-KWD is one of IF, WHEN, UNLESS

(defun parse-selectable-clauses (sel-clause-forms sel-clauses env keys)
  (let ((rest-forms sel-clause-forms)
        (next-token nil)
        )
    (next-form rest-forms next-token)
    
    (multiple-value-bind (sel-clause rest-forms ne)
        (apply #'parse-loop-clause (first sel-clause-forms)
               sel-clause-forms
               ()
               env
               keys)
      (setf sel-clauses (cons sel-clause sel-clauses))
      (next-form rest-forms next-token)
      (cond ((loop-kwd= next-token :and)
             ;; Continue parsing selectable clauses.
             (advance rest-forms)
             ;; Here we could isert some error checking.
             (parse-selectable-clauses rest-forms
                                       sel-clauses
                                       ne
                                       keys))
            (t
             (values (nreverse sel-clauses)
                     rest-forms
                     ne))
            ))))


(defun parse-conditional-clause (cond-kwd cond-form clauses env keys)
  (declare (ignore clauses))

  (assert (member (as-loop-kwd cond-kwd) '(:if :when :unless) :test #'eq))

  (let* ((cond-clause (make-loop-clause (as-loop-kwd cond-kwd) ()))
         (rest-form cond-form)
         (next-token (first rest-form))
         (result-env env)
         )
    (if (null (rest rest-form))
        (error 'ast-parse-error
               :format-control "Illegal LOOP syntax: missing test form after ~A."
               :format-arguments (list cond-kwd))
        (let ((test-form (second rest-form)))
          (cond ((is-loop-keyword test-form)
                 (error 'ast-parse-error
                        :format-control "LOOP keyword ~A found after ~A."
                        :format-arguments (list test-form cond-kwd)))
                (t
                 (push (apply #'parse test-form :environment env keys)
                       (subclauses cond-clause))
                 (advance rest-form 2)
                 (next-form rest-form next-token)
                 ))))

    ;; We have parsed the TEST...
    ;; Now we parse the rest, but remembering that we are parsing a
    ;; context-free sub-lamguage.

    (multiple-value-bind (then-clauses then-rest-form ne)
        (parse-selectable-clauses rest-form () env keys)
      (push (make-loop-subclause :then then-clauses)
            (subclauses cond-clause))
      (setf rest-form then-rest-form
            result-env ne)
      (next-form rest-form next-token)
      (when (loop-kwd= next-token :else)
        (advance rest-form)
        (next-form rest-form next-token)
        (multiple-value-bind (else-clauses else-rest-form ne)
            (parse-selectable-clauses rest-form () ne keys)
          (push (make-loop-subclause :else else-clauses)
                (subclauses cond-clause))
          (setf rest-form else-rest-form
                result-env ne)
          (next-form rest-form next-token)
          ))
      
      (when (loop-kwd= next-token :end)
        (advance rest-form)
        (next-form rest-form next-token))
      
      (reverse-subclauses cond-clause)

      (values cond-clause
              rest-form
              result-env))
    ))
  

(defmethod parse-loop-clause ((loop-kwd (eql :if))
                              form
                              clauses
                              env
                              &rest keys
                              &key
                              &allow-other-keys
                              )
  (parse-conditional-clause loop-kwd form clauses env keys))


(defmethod parse-loop-clause ((loop-kwd (eql :when))
                              form
                              clauses
                              env
                              &rest keys
                              &key
                              &allow-other-keys
                              )
  (parse-conditional-clause loop-kwd form clauses env keys))


(defmethod parse-loop-clause ((loop-kwd (eql :unless))
                              form
                              clauses
                              env
                              &rest keys
                              &key
                              &allow-other-keys
                              )
  (parse-conditional-clause loop-kwd form clauses env keys))


(defmethod parse-loop-clause ((loop-kwd (eql :then))
                              form
                              clauses
                              env
                              &rest keys
                              &key
                              &allow-other-keys
                              )
  (declare (ignore form clauses env))
  (error 'ast-parse-error
         :format-control "Illegal LOOP syntax: THEN keyword found as clause head."))


(defmethod parse-loop-clause ((loop-kwd (eql :else))
                              form
                              clauses
                              env
                              &rest keys
                              &key
                              &allow-other-keys
                              )
  (declare (ignore form clauses env))
  (error 'ast-parse-error
         :format-control "Illegal LOOP syntax: ELSE keyword found as clause head."))


(defmethod parse-loop-clause ((loop-kwd (eql :end))
                              form
                              clauses
                              env
                              &rest keys
                              &key
                              &allow-other-keys
                              )
  (declare (ignore form clauses env))
  (error 'ast-parse-error
         :format-control "Illegal LOOP syntax: END keyword found as clause head."))


;;; termination clause --
;;; TERM-KWD is one of WHILE, UNTIL, REPEAT, ALWAYS, NEVER, THEREIS.

(defun parse-termination-clause (term-kwd
                                 form
                                 clauses
                                 env
                                 keys)
  (declare (ignore clauses))
  (let ((rest-form form) ; Just for simmetry....
        (term-form nil)
        )

    (assert (member (as-loop-kwd term-kwd)
                    '(:while :until :repeat :always :never :thereis)
                    :test #'eq))

    (advance rest-form)
    (next-form rest-form term-form) ; Yeah! Yeah! Yeah! Don't be so
                                    ; fussy dear reader.
    (multiple-value-bind (parsed-form ne)
        (apply #'parse term-form :environment env keys)
      (advance rest-form)
      (values (make-loop-clause (as-loop-kwd term-kwd) (list parsed-form))
              rest-form
              ne)
      )))


(defmethod parse-loop-clause ((loop-kwd (eql :while))
                              form
                              clauses
                              env
                              &rest keys
                              &key
                              &allow-other-keys
                              )
  (parse-termination-clause loop-kwd form clauses env keys))


(defmethod parse-loop-clause ((loop-kwd (eql :until))
                              form
                              clauses
                              env
                              &rest keys
                              &key
                              &allow-other-keys
                              )
  (parse-termination-clause loop-kwd form clauses env keys))


(defmethod parse-loop-clause ((loop-kwd (eql :repeat))
                              form
                              clauses
                              env
                              &rest keys
                              &key
                              &allow-other-keys
                              )
  (parse-termination-clause loop-kwd form clauses env keys))


(defmethod parse-loop-clause ((loop-kwd (eql :always))
                              form
                              clauses
                              env
                              &rest keys
                              &key
                              &allow-other-keys
                              )
  (parse-termination-clause loop-kwd form clauses env keys))


(defmethod parse-loop-clause ((loop-kwd (eql :never))
                              form
                              clauses
                              env
                              &rest keys
                              &key
                              &allow-other-keys
                              )
  (parse-termination-clause loop-kwd form clauses env keys))


(defmethod parse-loop-clause ((loop-kwd (eql :thereis))
                              form
                              clauses
                              env
                              &rest keys
                              &key
                              &allow-other-keys
                              )
  (parse-termination-clause loop-kwd form clauses env keys))



;;;;===========================================================================
;;;; Utilities.


;;; is-type-specifier

(defun is-type-specifier (x)
  (ignore-errors (subtypep x t)))


;;; collect-accumulation-vars
;;; This is a function that pre-processes a LOOP form to extract the
;;; INTO accumulation variables (and relative types).  In fact, these
;;; variables must be available to THEN and other LOOP forms during
;;; execution and therefore to the parse/walk machinery.

(defun collect-accumulation-vars (loop-form &optional (acc-vars ()))
  (let ((accumulation-clause
         (member-if #'is-loop-accumulation-keyword loop-form))
        )
    (if (null accumulation-clause)
        (nreverse acc-vars)
        (destructuring-bind (acc-kwd acc-form &rest rest-form)
            accumulation-clause
          (declare (ignore acc-form))
          (if (not (loop-kwd= (first rest-form) :into))
              (collect-accumulation-vars (rest rest-form) acc-vars)
              (case (as-loop-kwd acc-kwd)
                ((:collect :collecting
                  :nconc :nconcing)
                 (collect-accumulation-vars (cddr rest-form)
                                            (cons (list 'list (second rest-form))
                                                  acc-vars)))
                ((:sum :summing
                  :count :counting
                  :maximize :maximizing
                  :minimize :minimizing)
                 (flet ((guess-type (kwd)
                          ;; This is actually implementation
                          ;; dependent, according to the CLHS.
                          (case kwd
                            ((:count :counting) `(integer 0 ,most-positive-fixnum))
                            (t 'number)))
                        )
                   (destructuring-bind (acc-var &rest rest-form
                                                &aux (form1 (first rest-form)))
                       (rest rest-form)
                     (cond ((and rest-form
                                 (is-loop-keyword form1)
                                 (not (loop-kwd= form1 :of-type))
                                 )
                            (collect-accumulation-vars rest-form
                                                       (cons (list
                                                              (guess-type (as-loop-kwd acc-kwd))
                                                              acc-var)
                                                             acc-vars)))

                           ;; We have a type-spec.
                           ((and rest-form
                                 (is-loop-keyword form1)
                                 (loop-kwd= :of-type form1))
                            (collect-accumulation-vars (cddr rest-form)
                                                       (cons (list
                                                              (second rest-form)
                                                              acc-var)
                                                             acc-vars)))
                     
                           ;; "Short form"
                           ((and rest-form
                                 (not (is-loop-keyword form1)))
                            (if (is-type-specifier form1) ; Actually T, NIL, FLOAT, FIXNUM.
                                (collect-accumulation-vars (rest rest-form)
                                                           (cons (list form1 acc-var)
                                                                 acc-vars))
                                (collect-accumulation-vars rest-form
                                                           (cons (list
                                                                  (guess-type
                                                                   (as-loop-kwd acc-kwd))
                                                                  acc-var)
                                                                 acc-vars))
                                ))
                           (t
                            ;; REST-FORM is empty; we are done.  I
                            ;; could just return here, but it is
                            ;; better to be symmetric for readability.
                            (collect-accumulation-vars nil
                                                       (cons (list
                                                              (guess-type (as-loop-kwd acc-kwd))
                                                              acc-var)
                                                             acc-vars))
                            )
                           ))))
                )))
        )))
        

;;; associate-var-type --
;;; No error checking...

(defun associate-var-types (var-tree type-decls-tree)
  "Associates the types to vars in a destructuring LOOP declaration.

In LOOP you can have variable iterations of the form:

FOR (v1 v2 (v31 v32 . v3r) v4) OF-TYPE (fixnum fixnum (character . T) zot)
= (foo 42)

This function would take (v1 v2 (v31 v32 . v3r) v4)
and (t1 t2 (t31 t32 . t3r) t4) and it would return a list of
pairs ((t1 v1) (t2 v2) (t31 v31) (t32 v32) (t3r v3r) (t4 v4)).

Examples:

CLAST 5 > (associate-var-types '(v1 v2 (v31 v32 . v3r) v4) '(fixnum fixnum (character ugo . T) zot))
((FIXNUM V1) (FIXNUM V2) (CHARACTER V31) (UGO V32) (T V3R) (ZOT V4))

Notes:

This is an internal utility function that does not do any proper error
checking yet.
"
  (labels ((avt (var-tree type-decls-tree decls)
             (typecase var-tree
               (cons
                (cond ((null (first var-tree))
                       (avt (rest var-tree)
                            (rest type-decls-tree)
                            decls))
                      ((atom (first var-tree))
                       (avt (rest var-tree)
                            (rest type-decls-tree)
                            (cons (list (first type-decls-tree)
                                        (first var-tree))
                                  decls)))
                      (t
                       (avt (rest var-tree)
                            (rest type-decls-tree)
                            (avt (first var-tree)
                                 (first type-decls-tree)
                                 decls)))
                      ))
               (null decls)
               (t ; (atom var-tree)
                (cons (list type-decls-tree var-tree) decls))))
           )
    (if (eq type-decls-tree t)
        (mapcar (lambda (v) (list t v)) (flatten var-tree))
        (nreverse (avt var-tree type-decls-tree '() )))))


;;;; end of file -- parse-iteration.lisp --
