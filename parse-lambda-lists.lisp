;;;; -*- Mode: Lisp -*-

;;;; parse-lambda-list.lisp
;;;;
;;;; Special CLAST parsing of Lambda Lists.
;;;; These functions rely on the pre-parsing of lambda lists using the
;;;; PARSE-LL functions library (which eventually will be factored out
;;;; in the CLLLP library).
;;;;
;;;; See the file COPYING in the main folder for copyright and license information.


(in-package "CLAST")


;;; parse-lambda-list --
;;; The lambda list parsing machinery gives me back some specific
;;; information which I use in a kind of dumb way.  Cfr., the use of the
;;; "init forms" from LL-DEFAULT-FORMS.

(defun parse-lambda-list (ll-type lambda-list
                                  &rest keys
                                  &key
                                  enclosing-form
                                  environment
                                  macroexpand
                                  &allow-other-keys
                                  &aux
                                  (parsed-ll
                                   (parse-ll ll-type lambda-list))
                                  (ll-vars
                                   (ll-vars parsed-ll))
                                  (ll-default-forms
                                   (ll-default-forms parsed-ll))
                                  )
  (declare (type lambda-list-type ll-type)
           (type t_lambda-list parsed-ll)
           (type list ll-vars)
           (type list ll-default-forms))
  (declare (ignore enclosing-form macroexpand))

  (let ((ll-env
         (augment-environment environment
                              :variable (copy-list ll-vars)))
        )
    ;; Parsing the init forms in the LL-ENV is not quite right, given
    ;; the left-to-right LET* evaluation rules of CL.
    ;; But, FTTB, it appears to be good enough.
    
    (values
     parsed-ll
     ll-env
     (delete nil
             (mapcar #'(lambda (init-form)
                         (when (and init-form (listp init-form))
                           (apply #'parse (second init-form)
                                  :environment ll-env
                                  keys)))
                     ll-default-forms))
     ))
  )

;;;; end of file -- parse-lambda-list.lisp
