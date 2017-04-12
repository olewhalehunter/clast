;;;; -*- Mode: Lisp -*-

;;;; clast-missing-cltl2-envs.lisp.lisp --
;;;; Code lifted from IT.BESE.ARNESI and CL-WALKER.
;;;;
;;;; Environment functions.
;;;; Either you have CLtL2 environment functions or your
;;;; implementation sucks.
;;;;
;;;; See file COPYING for copyright and license information.


(in-package "CLAST")

;;;; The Magnificent (yet, in this specific case, neglected) 7.
;;;; CLtL2 environment manipulation manipulation functions.

(eval-when (:load-toplevel :compile-toplevel :execute)
  (warn "~3%CLAST needs the CLtL2 Environment introspection functions.
         ~A ~A does not seem to support them.
         You are advised to ask the implementors to provide them.~3%"
        (lisp-implementation-type)
        (lisp-implementation-version)))
        

;;;; end of file -- clast-missing-cltl2-envs.lisp --
