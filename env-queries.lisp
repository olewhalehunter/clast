;;;; -*- Mode: Lisp -*-

;;;; env-queries.lisp --

;;;; See file COPYING in main folder for licensing and copyright information.

;;;; Environment querying functions.
;;;; Either you have CLtL2 environment functions or your
;;;; implementation sucks.

(in-package "CLAST")


(defun special-variable-p (v env)
  (declare (type symbol v))
  (eq :special (variable-information v env)))


(defun constant-or-keyword-p (s env)
  (declare (type symbol s))
  (eq :constant (variable-information s env)))


(defun symbol-macro-p (s env)
  (declare (type symbol s))
  (eq :symbol-macro (variable-information s env)))


;;;; end of file -- env-queries.lisp --
