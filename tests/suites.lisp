;;;; -*- Mode: Lisp -*-

;;;; suites.lisp --

;;;; See file COPYING in main folder for licensing and copyright information.

(in-package :clast-tests)

(def-suite :parse)

(def-suite :parse-defclass :in :parse)
(def-suite :parse-defs :in :parse)
(def-suite :parse-defstruct :in :parse)
(def-suite :parse-loop :in :parse)
(def-suite :parse-base :in :parse)

;;;; end of file -- suites.lisp --
