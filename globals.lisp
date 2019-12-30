;;;; globals.lisp

(uiop:define-package #:streams/globals
    (:use #:cl)
  (:export #:*initial-ucounter*
           #:*universe*))

(in-package #:streams/globals)

(defparameter *universe* nil
  "The top-level structure for the universe.")

(defparameter *initial-ucounter* 1000
  "The initial unit counter value.")
