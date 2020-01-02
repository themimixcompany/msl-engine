;;;; globals.lisp

(uiop:define-package #:streams/globals
    (:use #:cl)
  (:nicknames #:s/globals)
  (:export #:*initial-acounter*
           #:*universe*))

(in-package #:streams/globals)

(defparameter *universe* nil
  "The top-level structure for the universe.")

(defparameter *initial-acounter* 1000
  "The initial atxm counter value.")
