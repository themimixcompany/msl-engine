;;;; globals.lisp

(uiop:define-package #:streams/globals
    (:use #:cl)
  (:nicknames #:s/globals)
  (:export #:*initial-acounter*
           #:*mx-universe*
           #:*mx-machine*
           #:*mx-world*
           #:*mx-stream*
           #:*mx-view*
           #:*mx-canon*
           #:*context*
           #:*base-context*))

(in-package #:streams/globals)

(defparameter *initial-acounter* 1000
  "The initial mx-atom counter value.")

(defparameter *mx-universe* nil
  "The top-level structure for the mx-universe.")

(defparameter *mx-machine* nil
  "The current machine context.")

(defparameter *mx-world* nil
  "The current world context.")

(defparameter *mx-stream* nil
  "The current stream context.")

(defparameter *mx-view* nil
  "The current view context.")

(defparameter *mx-canon* nil
  "The current canon context.")

;;; Note: enable support for context chaining
(defparameter *context* nil
  "The immediate surrounding context for any given operation. Defaults to MX-BASE.")

(defparameter *base-context* nil
  "The fallback context for @ operations.")
