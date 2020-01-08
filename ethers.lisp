;;;; ethers.lisp

(uiop:define-package #:streams/ethers
    (:use #:cl)
  (:nicknames #:s/ethers)
  (:export #:*initial-mcounter*
           #:*initial-wcounter*
           #:*initial-scounter*
           #:*initial-vcounter*
           #:*initial-ccounter*
           #:*initial-acounter*
           #:*mx-universe*
           #:*mx-machine*
           #:*mx-world*
           #:*mx-stream*
           #:*mx-view*
           #:*mx-canon*
           #:*context*
           #:*mx-base*))

(in-package #:streams/ethers)

(defvar *initial-mcounter* 10000000
  "The initial mx-machine counter value.")

(defvar *initial-wcounter* 1000000
  "The initial mx-world counter value.")

(defvar *initial-scounter* 100000
  "The initial mx-stream counter value.")

(defvar *initial-vcounter* 10000
  "The initial mx-view counter value.")

(defvar *initial-ccounter* 1000
  "The initial mx-canon counter value.")

(defvar *initial-acounter* 100
  "The initial mx-atom counter value.")

(defvar *mx-universe* nil
  "The top-level structure for the mx-universe.")

(defparameter *mx-machine* nil
  "The current machine context, also the fallback context for mx-atom operations.")

(defparameter *mx-world* nil
  "The current world context.")

(defparameter *mx-stream* nil
  "The current stream context.")

(defparameter *mx-view* nil
  "The current view context.")

(defparameter *mx-canon* nil
  "The current canon context.")

(defparameter *context* nil
  "The immediate surrounding context for any given operation.")
