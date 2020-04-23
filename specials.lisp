;;;; specials.lisp

(uiop:define-package #:streams/specials
  (:use #:cl
        #:marie))

(in-package #:streams/specials)

(defvar* (+self+ t)
  "streams"
  "The base name of the system.")

(defvar* (*universe* t) nil
  "The top-level structure for everything.")

(defvar* (*atom-counter* t) 100
  "The initial mx-atom counter value.")

(defvar* (*sub-atom-counter* t) 1000
  "The initial mx-sub-atom counter value.")

(defvar* (*metadata-counter* t) 10000
  "The initial metadata counter value.")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-constant* (+base-namespace-list+ t)
      '(("c" . "canon")
        ("m" . "machine")
        ("w" . "world")
        ("s" . "stream")
        ("v" . "view")
        ("@" . "atom"))
    "The list of base namespaces.")

  (define-constant* (+sub-namespace-list+ t)
      '(("d" . "datatype")
        ("f" . "format"))
    "The list of sub namespaces.")

  (define-constant* (+namespace-list+ t)
      (append +base-namespace-list+ +sub-namespace-list+)
    "The full list of namespaces, where the individual elements contain the
namespace alias and full namespace name"))

(define-constant* (+key-indicators+ t)
    '("=" "/" "[]")
  "The list of strings used for setting end values.")

(defvar* (*log-directory* t)
    (home (cat #\. +self+ #\/))
  "The path to the default configuration and storage directory.")

(define-constant* (+log-file-suffix+ t)
  "msl"
  "The default file suffix for log files.")

(define-constant* (+iso-8601-re+ t)
  "\\d{4}-\\d\\d-\\d\\dT\\d\\d:\\d\\d:\\d\\d(\\.\\d+)?(([+-]\\d\\d:\\d\\d)|Z)?"
  "The regular expression for ISO 8601 dates.")

(define-constant* (+day-names+ t)
    '("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday")
  "The enumeration of week day names.")

(defparameter* (*maximum-log-size* t)
    5242880
    "The maximum filesize of logging files in bytes.")

(defvar* (*machine* t)
  "my-machine"
  "The default name to use as the machine name.")
