;;;; specials.lisp

(uiop:define-package #:streams/specials
  (:use #:cl)
  (:export #:+self+
           #:*universe*
           #:*atom-counter*
           #:*sub-atom-counter*
           #:*metadata-counter*

           #:+base-namespace-list+
           #:+sub-namespace-list+
           #:+namespace-list+
           #:+key-indicators+

           #:*log-directory*
           #:+log-file-suffix+
           #:+iso-8601-re+
           #:+day-names+
           #:*maximum-log-size*
           #:*machine*))

(in-package #:streams/specials)

(defvar +self+
  "streams"
  "The base name of the system.")

(defvar *universe* nil
  "The top-level structure for everything.")

(defvar *atom-counter* 100
  "The initial mx-atom counter value.")

(defvar *sub-atom-counter* 1000
  "The initial mx-sub-atom counter value.")

(defvar *metadata-counter* 10000
  "The initial metadata counter value.")

(marie:define-constant +base-namespace-list+
  '(("c" . "canon")
    ("m" . "machine")
    ("w" . "world")
    ("s" . "stream")
    ("v" . "view")
    ("@" . "atom"))
  "The list of base namespaces.")

(marie:define-constant +sub-namespace-list+
  '(("d" . "datatype")
    ("f" . "format"))
  "The list of sub namespaces.")

(marie:define-constant +namespace-list+
  (append +base-namespace-list+ +sub-namespace-list+)
  "The full list of namespaces, where the individual elements contain the
namespace alias and full namespace name")

(marie:define-constant +key-indicators+
  '("=" "/" "[]")
  "The list of strings used for setting end values.")

(defvar *log-directory*
    (marie:home (marie:cat #\. +self+ #\/))
  "The path to the default configuration and storage directory.")

(marie:define-constant* +log-file-suffix+
  "msl"
  "The default file suffix for log files.")

(marie:define-constant* +iso-8601-re+
  "\\d{4}-\\d\\d-\\d\\dT\\d\\d:\\d\\d:\\d\\d(\\.\\d+)?(([+-]\\d\\d:\\d\\d)|Z)?"
  "The regular expression for ISO 8601 dates.")

(marie:define-constant* +day-names+
    '("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday")
  "The enumeration of week day names.")

(defparameter *maximum-log-size*
  5242880
  "The maximum filesize of logging files in bytes.")

(defvar *machine*
  "my-machine"
  "The default name to use as the machine name.")
