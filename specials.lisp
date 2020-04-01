;;;; specials.lisp

(uiop:define-package #:streams/specials
  (:use #:cl)
  (:export #:+self+
           #:*mx-universe*
           #:*atom-counter*
           #:*sub-atom-counter*
           #:*metadata-counter*
           #:+base-namespace-list+
           #:+sub-namespace-list+
           #:+namespace-list+
           #:+key-indicators+))

(in-package #:streams/specials)

(defvar +self+
  "streams"
  "The base name of the system.")

(defvar *mx-universe* nil
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
