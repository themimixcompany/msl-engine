;;;; ethers.lisp

(uiop:define-package #:streams/ethers
    (:use #:cl)
  (:nicknames #:s/ethers)
  (:export #:*initial-machine-counter*
           #:*initial-world-counter*
           #:*initial-stream-counter*
           #:*initial-view-counter*
           #:*initial-canon-counter*
           #:*initial-atom-counter*
           #:*mx-universe*
           #:*namespaces*
           #:*namespaces-names*))

(in-package #:streams/ethers)

(defvar *initial-machine-counter* 10000000
  "The initial mx-machine counter value.")

(defvar *initial-world-counter* 1000000
  "The initial mx-world counter value.")

(defvar *initial-stream-counter* 100000
  "The initial mx-stream counter value.")

(defvar *initial-view-counter* 10000
  "The initial mx-view counter value.")

(defvar *initial-canon-counter* 1000
  "The initial mx-canon counter value.")

(defvar *initial-atom-counter* 100
  "The initial mx-atom counter value.")

(defvar *mx-universe* nil
  "The top-level structure for the mx-universe.")

(defparameter *namespaces-names*
  '(("c" . "canon")
    ("m" . "machine")
    ("w" . "world")
    ("s" . "stream")
    ("v" . "view")
    ("@" . "atom")
    ("d" . "data-type")
    ("f" . "format"))
  "The assocation list of namespaces, where the CAR is the alias and the CDR is
the full name.")

(defparameter *namespaces*
  (mapcar #'(lambda (name)
              (read-from-string (string (car name))))
          *namespaces-names*)
  "The list of namespaces in simple form.")
