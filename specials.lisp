;;;; specials.lisp

(uiop:define-package #:streams/specials
  (:use #:cl)
  (:export #:*mx-universe*

           #:*atom-counter*
           #:*datatype-counter*
           #:*format-counter*
           #:*metadata-counter*

           #:*namespaces-names*
           #:*namespaces*

           #:*base-keys*
           #:*atom-keys*
           #:*metadata-keys*
           #:*datatype-keys*
           #:*format-keys*))

(in-package #:streams/specials)

(defvar *mx-universe* nil
  "The top-level structure for everything.")

(defvar *atom-counter* 100
  "The initial mx-atom counter value.")

(defvar *datatype-counter* 1000
  "The initial mx-datatype counter value.")

(defvar *format-counter* 10000
  "The initial mx-format counter value.")

(defvar *metadata-counter* 100000
  "The initial mx-metadata counter value.")

(defvar *namespaces-names*
  '(("c" . "canon")
    ("m" . "machine")
    ("w" . "world")
    ("s" . "stream")
    ("v" . "view")
    ("@" . "atom")
    ("d" . "datatype")
    ("f" . "format"))
  "The assocation list of namespaces, where the CAR is the alias and the CDR is
the full name.")

(defvar *namespaces*
  (mapcar #'(lambda (name)
              (read-from-string (string (car name))))
          *namespaces-names*)
  "The list of namespaces in simple form.")

(defvar *base-keys*
  '("=" "/")
  "The common keys used in atom tables.")

(defvar *atom-keys*
  (append *base-keys* '(":" "[]" "f" "d" "#"))
  "The keys used for atom data.")

(defvar *datatype-keys*
  (append *base-keys* '(":"))
  "The keys used for atom datatypes.")

(defvar *format-keys*
  (append *base-keys* '(":" "f" "d"))
  "The keys used for atom formats.")

(defvar *metadata-keys*
  (append *base-keys* '("f" "d"))
  "The keys used for atom metadata.")
