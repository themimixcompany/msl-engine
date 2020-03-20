;;;; specials.lisp

(uiop:define-package #:streams/specials
  (:use #:cl)
  (:export #:*mx-universe*

           #:*atom-counter*
           #:*datatype-counter*
           #:*format-counter*
           #:*metadata-counter*

           #:*namespace-list*
           #:*namespace-aliases*

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
  "The initial metadata counter value.")

(defvar *base-namespace-list*
  '(("c" "canon" 0)
    ("m" "machine" 1)
    ("w" "world" 2)
    ("s" "stream" 3)
    ("v" "view" 4)
    ("@" "atom" 5))
  "The list of base namespaces, where the individual elements contain the
namespace alias, full namespace name, and the rank. Table instances will be
stored on MX-ATOM.")

(defvar *sub-namespace-list*
  '(("d" "datatype" 9)
    ("f" "format" 9))
  "The list of sub atomic namespaces, where the individual elements contain the
namespace alias, full namespace name, and the rank. Table instances will be
stored on MX-SUB-ATOM.")

(defvar *namespace-list*
  (append *base-namespace-list* *sub-namespace-list*)
  "The full list of namespaces, where the individual elements contain the
namespace alias, full namespace name, and the rank.")

(defvar *namespace-aliases*
  (mapcar #'first *namespace-list*)
  "The list of namespaces in simple form.")

(defvar *base-keys*
  '("=" "/")
  "The common keys used in atom tables.")

(defvar *atom-keys*
  (append *base-keys* '("[]" "f" "d" "#"))
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
