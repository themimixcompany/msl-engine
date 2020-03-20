;;;; specials.lisp

(uiop:define-package #:streams/specials
  (:use #:cl)
  (:export #:*mx-universe*

           #:*atom-counter*
           #:*sub-atom-counter*
           #:*metadata-counter*

           #:*base-namespace-list*
           #:*sub-namespace-list*
           #:*namespace-list*

           #:*base-namespace-aliases*
           #:*sub-namespace-aliases*
           #:*namespace-aliases*))

(in-package #:streams/specials)

(defvar *mx-universe* nil
  "The top-level structure for everything.")

(defvar *atom-counter* 100
  "The initial mx-atom counter value.")

(defvar *sub-atom-counter* 1000
  "The initial mx-sub-atom counter value.")

(defvar *metadata-counter* 10000
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
  "The list of sub namespaces, where the individual elements contain the
namespace alias, full namespace name, and the rank. Table instances will be
stored on MX-SUB-ATOM.")

(defvar *namespace-list*
  (append *base-namespace-list* *sub-namespace-list*)
  "The full list of namespaces, where the individual elements contain the
namespace alias, full namespace name, and the rank.")

(defvar *base-namespace-aliases*
  (mapcar #'first *base-namespace-list*)
  "The list of base namespaces in simple form.")
(defvar *sub-namespace-aliases*
  (mapcar #'first *sub-namespace-list*)
  "The list of sub namespaces in simple form.")
(defvar *namespace-aliases*
  (mapcar #'first *namespace-list*)
  "The list of namespaces in simple form.")
