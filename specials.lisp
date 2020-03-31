;;;; specials.lisp

(uiop:define-package #:streams/specials
  (:use #:cl)
  (:export #:*self*
           #:*mx-universe*
           #:*atom-counter*
           #:*sub-atom-counter*
           #:*metadata-counter*
           #:*base-namespace-list*
           #:*sub-namespace-list*
           #:*meta-namespace-list*
           #:*namespace-list*
           #:*key-indicators*
           #:*max-file-size*
           #:*base-directory*
           #:*default-log-file*))

(in-package #:streams/specials)

(defvar *self*
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

(defvar *base-namespace-list*
  '(("c" . "canon")
    ("m" . "machine")
    ("w" . "world")
    ("s" . "stream")
    ("v" . "view")
    ("@" . "atom"))
  "The list of base namespaces.")

(defvar *sub-namespace-list*
  '(("d" . "datatype")
    ("f" . "format"))
  "The list of sub namespaces.")

(defvar *meta-namespace-list*
  '(":" . "metadata")
  "The list of metadata namespaces")

(defvar *namespace-list*
  (append *base-namespace-list* *sub-namespace-list*)
  "The full list of namespaces, where the individual elements contain the
namespace alias and full namespace name")

(defvar *key-indicators*
  '("=" "/" "[]")
  "The list of strings used for setting end values.")

(defvar *max-file-size*
  1572864
  "The maximum filesize of logging files in bytes.")

(defvar *base-directory*
  (marie:home (marie:cat *self* "/"))
  "The path to the default configuration and storage directory.")

(defvar *default-log-file*
  (flet ((fn (parent path)
           (uiop:merge-pathnames* parent path)))
    (fn *base-directory* (fn *self* ".log")))
  "The path to the default file for logging.")
