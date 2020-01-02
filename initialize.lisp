;;;; initialize.lisp

(uiop:define-package #:streams/initialize
    (:use #:cl)
  (:nicknames #:s/initialize)
  (:export #:initialize-universe))

(defun initialize-universe ()
  "Initialize the universe."
  (setf streams/globals:*universe* (streams/channels:make-universe)))

(initialize-universe)
