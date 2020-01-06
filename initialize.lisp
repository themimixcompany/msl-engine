;;;; initialize.lisp

(uiop:define-package #:streams/initialize
    (:use #:cl)
  (:nicknames #:s/initialize)
  (:export #:initialize-mx-universe))

(defun initialize-mx-universe ()
  "Initialize the mx-universe."
  (setf streams/globals:*mx-universe* (streams/channels:make-mx-universe)))

(defun initialize-base-context ()
  "Initialize the baseline context."
  (setf streams/globals:*mx-base* (streams/channels:make-mx-base)))

(initialize-mx-universe)
(initialize-base-context)
