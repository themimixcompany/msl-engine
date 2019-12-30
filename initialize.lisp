;;;; initialize.lisp

(uiop:define-package #:streams/initialize
    (:use #:cl)
  (:export #:initialize-universe))

(defun initialize-universe ()
  "Initialize the universe."
  (setf streams/globals:*universe* (streams/core:make-universe)))

(initialize-universe)
